// Copyright (c) Mysten Labs, Inc.
// SPDX-License-Identifier: Apache-2.0

import { parse } from 'valibot';

import type { BcsType } from '../bcs/index.js';
import { bcs } from '../bcs/index.js';
import type { SuiClient } from '../client/client.js';
import { normalizeSuiAddress, normalizeSuiObjectId, SUI_TYPE_ARG } from '../utils/index.js';
import type { Argument, CallArg, OpenMoveTypeSignature, Transaction } from './blockData/v2.js';
import { ObjectRef } from './blockData/v2.js';
import { Inputs } from './Inputs.js';
import { getPureBcsSchema, isTxContext, normalizedTypeToMoveTypeSignature } from './serializer.js';
import type { TransactionBlockDataBuilder } from './TransactionBlockData.js';

// The maximum objects that can be fetched at once using multiGetObjects.
const MAX_OBJECTS_PER_FETCH = 50;

// An amount of gas (in gas units) that is added to transactions as an overhead to ensure transactions do not fail.
const GAS_SAFE_OVERHEAD = 1000n;
const MAX_GAS = 50_000_000_000;

export interface BuildTransactionBlockOptions {
	client?: SuiClient;
	onlyTransactionKind?: boolean;
}

export interface SerializeTransactionBlockOptions extends BuildTransactionBlockOptions {
	supportedIntents?: string[];
}

export type TransactionBlockPlugin = (
	blockData: TransactionBlockDataBuilder,
	options: BuildTransactionBlockOptions,
	next: () => Promise<void>,
) => Promise<void>;

export async function resolveTransactionBlockData(
	blockData: TransactionBlockDataBuilder,
	options: BuildTransactionBlockOptions,
	next: () => Promise<void>,
) {
	await normalizeInputs(blockData, options);
	await resolveObjectReferences(blockData, options);

	if (!options.onlyTransactionKind) {
		await setGasPrice(blockData, options);
		await setGasBudget(blockData, options);
		await setGasPayment(blockData, options);
	}
	await validate(blockData);
	return await next();
}

async function setGasPrice(
	blockData: TransactionBlockDataBuilder,
	options: BuildTransactionBlockOptions,
) {
	if (!blockData.gasConfig.price) {
		blockData.gasConfig.price = String(await getClient(options).getReferenceGasPrice());
	}
}

async function setGasBudget(
	blockData: TransactionBlockDataBuilder,
	options: BuildTransactionBlockOptions,
) {
	if (blockData.gasConfig.budget) {
		return;
	}

	const dryRunResult = await getClient(options).dryRunTransactionBlock({
		transactionBlock: blockData.build({
			overrides: {
				gasData: {
					budget: String(MAX_GAS),
					payment: [],
				},
			},
		}),
	});

	if (dryRunResult.effects.status.status !== 'success') {
		throw new Error(
			`Dry run failed, could not automatically determine a budget: ${dryRunResult.effects.status.error}`,
			{ cause: dryRunResult },
		);
	}

	const safeOverhead = GAS_SAFE_OVERHEAD * BigInt(blockData.gasConfig.price || 1n);

	const baseComputationCostWithOverhead =
		BigInt(dryRunResult.effects.gasUsed.computationCost) + safeOverhead;

	const gasBudget =
		baseComputationCostWithOverhead +
		BigInt(dryRunResult.effects.gasUsed.storageCost) -
		BigInt(dryRunResult.effects.gasUsed.storageRebate);

	blockData.gasConfig.budget = String(
		gasBudget > baseComputationCostWithOverhead ? gasBudget : baseComputationCostWithOverhead,
	);
}

// The current default is just picking _all_ coins we can which may not be ideal.
async function setGasPayment(
	blockData: TransactionBlockDataBuilder,
	options: BuildTransactionBlockOptions,
) {
	if (!blockData.gasConfig.payment) {
		const coins = await getClient(options).getCoins({
			owner: blockData.gasConfig.owner || blockData.sender!,
			coinType: SUI_TYPE_ARG,
		});

		const paymentCoins = coins.data
			// Filter out coins that are also used as input:
			.filter((coin) => {
				const matchingInput = blockData.inputs.find((input) => {
					if (input.Object?.ImmOrOwnedObject) {
						return coin.coinObjectId === input.Object.ImmOrOwnedObject.objectId;
					}

					return false;
				});

				return !matchingInput;
			})
			.map((coin) => ({
				objectId: coin.coinObjectId,
				digest: coin.digest,
				version: coin.version,
			}));

		if (!paymentCoins.length) {
			throw new Error('No valid gas coins found for the transaction.');
		}

		blockData.gasConfig.payment = paymentCoins.map((payment) => parse(ObjectRef, payment));
	}
}

async function resolveObjectReferences(
	blockData: TransactionBlockDataBuilder,
	options: BuildTransactionBlockOptions,
) {
	// Keep track of the object references that will need to be resolved at the end of the transaction.
	// We keep the input by-reference to avoid needing to re-resolve it:
	const objectsToResolve = blockData.inputs.filter((input) => {
		return (
			(input.UnresolvedObject && !input.UnresolvedObject.version) ||
			input.UnresolvedObject?.initialSharedVersion
		);
	}) as Extract<CallArg, { UnresolvedObject: unknown }>[];

	const dedupedIds = [
		...new Set(
			objectsToResolve.map((input) => normalizeSuiObjectId(input.UnresolvedObject.objectId)),
		),
	];

	const objectChunks = dedupedIds.length ? chunk(dedupedIds, MAX_OBJECTS_PER_FETCH) : [];
	const resolved = (
		await Promise.all(
			objectChunks.map((chunk) =>
				getClient(options).multiGetObjects({
					ids: chunk,
					options: { showOwner: true },
				}),
			),
		)
	).flat();

	const responsesById = new Map(
		dedupedIds.map((id, index) => {
			return [id, resolved[index]];
		}),
	);

	const invalidObjects = Array.from(responsesById)
		.filter(([_, obj]) => obj.error)
		.map(([id, _obj]) => id);

	if (invalidObjects.length) {
		throw new Error(`The following input objects are invalid: ${invalidObjects.join(', ')}`);
	}

	const objects = resolved.map((object) => {
		if (object.error || !object.data) {
			throw new Error(`Failed to fetch object: ${object.error}`);
		}
		const owner = object.data.owner;
		const initialSharedVersion =
			owner && typeof owner === 'object' && 'Shared' in owner
				? owner.Shared.initial_shared_version
				: null;

		return {
			objectId: object.data.objectId,
			digest: object.data.digest,
			version: object.data.version,
			initialSharedVersion,
		};
	});

	const objectsById = new Map(
		dedupedIds.map((id, index) => {
			return [id, objects[index]];
		}),
	);

	for (const input of blockData.inputs) {
		if (!input.UnresolvedObject) {
			continue;
		}

		let updated: CallArg | undefined;
		const id = normalizeSuiAddress(input.UnresolvedObject.objectId);
		const object = objectsById.get(id);

		if (object?.initialSharedVersion) {
			updated = Inputs.SharedObjectRef({
				objectId: id,
				initialSharedVersion: object.initialSharedVersion,
				mutable: !!input.UnresolvedObject.mutable,
			});
		} else if (input.UnresolvedObject.receiving) {
			updated = Inputs.ReceivingRef(
				{
					objectId: id,
					digest: input.UnresolvedObject.digest ?? object?.digest!,
					version: input.UnresolvedObject.version ?? object?.version!,
				}!,
			);
		}

		blockData.inputs[blockData.inputs.indexOf(input)] =
			updated ??
			Inputs.ObjectRef({
				objectId: id,
				digest: input.UnresolvedObject.digest ?? object?.digest!,
				version: input.UnresolvedObject.version ?? object?.version!,
			});
	}
}

async function normalizeInputs(
	blockData: TransactionBlockDataBuilder,
	options: BuildTransactionBlockOptions,
) {
	const { inputs, transactions } = blockData;
	const moveCallsToResolve: Extract<Transaction, { MoveCall: unknown }>['MoveCall'][] = [];
	const moveFunctionsToResolve = new Set<string>();

	transactions.forEach((transaction) => {
		// Special case move call:
		if (transaction.MoveCall) {
			// Determine if any of the arguments require encoding.
			// - If they don't, then this is good to go.
			// - If they do, then we need to fetch the normalized move module.

			// If we already know the argument types, we don't need to resolve them again
			if (transaction.MoveCall.argumentTypes) {
				return;
			}

			const inputs = transaction.MoveCall.arguments.map((arg) => {
				if (arg.$kind === 'Input') {
					return blockData.inputs[arg.Input];
				}
				return null;
			});
			const needsResolution = inputs.some(
				(input) => input?.UnresolvedPure || input?.UnresolvedObject,
			);

			if (needsResolution) {
				const functionName = `${transaction.MoveCall.package}::${transaction.MoveCall.module}::${transaction.MoveCall.function}`;
				moveFunctionsToResolve.add(functionName);
				moveCallsToResolve.push(transaction.MoveCall);
			}
		}

		// Special handling for values that where previously encoded using the wellKnownEncoding pattern.
		// This should only happen when transaction block data was hydrated from an old version of the SDK
		switch (transaction.$kind) {
			case 'SplitCoins':
				transaction.SplitCoins.amounts.forEach((amount) => {
					normalizeRawArgument(amount, bcs.U64, blockData);
				});
				break;
			case 'TransferObjects':
				normalizeRawArgument(transaction.TransferObjects.recipient, bcs.Address, blockData);
				break;
		}
	});

	const moveFunctionParameters = new Map<string, OpenMoveTypeSignature[]>();
	if (moveFunctionsToResolve.size > 0) {
		const client = getClient(options);
		await Promise.all(
			[...moveFunctionsToResolve].map(async (functionName) => {
				const [packageId, moduleId, functionId] = functionName.split('::');
				const def = await client.getNormalizedMoveFunction({
					package: packageId,
					module: moduleId,
					function: functionId,
				});

				moveFunctionParameters.set(
					functionName,
					def.parameters.map((param) => normalizedTypeToMoveTypeSignature(param)),
				);
			}),
		);
	}

	if (moveCallsToResolve.length) {
		await Promise.all(
			moveCallsToResolve.map(async (moveCall) => {
				const parameters = moveFunctionParameters.get(
					`${moveCall.package}::${moveCall.module}::${moveCall.function}`,
				);

				if (!parameters) {
					return;
				}

				// Entry functions can have a mutable reference to an instance of the TxContext
				// struct defined in the TxContext module as the last parameter. The caller of
				// the function does not need to pass it in as an argument.
				const hasTxContext = parameters.length > 0 && isTxContext(parameters.at(-1)!);
				const params = hasTxContext ? parameters.slice(0, parameters.length - 1) : parameters;

				moveCall.argumentTypes = params;
			}),
		);
	}

	transactions.forEach((transaction) => {
		if (!transaction.MoveCall) {
			return;
		}

		const moveCall = transaction.MoveCall;
		const fnName = `${moveCall.package}::${moveCall.module}::${moveCall.function}`;
		const params = moveCall.argumentTypes;

		if (!params) {
			return;
		}

		if (params.length !== transaction.MoveCall.arguments.length) {
			throw new Error(`Incorrect number of arguments for ${fnName}`);
		}

		params.forEach((param, i) => {
			const arg = moveCall.arguments[i];
			if (arg.$kind !== 'Input') return;
			const input = inputs[arg.Input];

			// Skip if the input is already resolved
			if (!input.UnresolvedPure && !input.UnresolvedObject) {
				return;
			}

			const inputValue = input.UnresolvedPure?.value ?? input.UnresolvedObject?.objectId!;

			const schema = getPureBcsSchema(param.body);
			if (schema) {
				arg.type = 'pure';
				inputs[inputs.indexOf(input)] = Inputs.Pure(schema.serialize(inputValue));
				return;
			}

			if (typeof inputValue !== 'string') {
				throw new Error(
					`Expect the argument to be an object id string, got ${JSON.stringify(
						inputValue,
						null,
						2,
					)}`,
				);
			}

			arg.type = 'object';
			const unresolvedObject: typeof input = input.UnresolvedPure
				? {
						$kind: 'UnresolvedObject',
						UnresolvedObject: {
							objectId: inputValue,
						},
				  }
				: input;

			inputs[arg.Input] = unresolvedObject;

			if (param.ref === '&mut' || !param.ref) {
				unresolvedObject.UnresolvedObject.mutable = true;
			}

			if (isReceivingType(param)) {
				unresolvedObject.UnresolvedObject.receiving = true;
			}
		});
	});
}

function validate(blockData: TransactionBlockDataBuilder) {
	blockData.inputs.forEach((input, index) => {
		if (input.$kind !== 'Object' && input.$kind !== 'Pure') {
			throw new Error(
				`Input at index ${index} has not been resolved.  Expected a Pure or Object input, but found ${JSON.stringify(
					input,
				)}`,
			);
		}
	});
}

function normalizeRawArgument(
	arg: Argument,
	schema: BcsType<any>,
	blockData: TransactionBlockDataBuilder,
) {
	if (arg.$kind !== 'Input') {
		return;
	}
	const input = blockData.inputs[arg.Input];

	if (input.$kind !== 'UnresolvedPure') {
		return;
	}

	blockData.inputs[arg.Input] = Inputs.Pure(schema.serialize(input.UnresolvedPure.value));
}

function isReceivingType(type: OpenMoveTypeSignature): boolean {
	if (typeof type.body !== 'object' || !('datatype' in type.body)) {
		return false;
	}

	return (
		type.body.datatype.package === '0x2' &&
		type.body.datatype.module === 'transfer' &&
		type.body.datatype.type === 'Receiving'
	);
}

export function getClient(options: BuildTransactionBlockOptions): SuiClient {
	if (!options.client) {
		throw new Error(
			`No provider passed to Transaction#build, but transaction data was not sufficient to build offline.`,
		);
	}

	return options.client;
}

function chunk<T>(arr: T[], size: number): T[][] {
	return Array.from({ length: Math.ceil(arr.length / size) }, (_, i) =>
		arr.slice(i * size, i * size + size),
	);
}
