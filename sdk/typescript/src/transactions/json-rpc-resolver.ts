// Copyright (c) Mysten Labs, Inc.
// SPDX-License-Identifier: Apache-2.0

import { parse } from 'valibot';

import type { BcsType } from '../bcs/index.js';
import { bcs } from '../bcs/index.js';
import type { SuiClient } from '../client/client.js';
import type { ProtocolConfig } from '../client/index.js';
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

const LIMITS = {
	// The maximum gas that is allowed.
	maxTxGas: 'max_tx_gas',
	// The maximum number of gas objects that can be selected for one transaction.
	maxGasObjects: 'max_gas_payment_objects',
	// The maximum size (in bytes) that the transaction can be:
	maxTxSizeBytes: 'max_tx_size_bytes',
	// The maximum size (in bytes) that pure arguments can be:
	maxPureArgumentSize: 'max_pure_argument_size',
} as const;

type Limits = Partial<Record<keyof typeof LIMITS, number>>;

const DefaultOfflineLimits = {
	maxPureArgumentSize: 16 * 1024,
	maxTxGas: 50_000_000_000,
	maxGasObjects: 256,
	maxTxSizeBytes: 128 * 1024,
} satisfies Limits;

export interface BuildTransactionBlockOptions {
	client?: SuiClient;
	onlyTransactionKind?: boolean;
	/** Define a protocol config to build against, instead of having it fetched from the provider at build time. */
	protocolConfig?: ProtocolConfig;
	/** Define limits that are used when building the transaction. In general, we recommend using the protocol configuration instead of defining limits. */
	limits?: Limits;
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
	await validate(blockData, options);
	return next();
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
			maxSizeBytes: getLimit(options, 'maxTxSizeBytes'),
			overrides: {
				gasData: {
					budget: String(getLimit(options, 'maxTxGas')),
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
	const maxGasObjects = getLimit(options, 'maxGasObjects');
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
			.slice(0, maxGasObjects - 1)
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

	if (blockData.gasConfig.payment.length > maxGasObjects) {
		throw new Error(`Payment objects exceed maximum amount: ${maxGasObjects}`);
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

	if (objectsToResolve.length) {
		const dedupedIds = [
			...new Set(objectsToResolve.map((input) => normalizeSuiObjectId(input.UnresolvedObject.id))),
		];

		const objectChunks = chunk(dedupedIds, MAX_OBJECTS_PER_FETCH);
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

			const ownerAddress =
				owner && typeof owner === 'object'
					? 'AddressOwner' in owner
						? owner.AddressOwner
						: 'ObjectOwner' in owner
						? owner.ObjectOwner
						: null
					: null;

			return {
				objectId: object.data.objectId,
				digest: object.data.digest,
				version: object.data.version,
				owner: ownerAddress,
				initialSharedVersion,
			};
		});

		const objectsById = new Map(
			dedupedIds.map((id, index) => {
				return [id, objects[index]];
			}),
		);

		objectsToResolve.forEach((input) => {
			let updated: CallArg | undefined;
			const id = normalizeSuiAddress(input.UnresolvedObject.id);
			const object = objectsById.get(id)!;

			if (object.initialSharedVersion) {
				updated = Inputs.SharedObjectRef({
					objectId: id,
					initialSharedVersion: object.initialSharedVersion,
					mutable: !!input.UnresolvedObject.mutable,
				});
			} else if (input.UnresolvedObject.receiving) {
				updated = Inputs.ReceivingRef(
					{
						objectId: id,
						digest: object.digest,
						version: object.version,
					}!,
				);
			}

			blockData.inputs[blockData.inputs.indexOf(input)] =
				updated ??
				Inputs.ObjectRef({
					objectId: id,
					digest: object.digest,
					version: object.version,
				});
		});
	}
}

async function normalizeInputs(
	blockData: TransactionBlockDataBuilder,
	options: BuildTransactionBlockOptions,
) {
	const { inputs, transactions } = blockData;
	const moveModulesToResolve: Extract<Transaction, { MoveCall: unknown }>['MoveCall'][] = [];

	transactions.forEach((transaction) => {
		// Special case move call:
		if (transaction.MoveCall) {
			// Determine if any of the arguments require encoding.
			// - If they don't, then this is good to go.
			// - If they do, then we need to fetch the normalized move module.

			const inputs = transaction.MoveCall.arguments.map((arg) => {
				if (arg.$kind === 'Input') {
					return blockData.inputs[arg.Input];
				}
				return null;
			});
			const needsResolution = inputs.some(
				(input) => input && (input.RawValue || input.UnresolvedObject),
			);

			if (needsResolution) {
				moveModulesToResolve.push(transaction.MoveCall);
			}
		}

		// Special handling for values that where previously encoded using the wellKnownEncoding pattern.
		// This should only happen when transaction block data was hydrated from an old version of the SDK
		switch (transaction.$kind) {
			case 'SplitCoins':
				transaction.SplitCoins[1].forEach((amount) => {
					normalizeRawArgument(amount, bcs.U64, blockData);
				});
				break;
			case 'TransferObjects':
				normalizeRawArgument(transaction.TransferObjects[1], bcs.Address, blockData);
				break;
		}
	});

	if (moveModulesToResolve.length) {
		await Promise.all(
			moveModulesToResolve.map(async (moveCall) => {
				const parameters = (
					await getClient(options).getNormalizedMoveFunction(moveCall)
				).parameters.map((param) => normalizedTypeToMoveTypeSignature(param));

				// Entry functions can have a mutable reference to an instance of the TxContext
				// struct defined in the TxContext module as the last parameter. The caller of
				// the function does not need to pass it in as an argument.
				const hasTxContext = parameters.length > 0 && isTxContext(parameters.at(-1)!);
				const params = hasTxContext ? parameters.slice(0, parameters.length - 1) : parameters;

				if (params.length !== moveCall.arguments.length) {
					throw new Error('Incorrect number of arguments.');
				}

				params.forEach((param, i) => {
					const arg = moveCall.arguments[i];
					if (arg.$kind !== 'Input') return;
					const input = inputs[arg.Input];

					// Skip if the input is already resolved
					if (!input.RawValue && !input.UnresolvedObject) {
						return;
					}

					const inputValue = input.RawValue?.value ?? input.UnresolvedObject?.id!;

					const schema = getPureBcsSchema(param.body);
					if (schema) {
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

					const unresolvedObject: typeof input = input.RawValue
						? {
								$kind: 'UnresolvedObject',
								UnresolvedObject: {
									id: inputValue,
								},
						  }
						: input;

					inputs[arg.Input] = unresolvedObject;

					if (param.ref === '&mut') {
						unresolvedObject.UnresolvedObject.mutable = true;
					}

					if (isReceivingType(param)) {
						unresolvedObject.UnresolvedObject.receiving = true;
					}
				});
			}),
		);
	}
}

async function validate(
	blockData: TransactionBlockDataBuilder,
	options: BuildTransactionBlockOptions,
) {
	blockData.inputs.forEach((input, index) => {
		if (input.Pure) {
			const maxPureArgumentSize = getLimit(options, 'maxPureArgumentSize');
			if (input.Pure.length > maxPureArgumentSize) {
				throw new Error(
					`Input at index ${index} is too large, max pure input size is ${maxPureArgumentSize} bytes, got ${input.Pure.length} bytes`,
				);
			}
		}

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

	if (input.$kind !== 'RawValue') {
		return;
	}

	blockData.inputs[arg.Input] = Inputs.Pure(schema.serialize(input.RawValue.value));
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

export function getLimit(options: BuildTransactionBlockOptions, key: keyof typeof LIMITS): number {
	// Use the limits definition if that exists:
	if (options.limits && typeof options.limits[key] === 'number') {
		return options.limits[key]!;
	}

	if (!options.protocolConfig) {
		return DefaultOfflineLimits[key];
	}

	// Fallback to protocol config:
	const attribute = options.protocolConfig?.attributes[LIMITS[key]];
	if (!attribute) {
		throw new Error(`Missing expected protocol config: "${LIMITS[key]}"`);
	}

	const value =
		'u64' in attribute
			? attribute.u64
			: 'u32' in attribute
			? attribute.u32
			: 'u16' in attribute
			? attribute.u16
			: attribute.f64;

	if (!value) {
		throw new Error(`Unexpected protocol config value found for: "${LIMITS[key]}"`);
	}

	// NOTE: Technically this is not a safe conversion, but we know all of the values in protocol config are safe
	return Number(value);
}