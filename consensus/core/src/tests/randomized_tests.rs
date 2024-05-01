// Copyright (c) Mysten Labs, Inc.
// SPDX-License-Identifier: Apache-2.0

use std::sync::Arc;

use parking_lot::RwLock;
use rand::{rngs::StdRng, Rng, SeedableRng};

use crate::{
    block::{BlockAPI, Slot},
    commit::{LeaderStatus, DEFAULT_WAVE_LENGTH},
    context::Context,
    dag_state::DagState,
    leader_schedule::{LeaderSchedule, LeaderSwapTable},
    storage::mem_store::MemStore,
    test_dag::create_random_dag,
    universal_committer::universal_committer_builder::UniversalCommitterBuilder,
};

// Test builds a randomized dag with the following conditions:
// - Links to 2f+1 minimum ancestors
// - Links to leader of previous round.
#[test]
fn test_randomized_dag_direct_commit() {
    telemetry_subscribers::init_for_testing();
    let mut rng = StdRng::from_entropy();
    // TODO: use seed from env var
    let seed = rng.gen_range(0..10000);
    tracing::warn!("Using Random Seed: {seed}");

    let mut seeded_rng = StdRng::seed_from_u64(seed);
    let num_authorities = seeded_rng.gen_range(1..10);

    let context = Arc::new(Context::new_for_test(num_authorities).0);
    let leader_schedule = Arc::new(LeaderSchedule::new(
        context.clone(),
        LeaderSwapTable::default(),
    ));
    let dag_state = Arc::new(RwLock::new(DagState::new(
        context.clone(),
        Arc::new(MemStore::new()),
    )));

    // Create committer with pipelining and only 1 leader per leader round
    let committer =
        UniversalCommitterBuilder::new(context.clone(), leader_schedule, dag_state.clone())
            .with_pipeline(true)
            .build();

    let num_waves = seeded_rng.gen_range(0..100);
    let pipeline = num_waves % DEFAULT_WAVE_LENGTH as usize;
    let wave_number = committer.committers[pipeline].wave_number(num_waves as u32);
    let num_rounds = committer.committers[pipeline].decision_round(wave_number);
    let dag_builder = create_random_dag(seed, num_rounds, context.clone());

    dag_builder.persist_all_blocks(dag_state.clone());

    // dag_builder.print();

    tracing::warn!("Running test with committee size {num_authorities} & {num_waves} completed waves in the DAG...");

    let last_decided = Slot::new_for_test(0, 0);
    let sequence = committer.try_commit(last_decided);
    tracing::info!("Commit sequence: {sequence:#?}");

    assert_eq!(sequence.len(), num_waves);
    for (i, leader_block) in sequence.iter().enumerate() {
        // First sequenced leader should be in round 1.
        let leader_round = i as u32 + 1;
        if let LeaderStatus::Commit(ref block) = leader_block {
            assert_eq!(block.round(), leader_round);
            assert_eq!(block.author(), committer.get_leaders(leader_round)[0]);
        } else {
            panic!("Expected a committed leader")
        };
    }
}
