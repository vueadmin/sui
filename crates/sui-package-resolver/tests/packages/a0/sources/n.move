// Copyright (c) Mysten Labs, Inc.
// SPDX-License-Identifier: Apache-2.0

#[allow(unused_field)]
module a::n {
    public struct T0 {
        t: a::m::T1<u16, u32>,
        u: a::m::T2,
    }
}
