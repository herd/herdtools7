/*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 */

/*

    patches-std.asl
    ---------------

This file is a list of re-implementations of ASL functions from the ARM
Reference Manual. They are completely re-written or simply edited by hand. When
re-written completely, this is often time the minimal code that type-checks.
The code is also translated from ASLv0 to ASLv1 by hand.

These declarations are meant to be included only if the variant `vmsa` is NOT
activated, i.e. if we do not run stage 1 translation.

The ARM Reference Manual is available here:
    https://developer.arm.com/documentation/ddi0602/2023-09/

*/

// AArch64.TranslateAddress()
// ==========================
// Main entry point for translating an address

// From https://developer.arm.com/documentation/ddi0602/2023-09/Shared-Pseudocode/aarch64-translation-vmsa-translation?lang=en#AArch64.TranslateAddress.4
// We disable address translation

func AArch64_TranslateAddress(address:bits(64), accdesc:AccessDescriptor, aligned:boolean, size:integer) => AddressDescriptor
begin
  var full_addr : FullAddress;
  return CreateAddressDescriptor(address, full_addr, NormalWBISHMemAttr, accdesc);
end;

