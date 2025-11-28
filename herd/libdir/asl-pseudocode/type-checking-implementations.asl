/*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 */

/*
    type-checking-implementations.asl
    ---------------------------------

This file is a list of function signatures, inferred from usage in
shared_pseudocode.asl. No functional behaviour should be expected from this
file, as all entries are the minimum requirement to satisfy type signatures.

*/

readonly func IsFeatureImplemented(f: Feature) => boolean
begin
  return FALSE;
end;

readonly func ImpDefBool(s: string) => boolean
begin
  return ARBITRARY: boolean;
end;

readonly func ImpDefInt(s: string) => integer
begin
  return ARBITRARY: integer;
end;

readonly func ImpDefBits{N}(s: string) => bits(N)
begin
  return ARBITRARY: bits(N);
end;

readonly func ImpDefBit(s: string) => bits(1)
begin
  return ImpDefBits{1}(s);
end;

readonly func ImpDefMemoryAttributes(s: string) => MemoryAttributes
begin
  return ARBITRARY: MemoryAttributes;
end;

type IMPLEMENTATION_DEFINED of exception {-};

func ImpDef(s: string)
begin
  throw IMPLEMENTATION_DEFINED {-};
end;

type UNIMPLEMENTED of exception {-};

noreturn func Unimplemented()
begin
  throw UNIMPLEMENTED {-};
end;

noreturn func ReservedEncoding()
begin
  Unimplemented();
end;

func See(s: string)
begin
  print "See", s;
end;

constant NUM_PMU_COUNTERS = 123456789;

type TRBSRType of TRBSR_EL1_Type;
type PMBSRType of PMBSR_EL1_Type;
type S1PIRType of PIR_EL1_Type;
type S2PIRType of PIR_EL2_Type;
type MAIRType of bits(64);
type S1PORType of POR_EL1_Type;
type SCTLRType of SCTLR_EL1_Type;
type ESRType of ESR_EL1_Type;

// No definition provided, taking inferred type from usage in
// shared_pseudocode.asl
// Positions are complitely made up by me, Hadrien Renaud, and do not reflect
// anything real.
var EDPCSRhi: bits(32) {
  [26:3] PC,
  [2:1] EL,
  [0] NS,
};
