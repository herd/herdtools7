/*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 */

/*

    patches-vmsa.asl
    ----------------

This file is a list of re-implementations of ASL functions from the ARM
Reference Manual. They are completely re-written or simply edited by hand. When
re-written completely, this is often time the minimal code that type-checks.
The code is also translated from ASLv0 to ASLv1 by hand.

The ARM Reference Manual is available here:
    https://developer.arm.com/documentation/ddi0602/2023-09/

This file is complementary to the file patches.asl, to be included with it when
Stage 1 Translation is activated.

*/

// =============================================================================

// AArch64.S1Enabled()
// ===================
// Determine if stage 1 is enabled for the access type for this translation regime
// Stage 1 is the only translation regime implemented

// Optimisation: not overiding it costs ~10% loss of performance

func AArch64_S1Enabled(regime : Regime,acctype : AccessType) => boolean
begin
  return TRUE;
end;

// =============================================================================

// AArch64.DecodeDescriptorType()
// ==============================
// Determine whether the descriptor is a page, block or table
// Simplify: invalid or Leaf

func AArch64_DecodeDescriptorType {N}
  (descriptor:bits(N), d128:bit, ds:bit, tgx:TGx, level:integer)
    =>
  DescriptorType
begin
  if descriptor[0] == '0' then
    return DescriptorType_Invalid;
  else
    return DescriptorType_Leaf;
  end;
end;

// =============================================================================

// AArch64.S1SLTTEntryAddress()
// ============================
// Compute the first stage 1 translation table descriptor address within the
// table pointed to by the base at the start level

// We do this by using a herd prmitive that work on herd's symbolic addresses.

func AArch64_S1SLTTEntryAddress(level:integer, walparams:S1TTWParams,
                                ia:bits(64), tablebase:FullAddress)
  => FullAddress
begin
    var descaddress : FullAddress;
    descaddress.address = ComputePtePrimitive(ia);
    descaddress.paspace = tablebase.paspace;
    return descaddress;
end;

// =============================================================================

// DecodeShareability()
// ====================
// Decode shareability of target memory region

// Optimisation because the 2 bits in the argument are comming from the page
// table descriptor, which is still symbolic at the time where this function is
// interpreted. Using the version in shared_pseudocode would mean getting 6
// executions, only 1 of them valid after solving symbolic variables.

// Return maximal sharability

func DecodeShareability(sh:bits(2)) => Shareability
begin
  return Shareability_ISH;
end;

// =============================================================================

// AArch64.OAOutOfRange()
// ======================
// Returns whether output address is expressed in the configured size number of bits

// In shared_pseudocode, this function is implemented as a test on the
// high-order bits of the address. With symbolic addresses in herd, the address
// is guaranteed to be in range. On top of this, this function would be
// expensive performance-wise, because of the bit-operations on a still
// symbolic variable.

func AArch64_OAOutOfRange(address:bits(56), d128:bit, ps:bits(3), ds:bit,
                          tgx:TGx) => boolean
begin
  return FALSE;
end;

// =============================================================================

// AArch64.GetVARange()
// ====================
// Determines if the VA that is to be translated lies in LOWER or UPPER address range.

// In herd, by construction, all addresses are in the LOWER address range.
// In the shared-pseudocode, this is implemented as a read of to the bit 55 of
// the address, which is expensive performance-wise on a fully symbolic virtual
// address.

func AArch64_GetVARange(va:bits(64)) => VARange
begin
  return VARange_LOWER;
end;

// =============================================================================

// AArch64.VAIsOutOfRange()
// ========================
// Check bits not resolved by translation are identical and of accepted value

// In shared_pseudocode, this function is implemented as a test on the
// high-order bits of the address. With symbolic addresses in herd, the address
// is guaranteed to be in range. On top of this, this function would be
// expensive performance-wise, because of the bit-operations on a still
// symbolic variable.

func AArch64_VAIsOutOfRange(va_in: bits(64), acctype: AccessType,
                            regime: Regime, walkparams:S1TTWParams) => boolean
begin
  return FALSE;
end;

// =============================================================================

// AArch64.S1DirectBasePermissions()
// =================================
// Computes the stage 1 direct base permissions

// Optimisation: because the orginal function decoding the permissions splits
// up the execution many times, the performance are very degraded. We override
// it with a choice of reasonable permissions.

func AArch64_S1DirectBasePermissions(regime : Regime, walkstate : TTWState,
                                     walkparams : S1TTWParams,
                                     accdesc : AccessDescriptor) => S1AccessControls
begin

  let IS_EL0 = accdesc.el == EL0;
  let permissions = walkstate.permissions;

  var ap = walkstate.permissions.ap;
  if permissions.dbm == '1' && walkparams.hd == '1' then
     ap[2] = '0';
  end;
  let w       = if ap[2] == '0' then '1' else '0';
  let w_el0   = if ap[2:1] == '01' then '1' else '0';

  var s1perms : S1AccessControls;
  s1perms.r   = if IS_EL0 then ap[1] else '1';
  s1perms.w   = if IS_EL0 then w_el0 else w;
  s1perms.x   = '0';
  s1perms.gcs = '0';
  s1perms.wxn = '0';
  s1perms.overlay = TRUE;

  return s1perms;
end;

// =============================================================================

// StageOA()
// =========
// Given the final walk state (a page or block descriptor), map the untranslated
// input address bits to the output address

// Because of herd's representation of addresses as symbols, we override this
// function and use a primitive to compute the offset with respect to the
// output address.

func StageOA(ia:bits(64),d128:bit,tgx:TGx,walkstate:TTWState) => FullAddress
begin
var oa : FullAddress;
  oa.paspace = walkstate.baseaddress.paspace;
  oa.address = walkstate.baseaddress.address + OffsetPrimitive(ia);
  return oa;
end;

// =============================================================================

// AArch64.S1LeafBase()
// ====================
// Extract the address embedded in a block and page descriptor pointing to the
// base of a memory block

// Because of herd's representation of page table descriptors, we override this
// function and use a primitive to compute the base address (without offset) of
// the function.

func AArch64_S1LeafBase{N}(descriptor: bits(N), walkparams: S1TTWParams,
                           level:integer) => bits(56)
begin
  return GetOAPrimitive{N}(descriptor);
end;

// =============================================================================

// Exeption thrown at the end of an instruction. This is caught by the main
// fault handler that surrounds an instruction operational code, as generated
// by AArch64ASLSem.
type SilentExit of exception {-};

// AArch64.DataAbort()
// ===================

// Our implementation simply creates a fault effect here with the correct
// parameters. It then interrupts the execution of the instruction with the
// help of an exception that is caught in the asl main, as constructed by
// AArch64ASLSem.

func AArch64_DataAbort(fault:FaultRecord)
begin
  // Create a Fault Effect
  DataAbortPrimitive(fault.vaddress, fault.write, fault.statuscode, fault.accessdesc);

  // Interrupt control-flow and terminate execution of the instruction
  throw SilentExit {-};
end;

// =============================================================================

// AArch64.GetS1TTWParams()
// ========================
// Returns stage 1 translation table walk parameters from respective controlling
// System registers.

// Luc: we assume EL10 regime, return minimal parameters, use primitives
// returning configuration in herd

var D128:boolean;

func AArch64_GetS1TTWParams
  (regime:Regime, el:bits(2), ss:SecurityState, va:bits(64))
  => S1TTWParams
begin
  assert (regime == Regime_EL10);

  var walkparams : S1TTWParams;

  // We use the d128 variant to avoid some costly bitvector operations on
  // symbolic variables.
  walkparams.d128 = if D128 then '1' else '0'; // Much faster!

  // Hardware update settings from herd
  walkparams.ha = GetHaPrimitive();
  walkparams.hd = GetHdPrimitive();

  // Irrelevant in our case because we have overriden the corresponding chekcs
  // 16 passes the checks in S1Translate.
  walkparams.txsz = 16[5:0];

  return walkparams;
end;

// =============================================================================

// AArch64.ContiguousBit()
// =======================
// Get the value of the contiguous bit

// Luc: Returns 0 to avoid faults in 128 bit mode

func AArch64_ContiguousBit{N}
  (tgx:TGx, d128:bit,level:integer, descriptor:bits(N)) => bit
begin
  return '0';
end;

// =============================================================================

// AArch64.MAIRAttr()
// ==================
// Retrieve the memory attribute encoding indexed in the given MAIR
// Temporary ? Origin does not work for unknown index!
// the value returned is irrelevant because we override the S1DecodeMemAttrs

func AArch64_MAIRAttr(index:integer,  mair2:MAIRType, mair:MAIRType) => bits(8)
begin
  return Ones{8};
end;

// =============================================================================

// S1DecodeMemAttrs()
// ==================
// Decode MAIR-format memory attributes assigned in stage 1
// Luc: for speed, handle the case of Mormal memory, untagged, WB, ISH
// Hadrien: sh received here is read from the translation table descriptor, so
// is in general symbolic. The attr_in are a redirection away from being
// symbolic, see AArch64_MAIRAttr.
// Given this, the function DecodeShareability() results in 4+ executions: this
// is too much of a performance loss. We thus have to override
// DecodeShareability() or one of its parent. Because S1DecodeMemAttrs is just
// a wrapper around the decoding of sharability and the MAIR_Attr, that we had
// overiden earlier, we decided that it was more explicit to simply override
// S1DecodeMemAttrs.

func
  S1DecodeMemAttrs
  (attr_in:bits(8), sh:bits(2), s1aarch64:boolean,
  walparams:S1TTWParams,acctype:AccessType)
  => MemoryAttributes
begin
  return NormalWBISHMemAttr;
end;

// =============================================================================


// AArch64.CheckDebug()
// ====================
// Called on each access to check for a debug exception or entry to Debug state.

// We do not support debugging.

func AArch64_CheckDebug
  (vaddress:bits(64), accdesc:AccessDescriptor, size:integer)
  => FaultRecord
begin
    return NoFault(accdesc, vaddress);
end;

// =============================================================================

// CreateAccDescAtomicOp()
// =======================
// Access descriptor for atomic read-modify-write memory accesses

// We override this to allow the following non-supported behaviour in
// shared_pseudocode: if we correctly predict that the CAS is going to fail, we
// do not need to update the nDirty bit in the page table descriptor of the
// corresponding address.
// To support this, we create two executions: one where the CAS will fail, and
// we can write the CAS as a simple READ, and one where the CAS will succeed,
// and we can treat the CAS as a write. Once we've read in memory, we need to
// discard the executions where the CAS is a load, which is done in
// PhysMemWrite and DataAbort.

func
  CreateAccDescAtomicOp
    (modop:MemAtomicOp, acquire:boolean, release:boolean,
     tagchecked:boolean, privileged:boolean, Rt: integer, Rs: integer) => AccessDescriptor
begin
    let Rt2: integer = -1;
    let Rs2: integer = -1;

    return CreateAccDescAtomicOp(modop, acquire, release, tagchecked, privileged, Rt, Rt2, Rs, Rs2);
end;

func
  CreateAccDescAtomicOp
    (modop:MemAtomicOp, acquire:boolean, release:boolean,
     tagchecked:boolean, privileged:boolean, Rt: integer, Rt2: integer, Rs: integer, Rs2: integer) => AccessDescriptor
begin
  var accdesc = NewAccDesc(AccessType_GPR);
  accdesc.acqsc           = acquire;
  accdesc.el              = if !privileged then EL0 else PSTATE.EL;
  accdesc.relsc           = release;
  accdesc.atomicop        = TRUE;
  accdesc.modop           = modop;
  accdesc.read            = TRUE;

  // The next line is the one edited:
  accdesc.write           = modop != MemAtomicOp_CAS || SomeBoolean();

  accdesc.pan             = TRUE;
  accdesc.tagchecked      = tagchecked;
  accdesc.Rs              = Rs;
  accdesc.Rs2             = Rs2;
  accdesc.Rt              = Rt;
  accdesc.Rt2             = Rt2;

  return accdesc;
end;

// =============================================================================

// Work around to allow not setting the AF flag in case of a permission fault.
// Because the function AArch64_SetAccessFlag does not have the information
// about a possible permission fault, and because we want to avoid overriding
// AArch64_S1Translate, we do it in AArch64_SetDirtyState. We do the
// communication between SetAccessFlag and SetDirtyState with the help of a
// underlying boolean UpdatedAF, with which we can discard executions that
// do not satisfy conditions for not-setting the AFUpdate, namely when there is
// a permission fault.

// UpdatedAF is the underlying storage to communicate between
// SetAccessFlag and SetDirtyState.
var UpdatedAF : boolean = FALSE;

// NeedCheckPermissionFault is a flag indicating to SetDirtyState that
// SetAccessFlag would have needed to check that there was a permission fault.
var NeedCheckPermissionFault : boolean = FALSE;

// AArch64_SetAccessFlag()
// =======================
// Determine whether the access flag could be set by HW given the fault status

// We edit SetAccessFlag to allow not-setting the AccessFlag when there is a
// permission fault. When such a choice is made, we store it in the underlying
// storage UpdatedAF, which will be checked against the permission fault
// record in SetDirtyState.

func AArch64_SetAccessFlag(ha : bit, accdesc : AccessDescriptor, fault : FaultRecord) => boolean
begin
    if ha == '0' || !AArch64_SettingAccessFlagPermitted(fault) then
        return FALSE;
    elsif accdesc.acctype == AccessType_AT then
        return ImpDefBool("AT updates AF");
    elsif accdesc.acctype IN {AccessType_DC, AccessType_IC} then
        return ImpDefBool("Generate access flag fault on IC/DC operations");
    else
        // Only edited lines are the following:
        NeedCheckPermissionFault = TRUE;

        // Set descriptor AF bit
        if !accdesc.write && accdesc.modop != MemAtomicOp_CAS then
          // There can't be any permission fault, so we have to update AF
          UpdatedAF = TRUE;

        // There can be a permission fault, so we generate 2 executions:
        else
          UpdatedAF = ConstrainUnpredictableBool(Unpredictable_AFUPDATE);
        end;

        return UpdatedAF;
    end;
end;

// AArch64_SetDirtyState()
// =======================
// Determine whether dirty state is required to be updated by HW given the fault status

// This function is edited to guarantee that if there isn't a permission fault,
// the AF should have been updated.

func AArch64_SetDirtyState(hd : bits(1), dbm : bits(1),
                           accdesc : AccessDescriptor,
                           fault : FaultRecord,
                           fault_perm : FaultRecord) => boolean
begin
    // Only edited line is this added check:
    if NeedCheckPermissionFault && fault_perm.statuscode == Fault_None then
      // If there has not been a fault, we have to update AF
      CheckProp(UpdatedAF);
    end;

    if hd == '0' then
        return FALSE;
    elsif !AArch64_SettingDirtyStatePermitted(fault, fault_perm) then
        return FALSE;
    elsif accdesc.acctype IN {AccessType_AT, AccessType_IC, AccessType_DC} then
        return FALSE;
    elsif !accdesc.write then
        return FALSE;
    else
        return dbm == '1';
    end;
end;

