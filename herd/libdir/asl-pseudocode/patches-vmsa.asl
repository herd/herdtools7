// AArch64.S1Enabled()
// ===================
// Determine if stage 1 is enabled for the access type for this translation regime
// Stage 1 is the onlt translation regime implemented

func AArch64_S1Enabled(regme : Regime,acctype : AccessType) => boolean
begin
  return TRUE;
end;

// AArch64.PAMax()
// ===============
// Returns the IMPLEMENTATION DEFINED maximum number of bits capable of representing
// physical address for this processor
// Let us define it.

func AArch64_PAMax() => integer
begin
    return 48;
end;

// AArch64.DecodeDescriptorType()
// ==============================
// Determine whether the descriptor is a page, block or table
// Simplify: invalid or Leaf

func AArch64_DecodeDescriptorType
  (descriptor:bits(N), d128:bit, ds:bit, tgx:TGx, level:integer)
    =>
  DescriptorType
begin
  if descriptor[0] == '0' then
//    __DEBUG__(descriptor,DescriptorType_Invalid);
    return DescriptorType_Invalid;
  else
    return DescriptorType_Leaf;
  end;
end;

// FetchDescriptor()
// =================
// Fetch a translation table descriptor

func FetchDescriptor(ee:bit, walkaddress:AddressDescriptor,
                     walkacces:AccessDescriptor,
                     fault_in:FaultRecord,N:integer)
  => (FaultRecord, bits(N))
    // 32-bit descriptors for AArch32 Short-descriptor format
    // 64-bit descriptors for AArch64 or AArch32 Long-descriptor format
    // 128-bit descriptors for AArch64 when FEAT_D128 is set and {V}TCR_ELx.d128 is set
begin
//   __DEBUG__(walkaddress.paddress.address);
   let desc = ReadPtePrimitive(walkaddress.paddress.address);
//   __DEBUG__(walkaddress.paddress.address,desc);
   return (fault_in,desc);
end;

// AArch64.S1SLTTEntryAddress()
// ============================
// Compute the first stage 1 translation table descriptor address within the
// table pointed to by the base at the start level

func AArch64_S1SLTTEntryAddress(level:integer, walparams:S1TTWParams,
                                ia:bits(64), tablebase:FullAddress)
  =>  FullAddress
begin
    var descaddress : FullAddress;
    descaddress.address = ComputePtePrimitive(ia);
    descaddress.paspace = tablebase.paspace;
//    __DEBUG__(ia,descaddress.address);
    return descaddress;
end;

// AArch64.MAIRAttr()
// ==================
// Retrieve the memory attribute encoding indexed in the given MAIR
// Temporary ? Origin does not work for unknown index!

func AArch64_MAIRAttr(index:integer,  mair2:MAIRType, mair:MAIRType) => bits(8)
begin
  return Zeros{8};
end;

// DecodeShareability()
// ====================
// Decode shareability of target memory region
// Temporary: return maximal sharability

func DecodeShareability(sh:bits(2)) => Shareability
begin
  return Shareability_OSH;
end;

// AArch64.S1AMECFault()
// =====================
// Returns TRUE if a Translation fault should occur for Realm EL2 and Realm EL2&0
// stage 1 translated addresses to Realm PA space.
// Temporary: do not fault

func AArch64_S1AMECFault
  (wallparams:S1TTWParams, paspace:PASpace,regime:Regime,descriptor:bits(N))
=> boolean
begin
    return FALSE;
end;

// AArch64.OAOutOfRange()
// ======================
// Returns whether output address is expressed in the configured size number of bits

func AArch64_OAOutOfRange(address:bits(56), d128:bit, ps:bits(3), tgx:TGx)
=> boolean
begin
  return FALSE;
end;

// AArch64.GetVARange()
// ====================
// Determines if the VA that is to be translated lies in LOWER or UPPER address range.
// No hesitation, return LOWER

func AArch64_GetVARange(va:bits(64)) => VARange
begin
  return VARange_LOWER;
end;

// AArch64.VAIsOutOfRange()
// ========================
// Check bits not resolved by translation are identical and of accepted value

func AArch64_VAIsOutOfRange(va_in:bits(64),acctype:AccessType,
                               regime:Regime, walkparams:S1TTWParams)
=>
boolean
begin
  return FALSE;
end;


// AArch64.S1DirectBasePermissions()
// =================================
// Computes the stage 1 direct base permissions
// A choice of reasonable permissions

func
  AArch64_S1DirectBasePermissions
    (regime:Regime,walkstate:TTWState,
     waltparams:S1TTWParams,accdesc:AccessDescriptor)
=> S1AccessControls
begin
  var s1perms : S1AccessControls;
  s1perms.r   = '1';
  s1perms.w   =  if walkstate.permissions.ap[2] == '1' then '0' else '1';
  s1perms.x   = '0';
  s1perms.gcs = '0';
  s1perms.wxn = '0';
  s1perms.overlay = TRUE;
  return s1perms;
end;

// StageOA()
// =========
// Given the final walk state (a page or block descriptor), map the untranslated
// input address bits to the output address

func StageOA(ia:bits(64),d128:bit,tgx:TGx,walkstate:TTWState) => FullAddress
begin
var oa : FullAddress;
//  __DEBUG__(ia,walkstate);
  oa.paspace = walkstate.baseaddress.paspace;
  oa.address = walkstate.baseaddress.address + OffsetPrimitive(ia);
  return oa;
end;

// AArch64.LeafBase()
// ==================
// Extract the address embedded in a block and page descriptor pointing to the
// base of a memory block
// Modified -> extract oa field from descriptor

func
  AArch64_LeafBase(descriptor:bits(N),d128:bit,ds:bit,tgx:TGx,level:integer)
  => bits(56)
begin
  return GetOAPrimitive(descriptor);
end;


// AArch64.MemSwapTableDesc()
// ==========================
// Perform HW update of table descriptor as an atomic operation
// Modified -> disabled at the moment

func AArch64_MemSwapTableDesc
  (fault_in:FaultRecord,prev_desc:bits(N),new_desc:bits(N),
  ee:bit,descaccess:AccessDescriptor,descpaddr:AddressDescriptor)
=> (FaultRecord, bits(N))
begin
  return (fault_in,new_desc);
end;

// AArch64.DataAbort()
// ===================

type SilentExit of exception;

func AArch64_DataAbort(vaddress:bits(64),fault:FaultRecord)
begin
//  __DEBUG__(vaddress);
  DataAbortPrimitive(vaddress,fault.write,fault.statuscode);
  throw SilentExit {};
end;


// S1TranslationRegime()
// =====================
// Stage 1 translation regime for the given Exception level

func S1TranslationRegime(el:bits(2)) => bits(2)
begin
//  __DEBUG__(el);
  return EL1;
end;

func S1TranslationRegime() => bits(2)
begin
  return EL1;
end;

// PEErrorState()
// ==============
// Returns the error state of the PE on taking an error exception:
// The PE error state reported to software through the exception syndrome also
// depends on how the exception is taken, and so might differ from the value
// returned from this function.
// LUC: Dubious

func PEErrorState(fault:FaultRecord) => ErrorState
begin
  return ErrorState_UEO;
end;

// IsFault()
// =========
// Return TRUE if a fault is associated with status returned by memory.
// Luc: and then call  HandleExternalAbort()
//func IsFault(retstatus:PhysMemRetStatus) => boolean
// begin
//  return FALSE;
// end;


// HandleExternalAbort()
// =====================
// Takes a Synchronous/Asynchronous abort based on fault.
// Luc Should not be called.
func
  HandleExternalAbort
    (memretstatus:PhysMemRetStatus,iswrite:boolean,
     memaddrdesc:AddressDescriptor,size:integer,
     accdesc:AccessDescriptor)
begin
//  assert FALSE;
  return;
end;