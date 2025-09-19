// AArch64.S1Enabled()
// ===================
// Determine if stage 1 is enabled for the access type for this translation regime
// Stage 1 is the only translation regime implemented

func AArch64_S1Enabled(regime : Regime,acctype : AccessType) => boolean
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

func AArch64_OAOutOfRange
  (address:bits(56), d128:bit, ps:bits(3), ds:bit, tgx:TGx)
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
     walkparams:S1TTWParams,accdesc:AccessDescriptor)
=> S1AccessControls
begin
  var permissions = walkstate.permissions;
  if permissions.dbm == '1' && walkparams.hd == '1' then
     permissions.ap[2] = '0';
  end;
  let IS_EL0 = accdesc.el == EL0;
  var s1perms : S1AccessControls;
  s1perms.r   = if IS_EL0 then permissions.ap[1] else '1';
  let w = if permissions.ap[2] == '0' then '1' else '0';
  let w_el0 = if  permissions.ap[2:1] == '01' then '1' else '0';
  s1perms.w   = if IS_EL0 then w_el0 else w;
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
  oa.paspace = walkstate.baseaddress.paspace;
  oa.address = walkstate.baseaddress.address + OffsetPrimitive(ia);
  return oa;
end;

// AArch64.S1LeafBase()
// ====================
// Extract the address embedded in a block and page descriptor pointing to the
// base of a memory block
func
  AArch64_S1LeafBase(descriptor:bits(N),walkparams:S1TTWParams,level:integer)
  => bits(56)
begin
  return GetOAPrimitive(descriptor);
end;


// AArch64.MemSwapTableDesc()
// ==========================
// Perform HW update of table descriptor as an atomic operation
// Modified -> write only

func AArch64_MemSwapTableDesc
  {N}(fault_in:FaultRecord,prev_desc:bits(N),new_desc:bits(N),
   ee:bit,descaccess:AccessDescriptor,descpaddr:AddressDescriptor,n:integer)
=> (FaultRecord, bits(N))
begin
   let addr = descpaddr.paddress.address;
   let mem_desc = ReadPteAgainPrimitive{N}(addr,descaccess.write);
// __debug__(N,mem_desc,prev_desc,descaccess.write);
// For speed, using "if" construct us much more expensive
   CheckProp(mem_desc == prev_desc);
   WritePtePrimitive{N}(addr,new_desc,descaccess.write);
   return (fault_in,new_desc);
end;

// AArch64.DataAbort()
// ===================


// Identify writes by their access descriptor.
func IsWrite(a:AccessDescriptor) => boolean
begin
  return a.write;
end;

// Get rid of CAS as LOAD execution

func IsCasAsLoad(a:AccessDescriptor) => boolean
begin
  return a.modop == MemAtomicOp_CAS && !a.write;
end;

func CheckNotCasAsLoad(a:AccessDescriptor)
begin
  CheckProp(!(IsCasAsLoad(a)));
end;

type SilentExit of exception {-};

func AArch64_DataAbort(fault:FaultRecord)
begin
// __debug__(123,fault.statuscode,fault.accessdesc.write);
  CheckNotCasAsLoad(fault.accessdesc);
  DataAbortPrimitive(fault.vaddress,fault.write,fault.statuscode,fault.accessdesc);
  throw SilentExit {-};
end;


// S1TranslationRegime()
// =====================
// Stage 1 translation regime for the given Exception level

func S1TranslationRegime(el:bits(2)) => bits(2)
begin
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

//
// Previous walkparams
// {aie:'0',amec:'0',cmow:'0',d128:'0',dc:'0',dct:'0',disch:'0',ds:'0',
// e0pd:'0',ee:'0',emec:'0',epan:'0',fng:'0',ha:'0',haft:'0',hd:'0',
// hpd:'0',irgn:'01',
// mair:'0000000000000000000000000000000000000000000000000000000000000000',
// mair2:'0000000000000000000000000000000000000000000000000000000000000000',
// mtx:'0',nfd:'0',ntlsmd:'1',nv1:'0',orgn:'01',pie:'0',pir:'',pire0:'',
// pnch:'0',ps:'100',sh:'11',sif:'0',skl:'00',t0sz:'000',t1sz:'000',
// tbi:'0',tbid:'0',tgx:2,txsz:'010000',uwxn:'0',wxn:'0',}


// AArch64.GetS1TTWParams()
// ========================
// Returns stage 1 translation table walk parameters from respective controlling
// System registers.
// Luc: we assume EL10 regime, return minimal parameters

var D128:boolean;

func AArch64_GetS1TTWParams
  (regime:Regime, el:bits(2), ss:SecurityState, va:bits(64))
  => S1TTWParams
begin
  var walkparams : S1TTWParams;
  assert (regime == Regime_EL10);
  walkparams.d128 = if D128 then '1' else '0'; // Much faster!
  walkparams.ha = GetHaPrimitive();
  walkparams.hd = GetHdPrimitive();
  return walkparams;
end;

// AArch64.ContiguousBit()
// =======================
// Get the value of the contiguous bit
// Luc: Returns 0 to avoid faults in 128 bit mode

func AArch64_ContiguousBit
  (tgx:TGx, d128:bit,level:integer, descriptor:bits(N)) => bit
begin
  return '0';
end;


// AArch64.S1TxSZFaults()
// ======================
// Detect whether configuration of stage 1 TxSZ field generates a fault
// Luc: Override: does not occur, never.

func AArch64_S1TxSZFaults (regime:Regime,walkparams:S1TTWParams) => boolean
begin
  return FALSE;
end;

// S1DecodeMemAttrs()
// ==================
// Decode MAIR-format memory attributes assigned in stage 1
// Luc: for speed (?) handle the case of Mormal memory, untagged, WB, ISH

func
  S1DecodeMemAttrs
  (attr_in:bits(8), sh:bits(2), s1aarch64:boolean,
  walparams:S1TTWParams,acctype:AccessType)
  => MemoryAttributes
begin
  var memattrs : MemoryAttributes;
  memattrs.memtype = MemType_Normal;
  memattrs.outer.attrs     = MemAttr_WB;
  memattrs.outer.hints     = MemHint_RWA;
  memattrs.outer.transient = FALSE;
  memattrs.inner.attrs     = MemAttr_WB;
  memattrs.inner.hints     = MemHint_RWA;
  memattrs.inner.transient = FALSE;
  memattrs.xs              = '0';
  memattrs.tags = MemTag_Untagged;
  memattrs.notagaccess = FALSE;
  memattrs.shareability = Shareability_ISH;
  return memattrs;
end;

// AArch64.CheckDebug()
// ====================
// Called on each access to check for a debug exception or entry to Debug state.

func AArch64_CheckDebug
  (vaddress:bits(64), accdesc:AccessDescriptor, size:integer)
=> FaultRecord
begin
    let fault = NoFault(accdesc, vaddress);
    return fault;
end;

// AArch64.BlocknTFaults()
// =======================
// Identify whether the nT bit in a block descriptor is effectively set
// causing a translation fault

func AArch64_BlocknTFaults{N}(d128:bit,descriptor:bits(N)) => boolean
begin
  return FALSE;
end;

// EL2Enabled()
// ============
// Returns TRUE if EL2 is present and executing
// - with the PE in Non-secure state when Non-secure EL2 is implemented, or
// - with the PE in Realm state when Realm EL2 is implemented, or
// - with the PE in Secure state when Secure EL2 is implemented and enabled, or
// - when EL3 is not implemented.

// Luc: Petty optimisation
func EL2Enabled() => boolean
begin
  return FALSE;
end;

// CreateAccDescAtomicOp()
// =======================
// Access descriptor for atomic read-modify-write memory accesses
// Luc: A CAS can reduce to a load.
func
  CreateAccDescAtomicOp
    (modop:MemAtomicOp, acquire:boolean, release:boolean,
     tagchecked:boolean, privileged:boolean) => AccessDescriptor
begin
  var accdesc = NewAccDesc(AccessType_GPR);
  accdesc.acqsc           = acquire;
  accdesc.el              = if !privileged then EL0 else PSTATE.EL;
  accdesc.relsc           = release;
  accdesc.atomicop        = TRUE;
  accdesc.modop           = modop;
  accdesc.read            = TRUE;
  accdesc.write           = modop != MemAtomicOp_CAS || SomeBoolean();
  accdesc.pan             = TRUE;
  accdesc.tagchecked      = tagchecked;
  accdesc.transactional   = IsFeatureImplemented(FEAT_TME) && TSTATE.depth > 0;

  return accdesc;
end;

// AArch64.SetAccessFlag()
// =======================
// Determine whether the access flag could be set by HW given the fault status

var ForceNoAFUpdate : boolean;

func
  AArch64_SetAccessFlag
    (ha:bit, accdesc:AccessDescriptor, fault:FaultRecord)
begin
    if ha == '0' || !AArch64_SettingAccessFlagPermitted(fault) then
        return FALSE;
    elsif accdesc.acctype == AccessType_AT then
        return ConstrainUnpredictableBool(Unpredictable_AFUPDATE);
    elsif accdesc.acctype IN {AccessType_DC, AccessType_IC} then
        return ConstrainUnpredictableBool(Unpredictable_AFUPDATE);
    else
        // Set descriptor AF bit
        let b = !IsWrite(accdesc) || ConstrainUnpredictableBool(Unpredictable_AFUPDATE);
        ForceNoAFUpdate = !b;
        return b;
    end;
end;

// AArch64.SetDirtyState()
// =======================
// Determine whether dirty state is required to be updated by HW given the fault status

func
  AArch64_SetDirtyState
    (hd:bits(1), dbm:bits(1),
     accdesc:AccessDescriptor,
     fault:FaultRecord, fault_perm:FaultRecord) => boolean
begin
  CheckProp(fault_perm.statuscode != Fault_None || !ForceNoAFUpdate);
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
