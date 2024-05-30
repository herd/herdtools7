// AArch64.S1Enabled()
// ===================
// Determine if stage 1 is enabled for the access type for this translation regime
// Stage 1 is the onlt translation regime implemented

func AArch64_S1Enabled(regme : Regime,acctype : AccessType) => boolean
begin
  return TRUE;
end

// AArch64.PAMax()
// ===============
// Returns the IMPLEMENTATION DEFINED maximum number of bits capable of representing
// physical address for this processor
// Let us define it.

func AArch64_PAMax() => integer
begin
    return 48;
end

// AArch64.DecodeDescriptorType()
// ==============================
// Determine whether the descriptor is a page, block or table
// Temporarily return Leaf

func AArch64_DecodeDescriptorType
  (descriptor:bits(N), d128:bit, ds:bit, tgx:TGx, level:integer)
    =>
  DescriptorType
begin
  return DescriptorType_Leaf;
end

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
   DEBUG(walkaddress.paddress.address);
   let desc = ReadPtePrimitive(walkaddress.paddress.address);
   DEBUG(desc);
   return (fault_in,desc);
end

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
    DEBUG(ia,descaddress.address);
    return descaddress;
end

// AArch64.MAIRAttr()
// ==================
// Retrieve the memory attribute encoding indexed in the given MAIR
// Temporary ? Origin does not work for unknown index!

func AArch64_MAIRAttr(index:integer,  mair2:MAIRType, mair:MAIRType) => bits(8)
begin
  return Zeros(8);
end

// DecodeShareability()
// ====================
// Decode shareability of target memory region
// Temporary: return maximal sharability

func DecodeShareability(sh:bits(2)) => Shareability
begin
  return Shareability_OSH;
end

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
end

// AArch64.OAOutOfRange()
// ======================
// Returns whether output address is expressed in the configured size number of bits

func AArch64_OAOutOfRange(address:bits(56), d128:bit, ps:bits(3), tgx:TGx)
=> boolean
begin
  return FALSE;
end