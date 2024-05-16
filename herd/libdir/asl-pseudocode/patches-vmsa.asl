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

func Aarch64_DecodeDescriptorType
  (descriptor:bits(N), d128:bit, ds:bit, tgx:TGx, level:integer)
    =>
  DescriptorType
begin
  return DescriptorType_Leaf;
end
