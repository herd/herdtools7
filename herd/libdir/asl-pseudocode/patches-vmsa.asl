// AArch64.S1Enabled()
// ===================
// Determine if stage 1 is enabled for the access type for this translation regime
// Stage 1 is the onlt translation regime implemented

func AArch64_S1Enabled(regme : Regime,acctype : AccessType) => boolean
begin
  return TRUE;
end