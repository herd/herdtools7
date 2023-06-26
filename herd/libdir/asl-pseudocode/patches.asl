// GenMPAMatEL()
// =============
// Returns MPAMinfo for the specified EL.
// May be called if MPAM is not implemented (but in an version that supports
// MPAM), MPAM is disabled, or in AArch32.  In AArch32, convert the mode to
// EL if can and use that to drive MPAM information generation.  If mode
// cannot be converted, MPAM is not implemented, or MPAM is disabled return
// default MPAM information for the current security state.

// MPAMinfo GenMPAMatEL(AccessType acctype, bits(2) el)
func GenMPAMatEL(acctype:: AccessType, el::bits(2)) => MPAMinfo
begin
  return UNKNOWN :: MPAMinfo;
end

getter TSTATE[] => TMState
begin
  return UNKNOWN :: TMState;
end


// IsAligned

func IsAligned(x :: bits(N), y::integer) => boolean
begin
  return TRUE;
end

func IsAligned(x::integer, y::integer) => boolean
begin
  return TRUE;
end

// BigEndian

func BigEndian(acctype:: AccessType) => boolean
begin
  return FALSE;
end

func IsFault(addrdesc:: AddressDescriptor) => boolean
begin
  return FALSE;
end

func AArch64_TranslateAddress(address::bits(64), accdesc::AccessDescriptor, aligned::boolean, size::integer) => AddressDescriptor
begin
  return CreateAddressDescriptor(address, UNKNOWN :: FullAddress, NormalNCMemAttr());
end

func ELStateUsingAArch32K(el::bits(2), secure::boolean) => (boolean, boolean)
begin
    return (TRUE, FALSE);
end

// ProcState
// =========
// Armv8 processor state bits.
// There is no significance to the field order.

type ProcState of bits(64) {
    [3] N,        // Negative condition flag
    [2] Z,        // Zero condition flag
    [1] C,        // Carry condition flag
    [0] V,        // Overflow condition flag
    [3] D,        // Debug mask bit                     [AArch64 only]
    [5] A,        // SError interrupt mask bit
    [6] I,        // IRQ mask bit
    [7] F,        // FIQ mask bit
    [8] EXLOCK,   // Lock exception return state
    [9] PAN,      // Privileged Access Never Bit        [v8.1]
    [10] UAO,      // User Access Override               [v8.2]
    [11] DIT,      // Data Independent Timing            [v8.4]
    [12] TCO,      // Tag Check Override                 [v8.5, AArch64 only]
    [13] PM,       // PMU exception Mask
    [14] PPEND,     // synchronous PMU exception to be observed
    [16:15] BTYPE,    // Branch Type                        [v8.5]
    [17] ZA,       // Accumulation array enabled         [SME]
    [18] SM,       // Streaming SVE mode enabled         [SME]
    [19] ALLINT,   // Interrupt mask bit
    [20] SS,       // Software step bit
    [21] IL,       // Illegal Execution state bit
    [23:22] EL,       // Exception level
    [24] nRW,      // Execution state: 0=AArch64, 1=AArch32
    [25] SP,       // Stack pointer select: 0=SP0, 1=SPx [AArch64 only]
    [26] Q,        // Cumulative saturation flag         [AArch32 only]
    [30:27] GE,       // Greater than or Equal flags        [AArch32 only]
    [31] SSBS,     // Speculative Store Bypass Safe
    [39:32] IT,       // If-then bits, RES0 in CPSR         [AArch32 only]
    [40] J,        // J bit, RES0                        [AArch32 only, RES0 in SPSR and CPSR]
    [41] T,        // T32 bit, RES0 in CPSR              [AArch32 only]
    [42] E,        // Endianness bit                     [AArch32 only]
    [47:42] M         // Mode field                         [AArch32 only]
};

