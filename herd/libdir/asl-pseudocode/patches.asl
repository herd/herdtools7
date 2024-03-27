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
  var x : MPAMinfo;
  return x;
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
  var full_addr : FullAddress;
  return CreateAddressDescriptor(address, full_addr, NormalNCMemAttr());
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
    [4] D,        // Debug mask bit                     [AArch64 only]
    [5] A,        // SError interrupt mask bit
    [6] I,        // IRQ mask bit
    [7] F,        // FIQ mask bit
    [8] EXLOCK,   // Lock exception return state
    [9] PAN,      // Privileged Access Never Bit        [v8.1]
    [10] UAO,      // User Access Override               [v8.2]
    [11] DIT,      // Data Independent Timing            [v8.4]
    [12] TCO,      // Tag Check Override                 [v8.5, AArch64 only]
    [13] PM,       // PMU exception Mask
    [14] PPEND,     // synchronous PMU exception to be_observed
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

var _PSTATE : ProcState;
var _NZCV : ProcState;

func isNZCV(n:integer) => boolean
begin
  return 0 <= n && n < 4 ;
end

getter PSTATE[] => ProcState
begin
 return _PSTATE;
end

setter PSTATE[] = v : ProcState
begin
  _PSTATE = v;
end

getter PSTATE[n:integer] => bits(1)
begin
  if isNZCV(n) then
    return _NZCV[n];
  else
    return _PSTATE[n];
  end
end

setter PSTATE[n:integer] = v : bits(1)
begin
  if isNZCV(n) then
    _NZCV[n] = v;
  else
    _PSTATE[n] = v;
  end
end


getter PSTATE[n:integer,m:integer] => bits(2)
begin
  if isNZCV(n) && isNZCV(m) then
    return _NZCV[n,m];
  else
    return _PSTATE[n,m];
  end
end

setter PSTATE[n:integer,m:integer] = v : bits(2)
begin
  if isNZCV(n) && isNZCV(m) then
    _NZCV[n,m] = v;
  else
    _PSTATE[n,m] = v;
  end
end

getter PSTATE[n:integer,m:integer,o:integer] => bits(3)
begin
  if isNZCV(n) && isNZCV(m) && isNZCV(o) then
    return _NZCV[n,m,o];
  else
    return _PSTATE[n,m,o];
  end
end

setter PSTATE[n:integer,m:integer,o:integer] = v : bits(3)
begin
  if isNZCV(n) && isNZCV(m) && isNZCV(o) then
    _NZCV[n,m,o] = v;
  else
    _PSTATE[n,m,o] = v;
  end
end

getter PSTATE[n:integer,m:integer,o:integer,p:integer] => bits(4)
begin
  if isNZCV(n) && isNZCV(m) && isNZCV(o) && isNZCV(p) then
    return _NZCV[n,m,o,p];
  else
    return _PSTATE[n,m,o,p];
  end
end

setter PSTATE[n:integer,m:integer,o:integer,p:integer] = v : bits(4)
begin
  if isNZCV(n) && isNZCV(m) && isNZCV(o) && isNZCV(p) then
    _NZCV[n,m,o,p] = v;
  else
    _PSTATE[n,m,o,p] = v;
  end
end

// GenerateAddress()
// =================
// Generate and address by adding a pointer with an offset and returning the result.
// If FEAT_CPA2 is implemented, the pointer arithmetic is checked.
// LUC simplify because failure of slice operatin on symbolic address.
func GenerateAddress(base:bits(64), offset:bits(64), accdesc:AccessDescriptor) => bits(64)
begin
  return base + offset;
end

// AArch64.BranchAddr()
// ====================
// Return the virtual address with tag bits removed.
// This is typically used when the address will be stored to the program counter.

func AArch64_BranchAddr
  (vaddress:bits(64), el:bits(2)) => bits(64)
begin
  return vaddress;
end

// BranchNotTaken()
// ================
// Called when a branch is not taken.
// Patched to add PC self assignment

func BranchNotTaken(branchtype:BranchType, branch_conditional:boolean)
begin
    _PC = _PC+4;
   let branchtaken = FALSE;
   if IsFeatureImplemented(FEAT_SPE) then
     SPEBranch
       (UNKNOWN:bits(64), branchtype, branch_conditional, branchtaken);
    end
    return;
end

// UsingAArch32()
// ==============
// Return TRUE if the current Exception level is using AArch32, FALSE if using AArch64.
// Let us return FALSE, called by BranchTo(...) for checking tgt address size.

func UsingAArch32() => boolean
begin
  return FALSE;
end
