// Herd compatibility

AddressDescriptor TranslateAddress(bits(64) address, AccessDescriptor accdesc, boolean aligned, integer size)
    return address;

boolean IsFault(AddressDescriptor addrdesc)
    return FALSE;

boolean BigEndian(AccessDescriptor accddesc)
    return FALSE;

_R[integer n] = bits(64) value
    write_register(n, 64, value);

bits(64) _R[integer n]
    return read_register(n, 64);
    

// ----------------------------------------------------------------------------
// Primitives defined by archex and redefined here

(PhysMemRetStatus, bits(8*size)) PhysMemRead(AddressDescriptor desc, integer size, AccessDescriptor accdesc)
    return (FALSE, read_memory(desc, 8*size));

PhysMemRetStatus PhysMemWrite(AddressDescriptor desc, integer size, AccessDescriptor accdesc, bits(8*size) value)
    write_memory(desc, 8*size, value);
    return FALSE;

// ----------------------------------------------------------------------------
// From aarch64/functions/registers.asl

// X[] - assignment form
// =====================
// Write to general-purpose register from either a 32-bit or a 64-bit value,
// where the size of the value is passed as an argument.

X[integer n, integer width] = bits(width) value
    assert n >= 0 && n <= 31;
    assert width IN {32,64};
    if n != 31 then
        _R[n] = ZeroExtend(value, 64);
    return;

// X[] - non-assignment form
// =========================
// Read from general-purpose register with an explicit slice of 8, 16, 32 or 64 bits.

bits(width) X[integer n, integer width]
    assert n >= 0 && n <= 31;
    assert width IN {8,16,32,64};
    if n != 31 then
        return _R[n]<width-1:0>;
    else
        return Zeros(width);

// ----------------------------------------------------------------------------
// From shared/translation/vmsa.asl

// AddressDescriptor
// =================
// Descriptor used to access the underlying memory array.

type AddressDescriptor is (
    FaultRecord      fault,      // fault.statuscode indicates whether the address is valid
    MemoryAttributes memattrs,
    FullAddress      paddress,
    bits(64)         vaddress
)

// ----------------------------------------------------------------------------
// From shared/functions/common.asl

// IsAligned()
// ===========

boolean IsAligned(bits(N) x, integer y)
    return x == Align(x, y);

// ----------------------------------------------------------------------------
// From shared/functions/system.ash

// SecurityState
// =============
// The Security state of an execution context

enumeration SecurityState {
    SS_NonSecure,
    SS_Secure
};

// ----------------------------------------------------------------------------
// From shared/functions/memory.ash

// AccessType
// ==========

enumeration AccessType {
    AccessType_IFETCH,  // Instruction FETCH
    AccessType_GPR,     // Software load/store to a General Purpose Register
    AccessType_ASIMD,   // Software ASIMD extension load/store instructions
    AccessType_IC,      // Sysop IC
    AccessType_DC,      // Sysop DC (not DC {Z,G,GZ}VA)
    AccessType_DCZero,  // Sysop DC {Z,G,GZ}VA
    AccessType_AT,      // Sysop AT
    AccessType_TTW      // Translation Table Walk
};

enumeration CacheOp {
    CacheOp_Clean,
    CacheOp_Invalidate,
    CacheOp_CleanInvalidate
};

enumeration CacheOpScope {
    CacheOpScope_SetWay,
    CacheOpScope_PoU,
    CacheOpScope_PoC,
    CacheOpScope_ALLU,
    CacheOpScope_ALLUIS
};

enumeration CacheType {
    CacheType_Data,
    CacheType_Instruction
};

// MemAtomicOp
// ===========
// Atomic data processing instruction types.

enumeration MemAtomicOp {
    MemAtomicOp_ADD,
    MemAtomicOp_BIC,
    MemAtomicOp_EOR,
    MemAtomicOp_ORR,
    MemAtomicOp_SMAX,
    MemAtomicOp_SMIN,
    MemAtomicOp_UMAX,
    MemAtomicOp_UMIN,
    MemAtomicOp_SWP,
    MemAtomicOp_CAS
};

// AccessDescriptor
// ================
// Memory access or translation invocation details that steer architectural behavior

type AccessDescriptor is (
    AccessType acctype,
    bits(2) el,             // Acting EL for the access
    SecurityState ss,       // Acting Security State for the access
    boolean acqsc,          // Acquire with Sequential Consistency
    boolean relsc,          // Release with Sequential Consistency
    boolean exclusive,      // Access has Exclusive semantics
    boolean atomicop,       // FEAT_LSE: Atomic read-modify-write access
    MemAtomicOp modop,      // FEAT_LSE: The modification operation in the 'atomicop' access
    boolean nontemporal,    // Hints the access is non-temporal
    boolean read,           // Read from memory or only require read permissions
    boolean write,          // Write to memory or only require write permissions
    CacheOp cacheop,        // DC/IC: Cache operation
    CacheOpScope opscope,   // DC/IC: Scope of cache operation
    CacheType cachetype,    // DC/IC: Type of target cache
)

// NewAccDesc()
// ============
// Create a new AccessDescriptor with initialised fields

AccessDescriptor NewAccDesc(AccessType acctype)
    AccessDescriptor accdesc;

    accdesc.acctype         = acctype;
    accdesc.el              = PSTATE.EL;
    accdesc.ss              = SecurityStateAtEL(PSTATE.EL);
    accdesc.acqsc           = FALSE;
    accdesc.relsc           = FALSE;
    accdesc.exclusive       = FALSE;
    accdesc.atomicop        = FALSE;
    accdesc.nontemporal     = FALSE;
    accdesc.read            = FALSE;
    accdesc.write           = FALSE;
    accdesc.nonfault        = FALSE;
    accdesc.firstfault      = FALSE;
    accdesc.first           = FALSE;
    accdesc.contiguous      = FALSE;

    return accdesc;

// CreateAccDescAtomicOp()
// =======================
// Access descriptor for atomic read-modify-write memory accesses

AccessDescriptor CreateAccDescAtomicOp(MemAtomicOp modop, boolean acquire, boolean release)
    AccessDescriptor accdesc = NewAccDesc(AccessType_GPR);

    accdesc.acqsc           = acquire;
    accdesc.relsc           = release;
    accdesc.atomicop        = TRUE;
    accdesc.modop           = modop;
    accdesc.read            = TRUE;
    accdesc.write           = TRUE;

    return accdesc;

// MemAtomic()
// ===========
// Performs load and store memory operations for a given virtual address.

bits(size) MemAtomic(bits(64) address, bits(size) cmpoperand, bits(size) operand,
                     AccessDescriptor accdesc_in)
    assert accdesc_in.atomicop;

    constant integer bytes = size DIV 8;
    assert bytes IN {1, 2, 4, 8, 16};

    bits(size) newvalue;
    bits(size) oldvalue;
    AccessDescriptor accdesc = accdesc_in;
    boolean aligned = IsAligned(address, bytes);

    if !aligned && AArch64.UnalignedAccessFaults(accdesc, address, bytes) then
        AArch64.Abort(address, AlignmentFault(accdesc));

    // MMU or MPU lookup
    AddressDescriptor memaddrdesc = AArch64.TranslateAddress(address, accdesc, aligned, size);

    // Check for aborts or debug exceptions
    if IsFault(memaddrdesc) then
        AArch64.Abort(address, memaddrdesc.fault);

    // Effect on exclusives
    // This section has to be commented as it is not compatible with herd
    // if memaddrdesc.memattrs.shareability != Shareability_NSH then
        // ClearExclusiveByAddress(memaddrdesc.paddress, ProcessorID(), size);

    // All observers in the shareability domain observe the following load and store atomically.
    PhysMemRetStatus memstatus;
    (memstatus, oldvalue) = PhysMemRead(memaddrdesc, bytes, accdesc);

    if IsFault(memstatus) then
        HandleExternalReadAbort(memstatus, memaddrdesc, bytes, accdesc);
    if BigEndian(accdesc.acctype) then
        oldvalue = BigEndianReverse(oldvalue);

    boolean cmpfail = FALSE;
    case accdesc.modop of
        when MemAtomicOp_ADD    newvalue = oldvalue + operand;
        when MemAtomicOp_BIC    newvalue = oldvalue AND NOT(operand);
        when MemAtomicOp_EOR    newvalue = oldvalue EOR operand;
        when MemAtomicOp_ORR    newvalue = oldvalue OR operand;
        when MemAtomicOp_SMAX   newvalue = Max(SInt(oldvalue), SInt(operand))<size-1:0>;
        when MemAtomicOp_SMIN   newvalue = Min(SInt(oldvalue), SInt(operand))<size-1:0>;
        when MemAtomicOp_UMAX   newvalue = Max(UInt(oldvalue), UInt(operand))<size-1:0>;
        when MemAtomicOp_UMIN   newvalue = Min(UInt(oldvalue), UInt(operand))<size-1:0>;
        when MemAtomicOp_SWP    newvalue = operand;
        when MemAtomicOp_CAS    newvalue = operand; cmpfail = cmpoperand != oldvalue;

    if !cmpfail then
        if BigEndian(accdesc.acctype) then
            newvalue = BigEndianReverse(newvalue);
        memstatus = PhysMemWrite(memaddrdesc, bytes, accdesc, newvalue);
        if IsFault(memstatus) then
            HandleExternalWriteAbort(memstatus, memaddrdesc, bytes, accdesc);

    // Load operations return the old (pre-operation) value
    return oldvalue;

func main()
begin
  bits(64) address;
  bits(datasize) comparevalue;
  bits(datasize) newvalue;
  bits(datasize) data;

  AccessDescriptor accdesc = CreateAccDescAtomicOp(MemAtomicOp_CAS, acquire, release);

  comparevalue = X[s, datasize];
  newvalue = X[t, datasize];

  if n == 31 then
      CheckSPAlignment();
      address = SP[];
  else
      address = X[n, 64];

  data = MemAtomic(address, comparevalue, newvalue, accdesc);

  X[s, regsize] = ZeroExtend(data, regsize);
end

