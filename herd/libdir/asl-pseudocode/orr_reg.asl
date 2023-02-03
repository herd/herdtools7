// Herd compatibility

_R[integer n] = bits(64) value
    write_register(n, 64, value);

bits(64) _R[integer n]
    return read_register(n, 64);


// ----------------------------------------------------------------------------
// From shared/functions/common.asl

// Zeros()
// =======

bits(N) Zeros(integer N)
    return Replicate('0',N);


// ZeroExtend()
// ============

bits(N) ZeroExtend(bits(M) x, integer N)
    assert N >= M;
    return Zeros(N-M) : x;

// LSL()
// =====

bits(N) LSL(bits(N) x, integer shift)
    assert shift >= 0;
    bits(N) result;
    if shift == 0 then
        result = x;
    else
        (result, -) = LSL_C(x, shift);
    return result;

// LSL_C()
// =======

(bits(N), bit) LSL_C(bits(N) x, integer shift)
    assert shift > 0 && shift < 256;
    extended_x = x : Zeros(shift);
    result = extended_x<N-1:0>;
    carry_out = extended_x<N>;
    return (result, carry_out);

// LSR()
// =====

bits(N) LSR(bits(N) x, integer shift)
    assert shift >= 0;
    bits(N) result;
    if shift == 0 then
        result = x;
    else
        (result, -) = LSR_C(x, shift);
    return result;

// LSR_C()
// =======

(bits(N), bit) LSR_C(bits(N) x, integer shift)
    assert shift > 0 && shift < 256;
    extended_x = ZeroExtend(x, shift+N);
    result = extended_x<(shift+N)-1:shift>;
    carry_out = extended_x<shift-1>;
    return (result, carry_out);


// ASR_C()
// =======

(bits(N), bit) ASR_C(bits(N) x, integer shift)
    assert shift > 0 && shift < 256;
    extended_x = SignExtend(x, shift+N);
    result = extended_x<(shift+N)-1:shift>;
    carry_out = extended_x<shift-1>;
    return (result, carry_out);

// ROR()
// =====

bits(N) ROR(bits(N) x, integer shift)
    assert shift >= 0;
    bits(N) result;
    if shift == 0 then
        result = x;
    else
        (result, -) = ROR_C(x, shift);
    return result;

// ROR_C()
// =======

(bits(N), bit) ROR_C(bits(N) x, integer shift)
    assert shift != 0 && shift < 256;
    m = shift MOD N;
    result = LSR(x,m) OR LSL(x,N-m);
    carry_out = result<N-1>;
    return (result, carry_out);

// IsZero()
// ========

boolean IsZero(bits(N) x)
    return x == Zeros(N);

// IsZeroBit()
// ===========

bit IsZeroBit(bits(N) x)
    return if IsZero(x) then '1' else '0';


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


// -----------------------------------------------------------------------------
// From aarch64/functions/shiftreg.asl

// ShiftType
// =========
// AArch64 register shifts.

enumeration ShiftType   {ShiftType_LSL, ShiftType_LSR, ShiftType_ASR, ShiftType_ROR};


// DecodeShift()
// =============
// Decode shift encodings

ShiftType DecodeShift(bits(2) op)
    case op of
        when '00'  return ShiftType_LSL;
        when '01'  return ShiftType_LSR;
        when '10'  return ShiftType_ASR;
        when '11'  return ShiftType_ROR;


// ShiftReg()
// ==========
// Perform shift of a register operand

bits(N) ShiftReg(integer reg, ShiftType shiftype, integer amount, integer N)
    bits(N) result = X[reg, N];
    case shiftype of
        when ShiftType_LSL result = LSL(result, amount);
        when ShiftType_LSR result = LSR(result, amount);
        when ShiftType_ASR result = ASR(result, amount);
        when ShiftType_ROR result = ROR(result, amount);
    return result;

// ----------------------------------------------------------------------------
// From aarch64/instrs/logicalop/LogicalOp

// LogicalOp
// =========
// Logical instruction types.

enumeration LogicalOp   {LogicalOp_AND, LogicalOp_EOR, LogicalOp_ORR};

// -----------------------------------------------------------------------------
// Main entrypoint
// From aarch64/instrs/integer/logical/shiftedregs/execute
// From aarch64/instrs/integer/logical/shiftedregs/decode
// From aarch64/instrs/integer/logical/decode_logical.ash
// From aarch64/instrs/integer/logical/execute_logical.ash

DecodeSpecificInstructions()
    integer d = UInt(Rd);
    integer n = UInt(Rn);
    integer m = UInt(Rm);
    integer datasize = if sf == '1' then 64 else 32;

    boolean setflags;
    LogicalOp op;
    case opc of
        when '00' op = LogicalOp_AND; setflags = FALSE;
        when '01' op = LogicalOp_ORR; setflags = FALSE;
        when '10' op = LogicalOp_EOR; setflags = FALSE;
        when '11' op = LogicalOp_AND; setflags = TRUE;

    ShiftType shift_type = DecodeShift(shift);
    integer shift_amount = UInt(imm6);
    boolean invert = (N == '1');

main()
    bits(datasize) operand1 = X[n, datasize];
    bits(datasize) operand2 = ShiftReg(m, shift_type, shift_amount, datasize);
    bits(datasize) result;

    if invert then operand2 = NOT(operand2);

    case op of
        when LogicalOp_AND result = operand1 AND operand2;
        when LogicalOp_ORR result = operand1 OR  operand2;
        when LogicalOp_EOR result = operand1 EOR operand2;

    if setflags then
        PSTATE.<N,Z,C,V> = result<datasize-1>:IsZeroBit(result):'00';

    X[d, datasize] = result;
