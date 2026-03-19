// ASL Standard Library
// ====================


// Standard integer functions
// ==========================


// UInt()
// ======
// The unsigned integer corresponding to the argument bit vector, viewed as
// unsigned. The leftmost bit is most significant, and the rightmost bit is
// least significant.

pure func UInt{N} (x: bits(N)) => integer{0..(2^N)-1}
begin
    var result: integer = 0;
    for i = 0 to N-1 do
        if x[i] == '1' then
            result = result + 2^i;
        end;
    end;
    return result as {0..2^N-1};
end;


// SInt()
// ======
// The signed integer corresponding to the argument bit vector, viewed under
// two's complement. The leftmost bit is most significant, and the rightmost
// bit is least significant.

pure func SInt{N} (x: bits(N))
  => integer{(if N == 0 then 0 else -(2^(N-1))) .. (if N == 0 then 0 else (2^(N-1))-1)}
begin
    var result: integer = UInt(x);
    if N > 0 && x[N-1] == '1' then
        result = result - 2^N;
    end;
    return result as {(if N == 0 then 0 else -(2^(N-1))) .. (if N == 0 then 0 else 2^(N-1)-1)};
end;


// Min()
// =====
// The minimum of two integers.

pure func Min(a: integer, b: integer) => integer
begin
  return if a < b then a else b;
end;


// Max()
// =====
// The maximum of two integers.

pure func Max(a: integer, b: integer) => integer
begin
  return if a > b then a else b;
end;


// Abs()
// =====
// The absolute value of an integer.

pure func Abs(x: integer) => integer
begin
  return if x < 0 then -x else x;
end;


// FloorLog2()
// ===========
// The base-2 logarithm of the positive integer argument, rounded down.

pure func FloorLog2(a: integer) => integer
begin
    assert a > 0;

    var result : integer = 0;
    var current : integer = 2;

    while a >= current looplimit 2^128 do // i.e. unbounded
        current = current * 2;
        result = result + 1;
    end;

    return result;
end;


// CeilLog2()
// ==========
// The base-2 logarithm of the positive integer argument, rounded up.

pure func CeilLog2(a: integer) => integer
begin
    assert a > 0;

    var result : integer = 0;
    var current : integer = 1;

    while a > current looplimit 2^128 do // i.e. unbounded
        current = current * 2;
        result = result + 1;
    end;

    return result;
end;


// IsEven()
// ========
// TRUE if the integer argument is even, FALSE otherwise.

pure func IsEven(a: integer) => boolean
begin
    return (a MOD 2) == 0;
end;


// IsOdd()
// =======
// TRUE if the integer argument is odd, FALSE otherwise.

pure func IsOdd(a: integer) => boolean
begin
    return (a MOD 2) == 1;
end;


// FloorPow2()
// ===========
// The largest power of 2 that is less than or equal to the positive integer
// argument.

pure func FloorPow2(x : integer) => integer
begin
    assert x > 0;
    // p2 stores twice the result until last line where it is divided by 2
    var p2 : integer = 2;
    while x >= p2 looplimit 2^128 do // i.e. unbounded
        p2 = p2 * 2;
    end;
    return p2 DIV 2;
end;


// CeilPow2()
// ==========
// The smallest power of 2 that is greater than or equal to the non-negative
// integer argument.

pure func CeilPow2(x : integer) => integer
begin
    assert x >= 0;
    if x <= 1 then return 1; end;
    return FloorPow2(x - 1) * 2;
end;


// IsPow2()
// ========
// TRUE if the integer argument is positive and a power of 2, FALSE otherwise.

pure func IsPow2(x : integer) => boolean
begin
    if x <= 0 then return FALSE; end;
    return FloorPow2(x) == CeilPow2(x);
end;


// AlignDownSize()
// ===============
// For non-negative integer argument x and positive integer argument size, the
// greatest multiple of size that is less than or equal to x.

pure func AlignDownSize(x: integer, size: integer) => integer
begin
    assert x >= 0 && size > 0;
    return (x DIVRM size) * size;
end;


// AlignUpSize()
// =============
// For non-negative integer argument x and positive integer argument size, the
// least multiple of size that is greater than or equal to x.

pure func AlignUpSize(x: integer, size: integer) => integer
begin
    assert x >= 0 && size > 0;
    return AlignDownSize(x + (size - 1), size);
end;


// IsAlignedSize()
// ===============
// For integer argument x and positive integer argument size, TRUE if x is
// aligned to a multiple of size, FALSE otherwise.

pure func IsAlignedSize(x: integer, size: integer) => boolean
begin
    assert size > 0;
    return (x MOD size) == 0;
end;


// AlignDownP2()
// =============
// For non-negative integer arguments x and p2, the greatest multiple of 2^p2
// that is less than or equal to x.

pure func AlignDownP2(x: integer, p2: integer) => integer
begin
    assert x >= 0 && p2 >= 0;
    return AlignDownSize(x, 2^p2);
end;


// AlignUpP2()
// ===========
// For non-negative integer arguments x and p2, the smallest multiple of 2^p2
// that is greater than or equal to x.

pure func AlignUpP2(x: integer, p2: integer) => integer
begin
    assert x >= 0 && p2 >= 0;
    return AlignUpSize(x, 2^p2);
end;


// IsAlignedP2()
// =============
// For integer argument x and non-negative integer argument p2, TRUE if x is
// aligned to a multiple of 2^p2, FALSE otherwise.

pure func IsAlignedP2(x: integer, p2: integer) => boolean
begin
    assert p2 >= 0;
    return IsAlignedSize(x, 2^p2);
end;


// Functions on reals
// ==================


// Real()
// ======
// The real value corresponding to the integer argument. Equivalent to
// multiplying by 1.0.

pure func Real(x: integer) => real
begin
    return x * 1.0;
end;


// RoundDown()
// ===========
// The nearest integer to the real argument, obtained by rounding down.

pure func RoundDown(x: real) => integer
begin
    let round = RoundTowardsZero(x);

    if x >= 0.0 || x == Real(round) then
        return round;
    else
        return round - 1;
    end;
end;


// RoundUp()
// =========
// The nearest integer to the real argument, obtained by rounding up.

pure func RoundUp(x: real) => integer
begin
    let round = RoundTowardsZero(x);

    if x <= 0.0 || x == Real(round) then
        return round;
    else
        return round + 1;
    end;
end;


// RoundTowardsZero()
// ==================
// The integer obtained by rounding the real argument towards zero.

pure func RoundTowardsZero(x: real) => integer
begin
    let x_pos = Abs(x);

    if x_pos < 1.0 then
        return 0;
    end;

    let log = ILog2(x_pos);
    var acc : integer = 2^log;

    for i=log-1 downto 0 do
        let next = acc + 2^i;
        if x_pos >= Real(next) then
          acc = next;
        end;
    end;

    return if x < 0.0 then -acc else acc;
end;


// Abs()
// =====
// The absolute value of a real.

pure func Abs(x: real) => real
begin
  return if x >= 0.0 then x else -x;
end;


// Max()
// =====
// The maximum of two reals.

pure func Max(a: real, b: real) => real
begin
  return if a > b then a else b;
end;


// Min()
// =====
// The minimum of two reals.

pure func Min(a: real, b: real) => real
begin
  return if a < b then a else b;
end;


// ILog2()
// =======
// The base-2 logarithm of the absolute value of the non-zero real argument,
// rounded down to an integer.

pure func ILog2(value : real) => integer
begin
    assert value != 0.0;
    var val : real = Abs(value);
    var low : integer;
    var high : integer;

    // Exponential search to find upper/lower power-of-2 exponent range
    if val >= 1.0 then
        low = 0; high = 1;
        while 2.0 ^ high <= val looplimit 2^128 do
            low = high;
            high = high * 2;
        end;
    else
        low = -1; high = 0;
        while 2.0 ^ low > val looplimit 2^128 do
            high = low;
            low = low * 2;
        end;
    end;

    // Binary search between low and high
    while low + 1 < high looplimit 2^128 do
        var mid = (low + high) DIVRM 2;
        if 2.0 ^ mid > val then
            high = mid;
        else
            low = mid;
        end;
    end;

    return low;
end;


// SqrtRounded()
// =============
// The square root of the non-negative real argument, where the positive
// integer argument gives the number of bits of precision after the leading '1'
// bit of a base-2 floating-point significand, with inexact values rounded to
// odd.
// Rounding to odd (RO) preserves any leftover fraction in the least
// significant bit so a subsequent IEEE rounding (RN/RZ/RP/RM) to a lower
// precision yields the same final result as a direct single-step rounding
// would have. It also ensures an Inexact flag is correctly signaled, as RO
// explicitly marks all inexact intermediates by setting the least significant
// bit to 1, which cannot be represented exactly when rounding to lower
// precision.

pure func SqrtRounded(value : real, fracbits : integer) => real
begin
    assert value >= 0.0 && fracbits > 0;
    if value == 0.0 then return 0.0; end;

    // Normalize value to the form 1.nnnn... x 2^exp
    var exp : integer = ILog2(value);
    var mant : real = value / (2.0 ^ exp);

    // Require value = 2.0^exp * mant, where exp is even and 1 <= mant < 4
    if exp MOD 2 != 0 then
        mant = 2.0 * mant;
        exp = exp - 1;
    end;

    // Set root to sqrt(mant) truncated to fracbits-1 bits
    var root = 1.0;
    var prec = 1.0;
    for n = 1 to fracbits - 1 do
        prec = prec / 2.0;
        if (root + prec) ^ 2 <= mant then
            root = root + prec;
        end;
    end;
    // prec == 2^(1-fracbits)

    // Final value of root is odd-rounded to fracbits bits
    if root ^ 2 < mant then
        root = root + (prec / 2.0);
    end;

    // Return sqrt(value) odd-rounded to fracbits bits
    return (2.0 ^ (exp DIV 2)) * root;
end;


// Functions on bit vectors
// ========================


// ReplicateBit()
// ==============
// The bit vector of given length obtained by replicating the argument bit.

pure func ReplicateBit{N}(isZero: boolean) => bits(N)
begin
  return if isZero then Zeros{N} else Ones{N};
end;


// Replicate()
// ===========
// The bit vector of length N that contains N DIV M copies of input bit vector
// of length M. N must be exactly divisble by M.

pure func Replicate{N,M}(x: bits(M)) => bits(N)
begin
  if M == 1 then
    return (if x[0] == '1' then Ones{N} else Zeros{N});
  else
    let items = N DIV M; // must be exact
    var result = Zeros{N};
    for i = 0 to items - 1 do
      result[i*:M] = x;
    end;
    return result;
  end;
end;


// Len()
// =====
// The length of the bit vector argument.

pure func Len{N}(x: bits(N)) => integer{N}
begin
  return N;
end;


// BitCount()
// ==========
// The number of '1' bits in the bit vector argument.

pure func BitCount{N}(x: bits(N)) => integer{0..N}
begin
  var result: integer = 0;
  for i = 0 to N-1 do
    if x[i] == '1' then
      result = result + 1;
    end;
  end;
  return result as integer{0..N};
end;


// LowestSetBit()
// ==============
// The position of the rightmost '1' bit in the bit vector argument, or its
// length all bits are '0'.

pure func LowestSetBit{N}(x: bits(N)) => integer{0..N}
begin
  for i = 0 to N-1 do
    if x[i] == '1' then
      return i as integer{0..N};
    end;
  end;
  return N as integer{0..N};
end;


// LowestSetBitNZ()
// ================
// The position of the rightmost '1' bit in the non-zero bit vector argument.

pure func LowestSetBitNZ{N}(x: bits(N)) => integer{0..N-1}
begin
    assert !IsZero(x);
    return LowestSetBit(x) as integer{0..N-1};
end;


// HighestSetBit()
// ===============
// The position of the leftmost '1' bit in the bit vector argument, or -1 if
// all bits are '0'.

pure func HighestSetBit{N}(x: bits(N)) => integer{-1..N-1}
begin
  for i = N-1 downto 0 do
    if x[i] == '1' then
      return i as integer{-1..N-1};
    end;
  end;
  return -1 as {-1..N-1};
end;


// HighestSetBitNZ()
// =================
// The position of the leftmost '1' bit in the non-zero bit vector argument.

pure func HighestSetBitNZ{N}(x: bits(N)) => integer{0..N-1}
begin
    assert !IsZero(x);
    return HighestSetBit(x) as integer{0..N-1};
end;


// Zeros()
// =======
// The bit vector of given length containing only '0' bits.

pure func Zeros{N}() => bits(N)
begin
  return 0[N-1:0];
end;


// Ones()
// ======
// The bit vector of given length containing only '1' bits.

pure func Ones{N}() => bits(N)
begin
  return NOT Zeros{N};
end;


// IsZero()
// ========
// TRUE if the bit vector argument contains only '0' bits, FALSE otherwise.

pure func IsZero{N}(x: bits(N)) => boolean
begin
  return x == Zeros{N};
end;


// IsOnes()
// ========
// TRUE if the bit vector argument contains only '1' bits, FALSE otherwise.

pure func IsOnes{N}(x: bits(N)) => boolean
begin
  return x == Ones{N};
end;


// SignExtend()
// ============
// The sign-extension of the M-bit bit vector argument to N bits, where M <= N.

pure func SignExtend{N,M}(x: bits(M)) => bits(N)
begin
  assert N >= M;
  return Replicate{N-M}(x[M-1]) :: x;
end;


// ZeroExtend()
// ============
// The zero-extension of the M-bit bit vector argument to N bits, where M <= N.

pure func ZeroExtend{N,M}(x: bits(M)) => bits(N)
begin
  assert N >= M;
  return Zeros{N - M} :: x;
end;


// Extend()
// ========
// The extension of the M-bit bit vector argument to N bits, where M <= N. If
// the Boolean argument is TRUE, then zero-extend, otherwise sign-extend.

pure func Extend{N,M}(x: bits(M), unsigned: boolean) => bits(N)
begin
  return if unsigned then ZeroExtend{N}(x) else SignExtend{N}(x);
end;


// CountLeadingZeroBits()
// ======================
// The number of consecutive leftmost bits of the bit vector argument that are
// equal to '0'.

pure func CountLeadingZeroBits{N}(x: bits(N)) => integer{0..N}
begin
  return (N - 1) - HighestSetBit(x);
end;


// CountLeadingSignBits()
// ======================
// The number of consecutive leftmost bits that are equal to the leftmost bit
// of the bit vector argument.

pure func CountLeadingSignBits{N}(x: bits(N)) => integer{0..N-1}
begin
  return CountLeadingZeroBits(x[N-1:1] XOR x[N-2:0]);
end;


// Bitvector alignment functions
// =============================


// AlignDown()
// ===========
// For bit vector argument x and positive integer y, align x down to the
// nearest multiple of 2^y when viewed as an integer.

pure func AlignDown{N}(x: bits(N), y: integer{1..N}) => bits(N)
begin
    return x[N-1:y] :: Zeros{y};
end;


// AlignUp()
// =========
// For bit vector argument x and positive integer y, align x up to the
// nearest multiple of 2^y when viewed as an integer.

pure func AlignUp{N}(x: bits(N), y: integer{1..N}) => bits(N)
begin
  if IsZero(x[y-1:0]) then
    return x;
  else
    return (x[N-1:y]+1) :: Zeros{y};
  end;
end;


// AlignDownSize()
// ===============
// For bit vector argument x and positive integer argument size,
// the greatest multiple of size that is less than or equal to x when viewed as
// an unsigned integer.

pure func AlignDownSize{N}(x: bits(N), size: integer{1..2^N}) => bits(N)
begin
    return AlignDownSize(UInt(x), size)[:N];
end;


// AlignUpSize()
// =============
// For bit vector argument x and positive integer argument size, the
// least multiple of size that is greater than or equal to x when viewed as an
// unsigned integer.

pure func AlignUpSize{N}(x: bits(N), size: integer{1..2^N}) => bits(N)
begin
    return AlignUpSize(UInt(x), size)[:N];
end;


// IsAlignedSize()
// ===============
// For bit vector argument x and positive integer argument size, TRUE if x is
// aligned to a multiple of size when viewed as an unsigned integer, FALSE
// otherwise.

pure func IsAlignedSize{N}(x: bits(N), size: integer{1..2^N}) => boolean
begin
    return IsAlignedSize(UInt(x), size);
end;


// AlignDownP2()
// =============
// For bit vector argument x and non-negative integer argument p2, the
// greatest multiple of 2^p2 that is less than or equal to x when viewed as an
// unsigned integer.

pure func AlignDownP2{N}(x: bits(N), p2: integer{0..N}) => bits(N)
begin
    if N == 0 then return x;
    elsif N == p2 then return Zeros{N};
    else return x[N-1:p2] :: Zeros{p2};
    end;
end;


// AlignUpP2()
// ===========
// For bit vector argument x and non-negative integer argument p2, the smallest
// multiple of 2^p2 that is greater than or equal to x when viewed as an
// unsigned integer.

pure func AlignUpP2{N}(x: bits(N), p2: integer{0..N}) => bits(N)
begin
    return AlignDownP2{N}(x + (2^p2 - 1), p2);
end;


// IsAlignedP2()
// =============
// For bit vector argument x and non-negative integer argument p2, TRUE if x
// is aligned to a multiple of 2^p2, FALSE otherwise.

pure func IsAlignedP2{N}(x: bits(N), p2: integer{0..N}) => boolean
begin
    if N == 0 || p2 == 0 then return TRUE; end;
    return IsZero(x[:p2]);
end;


// LSL()
// =====
// Logical left shift by a non-negative shift amount, shifting zero bits into
// rightmost bits.

pure func LSL{N}(x: bits(N), shift: integer) => bits(N)
begin
  assert shift >= 0;
  if shift < N then
    let bshift = shift as integer{0..N-1};
    return x[(N-bshift)-1:0] :: Zeros{bshift};
  else
    return Zeros{N};
  end;
end;


// LSL_C()
// =======
// Logical left shift by a positive shift amount, shifting zero bits into
// rightmost bits. Carry out, given by the single bit return value, is the last
// bit to be shifted out of leftmost bits.

pure func LSL_C{N}(x: bits(N), shift: integer) => (bits(N), bit)
begin
  assert shift > 0;
  if shift <= N then
    return (LSL{N}(x, shift), x[N-shift]);
  else
    return (Zeros{N}, '0');
  end;
end;


// LSR()
// =====
// Logical right shift by a non-negative shift amount, shifting zero bits into
// leftmost bits.

pure func LSR{N}(x: bits(N), shift: integer) => bits(N)
begin
  assert shift >= 0;
  if shift < N then
    let bshift = shift as integer{0..N-1};
    return ZeroExtend{N}(x[N-1:bshift]);
  else
    return Zeros{N};
  end;
end;


// LSR_C()
// =======
// Logical right shift by a non-negative shift amount, shifting zero bits into
// leftmost bits. Carry out, given by the single bit return value, is the last
// bit to be shifted out of rightmost bits.

pure func LSR_C{N}(x: bits(N), shift: integer) => (bits(N), bit)
begin
  assert shift > 0;
  if shift <= N then
    return (LSR{N}(x, shift), x[shift-1]);
  else
    return (Zeros{N}, '0');
  end;
end;


// ASR()
// =====
// Arithmetic right shift by a non-negative shift amount, shifting sign bits
// into leftmost bits.

pure func ASR{N}(x: bits(N), shift: integer) => bits(N)
begin
  assert shift >= 0;
  let bshift = Min(shift, N-1) as integer{0..N-1};
  return SignExtend{N}(x[N-1:bshift]);
end;


// ASR_C()
// =======
// Arithmetic right shift by a positive shift amount, shifting sign bits into
// leftmost bits. Carry out, given by the single bit return value, is the last
// bit to be shifted out of rightmost bits.

pure func ASR_C{N}(x: bits(N), shift: integer) => (bits(N), bit)
begin
  assert shift > 0;
  return (ASR{N}(x, shift), x[Min(shift-1, N-1)]);
end;


// ROR()
// =====
// Right rotation by non-negative shift amount. Rightmost bits are deleted and
// reinserted as leftmost bits.

pure func ROR{N}(x: bits(N), shift: integer) => bits(N)
begin
  assert shift >= 0;
  if N == 0 then return x; end;
  let cshift = (shift MOD N) as integer{0..N-1};
  return x[0+:cshift] :: x[N-1:cshift];
end;


// ROR_C()
// =======
// Right rotation by non-negative shift amount. Rightmost bits are deleted and
// reinserted as leftmost bits. Carry out, given by the single bit return
// value, is the leftmost bit of the result.

pure func ROR_C{N}(x: bits(N), shift: integer) => (bits(N), bit)
begin
  assert shift > 0;
  if N == 0 then return (x, '0'); end;
  let cpos = (shift-1) MOD N;
  return (ROR{N}(x, shift), x[cpos]);
end;


// ROL()
// =====
// Left rotation by non-negative shift amount. Leftmost bits are deleted and
// reinserted as rightmost bits.

pure func ROL{N}(x: bits(N), shift: integer) => bits(N)
begin
  assert shift >= 0;
  if N == 0 then return x; end;
  return ROR{N}(x, -shift MOD N);
end;


// ROL_C()
// =======
// Left rotation by non-negative shift amount. Leftmost bits are deleted and
// reinserted as rightmost bits. Carry out, given by the single bit return
// value, is the rightmost bit of the result.

pure func ROL_C{N}(x: bits(N), shift: integer) => (bits(N), bit)
begin
  assert shift > 0;
  if N == 0 then return (x, '0'); end;
  let rshift = -shift MOD N;
  return (ROR{N}(x, rshift), x[rshift]);
end;
