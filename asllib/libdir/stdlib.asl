//------------------------------------------------------------------------------
//
//                             ASL standard lib
//
//-----------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Standard integer functions

// UInt()
// ======
// Convert a bitvector to an unsigned integer, where bit 0 is LSB.

func UInt{N} (x: bits(N)) => integer{0..2^N-1}
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
// Convert a 2s complement bitvector to a signed integer.

func SInt{N} (x: bits(N))
  => integer{(if N == 0 then 0 else -(2^(N-1))) .. (if N == 0 then 0 else 2^(N-1)-1)}
begin
    var result: integer = UInt(x);
    if N > 0 && x[N-1] == '1' then
        result = result - 2^N;
    end;
    return result as {(if N == 0 then 0 else -(2^(N-1))) .. (if N == 0 then 0 else 2^(N-1)-1)};
end;

func Min(a: integer, b: integer) => integer
begin
  return if a < b then a else b;
end;

func Max(a: integer, b: integer) => integer
begin
  return if a > b then a else b;
end;

func Abs(x: integer) => integer
begin
  return if x < 0 then -x else x;
end;

// FloorLog2()
// ======
// Calculate the logarithm base 2 of the input integer, rounded down.

func FloorLog2(a: integer) => integer
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
// Calculate the logarithm base 2 of the input integer, rounded up.

func CeilLog2(a: integer) => integer
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

// Return true if integer is even (0 modulo 2).
func IsEven(a: integer) => boolean
begin
    return (a MOD 2) == 0;
end;

// Return true if integer is odd (1 modulo 2).
func IsOdd(a: integer) => boolean
begin
    return (a MOD 2) == 1;
end;

// FloorPow2()
// ===========
// For a strictly positive integer x, returns the largest power of 2 that is
// less than or equal to x

func FloorPow2(x : integer) => integer
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
// For a positive integer x, returns the smallest power of 2 that is greater or
// equal to x.

func CeilPow2(x : integer) => integer
begin
    assert x >= 0;
    if x <= 1 then return 1; end;
    return FloorPow2(x - 1) * 2;
end;

// IsPow2()
// ========
// Return TRUE if integer X is positive and a power of 2. Otherwise,
// return FALSE.

func IsPow2(x : integer) => boolean
begin
    if x <= 0 then return FALSE; end;
    return FloorPow2(x) == CeilPow2(x);
end;

// AlignDownSize()
// ===============
// For a non-negative integer x and positive integer size, returns the greatest
// multiple of size that is less than or equal to x.

func AlignDownSize(x: integer, size: integer) => integer
begin
    assert size > 0;
    return (x DIVRM size) * size;
end;

// AlignUpSize()
// =============
// For a non-negative integer x and positive integer size, returns the smallest
// multiple of size that is greater than or equal to x.

func AlignUpSize(x: integer, size: integer) => integer
begin
    assert size > 0;
    return AlignDownSize(x + (size - 1), size);
end;

// IsAlignedSize()
// ===============
// Return TRUE if integer x is aligned to a multiple of size (not necessarily a
// power of 2). Otherwise, return FALSE.

func IsAlignedSize(x: integer, size: integer) => boolean
begin
    assert size > 0;
    return (x MOD size) == 0;
end;

// AlignDownP2()
// =============
// For non-negative integers x and p2, returns the greatest multiple of 2^p2
// that is less than or equal to x.

func AlignDownP2(x: integer, p2: integer) => integer
begin
    assert p2 >= 0;
    return AlignDownSize(x, 2^p2);
end;

// AlignUpP2()
// ===========
// For non-negative integers x and p2, returns the smallest multiple of 2^p2
// that is greater than or equal to x.

func AlignUpP2(x: integer, p2: integer) => integer
begin
    assert p2 >= 0;
    return AlignUpSize(x, 2^p2);
end;

// IsAlignedP2()
// =============
// Return TRUE if integer x is aligned to a multiple of 2^p2.
// Otherwise, return FALSE.

func IsAlignedP2(x: integer, p2: integer) => boolean
begin
    assert p2 >= 0;
    return IsAlignedSize(x, 2^p2);
end;

//------------------------------------------------------------------------------
// Functions on reals

// Real()
// ======
// Convert an integer to a rational value.

func Real(x: integer) => real
begin
    return x * 1.0;
end;

// RoundDown()
// ===========
// Round a rational value down to the nearest integer
// (round towards negative infinity).

func RoundDown(x: real) => integer
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
// Round a rational value up to the nearest integer
// (round towards positive infinity).

func RoundUp(x: real) => integer
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
// Round a rational value towards zero.

func RoundTowardsZero(x: real) => integer
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

// Absolute value.
func Abs(x: real) => real
begin
  return if x >= 0.0 then x else -x;
end;

// Maximum of reals.
func Max(a: real, b: real) => real
begin
  return if a>b then a else b;
end;

// Minimum of reals.
func Min(a: real, b: real) => real
begin
  return if a<b then a else b;
end;

// ILog2()
// Return floor(log2(VALUE))

func ILog2(value : real) => integer
begin
    assert value > 0.0;
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
// Compute square root of VALUE with FRACBITS bits of precision after
// the leading 1, rounding inexact values to Odd

// Round to Odd (RO) preserves any leftover fraction in the least significant
// bit (LSB) so a subsequent IEEE rounding (RN/RZ/RP/RM) to a lower precision
// yields the same final result as a direct single-step rounding would have. It
// also ensures an Inexact flag is correctly signaled, as RO explicitly marks
// all inexact intermediates by setting the LSB to 1, which cannot be
// represented exactly when rounding to lower precision.

func SqrtRounded(value : real, fracbits : integer) => real
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

//------------------------------------------------------------------------------
// Standard bitvector functions and procedures

// For most of these functions, some implicitly dependently typed version
// exists in the specification. We do not yet support those.

// Externals

func ReplicateBit{N}(isZero: boolean) => bits(N)
begin
  return if isZero then Zeros{N} else Ones{N};
end;

// Returns a bitvector of width N, containing (N DIV M) copies of input bit
// vector x of width M. N must be exactly divisible by M.
func Replicate{N,M}(x: bits(M)) => bits(N)
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

func Len{N}(x: bits(N)) => integer {N}
begin
  return N;
end;

func BitCount{N}(x: bits(N)) => integer{0..N}
begin
  var result: integer = 0;
  for i = 0 to N-1 do
    if x[i] == '1' then
      result = result + 1;
    end;
  end;
  return result as integer {0..N};
end;

// LowestSetBit
// ============
// Returns the position of the lowest 1 bit in a bitvector, and the length of
// the bitvector if it constains only 0 bits.


func LowestSetBit{N}(x: bits(N)) => integer{0..N}
begin
  for i = 0 to N-1 do
    if x[i] == '1' then
      return i as integer{0..N};
    end;
  end;
  return N as integer {0..N};
end;

// LowestSetBitNZ
// ==============
// Returns the position of the lowest 1 bit in a bitvector.
// An assertion checks that the bitvector contains at least one 1 bit.

func LowestSetBitNZ{N}(x: bits(N)) => integer{0..N-1}
begin
    assert !IsZero(x);
    return LowestSetBit(x) as integer{0..N-1};
end;

// HighestSetBit
// =============
// Returns the position of the highest 1 bit in a bitvector, and -1 if all
// bits are 0.

func HighestSetBit{N}(x: bits(N)) => integer{-1..N-1}
begin
  for i = N-1 downto 0 do
    if x[i] == '1' then
      return i as integer {-1..N-1};
    end;
  end;
  return -1 as {-1..N-1};
end;

// HighestSetBitNZ
// ===============
// Returns the position of the highest 1 bit in a bitvector.
// An assertion checks that there is at least one 1 bit in the bitvector.

func HighestSetBitNZ{N}(x: bits(N)) => integer{0..N-1}
begin
    assert !IsZero(x);
    return HighestSetBit(x) as integer{0..N-1};
end;

func Zeros{N}() => bits(N)
begin
  return 0[N-1:0];
end;

func Ones{N}() => bits(N)
begin
  return NOT Zeros{N};
end;

func IsZero{N}(x: bits(N)) => boolean
begin
  return x == Zeros{N};
end;

func IsOnes{N}(x: bits(N)) => boolean
begin
  return x == Ones{N};
end;

func SignExtend {N,M} (x: bits(M)) => bits(N)
begin
  assert N >= M;
  return Replicate{N-M}(x[M-1]) :: x;
end;

func ZeroExtend {N,M} (x: bits(M)) => bits(N)
begin
  assert N >= M;
  return Zeros{N - M} :: x;
end;

func Extend {N,M} (x: bits(M), unsigned: boolean) => bits(N)
begin
  return if unsigned then ZeroExtend{N}(x) else SignExtend{N}(x);
end;

func CountLeadingZeroBits{N}(x: bits(N)) => integer {0..N}
begin
  return N - 1 - HighestSetBit(x);
end;

// Leading sign bits in a bitvector. Count the number of consecutive
// bits following the leading bit, that are equal to it.
func CountLeadingSignBits{N}(x: bits(N)) => integer{0..N-1}
begin
  return CountLeadingZeroBits(x[N-1:1] XOR x[N-2:0]);
end;

// Treating input as an integer, align down to nearest multiple of 2^y.
func AlignDown{N}(x: bits(N), y: integer{1..N}) => bits(N)
begin
    return x[N-1:y] :: Zeros{y};
end;

// Treating input as an integer, align up to nearest multiple of 2^y.
// Returns zero if the result is not representable in N bits.
func AlignUp{N}(x: bits(N), y: integer{1..N}) => bits(N)
begin
  if IsZero(x[y-1:0]) then
    return x;
  else
    return (x[N-1:y]+1) :: Zeros{y};
  end;
end;

// Bitvector alignment functions
// =============================

// AlignDownSize()
// ===============
// A variant of AlignDownSize where the bitvector x is viewed as an unsigned
// integer and the resulting integer is represented by its first N bits.

func AlignDownSize{N}(x: bits(N), size: integer {1..2^N}) => bits(N)
begin
    return AlignDownSize(UInt(x), size)[:N];
end;

// AlignUpSize()
// =============
// A variant of AlignUpSize where the bitvector x is viewed as an unsigned
// integer and the resulting integer is represented by its first N bits.

func AlignUpSize{N}(x: bits(N), size: integer {1..2^N}) => bits(N)
begin
    return AlignUpSize(UInt(x), size)[:N];
end;

// IsAlignedSize()
// ===============
// Return TRUE if when viewed as an unsigned integer, bitvector x is aligned
// to a multiple of size (not necessarily a power of 2).
// Otherwise, return FALSE.

func IsAlignedSize{N}(x: bits(N), size: integer {1..2^N}) => boolean
begin
    return IsAlignedSize(UInt(x), size);
end;

// AlignDownP2()
// =============
// A variant of AlignDownP2 where the bitvector x is viewed as an unsigned
// integer and the resulting integer is represented by its first N bits.

func AlignDownP2{N}(x: bits(N), p2: integer {0..N}) => bits(N)
begin
    if N == 0 then return x; end;
    return x[N-1:p2] :: Zeros{p2};
end;

// AlignUpP2()
// ===========
// A variant of AlignUpP2 where the bitvector x is viewed as an unsigned
// integer and the resulting integer is represented by its first N bits.

func AlignUpP2{N}(x: bits(N), p2: integer {0..N}) => bits(N)
begin
    return AlignDownP2{N}(x + (2^p2 - 1), p2);
end;

// IsAlignedP2()
// =============
// Return TRUE if when viewed as an unsigned integer, bitvector x is aligned
// to a multiple of 2^p2. Otherwise, return FALSE.

func IsAlignedP2{N}(x: bits(N), p2: integer {0..N}) => boolean
begin
    if N == 0 || p2 == 0 then return TRUE; end;
    return IsZero(x[:p2]);
end;

// The shift functions LSL, LSR, ASR and ROR accept a non-negative shift amount.
// The shift functions LSL_C, LSR_C, ASR_C and ROR_C accept a non-zero positive shift amount.

// Logical left shift
func LSL{N}(x: bits(N), shift: integer) => bits(N)
begin
  assert shift >= 0;
  if shift < N then
    let bshift = shift as integer{0..N-1};
    return x[(N-bshift)-1:0] :: Zeros{bshift};
  else
    return Zeros{N};
  end;
end;

// Logical left shift with carry out.
func LSL_C{N}(x: bits(N), shift: integer) => (bits(N), bit)
begin
  assert shift > 0;
  if shift <= N then
    return (LSL{N}(x, shift), x[N-shift]);
  else
    return (Zeros{N}, '0');
  end;
end;

// Logical right shift, shifting zeroes into higher bits.
func LSR{N}(x: bits(N), shift: integer) => bits(N)
begin
  assert shift >= 0;
  if shift < N then
    let bshift = shift as integer{0..N-1};
    return ZeroExtend{N}(x[N-1:bshift]);
  else
    return Zeros{N};
  end;
end;

// Logical right shift with carry out.
func LSR_C{N}(x: bits(N), shift: integer) => (bits(N), bit)
begin
  assert shift > 0;
  if shift <= N then
    return (LSR{N}(x, shift), x[shift-1]);
  else
    return (Zeros{N}, '0');
  end;
end;

// Arithmetic right shift, shifting sign bits into higher bits.
func ASR{N}(x: bits(N), shift: integer) => bits(N)
begin
  assert shift >= 0;
  let bshift = Min(shift, N-1) as integer{0..N-1};
  return SignExtend{N}(x[N-1:bshift]);
end;

// Arithmetic right shift with carry out.
func ASR_C{N}(x: bits(N), shift: integer) => (bits(N), bit)
begin
  assert shift > 0;
  return (ASR{N}(x, shift), x[Min(shift-1, N-1)]);
end;

// ROR()
// =====
// Rotate right by [shift] bits. The bits are deleted on the right and
// reinserted on the left, so the operation is effectively modulo N.

func ROR{N}(x: bits(N), shift: integer) => bits(N)
begin
  assert shift >= 0;
  if N == 0 then return x; end;
  let cshift = (shift MOD N) as integer{0..N-1};
  return x[0+:cshift] :: x[N-1:cshift];
end;


// ROR_C()
// =======
// Rotate right with carry out.
// As with [ROR], the operation is effectively modulo N.
// The carry bit is equal to the MSB of the result.

func ROR_C{N}(x: bits(N), shift: integer) => (bits(N), bit)
begin
  assert shift > 0;
  if N == 0 then return (x, '0'); end;
  let cpos = (shift-1) MOD N;
  return (ROR{N}(x, shift), x[cpos]);
end;

// ROL()
// =====
// Rotate left by [shift] bits.
// This corresponds to a right rotation by [-shift] bits.

func ROL{N}(x: bits(N), shift: integer) => bits(N)
begin
  assert shift >= 0;
  if N == 0 then return x; end;
  return ROR{N}(x, -shift MOD N);
end;

// ROL_C()
// =======
// Rotate left with carry out.
// The carry bit is equal to the LSB of the result.

func ROL_C{N}(x: bits(N), shift: integer) => (bits(N), bit)
begin
  assert shift > 0;
  if N == 0 then return (x, '0'); end;
  let rshift = -shift MOD N;
  return (ROR{N}(x, rshift), x[rshift]);
end;
