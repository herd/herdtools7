//------------------------------------------------------------------------------
//
//                             ASL standard lib
//
//-----------------------------------------------------------------------------


//------------------------------------------------------------------------------
// Externals

// UInt
// SInt

//------------------------------------------------------------------------------
// Standard integer functions and procedures

// SInt
// UInt

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

// Log2

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

//------------------------------------------------------------------------------
// Functions on reals

// Convert integer to rational value.
// func Real(x: integer) => real;

// Nearest integer, rounding towards negative infinity.
// func RoundDown(x: real) => integer;

// Nearest integer, rounding towards positive infinity.
// func RoundUp(x: real) => integer;

// Nearest integer, rounding towards zero.
// func RoundTowardsZero(x: real) => integer;

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
    while low <= high looplimit 2^128 do
        var mid = (low + high) DIVRM 2;
        if 2.0 ^ mid > val then
            high = mid - 1;
        else
            low = mid + 1;
        end;
    end;

    return high;
end;

// Calculate the square root of x to sf binary digits.
// The second tuple element of the return value is TRUE if the result is
// inexact, else FALSE.
func SqrtRoundDown(x: real, sf: integer) => (real, boolean)
begin
  assert x > 0.0 && sf > 0;

  // Following https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Heron's_method

  // Initial guess
  let x0 = x;

  let precision = 1.0 / (2.0 ^ sf);

  var xn: real = x0;
  while Abs(x - xn * xn) > precision looplimit 1000 do
    xn = (xn + x / xn) / 2.0 ;
  end;
  let root = xn;

  let inexact = x != root * root;

  return (root, inexact);
end;

//------------------------------------------------------------------------------
// Standard bitvector functions and procedures

// For most of these functions, some implicitely dependently typed version
// exists in the specification. We do not yet support those.

// Externals

func ReplicateBit{N}(isZero: boolean) => bits(N)
begin
  return if isZero then Zeros{N} else Ones{N};
end;

// Returns a bit vector of width N, containing (N DIV M) copies of input bit
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

func LowestSetBit{N}(x: bits(N)) => integer{0..N}
begin
  for i = 0 to N-1 do
    if x[i] == '1' then
      return i as integer{0..N};
    end;
  end;
  return N as integer {0..N};
end;

func HighestSetBit{N}(x: bits(N)) => integer{-1..N-1}
begin
  for i = N-1 downto 0 do
    if x[i] == '1' then
      return i as integer {-1..N-1};
    end;
  end;
  return -1 as {-1..N-1};
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
    return x[N-1:y]+1 :: Zeros{y};
  end;
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

// Rotate right.
// This function shifts by [shift] bits to the right, the bits deleted are
// reinserted on the left. This makes it operate effectively modulo N.
func ROR{N}(x: bits(N), shift: integer) => bits(N)
begin
  assert shift >= 0;
  let cshift = (shift MOD N) as integer{0..N-1};
  return x[0+:cshift] :: x[N-1:cshift];
end;

// Rotate right with carry out.
// As ROR, the function effectively operates modulo N.
func ROR_C{N}(x: bits(N), shift: integer) => (bits(N), bit)
begin
  assert shift > 0;
  let cpos = (shift-1) MOD N;
  return (ROR{N}(x, shift), x[cpos]);
end;
