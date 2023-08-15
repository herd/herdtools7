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
// Standard integer functions and procedures (§9.1)

// SInt
// UInt

func Min(a :: integer, b :: integer) => integer
begin
  return if a < b then a else b;
end

func Max(a :: integer, b :: integer) => integer
begin
  return if a > b then a else b;
end

func Abs(x :: integer) => integer
begin
  return if x < 0 then -x else x;
end

// Log2


//------------------------------------------------------------------------------
// Functions on reals (TODO, §9.2)

//------------------------------------------------------------------------------
// Standard bitvector functions and procedures

// For most of these functions, some implicitely dependently typed version
// exists in the specification. We do not yet support those.

// Externals

func Replicate{M}(x: bits(M), N: integer) => bits(M*N)
begin
  var r: bits(M*N) = Zeros(M*N);
  for i=0 to N-1 do
    var t: bits(M*N) = [Zeros(((N-1)-i)*M), x, Zeros(i*M)];
    r = r OR t;
  end
  return r;
end

func Len{N}(x :: bits(N)) => integer {N}
begin
  return N;
end

func BitCount{N}(x : bits(N)) => integer{0..N}
begin
  var result : integer = 0;
  for i = 0 to N-1 do
    if x[i] == '1' then
      result = result + 1;
    end
  end
  return result as integer {0..N};
end

func LowestSetBit{N}(x: bits(N)) => integer{0..N}
begin
  for i = 0 to N-1 do
    if x[i] == '1' then
      return i;
    end
  end
  return N;
end

func HighestSetBit{N}(x: bits(N)) => integer{-1..N-1}
begin
  for i = N-1 downto 0 do
    if x[i] == '1' then
      return i;
    end
  end
  return -1;
end

func Zeros(N :: integer) => bits(N)
begin
  return 0[N-1:0];
end

func Ones(N :: integer) => bits(N)
begin
  return NOT Zeros(N);
end

func IsZero(x :: bits(n)) => boolean
begin
  return BitCount(x) == 0;
end

func IsOnes{N}(x :: bits(N)) => boolean
begin
  return x == Ones(N);
end

func SignExtend(x::bits(M), N::integer) => bits(N)
begin
  return [Replicate(x[M-1], N - M), x];
end

func ZeroExtend(x :: bits(M), N::integer) => bits(N)
begin
  return [Zeros(N - M), x];
end

func Extend(x :: bits(M), N :: integer, unsigned::boolean) => bits(N)
begin
  return if unsigned then ZeroExtend(x, N) else SignExtend(x, N);
end

func CountLeadingZeroBits{N}(x :: bits(N)) => integer {0..N}
begin
  return N - 1 - HighestSetBit(x);
end

// Leading sign bits in a bitvector. Count the number of consecutive
// bits following the leading bit, that are equal to it.
func CountLeadingSignBits{N}(x::bits(N)) => integer{0..N}
begin
  return CountLeadingZeroBits(x[N-1:1] EOR x[N-2:0]);
end

// Treating input as an integer, align down to nearest multiple of 2^y.
func AlignDown{N}(x:: bits(N), y:: integer{1..N}) => bits(N)
begin
    return [x[N-1:y], Zeros(y)];
end

// Treating input as an integer, align up to nearest multiple of 2^y.
// Returns zero if the result is not representable in N bits.
func AlignUp{N}(x::bits(N), y::integer{1..N}) => bits(N)
begin
  if IsZero(x[y-1:0]) then
    return x;
  else
    return [x[N-1:y]+1, Zeros(y)];
  end
end

// The shift functions LSL, LSR, ASR and ROR accept a non-negative shift amount.
// The shift functions LSL_C, LSR_C, ASR_C and ROR_C accept a non-zero positive shift amount.

// Logical left shift
func LSL{N}(x:: bits(N), shift:: integer{0..N-1}) => bits(N)
begin
    return [x[N-shift-1:0], Zeros(shift)];
end

// Logical left shift with carry out.
func LSL_C{N}(x:: bits(N), shift:: integer{1..N-1}) => (bits(N), bit)
begin
    return (LSL(x, shift), x[N-shift]);
end

// Logical right shift, shifting zeroes into higher bits.
func LSR{N}(x:: bits(N), shift:: integer{0..N-1}) => bits(N)
begin
    return ZeroExtend(x[N-shift-1:shift], N);
end

// Logical right shift with carry out.
func LSR_C{N}(x:: bits(N), shift:: integer{1..N-1}) => (bits(N), bit)
begin
    return (LSR(x, shift), x[shift-1]);
end

// Arithmetic right shift, shifting sign bits into higher bits.
func ASR{N}(x:: bits(N), shift:: integer{0..N-1}) => bits(N)
begin
    return SignExtend(x[N-shift-1:shift], N);
end

// Arithmetic right shift with carry out.
func ASR_C{N}(x:: bits(N), shift:: integer{1..N-1}) => (bits(N), bit)
begin
    return (ASR(x, shift), x[shift-1]);
end

// Rotate right.
func ROR{N}(x:: bits(N), shift:: integer{0..N-1}) => bits(N)
begin
    return [x[0+:shift], x[N-1:shift]];
end

// Rotate right with carry out.
func ROR_C{N}(x:: bits(N), shift:: integer{1..N-1}) => (bits(N), bit)
begin
    return (ROR(x, shift), x[shift-1]);
end
