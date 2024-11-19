// Helper functions
func Foo{N}() => bits(N)
begin
  return Zeros{N};
end;

func Bar{N,M}(x: bits(M)) => bits(N)
begin
  return Zeros{N};
end;

func main() => integer
begin
  return 0;
end;

/*----------*/

// Parameters are declared as they appear textually left-to-right in return
// type then argument types
func ParameterOrder{A,B,C,D,E}(x: bits(D), y: bits(E)) => bits(A * B + C)
begin
  return Zeros{A * B + C};
end;


// Parameters must be used
func GoodInput{N}(x: bits(N)) => integer
begin
  return 0;
end;

func GoodOutput{N}(x: integer) => bits(N)
begin
  return Zeros{N};
end;

func GoodInputOutput{N}(x: bits(N)) => bits(N)
begin
  return x;
end;


// We do not need to provide () at a call site if we have explicit parameters
func elide_empty_argument_list()
begin
  let - = Foo{64}();
  let - = Foo{64};
  // This can combine with eliding parameters, but only as follows:
  let - : bits(64) = Foo{}();
end;


// We can elide parameters on right-hand sides
func elide_parameters()
begin
  let - : bits(4) = Foo{}();
  let - : bits(4) = Bar{,3}('111');
end;


// We can parametrise accessors
var _R : array [31] of bits(64);

getter X{N}(regno: integer) => bits(N)
begin
  assert N == 64;
  assert 0 <= regno && regno <= 31;
  return _R[[regno]][0+:N];
end;

setter X{N}(regno: integer) = value : bits(N)
begin
  assert N == 64;
  assert 0 <= regno && regno <= 31;
  _R[[regno]] = value as bits(64);
end;

func good()
begin
  let - = X{64}(0);
  let - : bits(64) = X{}(1);

  X{64}(2) = Zeros{64};
end;


// Standard library functions have special treatment: we need not specify their
// input parameters
func omit_lone_parameter_single_arity()
begin
  // Explicit versions:
  let - = UInt{2}('11');
  let - = SInt{2}('11');
  let - = Len{2}('11');
  let - = BitCount{2}('11');
  let - = LowestSetBit{2}('11');
  let - = HighestSetBit{2}('11');
  let - = IsZero{2}('11');
  let - = IsOnes{2}('11');
  let - = CountLeadingZeroBits{2}('11');
  let - = CountLeadingSignBits{2}('11');

  // Equivalent to:
  let - = UInt('11');
  let - = SInt('11');
  let - = Len('11');
  let - = BitCount('11');
  let - = LowestSetBit('11');
  let - = HighestSetBit('11');
  let - = IsZero('11');
  let - = IsOnes('11');
  let - = CountLeadingZeroBits('11');
  let - = CountLeadingSignBits('11');
end;

func omit_lone_parameter_two_arity()
begin
  // Explicit versions:
  let - = AlignDown{3}('111', 1);
  let - = AlignUp{3}('111', 1);
  let - = LSL{3}('111', 1);
  let - = LSL_C{3}('111', 1);
  let - = LSR{3}('111', 1);
  let - = LSR_C{3}('111', 1);
  let - = ASR{3}('111', 1);
  let - = ASR_C{3}('111', 1);
  let - = ROR{3}('111', 1);
  let - = ROR_C{3}('111', 1);

  // Equivalent to:
  let - = AlignDown('111', 1);
  let - = AlignUp('111', 1);
  let - = LSL('111', 1);
  let - = LSL_C('111', 1);
  let - = LSR('111', 1);
  let - = LSR_C('111', 1);
  let - = ASR('111', 1);
  let - = ASR_C('111', 1);
  let - = ROR('111', 1);
  let - = ROR_C('111', 1);
end;

func omit_one_of_two_parameters()
begin
  let - : bits(64) = SignExtend{64}('1111');
  let - : bits(64) = ZeroExtend{64}('1111');
  let - : bits(64) = Extend{64}('1111', TRUE);
end;

func elide_output_parameters()
begin
  let - : bits(64) = SignExtend{}('1111');
  let - : bits(64) = ZeroExtend{}('1111');
  let - : bits(64) = Extend{}('1111', TRUE);
end;
