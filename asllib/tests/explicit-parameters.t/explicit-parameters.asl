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
  - = Foo{64}();
  - = Foo{64};
  // This can combine with eliding parameters, but only as follows:
  let x : bits(64) = Foo{}();
end;


// We can elide parameters on right-hand sides
func elide_parameters()
begin
  let x : bits(4) = Foo{}();
  let y : bits(4) = Bar{,3}('111');
end;


// We can parametrise accessors
var _R : array [[31]] of bits(64);


accessor X{N}(regno: integer) <=> value: bits(N)
begin
  getter
    assert N == 64;
    assert 0 <= regno && regno <= 31;
    return _R[[regno]][0+:N];
  end;

  setter
    assert N == 64;
    assert 0 <= regno && regno <= 31;
    _R[[regno]] = value as bits(64);
  end;
end;

func good()
begin
  let x = X{64}(0);
  let y : bits(64) = X{}(1);

  X{64}(2) = Zeros{64};
end;


// Standard library functions have special treatment: we need not specify their
// input parameters
func omit_lone_parameter_single_arity()
begin
  // Explicit versions:
  - = UInt{2}('11');
  - = SInt{2}('11');
  - = Len{2}('11');
  - = BitCount{2}('11');
  - = LowestSetBit{2}('11');
  - = HighestSetBit{2}('11');
  - = IsZero{2}('11');
  - = IsOnes{2}('11');
  - = CountLeadingZeroBits{2}('11');
  - = CountLeadingSignBits{2}('11');

  // Equivalent to:
  - = UInt('11');
  - = SInt('11');
  - = Len('11');
  - = BitCount('11');
  - = LowestSetBit('11');
  - = HighestSetBit('11');
  - = IsZero('11');
  - = IsOnes('11');
  - = CountLeadingZeroBits('11');
  - = CountLeadingSignBits('11');
end;

func omit_lone_parameter_two_arity()
begin
  // Explicit versions:
  - = AlignDown{3}('111', 1);
  - = AlignUp{3}('111', 1);
  - = LSL{3}('111', 1);
  - = LSL_C{3}('111', 1);
  - = LSR{3}('111', 1);
  - = LSR_C{3}('111', 1);
  - = ASR{3}('111', 1);
  - = ASR_C{3}('111', 1);
  - = ROR{3}('111', 1);
  - = ROR_C{3}('111', 1);

  // Equivalent to:
  - = AlignDown('111', 1);
  - = AlignUp('111', 1);
  - = LSL('111', 1);
  - = LSL_C('111', 1);
  - = LSR('111', 1);
  - = LSR_C('111', 1);
  - = ASR('111', 1);
  - = ASR_C('111', 1);
  - = ROR('111', 1);
  - = ROR_C('111', 1);
end;

func omit_one_of_two_parameters()
begin
  let x : bits(64) = SignExtend{64}('1111');
  let y : bits(64) = ZeroExtend{64}('1111');
  let z : bits(64) = Extend{64}('1111', TRUE);
end;

func elide_output_parameters()
begin
  let x : bits(64) = SignExtend{}('1111');
  let y : bits(64) = ZeroExtend{}('1111');
  let z : bits(64) = Extend{}('1111', TRUE);
end;
