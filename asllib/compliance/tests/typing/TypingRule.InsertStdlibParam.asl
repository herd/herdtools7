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
  - = SignExtend{64}('1111');
  - = ZeroExtend{64}('1111');
  - = Extend{64}('1111', TRUE);
end;

func main() => integer
begin
    var bv : bits(8);
    assert UInt(bv) == UInt{8}(bv);
    assert ZeroExtend{16, 8}(bv) == Zeros{16};
    assert ZeroExtend{16}(bv) == Zeros{16};
    return 0;
end;
