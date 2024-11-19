func positive13(a : bits(64), b : bits(64), c : bits(64))
begin
    // Some operations will result in very large constraint sets. testA to testC are all constrained integers by type inference, and must
    // compile and run without any warnings.
    // NOTE: although the constraint set is massive and probably can't be handled by most tools, it's not actually used. One way of getting
    //       round the massive size of the constraint set is to handle it lazily. That way tools wouldn't need to compute the constraint set
    //       at all for cases like this.
    let testA =  UInt(a) * UInt(b);
    let testB =  SInt(a) * SInt(b);
    let testC = (SInt(a) * UInt(b)) + UInt(c);

    let testD = testA[63:0];
    let testE = testB[63:0];
    let testF = testC[63:0];
end;

func main() => integer
begin
  positive13(Zeros{64} + 123456789, Zeros{64} + 987654321, Zeros{64} + 987654321);
  return 0;
end;
