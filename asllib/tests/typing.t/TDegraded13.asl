func degraded13(a : bits(64), b : bits(64))
begin
    // In this case a very large constraint set is used (bit vectors must use constrained integers for their widths). Tools are permitted to
    // have degraded behaviour in constraint handling in cases like this. For example only tracking the min/max value of the constraints. It
    // would be useful if tools issued warnings if the behaviour is degraded like this. The threshold for when degraded behaviour starts to
    // apply is implementation defined.
    let temp               = UInt(a) * UInt(b);
    let testA : bits(temp) = Zeros(temp);
end;

func main() => integer
begin
  degraded13(Zeros(64) + 1234, Zeros(64) + 4321);
  // WARNING: aslref freezes
  // degraded13(Zeros(64) + 12345, Zeros(64) + 54321);
  return 0;
end;
