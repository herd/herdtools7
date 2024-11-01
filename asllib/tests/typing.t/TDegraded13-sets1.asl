func degraded13 (a: bits(64), b: bits(64))
begin
  var z = UInt(a) * UInt(b);
  let K = 2 ^ 64 - 1;
  // K       is the highest number in the type of UInt(a)
  // K*K     is the highest number in the type of z
  // K*(K-1) is the second highest number that z can take.
  // The distance between K*K and K*(K-1) is K, so (K*K)-1 is not a value reachable by z.
  z = (K*K)-1; // Is this legal?
end;
