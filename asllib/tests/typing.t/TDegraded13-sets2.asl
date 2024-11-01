func degraded13 (a: bits(64), b: bits(64))
begin
  var z = UInt(a) * UInt(b);
  z = (2 ^ 107) - 1;
  // This is a prime number according to https://www.mersenne.org/primes/
end;

