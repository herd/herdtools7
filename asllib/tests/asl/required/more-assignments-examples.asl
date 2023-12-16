func assignBits {N:integer, M: integer} (someWid: integer {32,64}, argN: bits(N), argM: bits(M))
begin
  // argN and argM are immutable under-constrained width bitvectors
  // assignments to them are illegal
  // legal since widths and domains match
  var eightBits: bits(8) = Zeros(8);

  // legal: someBits has undetermined width so we require RHS to be a
  // bitvector whose domain is a subset of {32,64} which it is by the
  // declaration of someWid
  // var someBits: bits({32,64}) = Zeros(someWid);

  // underconstrainedBits is a mutable under-constrained width bitvector
  // it can be assigned to
  var underconstrainedBits: bits(N);

  // underconstrainedBits has determined width `N`, so RHS must have same width
  underconstrainedBits = argN;      // legal since widths match
                                    // and domains are identical

  // underconstrainedBits = argM;      // illegal since widths do not match
  // underconstrainedBits = eightBits; // illegal since widths do not match
  // underconstrainedBits = someBits;  // illegal since widths do not match
                                       // (someWid==N may be false)

  // eightBits = underconstrainedBits; // illegal since widths do not match
  // someBits  = underconstrainedBits; // illegal since widths do not match
                                       // (someWid==N may be false)
end

func main () => integer
begin
  assignBits (32, '111', '0000');

  return 0;
end

// RUN: archex.sh --eval=':set asl=1.0' --eval=':set +syntax:aslv1_colon_colon' --eval=':load %s' --eval='assert main() == 0;' | FileCheck %s

