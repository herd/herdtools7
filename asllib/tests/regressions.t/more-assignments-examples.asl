func assignBits {N:integer, M: integer} (someWid: integer {32,64}, argN: bits(N), argM: bits(M))
begin
  // argN and argM are immutable parameterized width bitvectors
  // assignments to them are illegal
  // legal since widths and domains match
  var eightBits: bits(8) = Zeros{8};

  // underconstrainedBits is a mutable parameterized width bitvector
  // it can be assigned to
  var underconstrainedBits = Zeros {N};

  // underconstrainedBits has width `N`, so RHS must have same width
  underconstrainedBits = argN;      // legal since widths match
                                    // and domains are identical

  // underconstrainedBits = argM;      // illegal: widths do not match
  // underconstrainedBits = eightBits; // illegal: widths do not match
  // underconstrainedBits = someBits;  // illegal: widths do not match
                                       // (someWid==N may be false)

  // eightBits = underconstrainedBits; // illegal: widths do not match
  // someBits  = underconstrainedBits; // illegal: widths do not match
                                       // (someWid==N may be false)
end;

func main () => integer
begin
  assignBits{3,4}(32, '111', '0000');

  return 0;
end;

// RUN: archex.sh --eval=':set asl=1.0' --eval=':set +syntax:aslv1_colon_colon' --eval=':load %s' --eval='assert main() == 0;' | FileCheck %s
