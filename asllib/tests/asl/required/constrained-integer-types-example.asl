type Ity of integer {2,4,8};

func tsub02()
begin
  var A: integer {2,4,8};
  var B: integer {2,4};
  // A and B have anonymous types

  A = B; // legal: FMXK clause 2
  // B = A; // illegal: domain of B's type is not a subset of domain of A's type

  var I: Ity;
  I = A; // legal: FMXK clause 2
  I = B; // legal: FMXK clause 2
  // B = I; // illegal: subtype-satisfaction fails due to domains
  A = I; // legal: FMXK clause 2
end

var gInt: integer; // unconstrained global integer

func f1()
begin
  var myInt : integer         = gInt;   // Legal
  var myIntA: integer {1..10} = myInt as integer {1..10};
  // Legal: incurs execution-time check that (myInt IN {1..10})

  var myIntB: integer {0..20} = myIntA;
  // Legal: type satisfaction due to domains (no execution-time check required)
  // myIntA = myIntB;
  // Type check fail even if smart compiler believes myIntB holds
  // a value from myInt_A since
  // `integer {0..20}` does not type satisfy
  // `integer {1..10}` due to domains
end

func wid1() => integer {8,16}
begin
  return 8; // someWid1;
end

func wid2() => integer {4,8}
begin
  return 8; // someWid2;
end

func f2()
begin
  let w1: integer {2,4,8,16} = wid1();
  // RHS is not statically evaluable so the only thing the type
  // checker can deduce is that w1==>w1
  // We do not constrain w1 based on the return type of wid1
  // since this may be intentional to avoid checked type conversions later
  // e.g. to ensure b1 has the correct type.

  let w2: integer {4,8,16}   = wid2();
  // RHS is not statically evaluable so w2==>w2
  // The set of possible widths of a bitvector must be statically evaluable.
  // All of the following are:
  var b1: bits(w1); // type is bits(w1 as {2,4,8,16})
  var b2: bits(w2); // type is bits(w2 as {4,8,16})
  // b1 = b2; // Type check fail

  // Type checker cannot determine w1==w2
  // so we require a Checked Type Conversion:
  b1 = b2 as bits(w1); // Type check PASS
  // but requires an execution-time width check that (w2==w1)
end

func main () => integer
begin
  tsub02 ();

  gInt = 3;
  f1 ();
  f2 ();

  return 0;
end

// RUN: archex.sh --eval=':set asl=1.0' --eval=':set +syntax:aslv1_colon_colon' --eval=':load %s' --eval='assert main() == 0;' | FileCheck %s

