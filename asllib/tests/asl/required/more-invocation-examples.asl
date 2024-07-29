func bus {wid: integer} (arg0: bits(wid), arg1: bits(wid*2)) => bits(wid)
begin
    // When type-checking the declaration of func bus
// arg0 and arg1 are under-constrained width bitvectors
// of determined width `wid`
// Since wid is not a formal, it takes its value in an invocation from
// the width of one of the the corresponding actuals
return arg0;
end

// ------------------------------------------------------------
// Cases for invocation of function `bus`
// which has an under-constrained width bitvector formal
func legal_fun_fixed_width_actual () => bits(8)
begin
  let x: bits(8)  = Zeros(8);
  let y: bits(16) = Zeros(16);
  // bus's wid parameter takes its value as `8`
  // The invocation width of bus's arg0 is therefore `8`
  // x type satisfies arg0: bits(8)
  // The invocation width of bus's arg1 is therefore `8*2`
  // y type satisfies arg0: bits(16)
  return bus(x, y);
end

func legal_fun_underconstrained_actual (N: integer) => bits(N)
begin
  // N is a parameter, therefore it is an under-constrained integer
  var x: bits(N);
  var y: bits(N*2);
  // bus's wid parameter takes its value from the width of x
  // which is `N` which is an under-constrained integer
  // Therefore the type of arg0 with the invocation width `N` is
  // the under-constrained width bitvector of determined width `N`
  // which is type satisfied by x
  return bus(x, y);
end

// func legal_fun_constrained_actual (arg: bits({32,64})) => bits(32)
// begin
  // This invocation is OK because the actual has undetermined width
  // so the formal is treated as having undetermined width
  // and the domain of bits({32,64}) is a subset of the domain of the
  // undetermined width bitvector
  // return bus(arg, [arg,arg])[31:0];
  // return Zeros(32);
// end

// func illegal_fun_parameter_mismatch (N: integer{32,64}, M: integer{64,128})
// begin
  // var argN: bits(N);
  // var argM: bits(M);

  // Illegal invocation:
  // Either bus's wid takes its value from argN
  // in which case argM does not type satisfy arg1
  // OR bus's wid takes its value from argM
  // in which case argN does not type satisfy arg0
  // let illegal = bus(argN, argM);

  // A checked type conversion might be useful...
  // let legal = bus(argN, argM as bits(N*2));
// end

func main () => integer
begin
  let - = legal_fun_fixed_width_actual ();
  let - = legal_fun_underconstrained_actual (4);
  // let - = legal_fun_constrained_actual (Zeros(32));
  // let - = legal_fun_constrained_actual (Zeros(64));
  // illegal_fun_parameter_mismatch (32, 64);

  return 0;
end

// RUN: archex.sh --eval=':set asl=1.0' --eval=':set +syntax:aslv1_colon_colon' --eval=':load %s' --eval='assert main() == 0;' | FileCheck %s

