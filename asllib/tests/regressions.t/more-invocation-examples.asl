func bus {wid: integer} (arg0: bits(wid), arg1: bits(wid*2)) => bits(wid)
begin
    // When type-checking the declaration of func bus
// arg0 and arg1 are parameterized width bitvectors
// of width `wid`
// Since wid is not a formal, it takes its value in an invocation from
// the width of one of the the corresponding actuals
return arg0;
end;

// ------------------------------------------------------------
// Cases for invocation of function `bus`
// which has a parameterized width bitvector formal
func legal_fun_fixed_width_actual () => bits(8)
begin
  let x: bits(8)  = Zeros{8};
  let y: bits(16) = Zeros{16};
  // bus's wid parameter takes its value as `8`
  // The invocation width of bus's arg0 is therefore `8`
  // x type satisfies arg0: bits(8)
  // The invocation width of bus's arg1 is therefore `8*2`
  // y type satisfies arg0: bits(16)
  return bus{8}(x, y);
end;

func legal_fun_underconstrained_actual {N}() => bits(N)
begin
  // N is a parameter, therefore it is a parameterized integer
  var x = Zeros {N};
  var y = Zeros {N*2};
  // bus's wid parameter takes its value from the width of x
  // which is `N` which is a parameterized integer
  // Therefore the type of arg0 with the invocation width `N` is
  // the bitvector whose width is the parameterized integer `N`
  // which is type satisfied by x
  return bus{N}(x, y);
end;

func main () => integer
begin
  - = legal_fun_fixed_width_actual ();
  - = legal_fun_underconstrained_actual {4};
  // - = legal_fun_constrained_actual (Zeros{32});
  // - = legal_fun_constrained_actual (Zeros{64});
  // illegal_fun_parameter_mismatch (32, 64);

  return 0;
end;

// RUN: archex.sh --eval=':set asl=1.0' --eval=':set +syntax:aslv1_colon_colon' --eval=':load %s' --eval='assert main() == 0;' | FileCheck %s
