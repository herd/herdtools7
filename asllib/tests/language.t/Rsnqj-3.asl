//R_SNQJ: An expression or subexpression which may result in a zero-length 
//bitvector must not be side-effecting.
//The above rule is intended to allow implementations to transform 
//expressions to a form where zero-length bitvectors do not exist.

func R_sl()
begin
  let offset: integer = f();
  let k: integer {3, 7} = getWid();
  var src: bits(k);
  var dst: bits(k-1);

  dst = src[offset+k-2:offset]; // legal
  // but requires an execution-time check that
  // offset+k-2 < k
  // offset >= 0

  dst = src[offset+:k-1]; // legal
  // but requires an execution-time bounds check that
  // offset+k-1 <= k
  // offset >= 0

  dst = src[k:k-offset];
  // illegal (bitslice non-mutable width requirement) // since width is mutable `offset+1`
  let w = offset;
  dst =  src[w:1];
  // illegal (type-satisfaction requirement)
  // although width is a statically evaluable expression
  // since (w != width of dst)

  dst[0+:w] = src[0+:w];
  // legal
  // but requires an execution-time bounds check that:
  // max index of LHS bitslice <= max index of dst
  // and
  // max index of RHS bitslice <= max index of src

  var zw: integer{0,1,2};
  var zb = f()[0+:zw];
  // legal, as long as f() is side-effect-free

  let d = [f()[1+:zw], g()[1 +: 8]];
  // f() must not be side-effecting as it is used in a sub-expression
  // that may produce a zero-with bitvector.
  // g() may be side-effecting as the sub-expression it is in
  // produces a non-zero-width bitvector.
  // The RHS expression as a whole may be side-effecting.
end
