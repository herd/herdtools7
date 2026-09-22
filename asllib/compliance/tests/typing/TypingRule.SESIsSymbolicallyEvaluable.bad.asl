func returns_bitvector_symbolically_evaluable() => bits(10)
begin
    let x : integer{0..10} = 10;
    var bv: bits(x) = ARBITRARY: bits(x);
    // The type of bv is type bits(x) because x is immutable.
    return bv;
end;

func returns_bitvector_not_symbolically_evaluable() => bits(10)
begin
    var x : integer{0..10} = 9;
    var bv: bits(x) = ARBITRARY: bits(x);
    x = 10;
    // What is the type of bv here? It's certainly not bits(x).
    return bv;
end;
