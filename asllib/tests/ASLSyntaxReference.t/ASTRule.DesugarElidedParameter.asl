func Bar{N}(bv: bits(N)) => bits(N)
begin
    return bv XOR Ones{N};
end;

func Baz{A,B}(bv: bits(A), x: integer{0..B}) => bits(A)
begin
    return bv;
end;

func main() => integer
begin
    let bv = Zeros{64};
    let res : bits(64) = Bar{}(bv); // equivalent to Bar{64}(args)
    let sz = 32;
    let a : bits(64) = Baz{,sz}(bv, sz); // equivalent to Baz{64,sz}(bv, sz);
    // let res : bits(N) = Baz{}(bv); // illegal: - only 1 parameter can be omitted

    - = Zeros{64}; // can avoid empty argument list ()
    let b : bits(64) = Zeros{}();
    let c : bits(64) = Zeros{64};
    // let - : bits(64) = Zeros{}; // illegal: parsing conflict with empty record

    - = UInt('1111'); // equivalent to UInt{4}('1111');
    let d : bits(64) = ZeroExtend{64}('11'); // equivalent to ZeroExtend{64,2}
    let e : bits(64) = ZeroExtend{}('11'); // can also elide the output parameter N
    return 0;
end;
