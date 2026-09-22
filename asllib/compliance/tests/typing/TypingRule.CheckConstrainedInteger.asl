func foo{N}(bv : bits(N))
begin
    // The next declaration is legal as a parameterized integer type
    // is a constrained integer type.
    var a : bits(N) = Zeros{N};
    let x : integer{0..N} = N as integer{0..N};
    // The next declaration is legal as the type of 'x' is a well-constrained
    // integer type, which is considered a constrained integer type.
    var b : bits(x) = Zeros{x};
    // The next declaration is legal as 5 has the type integer{5},
    // which is a constrained integer type.
    var c : bits(5) = Zeros{5};

    let y : integer = 7;

    // Only constrained integer types allowed as bitvector widths.
    // The next declaration is illegal: real is not a constrained integer type.
    // var - : bits(5.0) = Zeros{N};
    // The next declaration is illegal: integer is not a constrained integer type.
    // var - = Zeros{y};
end;
