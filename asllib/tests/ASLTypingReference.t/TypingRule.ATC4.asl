func invokedN{N: integer{8,16,32}}(x: bits(N)) => bits(N)
begin
    var myBits: bits(N);
    // The type of myBits is bits(N as {8,16,32})
    if N == 8 then // "GUARD"
        // myBits = '10101010'; // Illegal
        // since the type of myBits is not of width 8

        myBits = '10101010' as bits(N); // Legal
        // This would be a dynamic error if `N != 8` but we can safely assert
        // this due to the guard `N == 8` above.
    else
        myBits = Zeros{N};
    end;
    return myBits;
end;

func test{M: integer{8, 16, 32}}(bv: bits(M))
begin
    var myVal: bits(M as integer{16,32}); // A dynamic error if (M IN !{16,32}).
    var myResult: bits(M as integer{8,16}); // A dynamic error if (M IN !{8,16}).

    // The return type of invokedN{M}(myVal) is bits(M)
    // which type-satisfies bits(M as integer {8, 16})
    myResult = invokedN{M}(myVal);
end;
