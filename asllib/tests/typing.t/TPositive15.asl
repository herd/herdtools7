func positive15(x : integer, n: integer {0..31})
begin
    let testA : bits(8) = 0xA55A1234[7:0];
    let testB : bits(8) = 0xA55A1234[n+7:n];  // RHS is type bits(n+7 - n + 1), which reduces to bits(8), and the width expression is a
                                              // constrained integer
    let testC : bits(8) = 0xA55A1234[x +: 8]; // RHS is type bits(8), position allowed to be an unconstrained integer
    let testD : bits(8) = 0xA55A1234[x *: 8];
    let testE : bits(n) = 0xA55A1234[x *: n]; // RHS is type bits(n), which is a constrained integer

    var testF           = Zeros(32);
    testF[x *: n]       = Zeros(n);           // Same rules apply to bit slices on LHS

    let testG           = 0xA55A1234[0 +: 0]; // Zero width bit slices permitted
    let testH : bit     = 0xA55A1234[n:];

    // bit slices of bit vectors
    let testI           = Zeros(2)[n:];        // statically allowed, but may fail at runtime if n > 1
end
