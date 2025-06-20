func sameWid{N: integer {2,4,8,16}}(A: bits(N), B: bits(N)) => bits(N)
begin
    return A;
end;

func test1()
begin
    var A1, B1: bits(8);
    var C1 = sameWid{8}(A1, B1);
    // The invocation type of sameWid's return type is bits(8)
    // so `C1` has type `bits(8)`
end;

func test2(N: integer{4,8})
begin
    let wid: integer {2,4,8} = N;
    var A2: bits(N);
    var D2: bits(wid);
    A2 = D2; // legal - matching widths
    D2 = A2; // legal - matching widths
    var result = sameWid{N}(A2, D2); // legal - matching widths
    // The invocation type of sameWid's return type is bits(N)
    // so `result` has type `bits(N)`
end;
