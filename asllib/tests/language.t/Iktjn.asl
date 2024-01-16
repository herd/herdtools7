// RUN: not interp %s | FileCheck %s

func sameWid {N: integer {2,4,8,16}} (A: bits(N), B: bits(N)) => bits(N)
begin
    return A;
end

func test1()
begin
    var A1, B1: bits(8);
    var C1 = sameWid(A1, B1);
    // The invocation type of sameWid's return type is bits(8)
    // so `C1` has type `bits(8)`
end

func test2(N: integer{4,8})
begin
    let wid: integer {2,4,8} = N; // A little unusual...
    var A2: bits(N); // bits(N as {4,8})
    var D2: bits(wid); // bits(wid as {2,4,8})
    A2 = D2; // legal - matching determined widths
    D2 = A2; // legal - matching determined widths
    // Although A2 and D2 have the same width, they have different
    // constraints so the following is illegal
    var result = sameWid(A2, D2);
    // If it were not illegal then the return type could not be determined
    // and is either bits(N as {4,8}) or bits(wid as {2,4,8})
end

func main() => integer
begin
    return 0;
end
