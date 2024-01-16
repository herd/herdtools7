// RUN: interp %s | FileCheck %s

func invokedN {N: integer {8,16,32}} (x: bits(N)) => bits(N)
begin
    var myBits: bits(N);
    // The type of myBits is bits(N as {8,16,32})
    if N == 8 then // "GUARD"
        // myBits = '10101010'; // ILLEGAL
        // since the type of myBits is not of width 8
        myBits = '10101010' as bits(N); // Legal (line AS01)
        // type checker inserts execution-time check `N==8`
    else
        myBits = Zeros(N);
    end
    return myBits;
end

func test(M: integer)
begin
    var myVal: bits(M as {16,32});
    // Incurs execution-time check that (M IN {16,32})
    var myResult: bits(M as {8,16});
    // Incurs execution-time check that (M IN {8,16})
    // myResult = invokedN(myVal); // ILLEGAL
    // The return type of invokedN(myVal) is bits(M as {16,32})
    // which does not type satisfy myResult
    myResult = invokedN(myVal as bits({8,16})); // returns a bits({8,16})
    // Execution-time check that M IN {8,16}
    // Note that the only value of M which can cause this invocation is `16`
    // so the execution-time check `N==8` in invokedN at "AS01" is not executed
    // due to its `if N==8` above failing
    myVal = invokedN(myResult) as bits({16, 32});
    // Execution-time check that the returned value is IN {16,32}
    // Invocation matches signature so it type-checks
    // Note that myResult may be a bits(8),
    // so the check at line AS01 may be executed
end

func main() => integer
begin
    return 0;
end
