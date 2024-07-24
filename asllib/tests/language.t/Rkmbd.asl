//R_KMBD: A bitvector type in a formal argument is parameter-defining for a
//parameter if its bitwidth is the value of that parameter.

// RUN: interp %s | FileCheck %s

func test{N: integer}(a: bits(N)) => bits(N)
begin
    return a;
end

func main() => integer
begin
    var a = test('1111 0000');

    return 0;
end
