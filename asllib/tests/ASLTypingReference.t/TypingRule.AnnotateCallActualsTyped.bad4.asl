func ones{N: integer}() => bits(N)
begin
    // N is a parameter so it is declared as a parameterized integer type.
    // The return type is therefore the
    // parameterized width bitvector of width N.
    // If N were an unconstrained integer type argument instead of being declared
    // as a parameterized integer then the following would fail since it is
    // illegal to declare `bits(expr)` if expr is an unconstrained integer.
    var ans: bits(N) = Zeros{N};
    // for any invocation, the returned bitvector's width is the actual
    // expression passed to the parameter.
    // For example:
    // If N is 1 in the invocation, then the returned value is bits(1).
    // If N is `x` in the invocation then the returned value is bits(x).
    return (NOT ans);
end;

config myWid: integer = 5;
// myWid is an unconstrained integer

func main() => integer
begin
    var arg = ones{myWid}();
    // Fails type satisfaction test since expressions passed
    // as parameters need to be constrained integers.
    return 0;
end;
