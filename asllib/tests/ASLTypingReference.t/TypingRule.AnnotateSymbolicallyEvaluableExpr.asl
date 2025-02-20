func pure_func(x: integer, y: integer) => integer
begin
    return x * y + y;
end;

type my_exception of exception {};

func impure_func(x: integer, y: integer) => integer
begin
    if x == 0 then
        throw my_exception{};
    end;
    return x * y + y;
end;

let I = pure_func(7, 3);
var M = pure_func(7, 3);

type MyInteger of integer; // The underlying type of MyInteger is integer.

func main() => integer
begin
    // Right-hand-side expression is symbolically evaluable?
    let i : MyInteger = 9;          // Yes: literals are immutable.
    var x = pure_func(5, 6) + 9;    // Yes: 'pure_func' is side-effect-free.
    var - = I;                      // Yes: 'I' is immutable.
    var - = impure_func(5, 6);      // No: 'impure_func' may throw an exception.
    var - = x;                      // No: 'x' is mutable.
    var - = M;                      // No: 'M' is mutable.

    // Only symbolically evaluable expressions whose underlying type is an
    // integer type can be used as array length expressions:
    var - : array[[pure_func(5, 6) + 9 + I]] of integer;
    var - : array[[i - 3]] of integer;
    return 0;
end;
