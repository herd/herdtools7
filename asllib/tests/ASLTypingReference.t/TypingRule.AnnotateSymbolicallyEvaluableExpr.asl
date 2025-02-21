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

func main() => integer
begin
    // Right-hand-side expression is symbolically evaluable?
    var - = 9;                      // Yes: literals are immutable
    var x = pure_func(5, 6) + 9;    // Yes: 'pure_func' is side-effect-free
    var - = I;                      // Yes: 'I' is immutable.
    var - = impure_func(5, 6);      // No: 'impure_func' may throw an exception.
    var - = x;                      // No: 'x' is mutable.
    var - = M;                      // Yes: 'M' is immutable.
    // Only symbolically evaluable expressions can be array length expressions:
    var - : array[[pure_func(5, 6) + 9 + I]] of integer;
    return 0;
end;
