func pure_func(x: integer, y: integer) => integer
begin
    return x * y + y;
end;

type my_exception of exception {-};

func impure_func(x: integer, y: integer) => integer
begin
    if x == 0 then
        throw my_exception{-};
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
    var a = I;                      // Yes: 'I' is immutable.
    var b = impure_func(5, 6);      // No: 'impure_func' may throw an exception.
    var c = x;                      // No: 'x' is mutable.
    var d = M;                      // No: 'M' is mutable.

    // Only symbolically evaluable expressions whose underlying type is an
    // integer type can be used as array length expressions:
    var e : array[[pure_func(5, 6) + 9 + I]] of integer;
    // Normalization simplifies (3*I + 9) - 2*I into I+9.
    var f : array[[(3*I + 9) - 2*I]] of integer;
    // Normalization simplifies i-3 into 6.
    var g : array[[i - 3]] of integer;
    return 0;
end;
