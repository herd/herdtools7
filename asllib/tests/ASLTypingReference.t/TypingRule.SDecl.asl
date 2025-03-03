func main() => integer
begin
    constant c1 : integer{1..1000} = 42;
    constant c2 = 42;
    // The next declaration is illegal as constant
    // storage elements require initialization.
    // constant c3;

    var a : integer = 42;
    var b : integer;
    var c = 42;
    let x : integer = 42;
    let z = 42;

    // The next declaration is illegal as mutable
    // storage elements require initialization.
    // let y : integer;
    return 0;
end;
