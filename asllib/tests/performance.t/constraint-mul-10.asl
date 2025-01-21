constant A = 1 << 13;
constant B = 1 << 13;

func myfunction(a : integer{0..A}, b : integer{0..B})
begin
    let n = a DIVRM b;     // 10 DIVRM 3 == 3
    var b1 = n * b;
    b1 = 4;
    b1 = A * B;
    b1 = (A * B) - 1; // Test if discrete or interval representation
end;

func main() => integer
begin
    myfunction(0, 1);
    myfunction(A, 1);
    myfunction(A DIVRM 2, B DIVRM 4);
    return 0;
end;


