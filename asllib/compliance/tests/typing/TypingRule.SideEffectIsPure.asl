constant cg = 20;
let lg = 30;
config cfg : integer{0..100} = 40;
var vg : integer = 50;

type MyException of exception{-};

readonly func factorial(n: integer) => integer recurselimit 100
begin
    return if n == 0 then 1 else n * factorial(n - 1);
end;

func main() => integer
begin        // Side effects for RHS expression
    var x : integer;
    - = x;   // LocalEffect(SE_Readonly), Immutability(FALSE)
    let ll : integer = 20;
    - = ll;  // LocalEffect(SE_Readonly), Immutability(TRUE)

    - = cg;  // GlobalEffect(SE_Pure), Immutability(TRUE)
    - = lg;  // GlobalEffect(SE_Readonly), Immutability(TRUE)
    - = cfg; // GlobalEffect(SE_Readonly), Immutability(TRUE)
    - = vg;  // GlobalEffect(SE_Readonly), Immutability(FALSE)

             // LocalEffect(SE_Readonly),
             // GlobalEffect(SE_Readonly),
             // Immutability(FALSE)
    - = ARBITRARY: integer;

             // LocalEffect(SE_Readonly), Immutability(FALSE)
    - = x as integer{0};

             // GlobalEffect(SE_Readonly)
    - = factorial(10);

             // Side effect for LHS expression
    vg = 60; // GlobalEffect(SE_Impure)
    x = 70;  // LocalEffect(SE_Impure)

             // Side effects for statement
             // LocalEffect(SE_Impure), GlobalEffect(SE_Impure)
    throw MyException{-};
    assert 1 == 1; // None

    return 0;
end;
