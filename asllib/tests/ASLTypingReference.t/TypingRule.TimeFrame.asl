constant cg = 20;
let lg = 30;
config cfg : integer{0..100} = 40;
var vg : integer = 50;

type MyException of exception;

func factorial(n: integer) => integer recurselimit 100
begin
    return if n == 0 then 1 else n * factorial(n - 1);
end;

func main() => integer
begin       // Side effect for RHS expression   Time frame
    var x : integer;
    - = x; // ReadLocal(x, Execution, FALSE)    Execution
    constant cl = 10;
    let ll : integer = 20;
    - = cl; // ReadLocal(cl, Constant, TRUE)    Constant
    - = ll; // ReadLocal(ll, Execution, TRUE)   Constant
    - = cg; // ReadGlobal(cg, Constant, TRUE)   Constant
    - = lg; // ReadGlobal(lg, Execution, TRUE)  Execution
    - = cg; // ReadGlobal(cg, Execution, TRUE)  Execution
    - = vg; // ReadGlobal(vg, Execution, FALSE) Execution

            // NonDeterministic                 Execution
    - = ARBITRARY: integer;

            // ReadLocal(x, Execution, FALSE)   Execution
            // PerformsAssertions               Constant
    - = x as integer{0};

            // RecursiveCall                    Execution
    - = factorial(10);

             // Side effect for LHS expression  Time frame
    vg = 60; // WriteGlobal(vg)                 Execution
    x = 70;  // WriteLocal(x)                   Execution

             // Statement                       Time frame
             // ThrowException                  Execution
    throw MyException{-};
    assert 1 == 1; // PerformsAssertions        Constant

    return 0;
end;
