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
begin       // Side effect for RHS expression   Pure?   Symbolically Evaluable?
    var x : integer;
    - = x; // ReadLocal(x, Execution, FALSE)    TRUE    FALSE
    constant cl = 10;
    let ll : integer = 20;
    - = cl; // ReadLocal(cl, Constant, TRUE)    TRUE    TRUE
    - = ll; // ReadLocal(ll, Execution, TRUE)   TRUE    TRUE
    - = cg; // ReadGlobal(cg, Constant, TRUE)   TRUE    TRUE
    - = lg; // ReadGlobal(lg, Execution, TRUE)  TRUE    TRUE
    - = cg; // ReadGlobal(cg, Execution, TRUE)  TRUE    TRUE
    - = vg; // ReadGlobal(vg, Execution, FALSE) TRUE    TRUE

            // NonDeterministic                 TRUE    FALSE
    - = ARBITRARY: integer;

            // ReadLocal(x, Execution, FALSE)   TRUE    FALSE
            // PerformsAssertions               TRUE    FALSE
    - = x as integer{0};

            // None                             TRUE   FALSE
    - = factorial(10);

             // Side effect for LHS expression  Pure?
    vg = 60; // WriteGlobal(vg)                 FALSE   FALSE
    x = 70;  // WriteLocal(x)                   FALSE   FALSE

             // Statement                       Pure?
             // ThrowException                  FALSE   FALSE
    throw MyException{-};
    assert 1 == 1; // PerformsAssertions        TRUE    FALSE

    return 0;
end;
