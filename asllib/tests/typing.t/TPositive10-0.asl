func foo() => integer {0..7} begin return UNKNOWN: integer {0..7}; end

func positive10(N : integer {8,16}, M : integer {8,16}, O : integer {8})
begin
    // The basic idea is expressions use in the width of a bitvector are executed (symbolically if necessary) by the type checker
    // to determine the width. This is then used to determine type satisfaction.
    let width8          = 8;
    let testA : bits(8) = Zeros(width8); // The value of width8 can be fully statically evaluated and determined to be 8
    let widthN          = N;
    let testB : bits(N) = Zeros(widthN); // The value of widthN can only be partially evaluated and requires symbolic execution, which
                                         // determines the width of widthN is N

    // The width of Zeros(letWidthFoo2) is symbolically evaluated to be letWidthFoo. NOTE: It's not varWidthFoo as the symbolic evaluation can
    // only handle immutable values, so only progresses back to letWidthFoo and not varWidthFoo.
    var varWidthFoo : integer {0..7} = foo();
    // <any arbitary code that further modifies varWidthFoo>
    let letWidthFoo                  = varWidthFoo;
    let letWidthFoo2                 = letWidthFoo;
    let testC : bits(letWidthFoo)    = Zeros(letWidthFoo2);
    // Both size of a type satisfaction undergo symbolic evaluation
    let letWidthFoo3                 = letWidthFoo;
    let testD : bits(letWidthFoo3)   = Zeros(letWidthFoo2); // Both RHS and LHS have width letWidthFoo

    let letWidthFooSub               = letWidthFoo - 1;
    let testE : bits(letWidthFoo)    = Zeros(letWidthFooSub) :: '0'; // RHS evaluates to type bits(letWidthFoo)

    let tempF1 : integer {0..7} = foo();
    let tempF2                  = tempF1;
    let tempF3A                 = tempF2;
    let tempF3B                 = tempF2;
    let testF : bits(tempF3A)   = Zeros(tempF3B); // The width of the LHS and RHS have multiple common symbols, so the type could be either
                                                  // bits(tempF2) or bits(tempF1), it doesn't actually matter if tools stop the symbolic
                                                  // evaluation at the first common point, or continue tracking backwards as far as possible.

    let tempG1 : integer {0..7} = foo();
    let tempG2                  = tempG1;
    let tempG3A                 = tempG2;
    let tempG3B                 = tempG2;
    // let testG : bits(tempG3A)   = Zeros(tempG3B) OR Zeros(tempG1); // In this case the type must be bits(tempG1) as this is the first
                                                                   // common point.

    let testH : bits(8) = Zeros(O); // as "O" only has a single allowed value, Zeros(O) evaluates to type bits(8). See R_QZJS
end
