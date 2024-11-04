func foo() => integer {0..7} begin return UNKNOWN: integer {0..7}; end;

func negative10(N : integer {8,16}, M : integer {8,16})
begin
    // Width of bit vectors must be "statically evaluable", which is better thought of as symbolically evaluable. To make this sane and
    // avoid the need for the type system to analyse potential changes to widthN after its initialised, we require all width expressions
    // to be immutable.
    var widthN          = N;
    // <some code>
    // let testA : bits(N) = Zeros{widthN};

    // Symbolic evaluation doesn't propagate back to a common point because it doesn't handle var's.
    // This is because we don't want the type system to have to analyse any arbitary complexity code
    // that could modify the var between different reads of of the var.
    let letWidthN1               = widthN;
    // <arbitrary code>
    let letWidthN2               = widthN;
    // let testB : bits(letWidthN1) = Zeros{letWidthN2}; // illegal as type bits(letWidthN1) is different from bits(letWidthN2).

    // Even though the widths used in both LHS and RHS are immutable constrained integers (tempC3A and tempC3B), and they are both
    // derived from a common immutable constrained integer (tempC1). The following is illegal as the chain of immutability is broken
    // at "var tempC2B".
    let tempC1 : integer {0..7} = foo();
    let tempC2A                 = tempC1;
    var tempC2B                 = tempC1;
    let tempC3A                 = tempC2A;
    let tempC3B                 = tempC2B;
    let testC : bits(tempC3A)   = Zeros{tempC3B}; // illegal, type bits(tempC1) != bits(tempC3B)
end;
