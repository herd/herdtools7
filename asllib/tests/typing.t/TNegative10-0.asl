func negative10(N : integer {8,16}, M : integer {8,16})
begin
    // Width of bit vectors must be "statically evaluable", which is better thought of as symbolically evaluable. To make this sane and
    // avoid the need for the type system to analyse potential changes to widthN after its initialised, we require all width expressions
    // to be immutable.
    var widthN          = N;
    // <some code>
    // let testA : bits(N) = Zeros(widthN);

    // Symbolic evaluation doesn't propagate back to a common point because it doesn't handle var's.
    // This is because we don't want the type system to have to analyse any arbitary complexity code
    // that could modify the var between different reads of of the var.
    let letWidthN1               = widthN;
    // <arbitrary code>
    let letWidthN2               = widthN;
    let testB : bits(letWidthN1) = Zeros(letWidthN2); // illegal as type bits(letWidthN1) is different from bits(letWidthN2).

end;
