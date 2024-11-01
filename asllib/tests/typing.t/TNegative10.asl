func negative10(N : integer {8,16}, M : integer {8,16})
begin
    // Width of bit vectors must be "statically evaluable", which is better thought of as symbolically evaluable. To make this sane and
    // avoid the need for the type system to analyse potential changes to widthN after its initialised, we require all width expressions
    // to be immutable.
    var widthN          = N;
    // <some code>
    let testA : bits(N) = Zeros(widthN);

end;
