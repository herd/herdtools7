// can't assign an arbitrary value to z
func negative3{N}(a: bits(N), x0: real, y0: real) => integer
begin
    var x: real = 0.0;
    var y: real = 0.0;
    var z = N;
    // some algorithm with an undeterminable loop count
    while x*x + y*y <= 2.0*2.0 do
        let xtemp = (x*x - y*y) + x0;
        y = 2.0*x*y + y0;
        x = xtemp;
        z = z + 1; // should be illegal without ATC
    end;
    let W = z;
    var bv: bits(W) = Ones(W);
    return BitCount(bv);
end;
