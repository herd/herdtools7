func main() => integer
begin
    let z = ARBITRARY: integer{0..1000};
    let w = ARBITRARY: integer{0..1000};

    var x : integer{3 * w, 0..5 * z - z - 2 * z,  w + z} = w + z;
    // The constraints for `x` are internally converted to the ones below
    // for `y`.
    var y : integer{0..z*2,            z + w, w * 3} = w + z;
    y = x;
    return 0;
end;
