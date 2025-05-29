constant A = 1 << 14;
constant B = 1 + 1 << 14;

func foo(a: integer {0..A}, b: integer{0..B})
begin
    var y: integer {-} = a * 2;

    // Illegal: the storage type for z includes precision loss.
    var z: integer {-} = b * 2;
end;
