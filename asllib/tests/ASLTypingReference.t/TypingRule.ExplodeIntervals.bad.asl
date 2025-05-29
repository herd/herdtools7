constant A = 1 << 14;
constant B = 1 + 1 << 14;
constant C = -(1 << 12);

func foo(a: integer {C..0, 0..A}, b: integer{C..0, 0..B})
begin
    var y: integer {-} = a * 2;

    // Illegal: the storage type for z includes precision loss.
    var z: integer {-} = b * 2;
end;
