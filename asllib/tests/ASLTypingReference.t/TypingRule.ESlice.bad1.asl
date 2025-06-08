func f() => integer{0..1000}
begin
    return ARBITRARY: integer{0..1000};
end;

func BitslicesDifferentWidths()
begin
    let offset = f();
    let k: integer {3, 7} = ARBITRARY: integer{3, 7};
    var src: bits(k);
    var dst: bits(k - 1);

    // Illegal since the width of the right-hand-side is different
    // to the width of the left-hand-side.
    let w = offset;
    dst = src[w:1];
end;
