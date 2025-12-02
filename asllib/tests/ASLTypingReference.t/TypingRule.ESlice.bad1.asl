func BitslicesDifferentWidths()
begin
    let k: integer {3, 7} = ARBITRARY: integer{3, 7};
    var src: bits(k);
    var dst: bits(k - 1);
    let w: integer{0..1000} = ARBITRARY: integer{0..1000};

    // illegal: the width of the right-hand-side (offset) is different
    // to the width of the left-hand-side (k-1).
    dst = src[w:1];
end;
