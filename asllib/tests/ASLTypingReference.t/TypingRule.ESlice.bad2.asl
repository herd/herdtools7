func BitslicesDifferentWidths()
begin
    let offset: integer{0..1000} = ARBITRARY: integer{0..1000};
    let k: integer {3, 7} = ARBITRARY: integer{3, 7};
    var src: bits(k);
    var dst: bits(k -1);
    let w = offset;

    dst = src[w:1];
    // Illegal although width is a symbolically evaluable expression
    // since (w != width of dst)
end;
