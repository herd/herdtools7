func main() => integer
begin
    let z = ARBITRARY: integer{0..1000};
    let w = ARBITRARY: integer{0..1000};

    var x : integer{3 * w} = ARBITRARY : integer{w + w + w};
    var y : integer{w..w + z + w} = ARBITRARY : integer{w..2 * w + z};
    return 0;
end;
