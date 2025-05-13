let z = ARBITRARY: integer{0..1000};
let w = ARBITRARY: integer{0..1000};

func main() => integer
begin
    var y : integer{w..2 * w + z, w + w + w} = 3 * w as
            integer{w..2 * w + z, w + w + w};
    var x : integer{3 * w,      w..w + z + w} = 3 * w as
            integer{3 * w,      w..w + z + w};

    - = x as integer{w..2 * w + z, w + w + w};
    - = x as integer{w + w + w, w..2 * w + z};
    return 0;
end;
