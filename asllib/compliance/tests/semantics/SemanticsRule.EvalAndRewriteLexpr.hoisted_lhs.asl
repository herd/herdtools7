var a : array[[2]] of bits(8);

func main() => integer
begin
    a[[0]] = Zeros{8};
    a[[1]] = Zeros{8};

    // This assignment has the same possible results as the assignment below.
    a[[ARBITRARY : integer {0, 1}]]
        [(ARBITRARY : integer {0..7}) +: 1] = '1';

    a[[0]] = Zeros{8};
    a[[1]] = Zeros{8};

    // The equivalent form makes the one-time evaluation order explicit.
    let index_tmp = ARBITRARY : integer {0, 1};
    let start_tmp = ARBITRARY : integer {0..7};
    let length_tmp = 1;
    a[[index_tmp]][start_tmp +: length_tmp] = '1';

    return 0;
end;
