func main() => integer
begin
    let value: integer{8, 16} = ARBITRARY: integer{8, 16};

    // *Mutable* storage elements of type integer {2} are not very useful.
    var factor2 = 2; // typed as integer{2}

    // c is typed as integer {8, 16, 32}.
    var c = if (factor2 == 2) then value * 2 else value;

    let j = value * (1 + 1);

    // (1+1) is a compile-time-constant expression
    // hence the type of (1+1) is integer {2}
    // and j is of type integer {16,32}
    var factor: integer = 2; // factor is of type integer

    // Note that without the explicit type, factor would be integer{2}
    let k = value * factor; // k is of type integer
    return 0;
end;
