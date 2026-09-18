func main() => integer
begin
    var a: integer{1, 2, 3} = 2;
    var b: integer{2, 3, 4} = a as integer{2, 3, 4};

    let c: integer {2, 3, 4} = b as {2, 3, 4};
    // b is already an integer{2, 3, 4}.
    // The asserting type conversion is redundant but legal.
    return 0;
end;
