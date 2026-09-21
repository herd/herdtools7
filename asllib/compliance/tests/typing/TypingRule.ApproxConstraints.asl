func main() => integer
begin
    let x: integer{1..6, 5..7} = ARBITRARY: integer{1..6, 5..7};
    // The constraints 1..6 and 5..7 are overapproximated
    // by the interval [1,7].
    let y: integer{7..10, 20..30} = ARBITRARY: integer{7..10, 20..30};
    let a: integer{x..y} = ARBITRARY: integer{7};
    return 0;
end;
