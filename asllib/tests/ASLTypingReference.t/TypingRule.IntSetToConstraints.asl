func main () => integer
begin
    let N: integer{1..5} = ARBITRARY: integer{1..5};
    // 2^N is overapproximated as {2} u {4} u {8} u {16} u {32}
    // and then represented by the list of constraints {2, 4, 8, 16, 32}.
    let p = 2 as integer {1..2^N};
    return 0;
end;
