func append{N,M}(x: bits(N), y: bits(M)) => bits(N + M)
begin
    return x :: y;
end;

func main() => integer
begin
    var inputA: bits(4);
    var outputA = append{4, 4}(inputA, inputA);
    // an invocation of append: (bits(4), bits(4))=>bits(8)
    let width1: integer {8,16} = if (ARBITRARY: boolean) then 8 else 16;
    var input1: bits(width1);
    let width2: integer {8,16} = if (ARBITRARY: boolean) then 8 else 16;
    var input2: bits(width2);
    var output12 = append{width1, width2}(input1, input2);
    return 0;
end;
