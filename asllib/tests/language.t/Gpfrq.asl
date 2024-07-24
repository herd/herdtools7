// RUN: interp %s | FileCheck %s

config cond: boolean = TRUE;

func f{N}(x: bits(N)) => bits(N + 1)
begin
    return [x, '0'];
end

func main() => integer
begin
    var inputA: bits(4);
    var outputA = f(inputA); // an invocation of f: bits(4)=>bits(5)
    let widthB: integer {8,16} = if (cond) then 8 else 16;
    var inputB: bits(widthB);
    var outputB = f(inputB); // an invocation of f: bits({8,16})=>bits({9,17})
    // outputB is of type bits({9,17})

    return 0;
end
