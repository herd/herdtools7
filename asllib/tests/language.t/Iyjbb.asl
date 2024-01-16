// RUN: interp %s | FileCheck %s

func CTCs(x: integer)
begin
    // var A: bit;
    // let B: integer = A as integer; // ILLEGAL
    // bit cannot be an integer
    let C: integer {8,16} = x as {8,16};
    // The execution-time check is `x IN {8,16}`
    let D: integer {8,16} = C as {8,16};
    // Type checker can determine that C is already an integer {8,16}
    // so no further check is required
end

func main() => integer
begin
    return 0;
end
