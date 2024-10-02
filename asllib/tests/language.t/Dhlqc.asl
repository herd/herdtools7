//D_HLQC: Any expression consisting of a primitive operation on
//statically evaluable operands is a statically evaluable expression

// RUN: interp %s | FileCheck %s

let x: integer = 10;
let y: integer = 10;
let z: integer = x + y + 10;

func main() => integer
begin
    return 0;
end
