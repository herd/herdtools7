// RUN: interp %s | FileCheck %s

let x: integer = 10;
let y: integer = x;
let z: integer = x + y;

func main() => integer
begin
    return 0;
end
