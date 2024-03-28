// RUN: interp %s | FileCheck %s

let x: integer = 10;
let y: integer = 10;
let z: integer = x + y + 10;

func main() => integer
begin
    return 0;
end
