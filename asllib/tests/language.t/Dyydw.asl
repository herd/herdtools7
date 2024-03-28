// RUN: interp %s | FileCheck %s

let x: integer = 10;
let y: integer = x;

func main() => integer
begin
    return 0;
end
