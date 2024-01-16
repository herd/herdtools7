// RUN: not interp %s | FileCheck %s

let x: integer = 10;
func main() => integer
begin
    x = 5;
    return 0;
end
