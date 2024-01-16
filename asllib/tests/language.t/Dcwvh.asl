// RUN: interp %s | FileCheck %s

constant x: integer = 10;
constant y: integer = x;
constant z: integer = x + y;

func main() => integer
begin
    return 0;
end
