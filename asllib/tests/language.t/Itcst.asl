// RUN: interp %s | FileCheck %s

config a: integer = 1;
var b: bits(a as integer{0..100});

func main() => integer
begin
    return 0;
end
