// RUN: not interp %s | FileCheck %s

config a: integer = 4;

func main() => integer
begin
    var b: bits(a);
    return 0;
end
