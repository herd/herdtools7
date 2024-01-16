// RUN: interp %s | FileCheck %s

config a: integer = 10;

func main() => integer
begin
    var b: bits(a as integer{10});
    return 0;
end
