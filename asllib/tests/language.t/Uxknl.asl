// RUN: interp %s | FileCheck %s

config a: integer{0..99999999999999999999999999999999} = 4;

func main() => integer
begin
    var b: bits(a);
    return 0;
end
