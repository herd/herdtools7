// RUN: not interp %s | FileCheck %s

var a : integer = 10;

func sideeffect() => integer
begin
    a = a + 1;
    return a;
end

var b = a;

func main() => integer
begin
    return 0;
end
