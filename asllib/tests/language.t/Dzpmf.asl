// RUN: interp %s | FileCheck %s

var a : integer = 10;

getter aa => integer
begin
    a = a + 1;
    return a;
end

func aaa() => integer
begin
    a = a + 2;
    return a;
end

func main() => integer
begin
    var b = a + 1;
    var bb = aa + 1;
    var bbb = aaa() + 1;

    return 0;
end
