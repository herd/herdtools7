// RUN: interp %s | FileCheck %s

constant a : integer = 10;

getter aa => integer
begin
    return a;
end

func aaa() => integer
begin
    return a;
end

func main() => integer
begin
    var b = 10 + 1;
    var c = a + 1;
    var d = aa + 1;
    var e = aaa() + 1;

    return 0;
end
