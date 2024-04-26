// RUN: interp %s | FileCheck %s

func a(n: integer) => bits(n)
begin
    var b: bits(n);
    return b;
end

func main() => integer
begin
    return 0;
end