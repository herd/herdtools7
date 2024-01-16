// RUN: not interp %s | FileCheck %s

var t: integer = 0;

func a() => bits(0)
begin
    t = t + 1;
    return Zeros(0);
end

func main() => integer
begin
    return 0;
end
