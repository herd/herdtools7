//I_BCWW: A side-effect-free subprogram may read mutable global storage
//elements, and may call other side-effect-free subprograms. It may declare
//local variables and assign to them.

// RUN: interp %s | FileCheck %s

var globe: integer = 10;

func a() => integer
begin
    return globe;
end

func b() => integer
begin
    return a();
end

func c()
begin
    var t : integer = 10;
    t = 5;
end

func main() => integer
begin
    return 0;
end
