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
