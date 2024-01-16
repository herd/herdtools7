// RUN: not interp %s | FileCheck %s

func a(n: integer{}, b: bits(n))
begin
    if (n == 4) then
        var c: bits(4) = b;
    else
        pass;
    end
end

func main() => integer
begin
    return 0;
end
