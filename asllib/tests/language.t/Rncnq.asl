// RUN: interp %s | FileCheck %s

func a(n: integer{})
begin
    var b: bits(n);
end

func main() => integer
begin
    return 0;
end
