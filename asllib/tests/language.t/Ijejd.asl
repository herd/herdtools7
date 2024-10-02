// RUN: interp %s | FileCheck %s

func t(a: integer{0, 1}, b: integer{3, 2})
begin
    var c = '1111 1111';
    var d = c[b:a];
end

func main() => integer
begin
    return 0;
end
