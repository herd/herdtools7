// RUN: interp %s | FileCheck %s

func test(a: integer) => integer
begin
    return a;
end

func main() => integer
begin
    var a = test(10);
    return 0;
end
