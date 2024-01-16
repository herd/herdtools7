// RUN: not interp %s | FileCheck %s

var a = '1111 1111';

func test(b: integer) => integer
begin
    return a[b:0];
end

func main() => integer
begin
    return 0;
end
