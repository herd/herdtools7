// RUN: interp %s | FileCheck %s

var a: integer = 10;

func test()
begin
    a = a + 10;
end

type ty_test of array[10] of integer;

func main() => integer
begin
    return 0;
end
