// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: array[3] of integer;
    a[0] = 10;

    return 0;
end
