// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    var a: (integer, integer);
    a.item0 = 10;
    return 0;
end
