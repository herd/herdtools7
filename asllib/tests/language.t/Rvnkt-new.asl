// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: integer{1..10} = 2;
    var b = (10 DIV a) == 5;
    return 0;
end