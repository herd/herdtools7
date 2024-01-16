// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: integer = 10;
    var b: integer = a;
    return 0;
end
