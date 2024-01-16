// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: integer = 10;
    a = 4;
    return 0;
end
