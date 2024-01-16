// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    var a: integer = 10 - 5 - 3;
    return 0;
end
