// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    var a: integer = 0.0;
    return 0;
end
