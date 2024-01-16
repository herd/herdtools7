// RUN: not interp %s | FileCheck %s

var a: integer;

func main() => integer
begin
    var a: integer;
    return 0;
end
