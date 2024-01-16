// RUN: interp %s | FileCheck %s

var a: integer;
var b = a;

func test()
begin
    var c = a;
end

func main() => integer
begin
    var d = a;
    return 0;
end
