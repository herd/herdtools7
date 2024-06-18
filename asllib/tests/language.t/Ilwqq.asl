// RUN: interp %s | FileCheck %s

var a = b;
var b = c;
var c : integer;

func main() => integer
begin
    return 0;
end
