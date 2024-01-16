// RUN: not interp %s | FileCheck %s

var a : integer = b;
func main() => integer
begin
    return 0;
end
