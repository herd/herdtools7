// RUN: not interp %s | FileCheck %s

var a = '1111 1111';

func main() => integer
begin
    var b = a[1:4];
    return 0;
end
