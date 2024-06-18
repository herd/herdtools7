// RUN: interp %s | FileCheck %s

var a = '1111 1111';

func main() => integer
begin
    var b = a[0];
    var c = a[3:1];
    var d = a[3:1, 6:5];

    return 0;
end
