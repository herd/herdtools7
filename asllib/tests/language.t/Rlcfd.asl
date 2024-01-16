// RUN: not interp %s | FileCheck %s

var a = 10;

func main() => integer
begin
    var a = 4;
    print(a);

    return 0;
end
