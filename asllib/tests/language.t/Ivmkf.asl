// RUN: interp %s | FileCheck %s
// CHECK: TRUE

func main() => integer
begin
    var a: bits(4) = '1000';
    print((a IN '1xxx'));
    return 0;
end
