// RUN: interp %s | FileCheck %s
// CHECK: TRUE

func main() => integer
begin
    var a: bits(2);
    print((a == '00'));
    return 0;
end
