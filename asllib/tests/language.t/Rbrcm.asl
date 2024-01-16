// RUN: interp %s | FileCheck %s
// CHECK: TRUE

func main() => integer
begin
    var T: boolean = [ '1111', '0000' ] == '11110000';

    print(T);
    return 0;
end
