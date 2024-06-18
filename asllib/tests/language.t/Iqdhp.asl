// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var myData: bits(16) {[4] flag,
        [3:0, 8:5] data,
        [9:0] value} ='1111 0101 1010 0000';
    return 0;
end
