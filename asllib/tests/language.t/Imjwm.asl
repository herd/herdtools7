// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: bits(4) = '1111 1111 1111'[3:0];
    return 0;
end
