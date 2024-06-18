// RUN: interp %s | FileCheck %s

var a: bits(8) = '11110000';
var b: bits(8) = '1111 0000';

func main() => integer
begin
    return 0;
end
