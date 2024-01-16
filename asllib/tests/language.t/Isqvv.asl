// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: bits(0) = '';
    var b: bits(4) = '1101';
    return 0;
end
