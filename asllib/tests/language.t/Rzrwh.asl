// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: bits(16) = '0000 0001 0010 0100';
    return 0;
end
