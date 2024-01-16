// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: bits(2) = '01';
    return 0;
end
