// RUN: interp %s | FileCheck %s

var a: bits(5) = '11111';

func main() => integer
begin
    return 0;
end
