// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a : bits(8) = '1111 1111';
    var b : bits(8){} = '1111 1111';
    var c = a == b;

    return 0;
end
