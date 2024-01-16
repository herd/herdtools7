// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a : bits(2) {[0] a [1] b} = '11';
    var b : bits(2) {[0] a [1] b} = a;

    return 0;
end
