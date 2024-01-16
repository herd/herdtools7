// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    var a : integer{0..4} = 4;
    var b = a as integer{0..3};
    return 0;
end
