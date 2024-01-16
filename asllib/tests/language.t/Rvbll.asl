// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a : integer{2} = 2;
    var b = a as integer{0..3};
    return 0;
end
