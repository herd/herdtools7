// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a : integer{4} = 4;
    var b = a as integer{0..4};
    return 0;
end
