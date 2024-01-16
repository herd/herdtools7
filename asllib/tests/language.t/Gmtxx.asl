// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    var t: integer{0..10};
    var s: integer{11} = t;
    return 0;
end
