// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: integer{1} = 1;
    var b: integer{0} = 0;

    var c = a == b;
    return 0;
end
