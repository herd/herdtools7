// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: integer{1} = 1;
    let b: integer{2} = 2;
    constant c: integer{3} = 3;

    var d: integer{6} = a + b + c;
    return 0;
end
