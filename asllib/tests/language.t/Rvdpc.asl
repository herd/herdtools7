// RUN: interp %s | FileCheck %s

func a()
begin
    var bb: integer{0..2} = 1;
    var cc = bb + 1;
    var dd: integer{2..4} = cc + 1;
end

func main() => integer
begin
    return 0;
end
