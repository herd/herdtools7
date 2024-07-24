// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: integer{3} = 1 + 2;

    return 0;
end
