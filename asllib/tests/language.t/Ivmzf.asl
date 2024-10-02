// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: integer{10} = (3 + 2) * 2;

    return 0;
end
