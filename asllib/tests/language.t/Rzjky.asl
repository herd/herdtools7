// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: integer{10} = 10;

    return 0;
end
