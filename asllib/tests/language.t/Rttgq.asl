// RUN: not interp %s | FileCheck %s

type a of boolean;

func main() => integer
begin
    var aa: a;
    var aaa: integer;

    var b = aa || aaa;
    return 0;
end
