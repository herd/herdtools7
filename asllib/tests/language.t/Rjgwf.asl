// RUN: interp %s | FileCheck %s

type a of boolean;

func main() => integer
begin
    var aa: a;
    var aaa: a;

    var b = aa || aaa;
    return 0;
end
