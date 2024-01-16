// RUN: interp %s | FileCheck %s

type a of integer;

func main() => integer
begin
    var aa: a = 10;
    var aaa: integer = aa;
    var aaaa: a = aaa;
    return 0;
end
