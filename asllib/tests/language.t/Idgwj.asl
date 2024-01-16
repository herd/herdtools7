// RUN: not interp %s | FileCheck %s

type a of integer;
type b of integer;

func main() => integer
begin
    var aa: a = 10;
    var bb: b = aa;
    return 0;
end
