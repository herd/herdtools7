// RUN: interp %s | FileCheck %s

type a of integer;
type b subtypes a;

func aa() => a
begin
    var bb: b;
    return bb;
end

func main() => integer
begin
    return 0;
end
