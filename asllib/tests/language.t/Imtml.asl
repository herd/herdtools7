// RUN: interp %s | FileCheck %s

type a of integer;
type b subtypes a;
type c subtypes b;

func test(aa: a)
begin
    pass;
end

func main() => integer
begin
    var cc: c;
    test(cc);
    return 0;
end
