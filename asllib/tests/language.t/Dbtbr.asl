// RUN: not interp %s | FileCheck %s

type a of integer;
type b of a;

func testa(aa: a)
begin
    pass;
end

func testa(bb: b)
begin
    pass;
end

func main() => integer
begin
    return 0;
end
