// RUN: not interp %s | FileCheck %s

type a of integer;
type b of a;

func testa(aa: a) => integer
begin
    return 0;
end

func testa(bb: b) => real
begin
    return 0.0;
end

func main() => integer
begin
    return 0;
end
