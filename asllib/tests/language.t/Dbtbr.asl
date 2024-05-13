//D_BTBR: Two subprograms clash if all of the following hold:
// - they have the same name
// - they are the same kind of subprogram
// - they have the same number of formal arguments
// - every formal argument in one type-clashes with the
// corresponding formal argument in the other

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
