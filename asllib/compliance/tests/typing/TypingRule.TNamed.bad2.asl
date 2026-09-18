type TypeA of integer;
type TypeB of TypeA;
type TypeC of record { f: TypeB };
type TypeD of TypeC;

func foo(x: TypeA)
begin pass; end;

func main() => integer
begin
    var y: TypeD;
    foo(y.f); // illegal: y.f is of type TypeB which does not type-satisfy TypeA.
    return 0;
end;
