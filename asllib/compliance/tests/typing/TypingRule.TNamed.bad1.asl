type TypeA of integer;
type TypeB of TypeA;
type TypeC of record { f: TypeB };

func foo(x: TypeA)
begin pass; end;

func main() => integer
begin
    var x: TypeC;
    foo(x.f); // Illegal: x.f is of type TypeB which does not type-satisfy TypeA.
    return 0;
end;
