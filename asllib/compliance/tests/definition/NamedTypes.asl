type TypeA of integer;
type TypeB of TypeA;
type TypeC of record { f: TypeB };
type TypeD of TypeC;

func foo(x: TypeA)
begin pass; end;

func bar(x: TypeB)
begin pass; end;

func baz(x: integer)
begin pass; end;

func main() => integer
begin
    var x: TypeC;
    bar(x.f); // Legal
    baz(x.f); // Legal: TypeB type-satisfies integer
    var y: TypeD;
    bar(y.f); // Legal
    baz(y.f); // Legal: TypeB type-satisfies integer
    return 0;
end;
