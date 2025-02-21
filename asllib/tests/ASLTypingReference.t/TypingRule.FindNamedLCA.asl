type A1 of integer;
type A2 subtypes A1;
type B1 subtypes A2;
type B2 subtypes B1;
type C1 subtypes A2;
type C2 subtypes C1;
type D1 of real;

func main() => integer
begin
    var - : A2 = if ARBITRARY: boolean then (1 as B2) else (2 as C2);
    // The following line is illegal. In particular B2 and D1 have no
    // named lowest common ancestor.
    // var - = if ARBITRARY: boolean then (1 as B2) else (2.0 as D1);
    return 0;
end;
