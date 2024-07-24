//R_YBWY: Named type declarations must not be recursive.

type T1 of integer;        // the named type "T1" whose structure is integer
type T2 of (integer, T1);  // the named type "T2" whose structure is (integer, integer)

//In the following code, TypeC.f and TypeD.f have the same type: TypeB (and
//not integer).

type TypeA of integer;
type TypeB of TypeA;
type TypeC of record { f: TypeB };
type TypeD of TypeC;

func foo(x: TypeA)
begin 
  pass;
end

func bar(x: TypeB)
begin
  pass; 
end

func baz(x: integer)
begin 
  pass;
end

func main() => integer
begin
  var x: TypeC;
  foo(x.f); // illegal -- x.f is of type TypeB which does not type-satisfy TypeA.
  bar(x.f); // legal
  baz(x.f); // legal -- TypeB type-satisfies integer
  var y: TypeD;
  foo(y.f); // illegal -- y.f is of type TypeB which does not type-satisfy TypeA.
  bar(y.f); // legal
  baz(y.f); // legal -- TypeB type-satisfies integer
  return 0; 
end
