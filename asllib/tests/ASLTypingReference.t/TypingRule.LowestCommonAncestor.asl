type Word1 of integer{0..31};
type Word2 of integer{32..64};

type SuperInt of integer;
type SubInt1 of integer subtypes SuperInt;
type SubInt2 of integer subtypes SuperInt;

func nondet() => boolean
begin
    return ARBITRARY: boolean;
end;

func lca_parameterized{N, M}(bv_n: bits(N), bv_m: bits(M))
begin
    // The type of 'N' is integer{N}
    // The type of 'M' is integer{M}
    //      LCA type                         Type 1         Type 2
    var - : integer{N, M} = if nondet() then N         else M;
end;

type BitsA of bits(8) {[0] f1, [7] f2};
type BitsB of bits(8) {[5:0] f3};

type SuperRec of record {i: integer};
type SubRec1 subtypes SuperRec with {b: boolean};
type SubRec2 subtypes SuperRec with {c: real};
type Exc of exception {i: integer};

func main() => integer
begin
    // LCA type                                  Type 1                 Type 2
    var - : SuperInt = if nondet() then 1     as SubInt1     else 2  as SubInt2;
    var - : SubInt1 = if nondet() then (1     as SubInt1)    else (2 as integer);
    var - : SubInt2 = if nondet() then (1     as integer)    else (2 as SubInt2);
    var - : integer = if nondet() then (1     as integer{1}) else (2 as integer);
    var - : integer = if nondet() then 1      as integer{1}  else (2 as integer);
    var - : integer = if nondet() then 1                     else 2  as integer;
    var - : integer{1,2} = if nondet()   then 1              else 2  as integer{1,2};
    var - : integer{0..64} = if nondet() then 1 as Word1     else 32 as Word2;
    var - : bits(8) = if nondet() then Zeros{8} as BitsA     else Zeros{8} as BitsB;
    var - : bits(8) = if nondet() then Zeros{8}              else Zeros{8} as BitsB;

    var arr1 : array[[8]] of Word1;
    var arr2 : array[[8]] of Word2;
    var - : array [[8]] of integer {0..31, 32..64} = if nondet() then arr1 else arr2;

    var - : (SuperInt, SuperInt) = if nondet()  then (1 as SubInt1, 2 as SubInt2)
                                                else (2 as SubInt2, 1 as SubInt1);

    var sup : SuperRec;
    var r1 : SubRec1;
    var r2 : SubRec2;
    //      LCA type                            Type 1              Type 2
    var - : SuperRec =  if nondet() then sup as SuperRec else r1 as SubRec1;
    var - : SuperRec =  if nondet() then r1  as SubRec1  else r2 as SubRec2;
    var ex : Exc;
    // The following statement is illegal as SuperRec and Exc do not have
    // lowest common ancestor.
    // var -  = if nondet() then r1 else ex;
    return 0;
end;
