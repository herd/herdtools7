type Word1 of integer{0..31};
type Word2 of integer{32..64};

type SuperInt of integer;

func nondet() => boolean
begin
    return ARBITRARY: boolean;
end;

func lca_parameterized{N, M}(bv_n: bits(N), bv_m: bits(M))
begin
    // The type of 'N' is integer{N}
    // The type of 'M' is integer{M}
    //      LCA type                         Type 1         Type 2
    var x : integer{N, M} = if nondet() then N         else M;
end;

type BitsA of bits(8) {[0] f1, [7] f2};
type BitsB of bits(8) {[5:0] f3};

type SuperRec of record {i: integer};

func main() => integer
begin
    // LCA type                                  Type 1                 Type 2
    var d : integer = if nondet() then (1     as integer{1}) else (2 as integer);
    var e : integer = if nondet() then 1      as integer{1}  else (2 as integer);
    var f : integer = if nondet() then 1                     else 2  as integer;
    var g : integer{1,2} = if nondet()   then 1              else 2  as integer{1,2};
    var h : integer{0..64} = if nondet() then 1 as Word1     else 32 as Word2;
    var i : bits(8) = if nondet() then Zeros{8} as BitsA     else Zeros{8} as BitsB;
    var j : bits(8) = if nondet() then Zeros{8}              else Zeros{8} as BitsB;

    var arr1 : array[[8]] of Word1;
    var arr2 : array[[8]] of Word2;
    var k : array [[8]] of integer {0..31, 32..64} = if nondet() then arr1 else arr2;

    return 0;
end;
