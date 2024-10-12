constant W = 4;

func signature_example{A}(
    B: integer,
    bv: bits(A),
    bv2: bits(W),
    bv3: bits(A+B),
    C: integer) => bits(A+B)
begin
    return bv :: Ones(B);
end
