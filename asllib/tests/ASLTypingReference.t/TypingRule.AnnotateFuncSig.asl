constant W = 400;

func signature_example{A,B}(
    bv: bits(A),
    bv2: bits(W),
    bv3: bits(A+B),
    C: integer) => bits(A+B) recurselimit(W)
begin
    return bv :: Ones{B};
end;
