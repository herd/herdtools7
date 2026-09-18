var W = 400;

// Illegal: limit expressions must be constrained.
func signature_example(bv: bits(8)) => bits(16) recurselimit(W)
begin
    return bv :: Ones{8};
end;
