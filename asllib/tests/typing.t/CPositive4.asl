// any width bitvector may be passed to sub
func sub{N}(arg : bits(N))
begin
    pass;
end;
func positive4(w: integer{1, 2, 3})
begin
    sub(Zeros(12));
    sub(Zeros(w));
end;
