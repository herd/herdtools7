// The following declaration is illegal,
// since the type of the return value is integer{N}, which represents
// exactly one value --- the runtime value passed to the parameter N,
// whereas the type of the return expression N is integer{2, 4}.
func MyUInt{N: integer{2, 4}}(x: bits(N)) => integer{N}
begin
    return N;
end;
