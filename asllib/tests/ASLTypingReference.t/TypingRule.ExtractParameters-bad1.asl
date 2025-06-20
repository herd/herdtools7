
func unsupported_arguments{N: integer {8,16,32}}(
    arg0: bits(N as integer{8,16,32}),
    arg1: bits((N + 1) as integer{9,17})
)
begin
    return;
end;
