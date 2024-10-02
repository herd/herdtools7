// RUN: not interp %s | FileCheck %s

func illegal_constraints {N: integer {8,16,32}} (
    arg0: bits(N as {8,16,32}),
    arg1: bits((N+1) as {9,17})
)
begin
    return;
end

func main() => integer
begin
    return 0;
end
