// RUN: not interp %s | FileCheck %s

func a(N: integer{}, M: integer{})
begin
    var b = N;
    b = M;
end

func main() => integer
begin
    return 0;
end
