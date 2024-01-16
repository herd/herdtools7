// RUN: interp %s | FileCheck %s

func a(N: integer{}, M: integer{})
begin
    var b = N;
    var c: integer{};
end

func main() => integer
begin
    return 0;
end
