// RUN: not interp %s | FileCheck %s

func test{N:integer}(N:integer)
begin
    pass;
end

func main() => integer
begin
    return 0;
end
