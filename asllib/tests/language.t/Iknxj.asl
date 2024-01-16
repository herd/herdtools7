// RUN: interp %s | FileCheck %s

func testing(a: bits({2, 4, 8}))
begin
    pass;
end

func test(a: bits({2}))
begin
    testing(a);
end

func main() => integer
begin
    return 0;
end
