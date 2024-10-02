// RUN: interp %s | FileCheck %s

func testing(a: bits({0..10}))
begin
    pass;
end

func test(a: bits({0..3}))
begin
    testing(a);
end

func main() => integer
begin
    return 0;
end
