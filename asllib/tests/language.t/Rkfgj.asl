// RUN: interp %s | FileCheck %s

func test()
begin
    pass;
end

func test(a: bits(10))
begin
    pass;
end

func main() => integer
begin
    return 0;
end
