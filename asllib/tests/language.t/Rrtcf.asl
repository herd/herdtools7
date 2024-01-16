// RUN: not interp %s | FileCheck %s

func test()
begin
    pass;
end

func main() => integer
begin
    test(10);
    return 0;
end
