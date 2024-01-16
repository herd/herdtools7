// RUN: not interp %s | FileCheck %s

func test()
begin
    pass;
end

func main() => integer
begin
    test2();
    return 0;
end
