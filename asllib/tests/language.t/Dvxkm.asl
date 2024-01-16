// RUN: interp %s | FileCheck %s

func test()
begin
    pass;
end

func main() => integer
begin
    test();
    return 0;
end
