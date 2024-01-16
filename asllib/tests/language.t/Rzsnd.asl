// RUN: interp %s | FileCheck %s

func testa(N: integer)
begin
    for x = 0 to N do
        pass;
    end
end

func testb(N: integer{})
begin
    for x = 0 to N do
        pass;
    end
end

func main() => integer
begin
    return 0;
end
