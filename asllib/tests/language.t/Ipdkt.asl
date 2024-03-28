// RUN: interp %s | FileCheck %s

func test(N: integer{}) => bits(N)
begin
    return Zeros(N);
end

func main() => integer
begin
    return 0;
end
