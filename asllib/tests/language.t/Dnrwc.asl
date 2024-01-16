// RUN: interp %s | FileCheck %s

func constrained(N: integer{1..10}) => bits(N)
begin
    return Zeros(N);
end

func fixed(N: integer{10}) => bits(N)
begin
    return Zeros(N);
end

func main() => integer
begin
    return 0;
end
