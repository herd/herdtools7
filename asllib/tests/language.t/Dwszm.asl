// RUN: interp %s | FileCheck %s

func undetermined(N: integer{1..10}) => bits(N)
begin
    return Zeros(N);
end

func determined() => bits(10)
begin
    return Zeros(10);
end

func main() => integer
begin
    return 0;
end
