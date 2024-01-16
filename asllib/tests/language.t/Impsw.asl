// RUN : interp %s | FileCheck %s

func f(N: integer) => bits(N)
begin
    let x: bits(N+1) = Zeros(N);
    return x;
end

func main() => integer
begin
    - = f(32);
    return 0;
end
