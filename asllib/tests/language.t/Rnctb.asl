// RUN: interp %s | FileCheck %s


getter a{N:integer{1..100}}[t: bits(N)] => bits(N)
begin
    return Zeros(N);
end

setter a{N:integer}[t: bits(N)] = value: bits(N)
begin
    return;
end

func main() => integer
begin
    return 0;
end
