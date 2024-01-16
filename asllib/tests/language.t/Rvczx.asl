// RUN: interp %s | FileCheck %s

func b(n: bits({1,3,5}))
begin
    pass;
end

func main() => integer
begin
    var a: bits({1,3,5}) = '011' as bits({1,3,5});
    return 0;
end
