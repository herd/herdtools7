// RUN: interp %s | FileCheck %s

var only: bits(1) = Zeros(1);

func main() => integer
begin
    return 0;
end