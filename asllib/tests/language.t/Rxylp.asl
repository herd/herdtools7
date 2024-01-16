// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: bits({1..10}) = '111';

    return 0;
end
