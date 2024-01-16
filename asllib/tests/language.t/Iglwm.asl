// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: bits({0..10}) = '00';
    return 0;
end
