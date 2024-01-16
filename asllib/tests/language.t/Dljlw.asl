// RUN: interp %s | FileCheck %s

getter test => integer
begin
    return 10;
end

func main() => integer
begin
    var a = test;
    return 0;
end
