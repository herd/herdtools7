// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: bits(-:integer{10});
    return 0;
end
