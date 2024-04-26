// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a = '111';
    assert a == '111';
    return 0;
end