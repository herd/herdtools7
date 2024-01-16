// RUN: not interp %s | FileCheck %s

var a = (10, 10);

func main() => integer
begin
    a.item0 = 4;

    return 0;
end
