// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: bits(4) = ['11', '00'];

    return 0;
end
