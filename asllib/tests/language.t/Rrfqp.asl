// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a = (1 + 1 * (3 - 3)) == (4 - (3 - 4));
    return 0;
end
