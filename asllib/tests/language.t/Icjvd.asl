// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var (a, b) : (boolean, integer) = (TRUE, 1);

    return 0;
end
