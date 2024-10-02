// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var (a, -) = (1, 2);

    return 0;
end
