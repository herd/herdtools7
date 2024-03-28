// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    var (a, b) = (1, 2, 3);

    return 0;
end
