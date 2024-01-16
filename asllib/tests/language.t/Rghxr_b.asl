// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    print(frem_int(6, -3));
    return 0;
end
