// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    print(fdiv_int(6, 0));
    return 0;
end
