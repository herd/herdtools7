// RUN: not interp %s | FileCheck %s


func main() => integer
begin
    print(shiftleft_int(100, -3));
    return 0;
end
