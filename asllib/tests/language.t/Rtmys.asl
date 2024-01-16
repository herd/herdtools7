// RUN: interp %s | FileCheck %s
// CHECK: 3

func main() => integer
begin
    if FALSE then
        print("1");
    elsif FALSE then
        print("2");
    elsif TRUE then
        print("3");
    end
    return 0;
end
