// RUN: interp %s | FileCheck %s
// CHECK: 3

func main() => integer
begin
    case '1010' of
        when '0000' => print("1");
        when '1000' => print("2");
        when '1010' => print("3");
        when '1111' => print("4");
        otherwise => pass;
    end
    return 0;
end
