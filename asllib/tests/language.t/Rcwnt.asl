// RUN: interp %s | FileCheck %s
// CHECK: 3

func test(d: bits(2), a: integer) => integer
begin
    case d of
        when '00' => return 1;
        when '01' where a > 10 => return 2;
        when '01' => return 3;
        when '10' => return 4;
        when '11' => return 5;
    end
    return 6;
end

func main() => integer
begin
    print(test('01', 1));
    return 0;
end
