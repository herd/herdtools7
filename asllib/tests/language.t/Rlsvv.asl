//R_LSVV: The for statement is executed by initializing the for-loop counter
//with the value of the start expression, and then repeating the following
//actions until the for statement is completed:
//• if the direction is to (downto) and the loop counter value is greater
//than (less than) the end expression then the for statement is considered
//completed
//• the body of the for statement (stmt_list) is executed
//• the value of the for-loop counter is updated by incrementing
//(decrementing) it by 1 if the direction is to (downto)


// RUN: interp %s | FileCheck %s
// CHECK: 1
// CHECK-NEXT: 2
// CHECK-NEXT: 3
// CHECK-NEXT: 4
// CHECK-NEXT: 5
// CHECK-NEXT: 6
// CHECK-NEXT: 7
// CHECK-NEXT: 8
// CHECK-NEXT: 9
// CHECK-NEXT: 10

func main() => integer
begin
    for x = 1 to 10 do
        print(x);
    end
    return 0;
end
