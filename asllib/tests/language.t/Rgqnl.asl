// RUN: interp %s | FileCheck %s
// CHECK: TRUE
// CHECK-NEXT: TRUE
// CHECK-NEXT: FALSE
// CHECK-NEXT: TRUE
// CHECK-NEXT: Hello
// CHECK-NEXT: World
// CHECK-NEXT: FALSE
// CHECK-NEXT: World
// CHECK-NEXT: TRUE


func true_writer() => boolean
begin
    print("Hello");
    return TRUE;
end

func false_writer() => boolean
begin
    print("World");
    return FALSE;
end

func main() => integer
begin
    print(FALSE --> FALSE);
    print(FALSE --> TRUE);
    print(TRUE --> FALSE);
    print(TRUE --> TRUE);

    var a: boolean = true_writer() --> false_writer();
    print(a);


    var b: boolean = false_writer() --> false_writer();
    print(b);
    return 0;
end
