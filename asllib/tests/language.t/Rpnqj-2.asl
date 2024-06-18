// RUN: lit_run %s --version=standalone --roots='main'  --eval=":set asl=1.0" --eval=":load %s" | FileCheck %s
// this test is expected to be valid syntax and to print "PASS"


func main() => integer
begin
    var (a, b) = (10, 20);

    if ( a+b == 30 ) then
        println("PASS");
       return 0;
    else
        println("FAIL");
       return a + b;
    end
end
// CHECK:      PASS
