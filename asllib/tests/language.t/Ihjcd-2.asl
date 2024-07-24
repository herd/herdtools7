//I_HJCD: Underscores in numbers are not significant, and their only purpose
//is to separate groups of digits to make constants
//such as 0xefff_fffe, 1_000_000 or 3.141_592_654 easier to read.

// RUN: interp %s | FileCheck %s
// CHECK: 1000000
// CHECK-NEXT: 1000000

func main() => integer
begin
    print(1000000);
    print("\n");
    print(1_000_000);
    return 0;
end
