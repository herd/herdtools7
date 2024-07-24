//I_XFPV: Tuple element selection is position-based. The first element is
//selected with .item0, the second with .item1, the third with item2 and so
//on. It is invalid to specify an element beyond the size of the tuple.

// RUN: not interp %s | FileCheck %s
// CHECK: 10
// CHECK-NEXT: 20

var a = (10, 20);

func main() => integer
begin
    var c = a.item2;

    return 0;
end
