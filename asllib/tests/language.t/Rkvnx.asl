//R_KVNX: Individual elements of a tuple may be accessed read-only. Writing
//values to an individual element of a tuple is not supported.

// RUN: not interp %s | FileCheck %s

var a = (10, 10);

func main() => integer
begin
    a.item0 = 4;

    return 0;
end
