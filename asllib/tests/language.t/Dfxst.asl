//D_FXST: Storage elements and identifiers which denote storage elements
//are either mutable or immutable.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    let a = 10;
    var b = 10;

    return 0;
end
