//R_QDQD: A single local identifier or a list of local identifiers may be
//declared along with an initialization expression. Each identifier may have
//an optional type annotation.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a, b = 10;

    return 0;
end
