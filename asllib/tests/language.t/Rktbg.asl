//R_KTBG: It is an error if any bits selected by a bitslice are not in range
//for the expression being sliced. If the offset of a bitslice depends on a
//statically evaluable expression then this shall be checked at compile
//time. Otherwise a bounds check will occur at execution-time and an
//implementation defined exception shall be thrown if it fails.

// RUN: not interp %s | FileCheck %s

func a()
begin
    var b: bits(4);
    var c = b[10:0];
end

func main() => integer
begin
    return 0;
end
