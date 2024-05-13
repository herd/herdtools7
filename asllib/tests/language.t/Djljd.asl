// D_JLJD: A type is an execution-time type if its structure depends on
// either:
// - an execution-time type
// - an execution-time expression

// RUN: interp %s | FileCheck %s

func execType(wid: integer{})
begin
    // structure of R's type depends on execution time value `wid`
    var R: bits(wid);
end

func main() => integer
begin
    return 0;
end
