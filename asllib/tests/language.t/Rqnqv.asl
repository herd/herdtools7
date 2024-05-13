//R_QNQV: String literals have type string.

// RUN: interp %s | FileCheck %s

var a: string = "test";

func main() => integer
begin
    return 0;
end
