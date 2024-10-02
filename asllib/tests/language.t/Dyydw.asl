//D_YYDW: Any expression consisting solely of an immutable storage
//element or a [literal constant] is a statically evaluable expression.

// RUN: interp %s | FileCheck %s

let x: integer = 10;
let y: integer = x;

func main() => integer
begin
    return 0;
end
