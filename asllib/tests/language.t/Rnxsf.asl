// RUN: interp %s | FileCheck %s

type a of integer;

func main() => integer
begin
    var b: a;

    return 0;
end
