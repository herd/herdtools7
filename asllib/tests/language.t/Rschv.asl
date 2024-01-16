// RUN: not interp %s | FileCheck %s

type a of integer;

func b()
begin
    var aa : ty;
end

func main() => integer
begin
    return 0;
end
