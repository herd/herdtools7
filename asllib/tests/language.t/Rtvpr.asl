// RUN: interp %s | FileCheck %s

type a of (integer, integer);

func main() => integer
begin
    return 0;
end
