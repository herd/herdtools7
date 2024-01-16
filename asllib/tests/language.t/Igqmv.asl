// RUN: interp %s | FileCheck %s

type a of bits(4 as integer {4,8});

func main() => integer
begin
    return 0;
end
