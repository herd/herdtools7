// RUN: interp %s | FileCheck %s

type a of record {b: integer};

func main() => integer
begin
    return 0;
end
