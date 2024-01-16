// RUN: interp %s | FileCheck %s

type a of record;
type b of record{};

func main() => integer
begin
    return 0;
end
