// RUN: not interp %s | FileCheck %s

type a of integer;
type b of string subtypes a;

func main() => integer
begin
    return 0;
end
