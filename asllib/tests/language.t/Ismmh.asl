// RUN: interp %s | FileCheck %s

type c subtypes b;
type b subtypes a;
type a of integer;

func main() => integer
begin
    return 0;
end
