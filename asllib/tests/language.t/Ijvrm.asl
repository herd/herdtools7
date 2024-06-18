// RUN: not interp %s | FileCheck %s

type a subtypes b;
type b subtypes a;

func main() => integer
begin
    return 0;
end
