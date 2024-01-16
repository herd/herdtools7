// RUN: interp %s | FileCheck %s

type a of integer;
type b subtypes a;

func main() => integer
begin
    return 0;
end
