// RUN: not interp %s | FileCheck %s

type b of integer subtypes a;

func main() => integer
begin
    return 0;
end
