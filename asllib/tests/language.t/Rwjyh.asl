// RUN: interp %s | FileCheck %s

type unconstrained of integer;

func main() => integer
begin
    return 0;
end
