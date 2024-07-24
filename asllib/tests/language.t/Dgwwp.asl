//D_GWWP: A side-effect-free subprogram is one that does not mutate
//global storage elements.

// RUN: interp %s | FileCheck %s

func a() => integer
begin
    return 10;
end

func main() => integer
begin
    return 0;
end
