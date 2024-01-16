// RUN: interp %s | FileCheck %s

type well_constrained of integer {10, 11, 30};

func main() => integer
begin
    return 0;
end
