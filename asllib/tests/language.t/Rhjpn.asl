// RUN: interp %s | FileCheck %s

type constrained of integer {10};

func main() => integer
begin
    return 0;
end
