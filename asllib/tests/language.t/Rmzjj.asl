//R_MZJJ: The getter keyword introduces a getter declaration.

// RUN: interp %s | FileCheck %s

getter a[] => integer
begin
    return 10;
end

func main() => integer
begin
    return 0;
end
