// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    throw;
    return 0;
end
