// RUN: not interp %s | FileCheck %s

func a()
begin

end

func main() => integer
begin
    return 0;
end
