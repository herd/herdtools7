// RUN: interp %s | FileCheck %s

func function() => integer
begin
    return 10;
end

func procedure()
begin
    pass;
end

func main() => integer
begin
    return 0;
end
