//D_HTPL: A return statement returns the control flow to the caller of a
//subprogram.

// RUN: interp %s | FileCheck %s

func a()
begin
    return;
end

func main() => integer
begin
    return 0;
end
