//R_KGXL: The syntax exception (with no field_list) is syntactic sugar for
//exception {}.

// RUN: interp %s | FileCheck %s

type a of exception;
type b of exception{};

func main() => integer
begin
    return 0;
end
