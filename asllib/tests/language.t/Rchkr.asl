//R_CHKR: Exception field names must be unique with respect to other fields
//in the same type.

// RUN: not interp %s | FileCheck %s

type a of exception{
    b: integer,
    b: integer
};

func main() => integer
begin
    return 0;
end
