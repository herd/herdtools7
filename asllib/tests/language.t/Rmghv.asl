//R_MGHV: An exception type is described using the syntax exception
//{field_list} where each element of the field_list specified the name and
//type of the exception’s field identifiers.

// RUN: interp %s | FileCheck %s

type a of exception{
    b: integer
};

func main() => integer
begin
    return 0;
end
