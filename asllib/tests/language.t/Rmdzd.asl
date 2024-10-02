//R_MDZD: Record field names must be unique with respect to other fields in
//the same type.

// RUN: not interp %s | FileCheck %s

type a of record {
    b: integer,
    b: integer
};

func main() => integer
begin
    return 0;
end
