//R_WBCQ: The identifier in a record expression must be a named type with
//the structure of a record type, and whose fields have the values given in
//the field_assignment_list.

// RUN: interp %s | FileCheck %s

type a of record {
    x: integer,
    y: integer
};

func main() => integer
begin
    var b = a { x = 10, y = 5 };
    return 0;
end
