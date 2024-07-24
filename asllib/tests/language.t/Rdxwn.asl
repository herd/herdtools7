//R_DXWN: A record type is described using the syntax record {field_list}
//where each element of the field_list specifies the name and type of the
//record’s field identifiers.

// RUN: interp %s | FileCheck %s

type a of record {b: integer};

func main() => integer
begin
    return 0;
end
