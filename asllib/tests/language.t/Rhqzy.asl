//R_HQZY: The syntax type id1 subtypes id2 with {new-fields...} declares a 
//new named type id1 which has the structure of id2, with additional fields. //id1 is also a subtype of id2. id2 must be a record or exception type. The
//declarationisequivalenttotype id1 of record {existing-id2-fields,
//new-fields...} subtypes id2.

// RUN: interp %s | FileCheck %s

type a of record {
    aa: integer
};

type b of record {
    aa: integer,
    bb: integer
} subtypes a;

type c subtypes a with {
    bb: integer
};

func main() => integer
begin
    return 0;
end
