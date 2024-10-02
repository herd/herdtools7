//D_WGQS: A record is a structured type consisting of a list of field
//identifiers which denote individual storage elements.

// RUN: interp %s | FileCheck %s

type a of record {
    b: integer
};

func main() => integer
begin
    return 0;
end
