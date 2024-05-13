//I_HLBL: Exception field names are not part of the global or local scope,
//nor do they clash with other fields in bitvectors, records or exceptions.

// RUN: interp %s | FileCheck %s

type a of exception {a: integer};

func main() => integer
begin
    return 0;
end
