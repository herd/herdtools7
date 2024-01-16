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
