//R_DYQZ: A record expression shall assign every field of the record.

// RUN: not interp %s | FileCheck %s

type a of record {
    x: integer,
    y: integer
};

func main() => integer
begin
    var b = a { x = 10 };
    return 0;
end
