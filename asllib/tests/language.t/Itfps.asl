// RUN: interp %s | FileCheck %s

type a of integer;

type b of record{a: integer};

type a_record_ty of record {
    flag : boolean,
    count: integer,
    data : bit
};

func main() => integer
begin
    var a_record = a_record_ty {
        flag = FALSE,
        count = 0,
        data = '0'
    };
    return 0;
end
