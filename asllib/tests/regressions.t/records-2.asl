
type a_record_ty of record {
    flag : boolean,
    count: integer,
    data : bit
};

type aa_record_ty of record {
    flag : boolean,
    count: integer,
    data : bit,
    foo: integer
} subtypes a_record_ty;

func equal_a_record_ty (x: a_record_ty, y: a_record_ty) => boolean
begin
  return
    x.flag == y.flag &&
    x.count == y.count &&
    x.data == y.data;
end

func main() => integer
begin
  let a = a_record_ty {
    flag = TRUE,
    count = 3,
    data = '1'
  };
  let aa = aa_record_ty {
    flag = TRUE,
    count = 3,
    data = '1',
    foo = 5
  };
  assert equal_a_record_ty (a, aa);

  return 0;
end

// RUN: archex.sh --eval=':set asl=1.0' --eval=':set +syntax:aslv1_colon_colon' --eval=':load %s' --eval='assert main() == 0;' | FileCheck %s

