
type SomeOtherType of integer;

type MyFieldType of record {
    subfieldA: boolean,
    subfieldB: integer,
};

type MyRecord of record {
  fieldA: integer,
  fieldB: MyFieldType,
  fieldC: SomeOtherType,
};

func build_record() => MyRecord
begin
  return MyRecord {
    fieldA = 3,
    fieldB = MyFieldType {
      subfieldA = TRUE,
      subfieldB = 4
    },
    fieldC = 5
  };
end

func access_subfieldA(obj:MyRecord) => boolean
begin
  return obj.fieldB.subfieldA;
end

func incr_subfieldB(obj:MyRecord) => MyRecord
begin
  var obj2 = obj;
  obj2.fieldB.subfieldB = obj2.fieldB.subfieldB + 1;
  return obj2;
end

func set_fieldC(obj:MyRecord, val:integer) => MyRecord
begin
  var obj2 = obj;
  obj2.fieldC = val;
  return obj2;
end

func build_and_access()
begin
  let obj = MyRecord {
    fieldA = 3,
    fieldB = MyFieldType {
      subfieldA = TRUE,
      subfieldB = 4
    },
    fieldC = 5
  };
  assert obj.fieldB.subfieldA;
end

func build_access()
begin
  assert MyRecord {
    fieldA = 3,
    fieldB = MyFieldType {
      subfieldA = TRUE,
      subfieldB = 4
    },
    fieldC = 5
  }.fieldB.subfieldA;
end

func main() => integer
begin
  let obj = build_record ();

  assert access_subfieldA(obj);

  assert incr_subfieldB(obj).fieldB.subfieldB == 5;
  assert obj.fieldB.subfieldB == 4; // No edit by reference

  assert set_fieldC(obj, 32).fieldC == 32;
  assert obj.fieldC == 5; // No edit by reference

  build_and_access ();
  build_access ();

  return 0;
end

// RUN: archex.sh --eval=':set asl=1.0' --eval=':set +syntax:aslv1_colon_colon' --eval=':load %s' --eval='assert main() == 0;' | FileCheck %s

