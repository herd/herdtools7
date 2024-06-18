//R_MBRM: The base value of a record type is a record whose elements have
//the base values of their types.

//Given the following declaration of a record type a_record_ty:
type a_record_ty of record {
  flag : boolean,
  count: integer,
  data : bit };

//declaring a local variable a_record of that type, with no initializer, is
//equivalent to the following where a_record is initialized with the base
//value of a_record_ty:
var a_record = a_record_ty {
  flag = FALSE,
  count = 0,
  data = '0' 
};
