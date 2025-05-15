/*--------------------*
   Setters
 *--------------------*/

func setter_accesses()
begin
  X() = '1100';

  X()[3:2] = '00';
  X().fld = '11';
  assert X() == '0011';
  X().fld[0] = '0';
  assert X() == '0010';
end;

var x: bits(4) { [1:0] fld };

accessor X() <=> v: bits(4) { [1:0] fld }
begin
  getter
    return x;
  end;

  setter
    x = v;
  end;
end;


/*--------------------*
   Nested accesses
 *--------------------*/

func nested_accesses()
begin
  state.arr[[7]] = '0000';
  state.arr[[7]][0] = '1';
  assert state.arr[[7]] == '0001';
end;

type state_type of record {
  arr: array[[31]] of bits(4)
};

var state: state_type;


/*--------------------*/

func main() => integer
begin
  setter_accesses();
  nested_accesses();

  return 0;
end;
