type BAD_OPCODE of exception;

type UNDEFINED_OPCODE of exception {reason: string, opcode: bits(16)};

constant opcode : bits(16) = '0111011101110111';

func throw_undefined_opcode()
begin
  throw UNDEFINED_OPCODE {
    reason="Undefined",
    opcode = opcode
  };
end

func try_opcode() => integer
begin
  try
    throw_undefined_opcode();
    return 1;
  catch
    when BAD_OPCODE => return 2;
    when e : UNDEFINED_OPCODE =>
      assert e.opcode == opcode;
      return 0;
    otherwise => return 3;
  end
end

var opcode_counter: integer = 0;

func rethrow_undefined_opcode ()
begin
  try throw_undefined_opcode(); assert FALSE;
  catch
    when BAD_OPCODE => assert FALSE;
    when UNDEFINED_OPCODE => opcode_counter = opcode_counter + 1; throw;
  end
  assert FALSE;
end

func try_rethrow ()
begin
  try
    rethrow_undefined_opcode (); assert FALSE;
  catch
    when e: UNDEFINED_OPCODE => assert e.opcode == opcode; assert opcode_counter == 1;
    otherwise => assert FALSE;
  end
end

type COUNTING of exception { counter: integer };

var counter : integer = 0;

func throw_counting ()
begin
  let x = counter;
  counter = x + 1;
  throw COUNTING { counter = x };
end

func throw_imbricated()
begin
  try
    throw_counting ();
    assert FALSE;
  catch
    when BAD_OPCODE => assert FALSE;
    when COUNTING =>
      try
        throw;
      catch
        when COUNTING => assert FALSE;
        otherwise => assert FALSE;
      end
      assert FALSE;
  end
  assert FALSE;
end

func try_imbricated()
begin
  try
    throw_imbricated ();
  catch
    when e: COUNTING =>
      assert e.counter == 0;
      counter = counter + 1;
    otherwise => assert FALSE;
  end
  assert counter == 2;
end

func try_with_local_variable ()
begin
  var local_counter: integer = 0;
  try
    try throw_undefined_opcode ();
    catch
      when UNDEFINED_OPCODE =>
        local_counter = local_counter + 1;
        throw;
    end
    assert FALSE;
  catch
    when UNDEFINED_OPCODE =>
      local_counter = local_counter + 1;
    otherwise => assert FALSE;
  end
  assert local_counter == 2;
end

func main () => integer
begin
  assert try_opcode() == 0;
  try_rethrow ();
  try_imbricated ();
  try_with_local_variable ();

  return 0;
end

// RUN: archex.sh --eval=':set asl=1.0' --eval=':set +syntax:aslv1_colon_colon' --eval=':load %s' --eval='assert main() == 0;' | FileCheck %s

