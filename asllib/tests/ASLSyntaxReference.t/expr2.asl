accessor g0_bits() <=> value_in: bits(4)
begin
  getter
    return '1000';
  end;

  setter
    Unreachable();
  end;
end;

accessor g1_bits(p: integer) <=> value_in: bits(4)
begin
  getter
    return '1000'[p, 2:0];
  end;

  setter
    Unreachable();
  end;
end;

type point of record{x: bits(4), y: bits(4)};
type except of exception;

func main() => integer
begin
  // E_Record 1: a record construction expression.
  var p = point{x = '1111', y = '0000'};
  // E_GetField 1: reading a single field.
  var b0 = p.x;
  // E_GetFields 1: reading multiple fields.
  var b8: bits(8)  = p.[x, y];
  // E_Concat 1: b0 :: b1 concatenates two bitvectors.
  b8 = b0 :: b0;
  // E_Tuple 1: constructing a pair of two 4-bit bitvectors.
  var t2 = (b0, b0);
  // E_GetField 2: reading the first tuple item.
  // E_Pattern 1: the condition in side the if is a pattern.
  if (t2.item0 IN {'1110'}) then
    // E_Record 2: an exception construction.
    throw except{-};
  end;

  return 0;
end;
