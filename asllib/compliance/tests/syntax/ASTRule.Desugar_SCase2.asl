func main () => integer
begin
  var x : integer;
  // The following case statement:
  case 3 of
    when 42 => x = 42;
    when <= 42 => x = 0;
  end;
  // can be desugared into the following statement:
  let discriminant_var: integer {3} = 3;
  if discriminant_var IN {42} then
    x = 42;
  else
    if discriminant_var IN {<= 42} then
      x = 0;
    else
      unreachable;
    end;
  end;

  return x;
end;