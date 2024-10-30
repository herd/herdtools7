type E of exception {};

func always_throws () => integer
begin
  throw E {};
end

func inherited_always_throws () => integer
begin
  let - = always_throws ();
end

func main () => integer
begin
  var y: integer = 0;
  try
    let x = inherited_always_throws ();
  catch
    when E => y = 42;
  end

  assert y == 42;

  return 0;
end


