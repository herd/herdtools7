type Ex of exception{-};

func main () => integer
begin
  try
    var y : integer = 5;
    throw Ex {-};
  catch
    when exn: Ex =>
      assert y == 5; // y should not be found in dynamic environment here
      var y : integer = 6;
      assert y == 6;
  end;

  return 0;
end;
