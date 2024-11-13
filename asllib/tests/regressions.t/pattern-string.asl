func myfunction( s : string)
begin
   case s of
       when "hello" =>
           print("helloworld\n");
        otherwise =>
           return;
    end;
end;

func main () => integer
begin
  myfunction ("Hello");
  myfunction ("helloworld\n");

  return 0;
end;
