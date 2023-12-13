func print_me ()
  begin
    for i = 0 to 42 do
      if i >= 3 then
      return;
     end
     print(i);
    end
  end

func main () => integer 
  begin 
    print_me (); 
    return 0; 
  end
