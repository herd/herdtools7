accessor X() <=> value_in: bits (4)
begin
  readonly getter
        return '1000';
    end;
    setter
        unreachable;
    end;
end;

func X() // Illegal as accessor `X` also declared.
begin pass; end;
