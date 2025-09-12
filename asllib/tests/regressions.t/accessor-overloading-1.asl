accessor H() <=> value : bits(64)
begin
    getter
        return Zeros{64};
    end;
    setter
        H('10') = value;
    end;
end;

accessor H(x : bits(2)) <=> value : bits(64)
begin
    getter
        return Ones{64};
    end;
    setter
        pass;
    end;
end;

accessor H(x : bits(2), y: bits(64)) <=> value : bits(64)
begin
    getter
        return Ones{64};
    end;
    setter
        pass;
    end;
end;
