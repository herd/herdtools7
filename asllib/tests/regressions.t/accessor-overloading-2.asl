type MyBits of bits(4) {
    [0] a
};

var arr : array[[16]] of bits(4);

accessor H() <=> value : bits(4)
begin
    getter
        println "nullary getter";
        return Zeros{4};
    end;
    setter
        println "nullary setter";
        arr[[UInt(H('11'))]] = H('01');
        H('10').a = '1';
    end;
end;

accessor H(x : bits(2)) <=> value : MyBits
begin
    getter
        println "unary getter";
        return Zeros{4};
    end;
    setter
        println "unary setter";
        pass;
    end;
end;

func main() => integer
begin
  H() = '1111';
  return 0;
end;
