// The initializer of config storage elements may be overridden before
// execution-time initializer expressions are evaluated.
config configWid: integer {32, 64} = 32;
config gExtra: integer {0, 8} = 0;

// Since configWid is a constrained integer we can declare:
var gReg: bits(configWid);
var gRegF: bits(configWid + gExtra);

constant wid = 32;
type busTy of bits(wid);
type recTy of record {bus: busTy, valid: bit};

func constType()
begin
    var R: bits(wid); // Legal since wid is a constant.
end;
