// The initializer of config storage elements may be overridden before
// execution-time initializer expressions are evaluated.
config configWid: integer {32, 64} = 32;
config gExtra: integer {0, 8} = 0;

// Since configWid is a constrained integer we can declare:
var gReg: bits(configWid); // i.e. bits({32,64})
var gRegF: bits(configWid + gExtra); // i.e. bits({32,40,64,72})
// The expression (configWid+gExtra) is an integer {32,40,64,72}
