// Experimental implementation of PSTATE as two independant variables.

type ProcState of bits(8) {
  [0] N,
  [1] Z,
  [2] C,
  [3] V,
  [7] SomethingElse,
};

var _PSTATE : ProcState;
var _NZCV : ProcState;

func isNZCV(n:integer) => boolean
begin
  return 0 <= n && n < 4 ;
end;

accessor PSTATE() <=> v: ProcState
begin
  getter
   return _PSTATE;
  end;

  setter
    _PSTATE = v;
  end;
end;

accessor PSTATE(n:integer) <=> v: bits(1)
begin
  getter
    if isNZCV(n) then
      return _NZCV[n];
    else
      return _PSTATE[n];
    end;
  end;

  setter
    if isNZCV(n) then
      _NZCV[n] = v;
    else
      _PSTATE[n] = v;
    end;
  end;
end;

accessor PSTATE(n:integer,m:integer) <=> v: bits(2)
begin
  getter
    if isNZCV(n) && isNZCV(m) then
      return _NZCV[n,m];
    else
      return _PSTATE[n,m];
    end;
  end;

  setter
    if isNZCV(n) && isNZCV(m) then
      _NZCV[n,m] = v;
    else
      _PSTATE[n,m] = v;
    end;
  end;
end;

accessor PSTATE(n:integer,m:integer,o:integer) <=> v: bits(3)
begin
  getter
    if isNZCV(n) && isNZCV(m) && isNZCV(o) then
      return _NZCV[n,m,o];
    else
      return _PSTATE[n,m,o];
    end;
  end;

  setter
    if isNZCV(n) && isNZCV(m) && isNZCV(o) then
      _NZCV[n,m,o] = v;
    else
      _PSTATE[n,m,o] = v;
    end;
  end;
end;

accessor PSTATE(n:integer,m:integer,o:integer,p:integer) <=> v: bits(4)
begin
  getter
    if isNZCV(n) && isNZCV(m) && isNZCV(o) && isNZCV(p) then
      return _NZCV[n,m,o,p];
    else
      return _PSTATE[n,m,o,p];
    end;
  end;

  setter
    if isNZCV(n) && isNZCV(m) && isNZCV(o) && isNZCV(p) then
      _NZCV[n,m,o,p] = v;
    else
      _PSTATE[n,m,o,p] = v;
    end;
  end;
end;

func main () => integer
begin
  - = PSTATE();
  - = PSTATE().N;
  - = PSTATE().[N, Z];
  PSTATE() = ARBITRARY: ProcState;
  PSTATE().N = '1';
  PSTATE().[N, Z] = '00';

  return 0;
end;
