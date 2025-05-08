// Experimental implementation of PSTATE as two independant variables.

var _PSTATE : ProcState;
var _PSTATE_N: bits(1);
var _PSTATE_Z: bits(1);
var _PSTATE_C: bits(1);
var _PSTATE_V: bits(1);

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
    if n == 3 then
      return _PSTATE_N;
    elsif n == 2 then
      return _PSTATE_Z;
    elsif n == 1 then
      return _PSTATE_C;
    elsif n == 0 then
      return _PSTATE_V;
    else
      return _PSTATE[n];
    end;
  end;

  setter
    if n == 3 then
      _PSTATE_N = v;
    elsif n == 2 then
      _PSTATE_Z = v;
    elsif n == 1 then
      _PSTATE_C = v;
    elsif n == 0 then
      _PSTATE_V = v;
    else
      _PSTATE[n] = v;
    end;
  end;
end;

accessor PSTATE(n:integer,m:integer) <=> v: bits(2)
begin
  getter
    return PSTATE(n) :: PSTATE(m);
  end;

  setter
    PSTATE(n) = v[1];
    PSTATE(m) = v[0];
  end;
end;

accessor PSTATE(n:integer,m:integer,o:integer) <=> v: bits(3)
begin
  getter
    return PSTATE(n) :: PSTATE(m) :: PSTATE(o);
  end;

  setter
    PSTATE(n) = v[2];
    PSTATE(m) = v[1];
    PSTATE(o) = v[0];
  end;
end;

accessor PSTATE(n:integer,m:integer,o:integer,p:integer) <=> v: bits(4)
begin
  getter
    return PSTATE(n) :: PSTATE(m) :: PSTATE(o) :: PSTATE(p);
  end;

  setter
    PSTATE(n) = v[3];
    PSTATE(m) = v[2];
    PSTATE(o) = v[1];
    PSTATE(p) = v[0];
  end;
end;
