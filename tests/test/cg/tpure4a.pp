{ %FAIL }
{ $OPT=-O2 -Sew }

{$MODE OBJFPC}

program tpure4a;

function TestFrac(d : ValReal) : ValReal; [internproc:fpc_in_frac_real];

function intpower(base : Double;exponent : longint) : Double; pure;
  begin
    if exponent<0 then
      begin
        base:=1.0/base;
        exponent:=-exponent;
      end;
    intpower:=1.0;
    while exponent<>0 do
      begin
        if exponent and 1<>0 then
          intpower:=intpower*base;
        exponent:=exponent shr 1;
        base:=sqr(base);
      end;
  end;

function Power(base,exponent : Double) : Double; pure;
  begin
    if Exponent=0.0 then
      result:=1.0
    else if (base=0.0) and (exponent>0.0) then
      result:=0.0
    else if (TestFrac(exponent)=0.0) and (abs(exponent)<=maxint) then
      result:=intpower(base,trunc(exponent))
    else
      result:=exp(exponent * ln (base));
  end;

begin
  if Power(2, 3) <> 8.0 then
    Halt(1);

  WriteLn('ok');
end.