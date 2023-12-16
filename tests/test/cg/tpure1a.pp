{ %OPT=-O2 -Sew }

program tpure1a;
{$MODE OBJFPC}
{$COPERATORS ON}

function Factorial(N: Cardinal): Cardinal; pure;
  var
    X: Integer;
  begin
    Result := 1;
    for X := N downto 2 do
      Result *= X;
  end;

var
  Output: Cardinal;
begin
  Output := Factorial(5);
  WriteLn('5! = ', Output);
  if Output <> 120 then
    Halt(1);

  WriteLn('ok');
end.