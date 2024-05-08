{ %OPT=-O2 -Sew }

program pure1b;
{$MODE OBJFPC}

function Factorial(N: Cardinal): Cardinal; pure;
  begin
    if N < 2 then
      Result := 1
    else
      Result := N * Factorial(N - 1);
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