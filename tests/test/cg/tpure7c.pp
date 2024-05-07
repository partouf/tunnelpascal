{ %NORUN }

{ Evaluate maliciously-written pure function that contains an infinite recursive loop }

{$MODE OBJFPC}
program tpure7b;

function MaliciousPure(Input: Boolean): Cardinal; pure;
  begin
    Result := 0;
    while (MaliciousPure(Input) = 0) do
      Inc(Result);
  end;
  
begin
  WriteLn(MaliciousPure(True));
end.