{ %NORUN }

{ Evaluate maliciously-written pure function that contains an infinite loop }

{$MODE OBJFPC}
program tpure7b;

function MaliciousPure(Input: Boolean): Cardinal; pure;
  begin
    Result := 0;
    while Input do
      Inc(Result);
  end;
  
begin
  WriteLn(MaliciousPure(True));
end.