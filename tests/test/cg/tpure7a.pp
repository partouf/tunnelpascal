{ %NORUN }

{ Evaluate maliciously-written pure function that contains an infinite recursive loop }

{$MODE OBJFPC}
program tpure7a;

function MaliciousPure(Input: Boolean): Cardinal; pure;
  begin
    Result := 0;
    repeat
      Inc(Result);
    until (MaliciousPure(not Input) = 0);
  end;
  
begin
  WriteLn(MaliciousPure(False));
end.