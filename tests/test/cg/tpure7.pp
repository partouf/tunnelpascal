{ %NORUN }

{ Evaluate maliciously-written pure function that contains an infinite loop }

{$MODE OBJFPC}
program tpure7;

function MaliciousPure(Input: Boolean): Cardinal; pure;
  begin
    Result := 0;
    repeat
      Inc(Result);
    until Input;
  end;
  
begin
  WriteLn(MaliciousPure(False));
end.