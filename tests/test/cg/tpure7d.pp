{ %NORUN }

{ Evaluate maliciously-written pure function that contains an infinite loop (the while condition should evaluate successfully to 0) }

{$MODE OBJFPC}
program tpure7b;

function MaliciousPure(Input: Boolean): Cardinal; pure;
  begin
    Result := 0;
    while (Input and (MaliciousPure(not Input) = 0)) do
      Inc(Result);
  end;
  
begin
  WriteLn(MaliciousPure(True));
end.