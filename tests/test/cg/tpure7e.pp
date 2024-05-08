{ %OPT=-Sew }

{ Ensure repeat..until with recursion is properly handled in pure functions }

{$MODE OBJFPC}
program tpure7e;

function RepeatRecursePure(Input: Boolean): Cardinal; pure;
  begin
    Result := 0;
    if Input then
      Exit;
      
    repeat
      Inc(Result);
    until (RepeatRecursePure(not Input) = 0);
  end;
  
begin
  if RepeatRecursePure(False) <> 1 then
    begin
      WriteLn('FAIL: Pure analysis returned wrong value');
      Halt(1);
    end;
  WriteLn('ok');
end.