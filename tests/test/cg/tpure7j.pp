{ %OPT=-Sew }

{ Test unconditional Break in repeat..until False (sort of) loop }

{$MODE OBJFPC}
program tpure7j;

function UnconditionalBreak(Input: Boolean): Boolean; pure;
  begin
    Result := not Input;
      
    repeat
      if Input then
        Break;
    until not Input;
  end;
  
begin
  if UnconditionalBreak(True) then
    begin
      WriteLn('FAIL: Pure analysis returned wrong value');
      Halt(1);
    end;
  WriteLn('ok');
end.