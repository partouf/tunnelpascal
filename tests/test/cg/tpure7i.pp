{ %OPT=-Sew }

{ Test unconditional Break in repeat..until False loop }

{$MODE OBJFPC}
program tpure7i;

function UnconditionalBreak(Input: Boolean): Boolean; pure;
  begin
    Result := not Input;
      
    repeat
      begin
        Break;
      end;
    until False;
  end;
  
begin
  if UnconditionalBreak(True) then
    begin
      WriteLn('FAIL: Pure analysis returned wrong value');
      Halt(1);
    end;
  WriteLn('ok');
end.