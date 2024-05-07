{ %OPT=-Sew }

{ Test unconditional Break in "while True do" loop }

{$MODE OBJFPC}
program tpure7g;

function UnconditionalBreak(Input: Boolean): Boolean; pure;
  begin
    Result := not Input;
      
    while True do
      Break;
  end;
  
begin
  if UnconditionalBreak(True) then
    begin
      WriteLn('FAIL: Pure analysis returned wrong value');
      Halt(1);
    end;
  WriteLn('ok');
end.