{ %OPT=-Sew }

{ Evaluate the use of Break in a for-loop (that's never executed) but also see
  if Result is set to Length(Message) + 1 (44) after pure function analysis }

{$MODE OBJFPC}

program tpure5;

function FindFirstOf(Message: ansistring; CharToFind: Char): LongWord; pure;
begin
  Result := 0;
  for Result := 1 to Length(Message) do
  begin
    if Message[Result] = CharToFind then
      Break;
  end;
end;

var
  FindResult: LongWord;
begin
  FindResult := FindFirstOf('The quick brown fox jumps over the lazy dog', '.');
  if FindResult <> 44 then
    begin
      WriteLn('FAIL: Returned ', FindResult, ' but expected 44');
      Halt(1);
    end;
  WriteLn('ok');
end.