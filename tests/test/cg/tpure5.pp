{ %OPT=-Sew }

{ Evaluate the use of Break in a for-loop }

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
  FindResult := FindFirstOf('The quick brown fox jumps over the lazy dog', 'j');
  if FindResult <> 21 then
    begin
      WriteLn('FAIL: Returned ', FindResult, ' but expected 21');
      Halt(1);
    end;
  WriteLn('ok');
end.