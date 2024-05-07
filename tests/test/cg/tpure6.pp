{ %OPT=-Sew }

program tpure6;
{$MODE OBJFPC}

function CollatzCount(N: Cardinal): Cardinal; pure;
  begin
    Result := 0;
    while N > 1 do
      begin
        if (N mod 2) = 0 then
          N := N div 2
        else
          N := (3 * N) + 1;
          
        Inc(Result);
      end;
  end;

const
  Expected: array[1..10] of Cardinal = (0, 1, 7, 2, 5, 8, 16, 3, 19, 6);

var
  Output: Cardinal;
begin
  { Loop manually unrolled so pure analysis kicks in }
  Output := CollatzCount(1);
  if Output <> Expected[1] then
    begin
      WriteLn('FAIL: CollatzCount(1) returned ', Output, ' but expected ', Expected[1]);
      Halt(1);
    end
  else
    WriteLn('CollatzCount(1) = ', Output);

  Output := CollatzCount(2);
  if Output <> Expected[2] then
    begin
      WriteLn('FAIL: CollatzCount(2) returned ', Output, ' but expected ', Expected[2]);
      Halt(2);
    end
  else
    WriteLn('CollatzCount(2) = ', Output);

  Output := CollatzCount(3);
  if Output <> Expected[3] then
    begin
      WriteLn('FAIL: CollatzCount(3) returned ', Output, ' but expected ', Expected[3]);
      Halt(3);
    end
  else
    WriteLn('CollatzCount(3) = ', Output);

  Output := CollatzCount(4);
  if Output <> Expected[4] then
    begin
      WriteLn('FAIL: CollatzCount(4) returned ', Output, ' but expected ', Expected[4]);
      Halt(4);
    end
  else
    WriteLn('CollatzCount(4) = ', Output);

  Output := CollatzCount(5);
  if Output <> Expected[5] then
    begin
      WriteLn('FAIL: CollatzCount(5) returned ', Output, ' but expected ', Expected[5]);
      Halt(5);
    end
  else
    WriteLn('CollatzCount(5) = ', Output);

  Output := CollatzCount(6);
  if Output <> Expected[6] then
    begin
      WriteLn('FAIL: CollatzCount(6) returned ', Output, ' but expected ', Expected[6]);
      Halt(6);
    end
  else
    WriteLn('CollatzCount(6) = ', Output);

  Output := CollatzCount(7);
  if Output <> Expected[7] then
    begin
      WriteLn('FAIL: CollatzCount(7) returned ', Output, ' but expected ', Expected[7]);
      Halt(7);
    end
  else
    WriteLn('CollatzCount(7) = ', Output);

  Output := CollatzCount(8);
  if Output <> Expected[8] then
    begin
      WriteLn('FAIL: CollatzCount(8) returned ', Output, ' but expected ', Expected[8]);
      Halt(8);
    end
  else
    WriteLn('CollatzCount(8) = ', Output);

  Output := CollatzCount(9);
  if Output <> Expected[9] then
    begin
      WriteLn('FAIL: CollatzCount(9) returned ', Output, ' but expected ', Expected[9]);
      Halt(9);
    end
  else
    WriteLn('CollatzCount(9) = ', Output);

  Output := CollatzCount(10);
  if Output <> Expected[10] then
    begin
      WriteLn('FAIL: CollatzCount(10) returned ', Output, ' but expected ', Expected[10]);
      Halt(10);
    end
  else
    WriteLn('CollatzCount(10) = ', Output);
    
  WriteLn('ok');
end.