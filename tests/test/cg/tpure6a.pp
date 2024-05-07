{ %OPT=-O3 -Sew -Co -OoLOOPUNROLL }

program tpure6a;
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

procedure DoPureTest;
  var
    Output, X: Cardinal;
  begin
    { See if loop unrollung works with pure functions }
    for X := 1 to 10 do
      begin
        Output := CollatzCount(X);
        if Output <> Expected[X] then
          begin
            WriteLn('FAIL: CollatzCount(', X, ') returned ', Output, ' but expected ', Expected[X]);
            Halt(X);
          end
        else
          WriteLn('CollatzCount(', X, ') = ', Output);
      end;
  end;
  
begin
  DoPureTest;    
  WriteLn('ok');
end.