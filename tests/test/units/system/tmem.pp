{ %OPT=-Sew }
{ the -Sew causes the compilation to fail if the target supports only unsigned numbers as count for move/fillchar }

{ This unit tests the basic routines         }
{ which are usually coded in assembler       }
{ Mainly used in porting to other processors }
{********************************************}
{ Tested against Delphi 6 and Delphi 3       }
{********************************************}

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$q-,r-}
program tmem;

type
  TByteArray = array[0 .. 0] of byte;


procedure test_move(n: integer; const fuzzValues: array of integer; large: boolean);

type
  FuzzModeEnum = (Fuzz0Plus, FuzzHalfMinus, FuzzHalfPlus, FuzzEndMinus);

  procedure PrepareArray(out p; n: integer);
  var
    i, v: integer;
  begin
    v := 1;
    for i := 0 to n - 1 do
    begin
      TByteArray(p)[i] := v;
      v := v + 1;
      if v > 255 then
        v := 1;
    end;
  end;

  function GetFuzzedValue(mode: FuzzModeEnum; n, fuzz: integer): integer;
  begin
    result := -1;
    case mode of
      Fuzz0Plus: result := fuzz;
      FuzzHalfMinus: if fuzz > 0 then result := integer(cardinal(n) div 2) - fuzz;
      FuzzHalfPlus: result := integer(cardinal(n) div 2) + fuzz;
      FuzzEndMinus: result := n - fuzz;
    end;
  end;

var
  orig, got: array of byte;

  procedure Test(src, dst, count: integer);
  var
    icheck, expect: integer;
  begin
{$ifdef CPUI8086}
    if count < 0 then
      exit;
{$endif}
    PrepareArray(got[0], n);
    Move(got[src], got[dst], count);
    for icheck := 0 to n - 1 do
    begin
      if (icheck >= dst) and (icheck < dst + count) then
        expect := orig[src + (icheck - dst)]
      else
        expect := orig[icheck];
      if got[icheck] <> expect then
      begin
        writeln('Move(src = ', src, ', dst = ', dst, ', count = ', count, '): got[', icheck, '] = ', got[icheck], ', expected ', expect, '.');
        halt(1);
      end;
    end;
  end;

var
  srcMode, dstMode, countMode: FuzzModeEnum;
  iSrcFuzz, iDstFuzz, iCountFuzz, src, dst, count: integer;

begin
  SetLength(orig, n);
  SetLength(got, n);
  PrepareArray(orig[0], n);
  for srcMode := Fuzz0Plus to FuzzEndMinus do
    for iSrcFuzz := 0 to High(fuzzValues) do
    begin
      src := GetFuzzedValue(srcMode, n, fuzzValues[iSrcFuzz]);
      if (src >= 0) and (src <= n) then
        for dstMode := Fuzz0Plus to FuzzEndMinus do
          for iDstFuzz := 0 to High(fuzzValues) do
          begin
            dst := GetFuzzedValue(dstMode, n, fuzzValues[iDstFuzz]);
            if (dst >= 0) and (dst <= n) then
            begin
              { negative move count }
              if not large then
                Test(src, dst, -12);
              for countMode := FuzzModeEnum(ord(Fuzz0Plus) + ord(large)) to FuzzEndMinus do
                for iCountFuzz := 0 to High(fuzzValues) do
                begin
                  count := GetFuzzedValue(countMode, n, fuzzValues[iCountFuzz]);
                  if (count >= 0) and (src + count <= n) and (dst + count <= n) then
                    Test(src, dst, count);
                end;
            end;
          end;
    end;
end;


type
  TFillProc = procedure(out p; n: integer; const value);


procedure test_fill(fill: TFillProc; tySize: integer; const tyName: string);
const
  MainBytes = 256;
  Misalignments: array[0 .. 3] of integer = (0, 1, 4, 15);
var
  va, vb: array[0 .. 7] of byte;
  data: array of byte;
  i, tysInData, tysInMainBytes, start, count, iMisalignment, misalignment, expect: integer;
begin
  for i := 0 to High(va) do
  begin
    va[i] := 1 + i;
    vb[i] := 100 + i;
  end;
  tysInData := (MainBytes + (Misalignments[High(Misalignments)] + 1) + tySize - 1) div tySize;
  tysInMainBytes := MainBytes div tySize;
  SetLength(data, tysInData * tySize);
  for iMisalignment := 0 to High(Misalignments) do
  begin
    misalignment := Misalignments[iMisalignment];
    for start := 0 to tysInMainBytes do
      for count := -1 to tysInMainBytes - start do
      begin
{$ifdef CPUI8086}
        { i8086 uses word as the count parameter for fillchar }
        if count < 0 then
          continue;
{$endif}
        fill(data[0], tysInData, va);
        fill(data[misalignment + start * tySize], count, vb);
        for i := 0 to High(data) do
        begin
          if (i >= misalignment + start * tySize) and (i < misalignment + (start + count) * tySize) then
            expect := vb[(i - start * tySize - misalignment) and (tySize - 1)]
          else
            expect := va[i and (tySize - 1)];
          if data[i] <> expect then
          begin
            writeln('Fill', tyName, '(start = ', start, ', count = ', count, ', misalignment = ', misalignment, '): byte #', i, ' = ', data[i], ', expected ', expect, '.');
            halt(1);
          end;
        end;
      end;
  end;
end;


procedure FillCharImpl(out p; n: integer; const value);
begin
  FillChar(p, n, byte(value));
end;


{$ifdef fpc}
procedure FillWordImpl(out p; n: integer; const value);
begin
  FillWord(p, n, unaligned(word(value)));
end;


procedure FillDWordImpl(out p; n: integer; const value);
begin
  FillDWord(p, n, unaligned(dword(value)));
end;


procedure FillQWordImpl(out p; n: integer; const value);
begin
  FillQWord(p, n, unaligned(qword(value)));
end;


procedure test_movechar0;
  procedure test(value, required: longint);
  begin
    if value <> required then
      begin
        writeln('Got ',value,' instead of ',required);
        halt(1);
      end;
  end;
const
  MAX_TABLE = 69;  { this value should not be even ! }
  DEFAULT_VALUE = $55;
  FILL_VALUE = $33;
var
  i : integer;
  dst_arraybyte : array[1..MAX_TABLE] of byte;
  src_arraybyte : array[1..MAX_TABLE] of byte;
begin
  { non-aligned count }
  write('testing movechar0 (non-aligned size)...');
  for i := 1 to MAX_TABLE do
  begin
    dst_arraybyte[i] := DEFAULT_VALUE;
    src_arraybyte[i] := FILL_VALUE;
  end;
  movechar0(src_arraybyte, dst_arraybyte, MAX_TABLE-2);
  test(dst_arraybyte[MAX_TABLE], DEFAULT_VALUE);
  test(dst_arraybyte[MAX_TABLE-1], DEFAULT_VALUE);
  for i:= 1 to MAX_TABLE-2 do
    test(dst_arraybyte[i], FILL_VALUE);
  writeln('Passed!');
  { modulo 2 count fill }
  { non-aligned count }
  write('testing movechar0 (aligned size)...');
  for i := 1 to MAX_TABLE do
  begin
    dst_arraybyte[i] := DEFAULT_VALUE;
    src_arraybyte[i] := FILL_VALUE;
  end;
  movechar0(src_arraybyte, dst_arraybyte, MAX_TABLE-1);
  test(dst_arraybyte[MAX_TABLE], DEFAULT_VALUE);
  for i:= 1 to MAX_TABLE-1 do
    test(dst_arraybyte[i], FILL_VALUE);
  writeln('Passed!');
  { zero movechar0 count }
  write('test movechar0 (zero count)...');
  for i := 1 to MAX_TABLE do
  begin
    dst_arraybyte[i] := DEFAULT_VALUE;
    src_arraybyte[i] := FILL_VALUE;
  end;
  movechar0(src_arraybyte,dst_arraybyte, 0);
  for i:= 1 to MAX_TABLE do
    test(dst_arraybyte[i], DEFAULT_VALUE);
  writeln('Passed!');
  { withh null value as first value in index }
  write('test movechar0 with null character...');
  for i := 1 to MAX_TABLE do
  begin
    dst_arraybyte[i] := DEFAULT_VALUE;
    src_arraybyte[i] := 0;
  end;
  movechar0(src_arraybyte, dst_arraybyte, MAX_TABLE-1);
  { nothing should have been moved }
  for i:= 1 to MAX_TABLE do
    test(dst_arraybyte[i], DEFAULT_VALUE);
  writeln('Passed!');
  { with null value as second value in index }
  write('test movechar0 with null character (and char)...');
  for i := 1 to MAX_TABLE do
  begin
    dst_arraybyte[i] := DEFAULT_VALUE;
  end;
  src_arraybyte[1] := FILL_VALUE;
  src_arraybyte[2] := 0;
  movechar0(src_arraybyte, dst_arraybyte, MAX_TABLE-1);
  test(dst_arraybyte[1], FILL_VALUE);
  { the rest should normally not have bene touched }
  test(dst_arraybyte[2], DEFAULT_VALUE);
  writeln('Passed!');
end;
{$endif}


begin
  test_fill(FillCharImpl, 1, 'Char');
  test_move(150, [0, 1, 3, 4, 5, 7, 8, 9, 15, 16, 17, 30, 31, 32, 47, 48, 49], false);
{$ifndef CPU16}
  test_move(500000, [0, 6], true);
{$endif CPU16}
{$ifdef fpc}
  test_fill(FillWordImpl, 2, 'Word');
  test_fill(FillDWordImpl, 4, 'DWord');
  test_fill(FillQWordImpl, 8, 'QWord');
  test_movechar0;
{$endif}
end.
