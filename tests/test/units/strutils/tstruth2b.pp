// tstruth2bu.pp defines tstruth2b_unicode and reuses this source.

{$IFDEF FPC}
{$ifdef tstruth2b_unicode}
  // PChar in Delphi is PWideChar so make sure the tests behave the same
  {$mode DelphiUnicode}
{$else}
  // PChar in Delphi < 2009 is PAnsiChar so make sure the tests behave the same but actually almost none of the functions are available...
  {$mode Delphi}
{$endif}
{$ELSE}
{$ifdef UNICODE} // For running in Delphi, detect unicodeness from Delphi UNICODE define.
{$define tstruth2b_unicode}
{$endif}
{$ENDIF}

uses
  SysUtils,
  {$IFDEF FPC}
    StrUtils
  {$ELSE}
    Classes
  {$ENDIF};

var
  BinValueBytes: TBytes;
  HexValueBytes: TBytes;
  HexInLen: Integer;
  ret: Integer;
  StaticBinValueBytes: array[0 .. 9] of byte;
  somethingFailed: boolean = false;

procedure PrepareBinValueBytes(n: integer);
begin
  SetLength(BinValueBytes, n);
  for n := n downto 1 do
    BinValueBytes[n - 1] := n; // 1, 2, 3, ..., n.
end;

procedure VerifyHexToBin(ret, expectedRet: integer; const expectedBinValueBytes: array of byte; const what: string);
var
  i: integer;
begin
  if (ret <> expectedRet) or not CompareMem(PByte(BinValueBytes), @expectedBinValueBytes[0], length(expectedBinValueBytes)) then
  begin
    writeln('Bad ', what, ', got:');
    writeln('ret = ', ret);
    for i := 0 to High(BinValueBytes) do
      write(BinValueBytes[i], ' ');
    writeln;
    writeln('expected:');
    writeln('ret = ', expectedRet);
    for i := 0 to High(expectedBinValueBytes) do
      write(expectedBinValueBytes[i], ' ');
    writeln;
    somethingFailed := true;
  end;
end;

const
  HexInputA: AnsiString = '1decaf';
  HexInputW: WideString = '1decaf';
  HexCorruptInputW: WideString = '9abcdefg';
  HexOffsetInputW: WideString = '608da975';

begin
  writeln('start testing of HexToBin methods');

  {* test simple methods *}
  // ansistring
  // write 2 bytes into 1 byte
  HexInLen := Length(HexInputA) * SizeOf(AnsiChar) div 2;

  PrepareBinValueBytes(HexInLen + 1);
  VerifyHexToBin(
    HexToBin(PAnsiChar(@HexInputA[1]), @BinValueBytes[0], HexInLen),
    3, [$1d, $ec, $af, 4], 'HexToBin(@AnsiString[1], @TBytes[0], Integer)');

  PrepareBinValueBytes(HexInLen + 1);
  VerifyHexToBin(
    HexToBin(PChar(HexInputA), PChar(BinValueBytes), HexInLen),
{$ifdef tstruth2b_unicode}
    0, [1, 2, 3, 4]
{$else}
    3, [$1d, $ec, $af, 4]
{$endif},
    'HexToBin(PChar(HexInputA), PChar(BinValueBytes), HexInLen)');

  PrepareBinValueBytes(HexInLen + 1);
  VerifyHexToBin(
    HexToBin(PAnsiChar(HexInputA), PAnsiChar(BinValueBytes), HexInLen),
    3, [$1d, $ec, $af, 4], 'HexToBin(PAnsiChar(HexInputA), PAnsiChar(BinValueBytes), HexInLen)');

  PrepareBinValueBytes(HexInLen + 1);
  VerifyHexToBin(
    HexToBin(PAnsiChar(HexInputA), Pointer(BinValueBytes), HexInLen),
    3, [$1d, $ec, $af, 4], 'HexToBin(PAnsiChar(HexInputA), Pointer(BinValueBytes), HexInLen)');

  PrepareBinValueBytes(HexInLen + 1);
  VerifyHexToBin(
    HexToBin(PAnsiChar(HexInputA), BinValueBytes, HexInLen),
    3, [$1d, $ec, $af, 4], 'HexToBin(PAnsiChar(HexInputA), BinValueBytes, HexInLen)');

  // Verify untyped overload.
  PrepareBinValueBytes(HexInLen + 1);
  Move(BinValueBytes[0], StaticBinValueBytes, HexInLen + 1);
  ret := HexToBin(PAnsiChar(HexInputA), StaticBinValueBytes, HexInLen);
  Move(StaticBinValueBytes, BinValueBytes[0], HexInLen + 1);
  VerifyHexToBin(ret, 3, [$1d, $ec, $af, 4], 'HexToBin(PAnsiChar(HexInputA), StaticBinValueBytes, HexInLen)');

  // widestring
  // write 4 bytes into 1 byte
  HexInLen := Length(HexInputW) * SizeOf(WideChar) div 4;

  PrepareBinValueBytes(HexInLen + 1);
  VerifyHexToBin(
    HexToBin(PWideChar(@HexInputW[1]), @BinValueBytes[0], HexInLen),
    3, [$1d, $ec, $af, 4], 'HexToBin(PWideChar(@HexInputW[1]), @BinValueBytes[0], HexInLen)');

  PrepareBinValueBytes(HexInLen + 1);
  VerifyHexToBin(
    HexToBin(PWideChar(HexInputW), PAnsiChar(BinValueBytes), HexInLen),
    3, [$1d, $ec, $af, 4], 'HexToBin(PWideChar(HexInputW), PAnsiChar(BinValueBytes), HexInLen)');

  PrepareBinValueBytes(HexInLen + 1);
  VerifyHexToBin(
    HexToBin(PWideChar(HexInputW), Pointer(BinValueBytes), HexInLen),
    3, [$1d, $ec, $af, 4], 'HexToBin(PWideChar(HexInputW), Pointer(BinValueBytes), HexInLen)');

  PrepareBinValueBytes(HexInLen + 1);
  VerifyHexToBin(
    HexToBin(PWideChar(HexInputW), BinValueBytes, HexInLen),
    3, [$1d, $ec, $af, 4], 'HexToBin(PWideChar(HexInputW), BinValueBytes, HexInLen)');

  // Verify untyped overload.
  PrepareBinValueBytes(HexInLen + 1);
  Move(BinValueBytes[0], StaticBinValueBytes, HexInLen + 1);
  ret := HexToBin(PWideChar(HexInputW), StaticBinValueBytes, HexInLen);
  Move(StaticBinValueBytes, BinValueBytes[0], HexInLen + 1);
  VerifyHexToBin(ret, 3, [$1d, $ec, $af, 4], 'HexToBin(PWideChar(HexInputW), StaticBinValueBytes, HexInLen)');

  // not fully valid widestring input
  HexInLen := Length(HexCorruptInputW) * SizeOf(WideChar) div 4;
  PrepareBinValueBytes(HexInLen + 1);
  VerifyHexToBin(
    HexToBin(PWideChar(HexCorruptInputW), PAnsiChar(BinValueBytes), HexInLen),
    3, [$9a, $bc, $de, 4], 'HexToBin(PWideChar(HexCorruptInputW), PAnsiChar(BinValueBytes), HexInLen)');

  PrepareBinValueBytes(HexInLen + 1);
  VerifyHexToBin(
    HexToBin(PWideChar(HexCorruptInputW), Pointer(BinValueBytes), HexInLen),
    3, [$9a, $bc, $de, 4], 'HexToBin(PWideChar(HexCorruptInputW), Pointer(BinValueBytes), HexInLen)');

  PrepareBinValueBytes(HexInLen + 1);
  VerifyHexToBin(
    HexToBin(PWideChar(HexCorruptInputW), BinValueBytes, HexInLen),
    3, [$9a, $bc, $de, 4], 'HexToBin(PWideChar(HexCorruptInputW), BinValueBytes, HexInLen)');

  {* test complex offset methods *}
  // ansistring
  HexInLen := Length(HexInputA) div 2;
{$ifdef tstruth2b_unicode}
  // only available as PWideChar in newer Delphi
  PrepareBinValueBytes(HexInLen + 3);
  VerifyHexToBin(
    HexToBin(PChar(HexInputA), 2, BinValueBytes, 2, HexInLen),
    0, [1, 2, 3], 'HexToBin(PChar(HexInputA), 2, BinValueBytes, 2, HexInLen)');
{$endif}
  HexValueBytes := TEncoding.ASCII.GetBytes(HexInputW);
  PrepareBinValueBytes(HexInLen + 3);
  VerifyHexToBin(
    HexToBin(HexValueBytes, 2, BinValueBytes, 2, HexInLen),
    2, [1, 2, $ec, $af, 5], 'HexToBin(HexValueBytes, 2, BinValueBytes, 2, HexInLen) #1');

  // widestring
  HexInLen := Length(HexInputW) div 2;
{$ifdef tstruth2b_unicode}
  // only available as PWideChar in newer Delphi
  PrepareBinValueBytes(HexInLen + 3);
  VerifyHexToBin(
    HexToBin(PChar(HexInputW), 2, BinValueBytes, 2, HexInLen),
    2, [1, 2, $ec, $af, 5], 'HexToBin(PChar(HexInputW), 2, BinValueBytes, 2, HexInLen)');
{$endif}
  HexValueBytes := TEncoding.ASCII.GetBytes(HexInputW);
  PrepareBinValueBytes(HexInLen + 3);
  VerifyHexToBin(
    HexToBin(HexValueBytes, 2, BinValueBytes, 2, HexInLen),
    2, [1, 2, $ec, $af, 5], 'HexToBin(HexValueBytes, 2, BinValueBytes, 2, HexInLen) #2');

  // documentation offset example
  HexInLen := Length(HexOffsetInputW) * SizeOf(WideChar) div 4;
{$ifdef tstruth2b_unicode}
  // only available as PWideChar in newer Delphi
  PrepareBinValueBytes(HexInLen + 1);
  VerifyHexToBin(
    HexToBin(PChar(HexOffsetInputW), 4, BinValueBytes, 0, HexInLen),
    2, [$a9, $75, 3], 'HexToBin(PChar(HexOffsetInputW), 4, BinValueBytes, 0, HexInLen)');
{$endif}
  HexValueBytes := TEncoding.ASCII.GetBytes(HexOffsetInputW);
  PrepareBinValueBytes(HexInLen + 1);
  VerifyHexToBin(
    HexToBin(HexValueBytes, 4, BinValueBytes, 0, HexInLen),
    2, [$a9, $75, 3], 'HexToBin(HexValueBytes, 4, BinValueBytes, 0, HexInLen)');

  writeln('testing of HexToBin methods ended');
  if somethingFailed then
    halt(1);
end.
