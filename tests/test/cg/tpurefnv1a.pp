{ %OPT=-Sew -OoNOPEEPHOLE }

{ This test evaluates the power of "pure" for a hash function }

program tpurefnv1a;

{$MODE OBJFPC}

const
  FNV64_offset_basis = QWord($CBF29CE484222325);
  FNV64_prime        = QWord($00000100000001B3);

{$PUSH}{$Q-}
function FNV1a64Digest(const Input: ansistring): QWord; pure;
var
  I: Integer;
begin
  Result := FNV64_offset_basis;
  for I := 1 to Length(Input) do
    Result := (Result xor Byte(Input[I])) * FNV64_prime;
end;
{$POP}

const
  StringConst = 'The quick brown fox jumps over the lazy dog';
  ForceNonConst: ansistring = 'The quick brown fox jumps over the lazy dog';
  ExpectedHash = QWord($F3F9B7F5E7E47110);
  
var
  HashRes, ControlRes: QWord;
begin
  HashRes := FNV1a64Digest(StringConst);
  ControlRes := FNV1a64Digest(ForceNonConst); { This will be called as a regular function }
  
  WriteLn('FNV-1a(''', StringConst, ''') = $', HexStr(HashRes, 16));
  
  if HashRes <> ControlRes then
    begin
	  WriteLn('FAIL: Expected $', hexstr(ControlRes, 16), ' but got $', hexstr(HashRes, 16));
      Halt(1);
	end;

  if HashRes <> ExpectedHash then
    begin
	  WriteLn('FAIL: FNV-1a algorithm is non-conformant');
	  Halt(2);
	end;
  WriteLn('ok');
end.