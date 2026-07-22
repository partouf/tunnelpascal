unit uimpfuncspez38a;
{$mode delphi}
{$modeswitch implicitfunctionspecialization}
interface
function Test(const A: RawByteString): Integer; overload;
implementation
function Test(const A: RawByteString): Integer;
begin
  Result := 1;
end;
end.
