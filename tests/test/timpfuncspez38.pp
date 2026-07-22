{ Implicit specialization must not crash when a generic function whose
  parameter is an inline specialization (TArray<T>) is imported from another
  unit compiled in the same invocation as its caller. See the generic-param
  scope fix in pgenutil.pas (partial specialization lifetime). }
program timpfuncspez38;

{$mode delphi}
{$modeswitch implicitfunctionspecialization}

uses
  uimpfuncspez38b, uimpfuncspez38a;

var
  S: RawByteString;
  A: TArray<Integer>;
begin
  { non-generic overload selected for the string argument }
  if Test(S) <> 1 then
    Halt(1);
  { generic overload implicitly specialized for the array argument }
  SetLength(A, 1);
  if Test(A) <> 2 then
    Halt(2);
  WriteLn('ok');
end.
