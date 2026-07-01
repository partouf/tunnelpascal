{ %OPT=-Sew }
{ %FAIL }

{ Delphi 13 NameOf intrinsic: NameOf(X) must fold to a compile-time string
  constant - verified by its observable effect.

  Because NameOf(gCounter) is the constant string "gCounter", the condition
  NameOf(gCounter) <> "gCounter" is constant-false, so the then-branch
  "i := 1" is unreachable and the compiler emits an Unreachable-code warning.
  Compiled with -Sew (warnings treated as errors) this becomes a fatal error,
  and the FAIL directive expects compilation to fail.

  If NameOf ever stops folding to a constant, no branch is unreachable, no
  warning is emitted, compilation SUCCEEDS, and this FAIL test fails --
  flagging the regression.

  NOTE: this file must contain no nested braces, so that under -Sew the only
  diagnostic is the Unreachable-code warning we are asserting. }

program tnameof3;

{$mode delphi}

var
  gCounter: Integer;
  i: Integer;
begin
  if NameOf(gCounter) <> 'gCounter' then
    i := 1
  else
    i := 2;
  WriteLn(i);
end.
