{ %OPT=-Sew }
{ %FAIL }

{ Delphi 13 inline "if" expression: compile-time folding of a constant
  condition — verified by its observable effect.

  The inner inline-if "if 2 > 1 then True else False" must fold to the
  constant True at compile time. That makes the outer if-statement's else
  branch "i := 2" unreachable, so the compiler emits an "Unreachable code"
  warning. Compiled with -Sew (warnings treated as errors) this becomes a
  fatal error, and the FAIL directive expects compilation to fail.

  If constant folding ever regresses, the inline-if would produce a runtime
  value instead of a constant, no branch would be unreachable, no warning
  would be emitted, compilation would SUCCEED, and this FAIL test would fail
  -- flagging the regression.

  NOTE: this file must contain no nested braces, so that under -Sew the only
  diagnostic is the Unreachable-code warning we are asserting. }

program tinlineif3;

{$mode delphi}

var
  i: Integer;
begin
  if (if 2 > 1 then True else False) then
    i := 1
  else
    i := 2;   { unreachable: reached only if the inline-if folded to a constant }
  WriteLn(i);
end.
