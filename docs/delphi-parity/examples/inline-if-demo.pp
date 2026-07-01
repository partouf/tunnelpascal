{ Delphi 13 "inline if" (ternary) expression — feature demo.

  An if/then/else usable in expression position, yielding a value.
  The `else` branch is mandatory in expression form, and the result
  type is the unified type of the two branches.

  Gated to {$mode delphi} (and objfpc via modeswitch, TBD). }

program inline_if_demo;

{$mode delphi}

var
  n: Integer;
  s: string;
begin
  n := 5;

  // simplest form
  WriteLn(if n > 0 then 'positive' else 'non-positive');

  // embedded in a larger expression (parenthesised)
  s := 'count is ' + (if n = 1 then 'one' else IntToStr(n));
  WriteLn(s);

  // nested
  WriteLn(if n < 0 then 'neg' else if n = 0 then 'zero' else 'pos');

  // as an assignment source with branch-type unification (Byte vs Integer -> Integer)
  n := if n > 3 then Byte(1) else 100000;
  WriteLn(n);
end.
