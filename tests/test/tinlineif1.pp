{ Delphi 13 inline "if" (ternary) expression — positive runtime test.

  Exercises:
   - basic boolean selection yielding a value
   - branch-type unification (narrow + wide ordinal -> wide)
   - string result branches
   - use inside a larger expression (concatenation)
   - nested inline-if
   - short-circuit: only the taken branch is evaluated (side effects)
   - use as a procedure/function argument and as a const-expr fold

  Exit code 0 = pass; halt(N) marks which check failed. }

program tinlineif1;

{$mode delphi}

var
  sidefx: Integer;

function Bump(aValue: Integer): Integer;
begin
  Inc(sidefx);
  Result := aValue;
end;

function PickStr(b: Boolean): string;
begin
  Result := if b then 'yes' else 'no';
end;

const
  { must fold at compile time when the condition is constant }
  CFold = if 2 > 1 then 41 else 99;

var
  i: Integer;
  s: string;
  by: Byte;
begin
  { 1. basic int selection }
  i := if True then 10 else 20;
  if i <> 10 then halt(1);
  i := if False then 10 else 20;
  if i <> 20 then halt(2);

  { 2. branch-type unification: Byte and Integer -> Integer-wide result }
  by := 7;
  i := if by > 3 then by else 100000;
  if i <> 7 then halt(3);
  i := if by > 100 then by else 100000;
  if i <> 100000 then halt(4);

  { 3. string branches via helper }
  if PickStr(True) <> 'yes' then halt(5);
  if PickStr(False) <> 'no' then halt(6);

  { 4. embedded in a larger expression }
  i := 1;
  s := 'count is ' + (if i = 1 then 'one' else 'many');
  if s <> 'count is one' then halt(7);

  { 5. nested inline-if }
  i := -3;
  s := if i < 0 then 'neg' else if i = 0 then 'zero' else 'pos';
  if s <> 'neg' then halt(8);

  { 6. short-circuit: only the taken branch is evaluated }
  sidefx := 0;
  i := if True then Bump(111) else Bump(222);
  if i <> 111 then halt(9);
  if sidefx <> 1 then halt(10);

  sidefx := 0;
  i := if False then Bump(111) else Bump(222);
  if i <> 222 then halt(11);
  if sidefx <> 1 then halt(12);

  { 7. compile-time constant fold (CFold folded to 41 at compile time) }
  i := CFold;
  if i <> 41 then halt(13);

  { 8. inline-if as a call argument }
  if PickStr(if 5 > 4 then True else False) <> 'yes' then halt(14);

  WriteLn('ok');
end.
