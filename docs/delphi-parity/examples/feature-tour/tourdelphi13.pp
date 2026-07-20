{ Delphi 13 "Florence" language additions.

  - Inline `if` expression (ternary), usable in expression position.
  - `NameOf` intrinsic, yielding the source identifier as a string.

  Both are fork additions; upstream fpc trunk rejects them with a
  syntax error in every mode. }

unit tourdelphi13;

interface

procedure Run;

implementation

type
  TRec = record
    Alpha: Integer;
  end;

procedure Run;
var
  n: Integer;
  r: TRec;
begin
  n := 7;

  WriteLn('1. Inline if expression');
  // simplest form; the else branch is mandatory in expression position
  WriteLn('    n=7 -> ', if n > 5 then 'big' else 'small');
  // chained
  WriteLn('    nested: ', if n > 10 then 'huge' else if n > 5 then 'big' else 'small');
  // branch-type unification: Byte vs Integer -> Integer
  WriteLn('    unified: ', if n > 3 then Byte(1) else 100000);

  WriteLn('2. NameOf intrinsic');
  WriteLn('    var=', NameOf(n), '  type=', NameOf(TRec), '  field=', NameOf(r.Alpha));
end;

end.
