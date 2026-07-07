{ %FAIL }

{ Delphi 13 inline "if" expression: the `else` branch is mandatory when
  `if` is used in expression position (an expression must always yield a
  value). Omitting it must be a compile-time error. }

program tinlineif2;

{$mode delphi}

var
  i: Integer;
begin
  i := if 1 > 0 then 42;   { no else -> must fail to compile }
  WriteLn(i);
end.
