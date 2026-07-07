{ %FAIL }

{ Delphi 13 NameOf intrinsic: the argument must denote a named entity
  (a variable, field, type, routine, ...). A computed expression has no
  declared name, so NameOf on one must be a compile-time error. }

program tnameof2;

{$mode delphi}

var
  i: Integer;
begin
  WriteLn(NameOf(i + 1));   { not a named entity -> must fail to compile }
end.
