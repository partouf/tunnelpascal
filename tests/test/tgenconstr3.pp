{ %FAIL }

{ Delphi 13 `unmanaged` generic constraint: the type argument must have no
  managed (reference-counted / finalized) fields. Specializing on a managed
  type (here AnsiString) must be a compile-time error. }

program tgenconstr3;

{$mode delphi}

type
  TBox<T: unmanaged> = record
    FVal: T;
  end;

var
  b: TBox<AnsiString>;   { AnsiString is managed -> must fail }
begin
  b.FVal := '';
end.
