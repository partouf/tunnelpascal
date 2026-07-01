{ %FAIL }

{ Delphi 13 `unmanaged` constraint is stricter than `record`: a record that
  itself contains a managed field is a value type (so it satisfies `record`),
  but it is NOT unmanaged. Specializing an `unmanaged`-constrained generic on
  such a record must be a compile-time error. }

program tgenconstr4;

{$mode delphi}

type
  TManaged = record
    s: AnsiString;   { managed field makes the whole record managed }
  end;

  TBox<T: unmanaged> = record
    FVal: T;
  end;

var
  b: TBox<TManaged>;   { managed record -> must fail under `unmanaged` }
begin
  b.FVal.s := '';
end.
