{ Helpers and literal syntax.

  - Record helper for a primitive type with NO "modeswitch typehelpers":
    this fork turns type helpers on by default in Delphi mode.
  - Digit separators (Delphi 12).
  - Multiline string literals (Delphi 12).

  Note: a helper call on an integer *literal* (`21.Doubled`) does not parse,
  because `21.` lexes as the start of a real. Delphi behaves the same way. }

unit tourhelpers;

interface

procedure Run;

type
  TIntHelper = record helper for Integer
    function Doubled: Integer;
  end;

implementation

function TIntHelper.Doubled: Integer;
begin
  Result := Self * 2;
end;

procedure Run;
var
  n, big: Integer;
  ml: string;
begin
  n := 7;

  WriteLn('5. Record helper on Integer, no {$modeswitch} needed');
  WriteLn('    n.Doubled = ', n.Doubled);

  WriteLn('6. Digit separators + multiline string literals');
  big := 1_000_000;
  ml := '''
      line one
      line two
      ''';
  WriteLn('    1_000_000 = ', big);
  WriteLn('    multiline literal ->');
  WriteLn(ml);
end;

end.
