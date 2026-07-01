{ Delphi parity: type/record helpers must be available by default in mode
  delphi, without an explicit typehelpers modeswitch. In Delphi, helpers are
  always on in the language; FPC previously required the modeswitch even in
  delphi mode.

  Exit code 0 = pass; halt(N) marks which check failed. }

program ttypehelperdef1;

{$mode delphi}

type
  TIntHelper = record helper for Integer
    function Twice: Integer;
    function IsEven: Boolean;
  end;

  TStrHelper = record helper for string
    function Shout: string;
  end;

function TIntHelper.Twice: Integer;
begin
  Result := Self * 2;
end;

function TIntHelper.IsEven: Boolean;
begin
  Result := (Self mod 2) = 0;
end;

function TStrHelper.Shout: string;
begin
  Result := Self + '!';
end;

var
  i: Integer;
  s: string;
begin
  i := 21;
  if i.Twice <> 42 then halt(1);
  if i.IsEven then halt(2);       { 21 is odd }

  Inc(i);
  if not i.IsEven then halt(3);   { 22 is even }

  s := 'hi';
  if s.Shout <> 'hi!' then halt(4);

  WriteLn('ok');
end.
