{ %NORUN }

program tw39126;

{$mode delphi}

type
  TTestType = type Integer;

procedure Test1<T; const C: Integer>(A: T);
begin
end;

procedure Test2<const C: Integer>;
begin
end;

begin
  Test1<TTestType, 1>(1);
  Test2<1>; // Test2 fails.
end.
