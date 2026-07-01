{ Delphi 13 generic type-parameter constraints: `interface` and `unmanaged`.

  Positive test: a generic with `T: interface` specialized on an interface
  type, and a generic with `T: unmanaged` specialized on unmanaged types
  (ordinal, float, POD record) must compile and work.

  Exit code 0 = pass; halt(N) marks which check failed. }

program tgenconstr1;

{$mode delphi}

type
  IMyIntf = interface
    function GetVal: Integer;
  end;

  TPoint = record
    x, y: Integer;
  end;

  { interface constraint: T must be an interface type }
  TWrap<T: interface> = class
    FIntf: T;
  end;

  { unmanaged constraint: T must have no managed fields }
  TBox<T: unmanaged> = record
    FVal: T;
  end;

var
  bi: TBox<Integer>;
  bd: TBox<Double>;
  bp: TBox<TPoint>;
  w: TWrap<IMyIntf>;
begin
  bi.FVal := 42;
  if bi.FVal <> 42 then halt(1);

  bd.FVal := 3.5;
  if bd.FVal <> 3.5 then halt(2);

  bp.FVal.x := 7;
  bp.FVal.y := 9;
  if bp.FVal.x + bp.FVal.y <> 16 then halt(3);

  w := TWrap<IMyIntf>.Create;
  if w.FIntf <> nil then halt(4);
  w.Free;

  WriteLn('ok');
end.
