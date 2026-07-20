{ Delphi 13 generic constraints.

  - `interface`  — T must be some interface type.
  - `unmanaged`  — T must contain no compiler-managed fields
                   (no string / dynarray / interface / variant).

  Both are fork additions. See PLAN.md for where this fork's `unmanaged`
  is stricter than Delphi's for non-record types. }

unit tourgenerics;

interface

procedure Run;

type
  IGreeter = interface
    ['{6B5A1C2E-0001-4000-8000-000000000001}']
    procedure Speak;
  end;

  TGreeter = class(TInterfacedObject, IGreeter)
    procedure Speak;
  end;

  // "any interface" constraint
  TBox<T: interface> = class
    procedure Use(const V: T);
  end;

  // "no managed fields" constraint
  TBuf<T: unmanaged> = record
    Items: array[0..2] of T;
    function Size: Integer;
  end;

implementation

procedure TGreeter.Speak;
begin
  WriteLn('    IGreeter.Speak dispatched');
end;

procedure TBox<T>.Use(const V: T);
begin
  (V as IGreeter).Speak;
end;

function TBuf<T>.Size: Integer;
begin
  Result := SizeOf(Items);
end;

procedure Run;
var
  box: TBox<IGreeter>;
  bi: TBuf<Integer>;
  bd: TBuf<Double>;
begin
  WriteLn('3. "interface" generic constraint');
  box := TBox<IGreeter>.Create;
  box.Use(TGreeter.Create);

  WriteLn('4. "unmanaged" generic constraint');
  WriteLn('    TBuf<Integer> = ', bi.Size, ' bytes,  TBuf<Double> = ', bd.Size, ' bytes');
  // TBuf<string> is rejected at specialization time:
  //   Error: Unmanaged type expected, but got "AnsiString"
end;

end.
