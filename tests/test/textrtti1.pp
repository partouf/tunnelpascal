{ Extended RTTI for classes: TRttiType.GetMethods / GetFields must return
  non-published (public) methods and fields, with parameter info, when a class
  enables extended RTTI via the RTTI directive.

  This is what serialization / DI / ORM frameworks rely on. Previously FPC
  emitted extended method/field RTTI tables for records but not for classes.

  Exit code 0 = pass; halt(N) marks which check failed. }

program textrtti1;

{$RTTI EXPLICIT METHODS([vcPublic, vcPublished]) FIELDS([vcPublic, vcPublished]) PROPERTIES([vcPublic, vcPublished])}
{$mode delphi}{$H+}

uses
  Rtti;

type
  TFoo = class
  public
    MyField: Integer;
    procedure DoIt(a: Integer);
    function Calc(x, y: Integer): Integer;
  end;

procedure TFoo.DoIt(a: Integer);
begin
end;

function TFoo.Calc(x, y: Integer): Integer;
begin
  Result := x + y;
end;

var
  ctx: TRttiContext;
  t: TRttiType;
  m: TRttiMethod;
  f: TRttiField;
  foundDoIt, foundCalc: Boolean;
  calcParams: Integer;
begin
  ctx := TRttiContext.Create;
  t := ctx.GetType(TFoo);

  foundDoIt := False;
  foundCalc := False;
  calcParams := -1;
  for m in t.GetMethods do
  begin
    if m.Name = 'DoIt' then
      foundDoIt := True;
    if m.Name = 'Calc' then
    begin
      foundCalc := True;
      calcParams := Length(m.GetParameters);
    end;
  end;

  if not foundDoIt then halt(1);
  if not foundCalc then halt(2);
  if calcParams <> 2 then halt(3);

  f := t.GetField('MyField');
  if f = nil then halt(4);

  ctx.Free;
  WriteLn('ok');
end.
