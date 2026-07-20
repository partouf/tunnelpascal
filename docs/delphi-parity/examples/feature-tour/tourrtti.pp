{ Extended RTTI, on by default.

  TDemo carries no "RTTI EXPLICIT" directive, yet its *private* field is
  visible through TRttiContext. Upstream fpc requires the directive.

  Two things to note in the output:
  - `_MonitorData` shows up. That is an FPC-internal TObject field that Delphi's
    TObject does not have, so it leaks into any RTTI-driven code that enumerates
    fields generically. See FEATURE-COMPARISON.md §2.
  - TRttiMethod.Invoke is NOT exercised here: on this platform it raises
    ENotImplemented ("Use external managers, e.g. ffi.manager"). }

unit tourrtti;

interface

procedure Run;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Rtti;
{$ELSE}
  Rtti;
{$ENDIF}

type
  TDemo = class
  private
    FSecret: Integer;
  public
    PublicField: string;
  end;

procedure Run;
var
  ctx: TRttiContext;
  f: TRttiField;
begin
  WriteLn('9. Extended RTTI default-on (no {$RTTI} directive)');
  ctx := TRttiContext.Create;
  for f in ctx.GetType(TypeInfo(TDemo)).GetFields do
    WriteLn('    field visible: ', f.Name);
end;

end.
