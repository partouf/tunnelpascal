program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TComposed = record
    A: Integer;
    B: Integer;
    contains child: record
      C: Integer;
    end;
    D: Integer;
  end;

var
  c: TComposed;
begin
  WriteLn('@c:         ', IntPtr(@c));
  WriteLn('@c.C:       ', IntPtr(@c.C));
  WriteLn('@c.child.C: ', IntPtr(@c.child.C));
  if (@c.C=@c.child.C) then
  begin
    WriteLn('ok');
    halt(0);
  end;
  halt(1);
end.
