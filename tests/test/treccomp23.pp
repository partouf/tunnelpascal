{ %FAIL }
{ %OPT=-Sew }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
    B: Integer;
  end;

  TComposed = record
    A: Integer;
    B: Integer;
    contains child: TChildRec;
    C: Integer;
  end;

var
  c: TComposed;
begin
  WriteLn('@c:         ', IntPtr(@c));
  WriteLn('@c.B:       ', IntPtr(@c.B));
  WriteLn('@c.child.B: ', IntPtr(@c.child.B));
  if (@c.B<>@c.child.B) then
  begin
    WriteLn('ok');
    halt(0);
  end;
  halt(1);
end.
