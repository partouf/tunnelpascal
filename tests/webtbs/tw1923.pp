{$mode delphi}

type
parent = class
end;

child = class
 procedure test;
end;

procedure child.test;
begin
inherited;
end;

begin
end.
