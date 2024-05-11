{ %target=win32,win64,wince,darwin,linux,freebsd,solaris,beos,aix,android,haiku }
{ %norun }

{ Don't run, because tw9089b.pp was disabled and the txt file will not be there}

uses
  SysUtils;

var
  t: text;
begin
  { see tw9089b.pp }
  assign(t,'tw9089b.txt');
{$i-}
  reset(t);
{$i+}
  if ioresult<>0 then
    halt(1);
  close(t);
  erase(t);
  writeln('ok');
end.
