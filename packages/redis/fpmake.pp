{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses {$ifdef unix}cthreads,{$endif} fpmkunit;

Var
  P : TPackage;
  T : TTarget;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('redis');
    P.ShortName:='redis';
    P.Author := 'Mario Ray Mahardhika';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Redis interface unit.';
    P.OSes := [freebsd,darwin,ios,solaris,netbsd,openbsd,linux,dragonfly,android];
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    P.Dependencies.Add('fcl-net');

    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('redis.pas');
    T.ResourceStrings := True;
    T:=P.Targets.AddUnit('tcpimpl.pas');
    T.ResourceStrings := True;

    P.NamespaceMap:='namespaces.lst';
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
