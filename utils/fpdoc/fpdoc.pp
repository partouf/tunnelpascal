{
    $Id$

    FPDoc  -  Free Pascal Documentation Tool
    Copyright (C) 2000 - 2003 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


program FPDoc;

uses
  SysUtils, Classes, Gettext, DOM, XMLWrite, PasTree, PParser,
  dGlobals, // GLobal definitions, constants.
  dwriter,  // TFPDocWriter definition.
  dwlinear, // Linear (abstract) writer
  dw_LaTeX, // TLaTex writer
  dw_XML,   // XML writer
  dw_HTML,  // HTML writer
  dw_ipf,   // IPF writer
  dw_txt;   // TXT writer

const
  OSTarget: String = {$I %FPCTARGETOS%};
  CPUTarget: String = {$I %FPCTARGETCPU%};

var
  Backend : String;
  BackendOptions : TStrings;
  InputFiles, DescrFiles: TStringList;
  PackageName, DocLang, ContentFile : String;
  Engine: TFPDocEngine;

Procedure Usage(AnExitCode : Byte);

begin
  Writeln(SCmdLineHelp);
  Halt(AnExitCode);
end;

procedure InitOptions;
begin
  InputFiles := TStringList.Create;
  DescrFiles := TStringList.Create;
  BackendOptions := TStringList.Create;
  Engine := TFPDocEngine.Create;
end;

procedure FreeOptions;
begin
  Engine.Free;
  BackendOptions.Free;
  DescrFiles.Free;
  InputFiles.Free;
end;

procedure ReadContentFile(const AParams: String);
var
  i: Integer;
begin
  i := Pos(',', AParams);
  Engine.ReadContentFile(Copy(AParams, 1, i - 1),
    Copy(AParams, i + 1, Length(AParams)));
end;

procedure ParseOption(const s: String);

  procedure AddToFileList(List: TStringList; const FileName: String);
  var
    f: Text;
    s: String;
  begin
    if Copy(FileName, 1, 1) = '@' then
    begin
      Assign(f, Copy(FileName, 2, Length(FileName)));
      Reset(f);
      while not EOF(f) do
      begin
        ReadLn(f, s);
        List.Add(s);
      end;
      Close(f);
    end else
      List.Add(FileName);
  end;

var
  i: Integer;
  Cmd, Arg: String;
  
begin
  if (s = '-h') or (s = '--help') then
    Usage(0)
  else if s = '--hide-protected' then
    Engine.HideProtected := True
  else if s = '--warn-no-node' then
    Engine.WarnNoNode := True
  else if s = '--show-private' then
    Engine.HidePrivate := False
  else
    begin
    i := Pos('=', s);
    if i > 0 then
      begin
      Cmd := Copy(s, 1, i - 1);
      Arg := Copy(s, i + 1, Length(s));
      end
    else
      begin
      Cmd := s;
      SetLength(Arg, 0);
      end;
    if Cmd = '--descr' then
      AddToFileList(DescrFiles, Arg)
    else if (Cmd = '-f') or (Cmd = '--format') then
      begin
      Arg:=UpperCase(Arg);
      If FindWriterClass(Arg)=-1 then
        WriteLn(StdErr, Format(SCmdLineInvalidFormat, [Arg]))
      else
        BackEnd:=Arg;
      end
    else if (Cmd = '-l') or (Cmd = '--lang') then
      DocLang := Arg
    else if (Cmd = '-i') or (Cmd = '--input') then
      AddToFileList(InputFiles, Arg)
    else if (Cmd = '-o') or (Cmd = '--output') then
      Engine.Output := Arg
    else if Cmd = '--content' then
      ContentFile := Arg
    else if Cmd = '--import' then
      ReadContentFile(Arg)
    else if Cmd = '--package' then
      PackageName := Arg
    else if Cmd = '--ostarget' then
      OSTarget := Arg
    else if Cmd = '--cputarget' then
      CPUTarget := Arg
    else
      begin
      BackendOptions.Add(Cmd);
      BackendOptions.Add(Arg);
      end;
    end;
end;

procedure ParseCommandLine;

var
  i: Integer;
  
begin
  for i := 1 to ParamCount do
    ParseOption(ParamStr(i));
  If (BackEnd='') then
    BackEnd:='html';
  if (PackageName='') then
    begin
    Writeln(SNeedPackageName);
    Usage(1);
    end;
end;

procedure CreateDocumentation;

var
  i: Integer;
  WriterClass : TFPDocWriterClass;
  Writer : TFPDocWriter;
  
begin
  for i := 0 to DescrFiles.Count - 1 do
    Engine.AddDocFile(DescrFiles[i]);
  Engine.SetPackageName(PackageName);
  if Length(DocLang) > 0 then
    TranslateDocStrings(DocLang);
  for i := 0 to InputFiles.Count - 1 do
    try
      ParseSource(Engine, InputFiles[i], OSTarget, CPUTarget);
    except
      on e: EParserError do
        WriteLn(StdErr, Format('%s(%d,%d): %s',
          [e.Filename, e.Row, e.Column, e.Message]));
    end;
  WriterClass:=GetWriterClass(Backend);
  Writer:=WriterClass.Create(Engine.Package,Engine);
  With Writer do
    Try
      If BackendOptions.Count>0 then
        for I:=0 to ((BackendOptions.Count-1) div 2) do
          If not InterPretOption(BackendOptions[I*2],BackendOptions[I*2+1]) then
            WriteLn(StdErr, Format(SCmdLineInvalidOption,[BackendOptions[I*2]+' '+BackendOptions[I*2+1]]));
      WriteDoc;
    Finally
      Free;
    end;
  if Length(ContentFile) > 0 then
    Engine.WriteContentFile(ContentFile);
end;



begin
{$IFDEF Unix}
  gettext.TranslateResourceStrings('/usr/local/share/locale/%s/LC_MESSAGES/fpdoc.mo');
{$ELSE}
  gettext.TranslateResourceStrings('intl/fpdoc.%s.mo');
{$ENDIF}
  WriteLn(STitle);
  WriteLn(SCopyright);
  WriteLn;
  InitOptions;
  Try
    ParseCommandLine;
    CreateDocumentation;
    WriteLn(SDone);
  Finally
    FreeOptions;
  end;
end.


{
  $Log$
  Revision 1.7  2005-01-12 21:11:41  michael
  + New structure for writers. Implemented TXT writer

  Revision 1.6  2005/01/09 15:59:50  michael
  + Split out latex writer to linear and latex writer

  Revision 1.5  2004/08/28 18:03:23  michael
  + Added warning if docnode not found (option --warn-no-node

  Revision 1.4  2003/10/08 11:41:54  yuri
  + Initial OS/2 IPF support added

  Revision 1.3  2003/03/27 17:14:13  sg
  * Added --ostarget and --cputarget

  Revision 1.2  2003/03/18 19:28:44  michael
  + Some changes to output handling, more suitable for tex output

  Revision 1.1  2003/03/17 23:03:20  michael
  + Initial import in CVS

  Revision 1.13  2003/03/13 22:02:13  sg
  * New version with many bugfixes and our own parser (now independent of the
    compiler source)

  Revision 1.12  2002/10/12 17:09:45  michael
  + Added check for package name

  Revision 1.11  2002/05/24 00:13:22  sg
  * much improved new version, including many linking and output fixes

  Revision 1.10  2002/03/12 10:58:36  sg
  * reworked linking engine and internal structure

  Revision 1.9  2002/01/08 13:00:06  michael
  + Added correct array handling and syntax highlighting is now optional

  Revision 1.8  2001/12/17 23:24:11  sg
  * Added "--package" switch
  * Now uses translation files written in lower-case

  Revision 1.7  2001/07/27 12:17:20  sg
  * Added "--html-search" command line argument

  Revision 1.6  2001/07/27 10:21:42  sg
  * Just a new, improved version ;)
    (detailed changelogs will be provided again with the next commits)
}
