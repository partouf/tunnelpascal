unit tcwebidl2wasmjob;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, webidltowasmjob, pascodegen;

type

  { TCustomTestWebIDL2WasmJob }

  TCustomTestWebIDL2WasmJob = Class(TTestCase)
  private
    FHeaderSrc: String;
    FWebIDLToPas: TWebIDLToPasWasmJob;
    procedure OnLog(Sender: TObject; LogType: TCodegenLogType; const Msg: String
      );
  protected
    procedure Setup; override;
    procedure TearDown; override;
  public
    procedure TestWebIDL(const WebIDLSrc, ExpectedPascalSrc: array of string); virtual;
    procedure CheckDiff(Msg, Expected, Actual: string); virtual;
    property WebIDLToPas: TWebIDLToPasWasmJob read FWebIDLToPas;
    property HeaderSrc: String read FHeaderSrc write FHeaderSrc;
  end;

  { TTestWebIDL2WasmJob }

  TTestWebIDL2WasmJob = Class(TCustomTestWebIDL2WasmJob)
  published
    procedure TestWJ_Empty;

    // typedefs
    procedure TestWJ_Typedef_Boolean;
    procedure TestWJ_Typedef_Sequence;

    // attributes
    procedure TestWJ_IntfAttribute_Boolean;
    // todo procedure TestWJ_IntfAttribute_Any;

    // functions
    procedure TestWJ_IntfFunction_Void;
    procedure TestWJ_IntfFunction_SetEventHandler;
  end;

function LinesToStr(Args: array of const): string;
function CheckSrcDiff(Expected, Actual: string; out Msg: string): boolean;

implementation

function LinesToStr(Args: array of const): string;
var
  s: String;
  i: Integer;
begin
  s:='';
  for i:=Low(Args) to High(Args) do
    case Args[i].VType of
      vtChar:         s += Args[i].VChar+LineEnding;
      vtString:       s += Args[i].VString^+LineEnding;
      vtPChar:        s += Args[i].VPChar+LineEnding;
      vtWideChar:     s += AnsiString(Args[i].VWideChar)+LineEnding;
      vtPWideChar:    s += AnsiString(Args[i].VPWideChar)+LineEnding;
      vtAnsiString:   s += AnsiString(Args[i].VAnsiString)+LineEnding;
      vtWidestring:   s += AnsiString(WideString(Args[i].VWideString))+LineEnding;
      vtUnicodeString:s += AnsiString(UnicodeString(Args[i].VUnicodeString))+LineEnding;
    end;
  Result:=s;
end;

function CheckSrcDiff(Expected, Actual: string; out Msg: string): boolean;
// search diff, ignore changes in spaces
const
  SpaceChars = [#9,#10,#13,' '];
var
  ExpectedP, ActualP: PChar;

  function FindLineEnd(p: PChar): PChar;
  begin
    Result:=p;
    while not (Result^ in [#0,#10,#13]) do inc(Result);
  end;

  function FindLineStart(p, MinP: PChar): PChar;
  begin
    while (p>MinP) and not (p[-1] in [#10,#13]) do dec(p);
    Result:=p;
  end;

  procedure SkipLineEnd(var p: PChar);
  begin
    if p^ in [#10,#13] then
    begin
      if (p[1] in [#10,#13]) and (p^<>p[1]) then
        inc(p,2)
      else
        inc(p);
    end;
  end;

  function HasSpecialChar(s: string): boolean;
  var
    i: Integer;
  begin
    for i:=1 to length(s) do
      if s[i] in [#0..#31,#127..#255] then
        exit(true);
    Result:=false;
  end;

  function HashSpecialChars(s: string): string;
  var
    i: Integer;
  begin
    Result:='';
    for i:=1 to length(s) do
      if s[i] in [#0..#31,#127..#255] then
        Result:=Result+'#'+hexstr(ord(s[i]),2)
      else
        Result:=Result+s[i];
  end;

  procedure DiffFound;
  var
    ActLineStartP, ActLineEndP, p, StartPos: PChar;
    ExpLine, ActLine: String;
    i, LineNo, DiffLineNo: Integer;
  begin
    writeln('Diff found "',Msg,'". Lines:');
    // write correct lines
    p:=PChar(Expected);
    LineNo:=0;
    DiffLineNo:=0;
    repeat
      StartPos:=p;
      while not (p^ in [#0,#10,#13]) do inc(p);
      ExpLine:=copy(Expected,StartPos-PChar(Expected)+1,p-StartPos);
      SkipLineEnd(p);
      inc(LineNo);
      if (p<=ExpectedP) and (p^<>#0) then
      begin
        writeln('= ',ExpLine);
      end else begin
        // diff line
        if DiffLineNo=0 then DiffLineNo:=LineNo;
        // write actual line
        ActLineStartP:=FindLineStart(ActualP,PChar(Actual));
        ActLineEndP:=FindLineEnd(ActualP);
        ActLine:=copy(Actual,ActLineStartP-PChar(Actual)+1,ActLineEndP-ActLineStartP);
        writeln('- ',ActLine);
        if HasSpecialChar(ActLine) then
          writeln('- ',HashSpecialChars(ActLine));
        // write expected line
        writeln('+ ',ExpLine);
        if HasSpecialChar(ExpLine) then
          writeln('- ',HashSpecialChars(ExpLine));
        // write empty line with pointer ^
        for i:=1 to 2+ExpectedP-StartPos do write(' ');
        writeln('^');
        Msg:='expected "'+ExpLine+'", but got "'+ActLine+'".';
        CheckSrcDiff:=false;
        // write up to ten following actual lines to get some context
        for i:=1 to 10 do begin
          ActLineStartP:=ActLineEndP;
          SkipLineEnd(ActLineStartP);
          if ActLineStartP^=#0 then break;
          ActLineEndP:=FindLineEnd(ActLineStartP);
          ActLine:=copy(Actual,ActLineStartP-PChar(Actual)+1,ActLineEndP-ActLineStartP);
          writeln('~ ',ActLine);
        end;
        exit;
      end;
    until p^=#0;

    // internal error:
    writeln('DiffFound Actual:-----------------------');
    writeln(Actual);
    writeln('DiffFound Expected:---------------------');
    writeln(Expected);
    writeln('DiffFound ------------------------------');
    Msg:='diff found, but lines are the same, internal error';
    CheckSrcDiff:=false;
  end;

var
  IsSpaceNeeded: Boolean;
  LastChar, Quote: Char;
begin
  Result:=true;
  Msg:='';
  if Expected='' then Expected:=' ';
  if Actual='' then Actual:=' ';
  ExpectedP:=PChar(Expected);
  ActualP:=PChar(Actual);
  repeat
    //writeln('TTestModule.CheckDiff Exp="',ExpectedP^,'" Act="',ActualP^,'"');
    case ExpectedP^ of
    #0:
      begin
      // check that rest of Actual has only spaces
      while ActualP^ in SpaceChars do inc(ActualP);
      if ActualP^<>#0 then
        begin
        DiffFound;
        exit;
        end;
      exit(true);
      end;
    ' ',#9,#10,#13:
      begin
      // skip space in Expected
      IsSpaceNeeded:=false;
      if ExpectedP>PChar(Expected) then
        LastChar:=ExpectedP[-1]
      else
        LastChar:=#0;
      while ExpectedP^ in SpaceChars do inc(ExpectedP);
      if (LastChar in ['a'..'z','A'..'Z','0'..'9','_','$'])
          and (ExpectedP^ in ['a'..'z','A'..'Z','0'..'9','_','$']) then
        IsSpaceNeeded:=true;
      if IsSpaceNeeded and (not (ActualP^ in SpaceChars)) then
        begin
        DiffFound;
        exit;
        end;
      while ActualP^ in SpaceChars do inc(ActualP);
      end;
    '''','"':
      begin
      while ActualP^ in SpaceChars do inc(ActualP);
      if ExpectedP^<>ActualP^ then
        begin
        DiffFound;
        exit;
        end;
      Quote:=ExpectedP^;
      repeat
        inc(ExpectedP);
        inc(ActualP);
        if ExpectedP^<>ActualP^ then
          begin
          DiffFound;
          exit;
          end;
        if (ExpectedP^ in [#0,#10,#13]) then
          break
        else if (ExpectedP^=Quote) then
          begin
          inc(ExpectedP);
          inc(ActualP);
          break;
          end;
      until false;
      end;
    else
      while ActualP^ in SpaceChars do inc(ActualP);
      if ExpectedP^<>ActualP^ then
        begin
        DiffFound;
        exit;
        end;
      inc(ExpectedP);
      inc(ActualP);
    end;
  until false;
end;

{ TCustomTestWebIDL2WasmJob }

procedure TCustomTestWebIDL2WasmJob.OnLog(Sender: TObject;
  LogType: TCodegenLogType; const Msg: String);
begin
  if LogType=cltInfo then ;
  if Sender=nil then ;
  writeln('TCustomTestWebIDL2WasmJob.OnLog ',Msg);
end;

procedure TCustomTestWebIDL2WasmJob.Setup;
begin
  inherited Setup;
  FWebIDLToPas:=TWebIDLToPasWasmJob.Create(nil);
  WebIDLToPas.OnLog:=@OnLog;
  WebIDLToPas.InputFileName:='test1.webidl';
  WebIDLToPas.InputStream:=TMemoryStream.Create;
  WebIDLToPas.OutputFileName:='test1.pas';
  WebIDLToPas.OutputStream:=TMemoryStream.Create;
  HeaderSrc:=LinesToStr([
    'Unit test1;',
    '',
    '{$MODE ObjFPC}',
    '{$H+}',
    'interface',
    '',
    'uses SysUtils, JOB_JS;',
    '']);
end;

procedure TCustomTestWebIDL2WasmJob.TearDown;
begin
  WebIDLToPas.InputStream.Free;
  WebIDLToPas.InputStream:=nil;
  WebIDLToPas.OutputStream.Free;
  WebIDLToPas.OutputStream:=nil;
  FreeAndNil(FWebIDLToPas);
  inherited TearDown;
end;

procedure TCustomTestWebIDL2WasmJob.TestWebIDL(const WebIDLSrc,
  ExpectedPascalSrc: array of string);
var
  i: Integer;
  Line, ExpectedSrc, InputSrc, OutputSrc: String;
  InputMS: TMemoryStream;
begin
  {$IFDEF VerboseWebidl2WasmJob}
  writeln('TCustomTestWebIDL2WasmJob.TestWebIDL WebIDL:----------------------');
  {$ENDIF}
  InputMS:=WebIDLToPas.InputStream as TMemoryStream;
  for i:=0 to high(WebIDLSrc) do
    begin
    Line:=WebIDLSrc[i]+sLineBreak;
    InputMS.Write(Line[1],length(Line));
    {$IFDEF VerboseWebidl2WasmJob}
    write(Line);
    {$ENDIF}
    end;
  InputMS.Position:=0;

  {$IFDEF VerboseWebidl2WasmJob}
  writeln('TCustomTestWebIDL2WasmJob.TestWebIDL ExpectedPascal: BEGIN--------');
  {$ENDIF}
  ExpectedSrc:=HeaderSrc;
  for i:=0 to high(ExpectedPascalSrc) do
    ExpectedSrc:=ExpectedSrc+ExpectedPascalSrc[i]+sLineBreak;
  {$IFDEF VerboseWebidl2WasmJob}
  writeln(ExpectedSrc);
  writeln('TCustomTestWebIDL2WasmJob.TestWebIDL ExpectedPascal END-----------');
  {$ENDIF}

  WebIDLToPas.Execute;

  SetLength(InputSrc{%H-},InputMS.Size);
  if length(InputSrc)>0 then
    Move(InputMS.Memory^,InputSrc[1],length(InputSrc));

  OutputSrc:=WebIDLToPas.Source.Text;
  {$IFDEF VerboseWebidl2WasmJob}
  writeln('TCustomTestWebIDL2WasmJob.TestWebIDL ActualPascal: BEGIN----------');
  writeln(OutputSrc);
  writeln('TCustomTestWebIDL2WasmJob.TestWebIDL ActualPascal: END------------');
  {$ENDIF}

  CheckDiff('TCustomTestWebIDL2WasmJob.TestWebIDL',ExpectedSrc,OutputSrc);
end;

procedure TCustomTestWebIDL2WasmJob.CheckDiff(Msg, Expected, Actual: string);
// search diff, ignore changes in spaces
var
  s: string;
begin
  if CheckSrcDiff(Expected,Actual,s) then exit;
  Fail(Msg+': '+s);
end;

{ TTestWebIDL2WasmJob }

procedure TTestWebIDL2WasmJob.TestWJ_Empty;
begin
  TestWebIDL([
  ''],
  ['Type',
  '  // Forward class definitions',
  'implementation',
  'end.',
  '']);
end;

procedure TTestWebIDL2WasmJob.TestWJ_Typedef_Boolean;
begin
  TestWebIDL([
  'typedef boolean PerformanceEntry;',
  ''],
  ['Type',
  '  // Forward class definitions',
  '  TPerformanceEntry = Boolean;',
  'implementation',
  'end.',
  '']);
end;

procedure TTestWebIDL2WasmJob.TestWJ_Typedef_Sequence;
begin
  TestWebIDL([
  'typedef boolean PerformanceEntry;',
  'typedef sequence <PerformanceEntry> PerformanceEntryList;',
  ''],
  ['Type',
  '  // Forward class definitions',
  '  TPerformanceEntry = Boolean;',
  '  TPerformanceEntryList = IJSArray; // array of TPerformanceEntry',
  'implementation',
  'end.',
  '']);
end;

procedure TTestWebIDL2WasmJob.TestWJ_IntfAttribute_Boolean;
begin
  TestWebIDL([
  'interface Attr {',
  '  attribute boolean aBoolean;',
  '};',
  ''],
  ['Type',
  '  // Forward class definitions',
  '  IJSAttr = interface;',
  '  TJSAttr = class;',
  '  { --------------------------------------------------------------------',
  '    TJSAttr',
  '    --------------------------------------------------------------------}',
  '',
  '  IJSAttr = interface(IJSObject)',
  '    [''{AA94F48A-7955-3EBA-B086-85B24440AF2A}'']',
  '    function _GetaBoolean: Boolean;',
  '    procedure _SetaBoolean(const aValue: Boolean);',
  '    property aBoolean: Boolean read _GetaBoolean write _SetaBoolean;',
  '  end;',
  '',
  '  TJSAttr = class(TJSObject,IJSAttr)',
  '  Private',
  '    function _GetaBoolean: Boolean;',
  '    procedure _SetaBoolean(const aValue: Boolean);',
  '  Public',
  '    class function Cast(Intf: IJSObject): IJSAttr;',
  '    property aBoolean: Boolean read _GetaBoolean write _SetaBoolean;',
  '  end;',
  '',
  'implementation',
  '',
  'function TJSAttr._GetaBoolean: Boolean;',
  'begin',
  '  Result:=ReadJSPropertyBoolean(''aBoolean'');',
  'end;',
  '',
  'procedure TJSAttr._SetaBoolean(const aValue: Boolean);',
  'begin',
  '  WriteJSPropertyBoolean(''aBoolean'',aValue);',
  'end;',
  '',
  'class function TJSAttr.Cast(Intf: IJSObject): IJSAttr;',
  'begin',
  '  Result:=TJSAttr.JOBCast(Intf);',
  'end;',
  '',
  'end.',
  '']);
end;

procedure TTestWebIDL2WasmJob.TestWJ_IntfFunction_Void;
begin
  TestWebIDL([
  'interface Attr {',
  '  void append(Attr node);',
  '};',
  ''],
  ['Type',
  '  // Forward class definitions',
  '  IJSAttr = interface;',
  '  TJSAttr = class;',
  '  { --------------------------------------------------------------------',
  '    TJSAttr',
  '    --------------------------------------------------------------------}',
  '',
  '  IJSAttr = interface(IJSObject)',
  '    [''{AA94F48A-84D7-3FAA-A2A6-208CA4B2AF2A}'']',
  '    procedure append(aNode: IJSAttr);',
  '  end;',
  '',
  '  TJSAttr = class(TJSObject,IJSAttr)',
  '  Private',
  '  Public',
  '    procedure append(aNode: IJSAttr);',
  '    class function Cast(Intf: IJSObject): IJSAttr;',
  '  end;',
  '',
  'implementation',
  '',
  'procedure TJSAttr.append(aNode: IJSAttr);',
  'begin',
  '  InvokeJSNoResult(''append'',[aNode]);',
  'end;',
  '',
  'class function TJSAttr.Cast(Intf: IJSObject): IJSAttr;',
  'begin',
  '  Result:=TJSAttr.JOBCast(Intf);',
  'end;',
  '',
  'end.',
  '']);
end;

procedure TTestWebIDL2WasmJob.TestWJ_IntfFunction_SetEventHandler;
begin
  TestWebIDL([
  '[LegacyTreatNonObjectAsNull]',
  'callback EventHandlerNonNull = any (long event);',
  'typedef EventHandlerNonNull? EventHandler;',
  '',
  'interface Attr {',
  '  void setEventHandler([TreatNonCallableAsNull] EventHandler handler);',
  '};',
  ''],
  ['Type',
  '  // Forward class definitions',
  '  IJSAttr = interface;',
  '  TJSAttr = class;',
  '  TEventHandlerNonNull = function (event: Integer): Variant of object;',
  '  TEventHandler = TEventHandlerNonNull;',
  '',
  '  { --------------------------------------------------------------------',
  '    TJSAttr',
  '    --------------------------------------------------------------------}',
  '',
  '  IJSAttr = interface(IJSObject)',
  '    [''{AA94F48A-121D-33BC-96FE-420246F2AF2A}'']',
  '    procedure setEventHandler(const aHandler: TEventHandler);',
  '  end;',
  '',
  '  TJSAttr = class(TJSObject,IJSAttr)',
  '  Private',
  '  Public',
  '    procedure setEventHandler(const aHandler: TEventHandler);',
  '    class function Cast(Intf: IJSObject): IJSAttr;',
  '  end;',
  '',
  'implementation',
  '',
  'function JOBCallTEventHandlerNonNull(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;',
  'var',
  '  event: Integer;',
  'begin',
  '  event:=H.GetLongInt;',
  '  Result:=H.AllocVariant(TEventHandlerNonNull(aMethod)(event));',
  'end;',
  '',
  'procedure TJSAttr.setEventHandler(const aHandler: TEventHandler);',
  'var',
  '  m: TJOB_Method;',
  'begin',
  '  m:=TJOB_Method.Create(TMethod(aHandler),@JOBCallTEventHandlerNonNull);',
  '  try',
  '    InvokeJSNoResult(''setEventHandler'',[m]);',
  '  finally',
  '    m.free;',
  '  end;',
  'end;',
  '',
  'class function TJSAttr.Cast(Intf: IJSObject): IJSAttr;',
  'begin',
  '  Result:=TJSAttr.JOBCast(Intf);',
  'end;',
  '',
  'end.',
  '']);
end;

initialization
  RegisterTests([TTestWebIDL2Wasmjob]);

end.

