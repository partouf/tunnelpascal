{
    This file is part of the Free Component Library

    JSON source lexical scanner
    Copyright (c) 2007 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}
{ $INLINE ON}

unit jsonscanner;

interface

uses SysUtils, Classes;

resourcestring
  SErrInvalidCharacter = 'Invalid character at line %d, pos %d: ''%s''';
  SUnterminatedComment = 'Unterminated comment at line %d, pos %d: ''%s''';
  SErrOpenString = 'string exceeds end of line %d';

type

  TJSONToken = (
    tkEOF,
    tkWhitespace,
    tkString,
    tkNumber,
    tkTrue,
    tkFalse,
    tkNull,
    // Simple (one-character) tokens
    tkComma,                 // ','
    tkColon,                 // ':'
    tkCurlyBraceOpen,        // '{'
    tkCurlyBraceClose,       // '}'
    tkSquaredBraceOpen,       // '['
    tkSquaredBraceClose,      // ']'
    tkIdentifier,            // Any Javascript identifier
    tkComment,
    tkUnknown
    );

  EScannerError = class(EParserError);

  TJSONOption = (joUTF8,joStrict,joComments,joIgnoreTrailingComma,joIgnoreDuplicates,joBOMCheck);
  TJSONOptions = set of TJSONOption;

Const
  DefaultOptions = [joUTF8];

Type

  { TJSONScanner }

  TJSONScanner = class
  private
    FSource: RawByteString;
    FCurPos : PAnsiChar; // Position inside total string
    FCurRow: Integer;
    FCurToken: TJSONToken;
    FCurTokenLength: Integer;
    FCurTokenStart: PAnsiChar;
    FCurLine: PChar;
    FTokenStr:  PAnsiChar; // position inside FCurLine
    FEOL : PAnsiChar; // EOL
    FOptions : TJSONOptions;
    function GetCurColumn: Integer; inline;
    function GetCurLine: string;
    function GetO(AIndex: TJSONOption): Boolean;
    procedure SetO(AIndex: TJSONOption; AValue: Boolean);
    function GetCurTokenString: string;
  protected
    procedure Error(const Msg: string);overload;
    procedure Error(const Msg: string;  Const Args: array of const);overload;
    class function DecodeJSONString(TokenStr: PAnsiChar; tokenLength: SizeInt; utf8: boolean): string; static;
//    function DoFetchToken: TJSONToken; inline;
  public
    constructor Create(Source : TStream; AUseUTF8 : Boolean = True); overload; deprecated 'use options form instead';
    constructor Create(Source: TStream; AOptions: TJSONOptions); overload;
    constructor Create(const aSource : RawByteString; AUseUTF8 : Boolean = True); overload; deprecated  'use options form instead';
    constructor Create(const aSource: RawByteString; AOptions: TJSONOptions); overload;

    function FetchToken: TJSONToken;

    property CurLine: string read GetCurLine;
    property CurRow: Integer read FCurRow;
    property CurColumn: Integer read GetCurColumn;

    property CurToken: TJSONToken read FCurToken;
    property CurTokenStart: PAnsiChar read FCurTokenStart;
    property CurTokenLength: Integer read FCurTokenLength;
    property CurTokenString: string read GetCurTokenString;
    // Use strict JSON: " for strings, object members are strings, not identifiers
    Property Strict : Boolean Index joStrict Read GetO Write SetO ; deprecated 'use options instead';
    // if set to TRUE, then strings will be converted to UTF8 ansistrings, not system codepage ansistrings.
    Property UseUTF8 : Boolean index joUTF8 Read GetO Write SetO; deprecated 'Use options instead';
    // Parsing options
    Property Options : TJSONOptions Read FOptions Write FOptions;
  end;

const
  TokenInfos: array[TJSONToken] of string = (
    'EOF',
    'Whitespace',
    'String',
    'Number',
    'True',
    'False',
    'Null',
    ',',
    ':',
    '{',
    '}',
    '[',
    ']',
    'identifier',
    'comment',
    ''
  );


implementation

constructor TJSONScanner.Create(Source : TStream; AUseUTF8 : Boolean = True);

Var
  O : TJSONOptions;

begin
  O:=DefaultOptions;
  if AUseUTF8 then
    Include(O,joUTF8)
  else
    Exclude(O,joUTF8);
  Create(Source,O);
end;

constructor TJSONScanner.Create(Source: TStream; AOptions: TJSONOptions);

  procedure SkipStreamBOM;
  Var
    OldPos : integer;
    Header : array[0..3] of byte;
  begin
    OldPos := Source.Position;
    FillChar(Header{%H-}, SizeOf(Header), 0);
    if Source.Read(Header, 3) = 3 then
      if (Header[0]=$EF) and (Header[1]=$BB) and (Header[2]=$BF) then
        exit;
    Source.Position := OldPos;
  end;


Var
  S : RawByteString;

begin
  if (joBOMCheck in aOptions) then
    SkipStreamBom;
  S:='';
  SetLength(S,Source.Size-Source.Position);
  if Length(S)>0 then
    Source.ReadBuffer(S[1],Length(S));
  Create(S,AOptions)
end;

constructor TJSONScanner.Create(const aSource : RawByteString; AUseUTF8 : Boolean = True);
Var
  O : TJSONOptions;

begin
  O:=DefaultOptions;
  if AUseUTF8 then
    Include(O,joUTF8)
  else
    Exclude(O,joUTF8);
  Create(aSource,O);
end;

constructor TJSONScanner.Create(const aSource: RawByteString; AOptions: TJSONOptions);
begin
  FSource:=aSource;
  FCurPos:=PAnsiChar(FSource);
  if FCurPos<>Nil then
    FCurRow:=1;
  FOptions:=AOptions;
end;

function TJSONScanner.GetCurColumn: Integer;
begin
  Result := FTokenStr - FCurLine;
end;


procedure TJSONScanner.Error(const Msg: string);
begin
  raise EScannerError.Create(Msg);
end;

procedure TJSONScanner.Error(const Msg: string; const Args: array of const);
begin
  raise EScannerError.CreateFmt(Msg, Args);
end;

class function TJSONScanner.DecodeJSONString(TokenStr: PAnsiChar; tokenLength: SizeInt; utf8: boolean): string;
var OldLength, SectionLength,  tstart,tcol, u1,u2: Integer;
  Procedure MaybeAppendUnicode;

  Var
    u : UTF8String;

  begin
  // if there is a leftover \u, append
  if (u1<>0) then
    begin
    if utf8 then
      U:=Utf8Encode(WideString(WideChar(u1))) // ToDo: use faster function
    else
      U:=String(WideChar(u1)); // WideChar converts the encoding. Should it warn on loss?
    result:=result+U;
    OldLength:=Length(result);
    u1:=0;
    u2:=0;
    end;
  end;
var
  TokenStart, TokenEnd: PAnsiChar;
  c2: Char;
  I : Integer;
  S : String[8];
begin
  TokenStart := TokenStr;
  OldLength := 0;
  result := '';
  u1:=0;
  TokenEnd := TokenStart + tokenLength;
  while TokenStr < TokenEnd do
    begin
    if (TokenStr^='\') then
      begin
      // Save length
      SectionLength := TokenStr - TokenStart;
      Inc(TokenStr);
      // Read escaped token
      Case TokenStr^ of
        '"' : S:='"';
        '''' : S:='''';
        't' : S:=#9;
        'b' : S:=#8;
        'n' : S:=#10;
        'r' : S:=#13;
        'f' : S:=#12;
        '\' : S:='\';
        '/' : S:='/';
        'u' : begin
              u2:=0;
              For I:=1 to 4 do
                begin
                Inc(TokenStr);
                c2:=TokenStr^;
                Case c2 of
                  '0'..'9': u2:=u2*16+ord(c2)-ord('0');
                  'A'..'F': u2:=u2*16+ord(c2)-ord('A')+10;
                  'a'..'f': u2:=u2*16+ord(c2)-ord('a')+10;
                end;
                end;
              if u1<>0 then
                begin
                // 4bytes, compose.
                if utf8 then
                  S:=Utf8Encode(WideString(WideChar(u1)+WideChar(u2))) // ToDo: use faster function
                else
                  S:=String(WideChar(u1)+WideChar(u2)); // WideChar converts the encoding. Should it warn on loss?
                u1:=0;
                end
              else
                begin
                // Surrogate start
                if (u2>=$D800) and (U2<=$DBFF) then
                  begin
                  u1:=u2;
                  S:='';
                  end
                else
                  begin
                  if utf8 then
                    S:=Utf8Encode(WideString(WideChar(u2))) // ToDo: use faster function
                  else
                    S:=String(WideChar(u2)); // WideChar converts the encoding. Should it warn on loss?
                  U1:=0;
                  U2:=0;
                  end;
                end;
              end;
      end;
      I:=Length(S);
      if (SectionLength+I>0) then
        begin
        // If length=1, we know it was not \uXX, but u1 can be nonzero, and we must first append it.
        // example: \u00f8\"
        if (I=1) and (u1<>0) then
          MaybeAppendUnicode;
        SetLength(result, OldLength + SectionLength+i);
        if SectionLength > 0 then
          Move(TokenStart^, result[OldLength + 1], SectionLength);
        if I>0 then
          Move(S[1],result[OldLength + SectionLength+1],i);
        Inc(OldLength, SectionLength+I);
        end;
      // Next char
      TokenStart := TokenStr+1;
      end
    else if u1<>0 then
      MaybeAppendUnicode;
    Inc(TokenStr);
    end;
  if u1<>0 then
    MaybeAppendUnicode;
  SectionLength := TokenStr - TokenStart;
  SetLength(result, OldLength + SectionLength);
  if SectionLength > 0 then
    Move(TokenStart^, result[OldLength + 1], SectionLength);
end;

function TJSONScanner.GetCurTokenString: string;
begin
  if FCurToken <> tkString then begin
    SetLength(result, FCurTokenLength);
    move(FCurTokenStart^, result[1], FCurTokenLength);
    exit;
  end else result := DecodeJSONString(FCurTokenStart, FCurTokenLength, (joUTF8 in Options) or (DefaultSystemCodePage=CP_UTF8) );
end;

function TJSONScanner.FetchToken: TJSONToken;

(*
  procedure dumpcurrent;

  begin
  Writeln('Start of line : ',FCurLine);
  Writeln('Cur pos : ',FCurPos);
  Writeln('Start of token : ',FTokenstr);
  Writeln('End of line : ',FTokenstr);
  end;
*)
  function FetchLine: Boolean;


  begin
    Result:=(FCurPos<>Nil) and (FCurPos^<>#0);
    if Result then
      begin
      FCurLine:=FCurPos;
      FTokenStr:=FCurPos;
      While Not (FCurPos^ in [#0,#10,#13]) do
        Inc(FCurPos);
      FEOL:=FCurPos;
      If (FCurPos^<>#0) then
//      While (FCurPos^<>#0) and (FCurPos^ in [#10,#13]) do
        begin
        if (FCurPos^=#13) and (FCurPos[1]=#10) then
          Inc(FCurPos); // Skip CR-LF
        Inc(FCurPos); // To start of next line
        Inc(FCurRow); // Increase line index
        end;
//      Len:=FEOL-FTokenStr;
//      FTokenStr:=FCurPos;
      end
    else
      begin
      FCurLine:=Nil;
      FTokenStr:=nil;
      end;
  end;

var
  it : TJSONToken;
  I : Integer;
  C: char;
  PendingUnicodeSurrogate: PAnsiChar;
  IsStar,EOC: Boolean;


begin
  if (FTokenStr = nil) or (FTokenStr=FEOL) then
    begin
    if not FetchLine then
      begin
      Result := tkEOF;
      FCurToken := Result;
      exit;
      end;
    end;
  case FTokenStr^ of
    #0:         // Empty line
      begin
      FetchLine;
      Result := tkWhitespace;
      end;
    #9, ' ', #10, #13:
      begin
      Result := tkWhitespace;
      repeat
        if FTokenStr = FEOL then
          begin
          if not FetchLine then
            begin
            FCurToken := Result;
            exit;
            end
          end
        else
          Inc(FTokenStr);
      until not (FTokenStr[0] in [#9, ' ']);
      end;
    '"','''':
      begin
        C:=FTokenStr^;
        If (C='''') and (joStrict in Options) then
          Error(SErrInvalidCharacter, [CurRow,CurColumn,FTokenStr[0]]);
        Inc(FTokenStr);
        FCurTokenStart := FTokenStr;
        PendingUnicodeSurrogate := nil;
        while not (FTokenStr^ in [#0,C]) do
          begin
          if (FTokenStr^='\') then
            begin
            Inc(FTokenStr);
            // Read escaped token
            Case FTokenStr^ of
              '"','''','t','b','n','r','f','\','/': inc(FTokenStr); //single letter escapes
              'u' : begin
                    inc(FTokenStr);
                    if PendingUnicodeSurrogate <> nil then begin
                      //expect ((\u....>=$DC00) and (\u....<=$DFFF))
                      if not ( (FTokenStr[0] in ['D', 'd']) and (FTokenStr[1] in ['C'..'F', 'c'..'f']) ) then
                        Error(SErrInvalidCharacter, [CurRow,CurColumn,FTokenStr[0]+FTokenStr[1]]);
                      PendingUnicodeSurrogate := nil;
                    end else begin
                      //surrogate start (\u....>=$D800) and (\u....<=$DBFF) then
                      if (FTokenStr[0] in ['D', 'd']) and (FTokenStr[1] in ['8','9','A','B','a','b']) then
                        PendingUnicodeSurrogate := FTokenStr;
                    end;
                    For I:=1 to 4 do
                      begin
                      if not (FTokenStr^ in ['0'..'9','A'..'F','a'..'f']) then
                        Error(SErrInvalidCharacter, [CurRow,CurColumn,FTokenStr[0]]);
                      Inc(FTokenStr);
                      end;
                    end;
              #0  : Error(SErrOpenString,[FCurRow]);
            else
              Error(SErrInvalidCharacter, [CurRow,CurColumn,FTokenStr[0]]);
            end;
            end else begin
              if FTokenStr^ < #$20 then
                if FTokenStr^ = #0 then Error(SErrOpenString,[FCurRow])
                else if joStrict in Options then Error(SErrInvalidCharacter, [CurRow,CurColumn,FTokenStr[0]]);
              Inc(FTokenStr);
            end;
          end;
        if FTokenStr^ = #0 then
          Error(SErrOpenString,[FCurRow]);
        FCurTokenLength := FTokenStr - FCurTokenStart;
        if PendingUnicodeSurrogate <> nil then
          Error(SErrInvalidCharacter, [CurRow,CurColumn,PendingUnicodeSurrogate^]);
        inc(FTokenStr);
        Result := tkString;
      end;
    ',':
      begin
        Inc(FTokenStr);
        Result := tkComma;
      end;
    '0'..'9','.','-':
      begin
        FCurTokenStart := FTokenStr;
        if FTokenStr^ = '-' then inc(FTokenStr);
        case FTokenStr^ of
          '1'..'9': Inc(FTokenStr);
          '0': begin
            Inc(FTokenStr);
            if (joStrict in Options) and (FTokenStr^ in ['0'..'9']) then
              Error(SErrInvalidCharacter, [CurRow,CurColumn,FTokenStr[0]]);
          end;
          '.': if joStrict in Options then
                 Error(SErrInvalidCharacter, [CurRow,CurColumn,FTokenStr[0]]);
          else
            Error(SErrInvalidCharacter, [CurRow,CurColumn,FTokenStr[0]]);
        end;
        while true do
        begin
          case FTokenStr^ of
            '0'..'9': inc(FTokenStr);
            '.':
              begin
                case FTokenStr[1] of
                  '0'..'9': Inc(FTokenStr, 2);
                  'e', 'E': begin
                    if joStrict in Options then
                      Error(SErrInvalidCharacter, [CurRow,CurColumn,FTokenStr[0]]);
                    Inc(FTokenStr);
                  end;
                  else Error(SErrInvalidCharacter, [CurRow,CurColumn,FTokenStr[0]]);
                end;
                while FTokenStr^ in ['0'..'9'] do
                  inc(FTokenStr);
                break;
              end;
          else
            break;
          end;
        end;
        if FTokenStr^ in ['e', 'E'] then begin
          Inc(FTokenStr);
          if FTokenStr^ in ['-','+']  then
            Inc(FTokenStr);
          if not (FTokenStr^ in ['0'..'9']) then
            Error(SErrInvalidCharacter, [CurRow,CurColumn,FTokenStr[0]]);
          repeat
            Inc(FTokenStr);
          until not (FTokenStr^ in ['0'..'9']);
        end;
        if {(FTokenStr<>FEOL) and }not (FTokenStr^ in [#13,#10,#0,'}',']',',',#9,' ']) then
          Error(SErrInvalidCharacter, [CurRow,CurColumn,FTokenStr[0]]);
        FCurTokenLength := FTokenStr - FCurTokenStart;
        Result := tkNumber;
      end;
    ':':
      begin
        Inc(FTokenStr);
        Result := tkColon;
      end;
    '{':
      begin
        Inc(FTokenStr);
        Result := tkCurlyBraceOpen;
      end;
    '}':
      begin
        Inc(FTokenStr);
        Result := tkCurlyBraceClose;
      end;
    '[':
      begin
        Inc(FTokenStr);
        Result := tkSquaredBraceOpen;
      end;
    ']':
      begin
        Inc(FTokenStr);
        Result := tkSquaredBraceClose;
      end;
    '/' :
      begin
      if Not (joComments in Options) then
        Error(SErrInvalidCharacter, [CurRow,CurCOlumn,FTokenStr[0]]);
      Inc(FTokenStr);
      Case FTokenStr^ of
        '/' : begin
              Inc(FTokenStr);
              FCurTokenStart:=FTokenStr;
              FCurTokenLength := PChar(FEOL)-FCurTokenStart;
              FetchLine;
              end;
        '*' :
          begin
          IsStar:=False;
          Inc(FTokenStr);
          FCurTokenStart:=FTokenStr;
          Repeat
            While (FTokenStr=FEOL) do
              begin
              if not fetchLine then
                Error(SUnterminatedComment, [CurRow,CurCOlumn,FTokenStr[0]]);
              end;
            IsStar:=FTokenStr^='*';
            Inc(FTokenStr);
            EOC:=(isStar and (FTokenStr^='/'));
          Until EOC;
          if EOC then
            begin
            FCurTokenLength := PChar(FEOL)-FCurTokenStart;
            Inc(FTokenStr);
            end;
          end;
      else
        Error(SErrInvalidCharacter, [CurRow,CurCOlumn,FTokenStr[0]]);
      end;
      Result:=tkComment;
      end;
    'a'..'z','A'..'Z','_':
      begin
        FCurTokenStart := FTokenStr;
        Result:=tkIdentifier;
        case FTokenStr^ of
          't': if (FTokenStr[1] = 'r') and (FTokenStr[2] = 'u') and (FTokenStr[3] = 'e') then
            Result:=tkTrue;
          'f': if (FTokenStr[1] = 'a') and (FTokenStr[2] = 'l') and (FTokenStr[3] = 's') and (FTokenStr[4] = 'e') then
            Result:=tkFalse;
          'n': if (FTokenStr[1] = 'u') and (FTokenStr[2] = 'l') and (FTokenStr[3] = 'l') then
            Result:=tkNull;
        end;
        if result <> tkIdentifier then inc(FTokenStr, length(TokenInfos[result]) - 1);
        repeat
          Inc(FTokenStr);
        until not (FTokenStr^ in ['A'..'Z', 'a'..'z', '0'..'9', '_']);
        FCurTokenLength := FTokenStr - FCurTokenStart;
        if (result = tkIdentifier) or (FCurTokenLength <> length(TokenInfos[result])) then begin
          if (joStrict in Options) then
            Error(SErrInvalidCharacter, [CurRow,CurColumn - FCurTokenLength, FCurTokenStart[0]]);
          for it := tkTrue to tkNull do
            if (Length(TokenInfos[it]) = FCurTokenLength) and (strlicomp(FCurTokenStart, PAnsiChar(TokenInfos[it]), FCurTokenLength) = 0) then
              begin
              Result := it;
              FCurToken := Result;
              exit;
              end;
        end;
      end;
  else
    Error(SErrInvalidCharacter, [CurRow,CurColumn,FTokenStr[0]]);
  end;
  FCurToken := Result;
end;

{function TJSONScanner.FetchToken: TJSONToken;

begin
  Result:=DoFetchToken;
end;}

function TJSONScanner.GetCurLine: string;
begin
  Result:='';
  if FCurLine<>Nil then
    begin
    SetLength(Result,FEOL-FCurLine);
    if Length(Result)>0 then
      Move(FCurLine^,Result[1],Length(Result));
    end;
end;

function TJSONScanner.GetO(AIndex: TJSONOption): Boolean;
begin
  Result:=AIndex in FOptions;
end;

procedure TJSONScanner.SetO(AIndex: TJSONOption; AValue: Boolean);
begin
  If AValue then
    Include(Foptions,AIndex)
  else
    Exclude(Foptions,AIndex)
end;


end.
