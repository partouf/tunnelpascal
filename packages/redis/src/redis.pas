unit redis;

{$mode objfpc}{$H+}

interface

uses
  ssockets;

const
  DefaultHost           = '127.0.0.1';
  DefaultPort           = 6379;
  DefaultConnectTimeout = 100;
  DefaultCanReadTimeout = 100;
  CRLF                  = #13#10;
  CR                    = #13;

type

  TRESPType = (rtError,rtString,rtInteger,rtArray);

  TRESP = class
  private
    FRESPType: TRESPType;
    FErrorType: String;
    FStrValue: String;
    FIntValue: Integer;
    FElements: array of TRESP;
    function GetElement(const i: Integer): TRESP;
    procedure SetElement(const i: Integer; const AValue: TRESP);
    function GetElementCount: Integer;
    procedure SetElementCount(const AValue: Integer);
  public
    property RESPType: TRESPType read FRESPType write FRESPType;
    property ErrorType: String read FErrorType write FErrorType;
    property StrValue: String read FStrValue write FStrValue;
    property IntValue: Integer read FIntValue write FIntValue;
    property Elements[const i: Integer]: TRESP read GetElement write SetElement;
    property ElementCount: Integer read GetElementCount write SetElementCount;
    destructor Destroy; override;
  end;

  TAbstractTCPClient = class abstract
  public
    constructor Create(const AHost: String; const APort: Word; AConnectTimeout,ACanReadTimeout: Integer); virtual; abstract;
    function Send(const AMsg: String): String; virtual; abstract;
  end;
  TAbstractTCPClientClass = class of TAbstractTCPClient;

  TSSocketsTCPClient = class(TAbstractTCPClient)
  private
    FConn: TInetSocket;
    FCanReadTimeout: Integer;
  public
    constructor Create(const AHost: String; const APort: Word; AConnectTimeout,ACanReadTimeout: Integer); override;
    destructor Destroy; override;
    function Send(const AMsg: String): String; override;
  end;

  TRedis = class
  private
    FConn: TAbstractTCPClient;
  public
    constructor Create(AConn: TAbstractTCPClient);
    function SendCommand(AParams: array of const): TRESP;
  end;

implementation

uses
  Classes,
  SysUtils,
  {$ifdef debug}
  StrUtils,
  {$endif debug}
  Strings;

{ TRESP }

function TRESP.GetElement(const i: Integer): TRESP;
begin
  Result := FElements[i];
end;

procedure TRESP.SetElement(const i: Integer; const AValue: TRESP);
begin
  FElements[i] := AValue;
end;

function TRESP.GetElementCount: Integer;
begin
  Result := Length(FElements);
end;

procedure TRESP.SetElementCount(const AValue: Integer);
begin
  SetLength(FElements, AValue);
end;

destructor TRESP.Destroy;
var
  i: Integer;
begin
  if RESPType = rtArray then begin
    for i := 0 to ElementCount - 1 do
      Elements[i].Free;
  end;
  inherited Destroy;
end;

{ TSSocketsTCPClient }

constructor TSSocketsTCPClient.Create(const AHost: String; const APort: Word; AConnectTimeout,ACanReadTimeout: Integer);
begin
  FConn := TInetSocket.Create(AHost, APort, AConnectTimeout);
  FCanReadTimeout := ACanReadTimeout;
end;

destructor TSSocketsTCPClient.Destroy;
begin
  FConn.Free;
  inherited Destroy;
end;

function TSSocketsTCPClient.Send(const AMsg: String): String;
const
  ChunkSize = 255;
var
  LLengthSoFar,LRecvSize: Integer;
begin
  {$ifdef debug}
  WriteLn('send: ' + StringsReplace(AMsg,[#13,#10],['\r','\n'],[rfReplaceAll]));
  {$endif debug}
  FConn.Write(AMsg[1],Length(AMsg));

  while not FConn.CanRead(FCanReadTimeout) do Sleep(1); // better than no-op, will not hog CPU
  LLengthSoFar := 0;
  repeat
    SetLength(Result, LLengthSoFar + ChunkSize);
    LRecvSize := FConn.Read(Result[LLengthSoFar + 1], ChunkSize);
    Inc(LLengthSoFar, LRecvSize);
  until LRecvSize < ChunkSize;
  SetLength(Result, LLengthSoFar);
  {$ifdef debug}
  WriteLn('recv: ' + StringsReplace(Result,[#13,#10],['\r','\n'],[rfReplaceAll]));
  {$endif debug}
end;

{ TRedis }

constructor TRedis.Create(AConn: TAbstractTCPClient);
begin
  FConn := AConn;
end;

function ArrayOfConstToRESPString(AParams: array of const): String;
var
  LStrs: TStrings;
  i: Integer;
  LParam: TVarRec;
  LStr: String;
begin
  LStrs := TStringList.Create;
  LStrs.TextLineBreakStyle := tlbsCRLF;

  try
    LStrs.Add('*' + IntToStr(Length(AParams)));
    for i := Low(AParams) to High(AParams) do begin
      LParam := AParams[i];
      case LParam.vtype of
        vtAnsiString: begin
          LStr := AnsiString(LParam.VAnsiString);
          LStrs.Add('$' + IntToStr(Length(LStr)));
          LStrs.Add(LStr);
        end;
        vtChar: begin
          LStr := LParam.VChar;
          LStrs.Add('$1');
          LStrs.Add(LStr);
        end;
        vtInteger: begin
          LStr := IntToStr(LParam.VInteger);
          LStrs.Add(':' + LStr);
        end;
        { $define exceptiononunsupportedvariant}
        {$ifdef exceptiononunsupportedvariant}
        else begin
          raise Exception.Create('Unsupported variant type: ' + IntToStr(LParam.vtype));
        end
        {$endif exceptiononunsupportedvariant}
      end;
    end;
    Result := LStrs.Text;
  finally
    LStrs.Free;
  end;
end;

function RESPStringToRESP(const ARespString: String): TRESP;

  function RESPPCharToRESP(var APC: PChar): TRESP;
  var
    LPos: PChar;
    LCount,i: Integer;
    LStr: String;
  begin
    case APC^ of
      '+': begin
        LPos := StrPos(APC, CRLF);
        LCount := LPos - APC - 1;
        if LCount > 0 then begin
          SetLength(LStr, LCount);
          StrLCopy(@LStr[1], APC + 1, LCount);
          Result := TRESP.Create;
          Result.RESPType := rtString;
          Result.StrValue := LStr;
        end;
        APC := LPos + 2;
      end;
      '-': begin
        LPos := StrPos(APC, ' ');
        // the spec says space or newline, this is just to comply although when this is true that means the error has no StrValue at all
        if not Assigned(LPos) then
          LPos := StrPos(APC, CRLF);
        if Assigned(LPos) then begin
          LCount := LPos - APC - 1;
          SetLength(LStr, LCount);
          StrLCopy(@LStr[1], APC + 1, LCount);
          Result := TRESP.Create;
          Result.ErrorType := LStr;
          // current char not CR means it's an not empty error, get the StrValue
          if LPos^ <> CR then begin
            APC := LPos + 1;
            LPos := StrPos(APC, CRLF);
            LCount := LPos - APC - 1;
            SetLength(LStr, LCount);
            StrLCopy(@LStr[1], APC, LCount);
            Result.RESPType := rtError;
            Result.StrValue := LStr;
          end;
        end;
      end;
      ':': begin
        LPos := StrPos(APC, CRLF);
        LCount := LPos - APC - 1;
        if LCount > 0 then begin
          SetLength(LStr, LCount);
          StrLCopy(@LStr[1], APC + 1, LCount);
          Result := TRESP.Create;
          Result.RESPType := rtInteger;
          Result.IntValue := StrToInt(LStr);
        end;
        APC := LPos + 2;
      end;
      '$': begin
        LPos := StrPos(APC, CRLF);
        LCount := LPos - APC - 1;
        if LCount > 0 then begin
          SetLength(LStr, LCount);
          StrLCopy(@LStr[1], APC + 1, LCount);
          LCount := StrToInt(LStr);
        end;

        Result := TRESP.Create;
        Result.RESPType := rtString;
        case LCount of
          0: begin
            Result.StrValue := '';
          end;
          else begin
            APC := LPos + 2;
            SetLength(LStr, LCount);
            StrLCopy(@LStr[1], APC, LCount);
            Result.StrValue := LStr;
          end;
        end;
        Inc(APC, LCount + 2);
      end;
      '*': begin
        LPos := StrPos(APC, CRLF);
        LCount := LPos - APC - 1;
        if LCount > 0 then begin
          SetLength(LStr, LCount);
          StrLCopy(@LStr[1], APC + 1, LCount);
          LCount := StrToInt(LStr);
        end;
        APC := LPos + 2;

        Result := TRESP.Create;
        Result.RESPType := rtArray;
        Result.ElementCount := LCount;
        for i := 0 to LCount - 1 do begin
          Result.Elements[i] := RESPPCharToRESP(APC);
        end;
      end;
    end;
  end;

var
  LPC: PChar;
begin
  LPC := @ARespString[1];
  Result := RESPPCharToRESP(LPC);
end;

function TRedis.SendCommand(AParams: array of const): TRESP;
var
  LStr: String;
begin
  LStr := ArrayOfConstToRESPString(AParams);
  LStr := FConn.Send(LStr);
  Result := RESPStringToRESP(LStr);
end;

end.
