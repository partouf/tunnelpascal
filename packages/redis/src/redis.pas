unit redis;

{$mode objfpc}{$H+}

interface

uses
  tcpimpl;

const
  DefaultHost           = '127.0.0.1';
  DefaultPort           = 6379;
  DefaultConnectTimeout = 100;
  DefaultCanReadTimeout = 100;

type

  TRESPType = (rtError,rtString,rtInteger,rtArray);

  TRESP = class
  public
    RESPType: TRESPType;
    ErrorType: String;
    StrValue: String;
    IntValue: Integer;
    Elements: array of TRESP;
    destructor Destroy; override;
  end;

  TRedis = class
  private
    FConn: TTCPConn;
  public
    constructor Create(const AHost: String; const APort: Word);
    destructor Destroy; override;
    function SendCommand(AParams: array of const): TRESP;
  end;

implementation

uses
  Classes,
  SysUtils,
  Strings;

constructor TRedis.Create(const AHost: String; const APort: Word);
begin
  FConn := TTCPConn.Create(AHost, APort, DefaultConnectTimeout, DefaultCanReadTimeout);
end;

destructor TRedis.Destroy;
begin
  FConn.Free;
  inherited Destroy;
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
        LPos := StrPos(APC, #13#10);
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
          LPos := StrPos(APC, #13#10);
        if Assigned(LPos) then begin
          LCount := LPos - APC - 1;
          SetLength(LStr, LCount);
          StrLCopy(@LStr[1], APC + 1, LCount);
          Result := TRESP.Create;
          Result.ErrorType := LStr;
          if LPos^ <> #13 then begin
            APC := LPos + 1;
            LPos := StrPos(APC, #13#10);
            LCount := LPos - APC - 1;
            SetLength(LStr, LCount);
            StrLCopy(@LStr[1], APC, LCount);
            Result.RESPType := rtError;
            Result.StrValue := LStr;
          end;
        end;
      end;
      ':': begin
        LPos := StrPos(APC, #13#10);
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
        LPos := StrPos(APC, #13#10);
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
        LPos := StrPos(APC, #13#10);
        LCount := LPos - APC - 1;
        if LCount > 0 then begin
          SetLength(LStr, LCount);
          StrLCopy(@LStr[1], APC + 1, LCount);
          LCount := StrToInt(LStr);
        end;
        APC := LPos + 2;

        Result := TRESP.Create;
        Result.RESPType := rtArray;
        SetLength(Result.Elements, LCount);
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

destructor TRESP.Destroy;
var
  i: Integer;
begin
  if RESPType = rtArray then begin
    for i := 0 to Length(Elements) - 1 do
      Elements[i].Free;
  end;
  inherited Destroy;
end;

end.
