unit redis;

{$mode objfpc}{$H+}

interface

uses
  tcpimpl;

const
  DefaultHost = '127.0.0.1';
  DefaultPort = 6379;

type
  TRedis = record
    Conn: TTCPConn;
  end;
  PRedis = ^TRedis;

  TRESPType = (rtError,rtString,rtInteger,rtArray);
  TRESP = record
    RESPType: TRESPType;
    ErrorType: String;
    StrValue: String;
    IntValue: Integer;
    Elements: array of ^TRESP;
  end;
  PRESP = ^TRESP;

function Connect(const AHost: String; const APort: Word): PRedis;
procedure Disconnect(ARedis: PRedis);

function SendCommand(ARedis: PRedis; AParams: array of const): PRESP;
procedure DisposeRESP(ARESP: PRESP);

implementation

uses
  Classes,
  SysUtils,
  Strings;

function Connect(const AHost: String; const APort: Word): PRedis;
begin
  New(Result);
  Result^.Conn := TTCPConn.Create;
  Result^.Conn.Connect(AHost, APort);
end;

procedure Disconnect(ARedis: PRedis);
begin
  if Assigned(ARedis) then begin
    ARedis^.Conn.Free;
  end;
  Dispose(ARedis);
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

function RESPStringToRESP(const ARespString: String): PRESP;

  function RESPPCharToRESP(var APC: PChar): PRESP;
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
          New(Result);
          Result^.RESPType := rtString;
          Result^.StrValue := LStr;
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
          New(Result);
          Result^.ErrorType := LStr;
          if LPos^ <> #13 then begin
            APC := LPos + 1;
            LPos := StrPos(APC, #13#10);
            LCount := LPos - APC - 1;
            SetLength(LStr, LCount);
            StrLCopy(@LStr[1], APC, LCount);
            Result^.RESPType := rtError;
            Result^.StrValue := LStr;
          end;
        end;
      end;
      ':': begin
        LPos := StrPos(APC, #13#10);
        LCount := LPos - APC - 1;
        if LCount > 0 then begin
          SetLength(LStr, LCount);
          StrLCopy(@LStr[1], APC + 1, LCount);
          New(Result);
          Result^.RESPType := rtInteger;
          Result^.IntValue := StrToInt(LStr);
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

        New(Result);
        Result^.RESPType := rtString;
        case LCount of
          0: begin
            Result^.StrValue := '';
          end;
          else begin
            APC := LPos + 2;
            SetLength(LStr, LCount);
            StrLCopy(@LStr[1], APC, LCount);
            Result^.StrValue := LStr;
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

        New(Result);
        Result^.RESPType := rtArray;
        SetLength(Result^.Elements, LCount);
        for i := 0 to LCount - 1 do begin
          Result^.Elements[i] := RESPPCharToRESP(APC);
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

function SendCommand(ARedis: PRedis; AParams: array of const): PRESP;
var
  LStr: String;
begin
  LStr := ArrayOfConstToRESPString(AParams);
  LStr := ARedis^.Conn.Send(LStr);
  Result := RESPStringToRESP(LStr);
end;

procedure DisposeRESP(ARESP: PRESP);
var
  i: Integer;
begin
  if Assigned(ARESP) then begin
    if ARESP^.RESPType = rtArray then
      for i := 0 to Length(ARESP^.Elements) - 1 do
        DisposeRESP(ARESP^.Elements[i]);
    Dispose(ARESP);
  end;
end;

end.
