unit tcpimpl;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  lNet;

type
  EDisconnect = class(Exception) end;
  EConnError = class(Exception) end;

  TTCPConn = class
  private
    FConn: TLTCP;
    FWaiting: Boolean;
    FMessage: String;
    procedure DoReceive(aSocket: TLSocket);
    procedure DoDisconnect(aSocket: TLSocket); 
    procedure DoError(const msg: string; aSocket: TLSocket);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect(const AHost: String; const APort: Word);
    function Send(const AMsg: String): String;
    property Message: String read FMessage;
  end;

implementation

{ $define debug}

{$ifdef debug}
uses
  strutils;
{$endif debug}

{ TTCPConn }

procedure TTCPConn.DoReceive(aSocket: TLSocket);
{ $define readasmaxaspossible}
{$ifdef readasmaxaspossible}
const
  ChunkSize = 255;
var
  LLengthSoFar,LRecvSize: Integer;
begin
  LLengthSoFar := 0;
  repeat
    SetLength(FMessage, LLengthSoFar + ChunkSize);
    LRecvSize := aSocket.Get(FMessage[LLengthSoFar + 1], ChunkSize);
    Inc(LLengthSoFar, LRecvSize);
  until LRecvSize < ChunkSize;
  SetLength(FMessage, LLengthSoFar);
{$else}
begin
  aSocket.GetMessage(FMessage);
{$endif}
  FWaiting := false;  
end;

procedure TTCPConn.DoDisconnect(aSocket: TLSocket); 
begin
  raise EDisconnect.Create('');
end;

procedure TTCPConn.DoError(const msg: string; aSocket: TLSocket);
begin
  raise EConnError.Create(msg);
end;

constructor TTCPConn.Create;
begin
  FConn := TLTCP.Create(nil);
  FConn.OnReceive    := @DoReceive;
  FConn.OnDisconnect := @DoDisconnect;
  FConn.OnError      := @DoError;
  FConn.Timeout      := 100;
end;

destructor TTCPConn.Destroy;
begin
  FConn.Free;
end;

procedure TTCPConn.Connect(const AHost: String; const APort: Word);
begin
  if FConn.Connect(AHost, APort) then begin
    repeat
      FConn.CallAction;
    until not FConn.Connecting;

    if not FConn.Connected then
      raise EDisconnect.Create('');
  end;
end;

function TTCPConn.Send(const AMsg: String): String;
begin
  {$ifdef debug}
  WriteLn('send: ' + StringsReplace(AMsg,[#13,#10],['\r','\n'],[rfReplaceAll]));
  {$endif debug}
  FConn.SendMessage(AMsg);
  FWaiting := true;
  repeat
    FConn.CallAction;
  until not FWaiting;
  {$ifdef debug}
  WriteLn('recv: ' + StringsReplace(FMessage,[#13,#10],['\r','\n'],[rfReplaceAll]));
  {$endif debug}
  Result := FMessage;
end;

end.
