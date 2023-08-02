unit tcpimpl;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  ssockets;

type

  TTCPConn = class
  private
    FConn: TInetSocket;
    FCanReadTimeout: Integer;
  public
    constructor Create(const AHost: String; const APort: Word; AConnectTimeout,ACanReadTimeout: Integer);
    destructor Destroy; override;
    function Send(const AMsg: String): String;
  end;

implementation

{ $define debug}

{$ifdef debug}
uses
  strutils;
{$endif debug}

{ TTCPConn }

constructor TTCPConn.Create(const AHost: String; const APort: Word; AConnectTimeout,ACanReadTimeout: Integer);
begin
  FConn := TInetSocket.Create(AHost, APort, AConnectTimeout);
  FCanReadTimeout := ACanReadTimeout;
end;

destructor TTCPConn.Destroy;
begin
  FConn.Free;
  inherited Destroy;
end;

function TTCPConn.Send(const AMsg: String): String;
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

end.
