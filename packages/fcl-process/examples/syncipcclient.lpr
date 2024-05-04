program syncipcclient;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  simpleipc, syncipc;

const
  TEST_SERVER_NAME = 'TestSyncIPCServer';
  MSG_TEST_STOP = 101;

type
  { TTestSyncIPCClientApp }

  TTestSyncIPCClientApp = class(TCustomApplication)
  protected
    CommsClient: TSyncIPCClient;

    procedure DoRun; override;
    procedure btStopClick;
    procedure btStringClick;
    procedure btRectVarClick;
    procedure btIntClick;
    procedure btStreamClick;
    procedure btPRectClick;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TTestSyncIPCServerApp }

procedure TTestSyncIPCClientApp.DoRun;
var
   uSel:Char;

begin
  try
     CommsClient :=TSyncIPCClient.Create(nil);
     CommsClient.ServerID:=TEST_SERVER_NAME {$ifdef UNIX} + '-' + GetEnvironmentVariable('USER'){$endif};
     CommsClient.Connect;
     if CommsClient.ServerRunning then
     begin
          repeat
            Writeln; Writeln('Select what to send :');
            Writeln('   1 (String)');
            Writeln('   2 (TRect)');
            Writeln('   3 (Int)');
            Writeln('   4 (Stream)');
            Writeln('   5 (PRect)');
            Writeln('   6  STOP (null)');
            Readln(uSel);

            Case uSel of
            '1': btStringClick;
            '2': btRectVarClick;
            '3': btIntClick;
            '4': btStreamClick;
            '5': btPRectClick;
            '6': btStopClick;
            end;
          until (uSel='6');

          CommsClient.Free;
          Terminate;
          Exit;
     end;
  except
    On E:Exception do begin
      ShowException(E);
      CommsClient.Free;
      Terminate;
      Exit;
    end;

  end;
end;

procedure TTestSyncIPCClientApp.btStringClick;
Var
   recStr:String;
   resType:TMessageType;

begin
  Writeln('SendSyncMessage 1 (mtSync_String):');
  resType :=CommsClient.SendSyncMessage(30000, 1, 'Ciao SyncMsg1', recStr);
  Writeln('SendSyncMessage 1 Return ('+IntToStr(resType)+'):'+recStr);
end;

procedure TTestSyncIPCClientApp.btStopClick;
Var
   recSize, recBuf:Longint;
   resType:TMessageType;

begin
  Writeln('SendSyncMessage STOP (mtSync_Null):');
  resType :=CommsClient.SendSyncMessage(30000, MSG_TEST_STOP, mtSync_Null, recBuf, 0, recBuf, recSize);
  if (resType=mtSync_Integer) then
  begin
    Writeln('SendSyncMessage STOP Return ('+IntToStr(resType)+'):'+IntToHex(recBuf)+'-'+IntToStr(recSize));
  end;
end;

procedure TTestSyncIPCClientApp.btRectVarClick;
Var
   recBuf:TRect;
   recSize:Integer;
   resType:TMessageType;

begin
  recBuf.Top:=666;
  recBuf.Left:=999;
  recBuf.Bottom:=789;
  recBuf.Right:=456;
  recSize:=sizeof(TRect);
  Writeln('SendSyncMessage 2 (mtSync_Var):'+#13#10+
        IntToStr(recBuf.Top)+'-'+IntToStr(recBuf.Left)+'-'+IntToStr(recBuf.Bottom)+'-'+IntToStr(recBuf.Right));
  resType :=CommsClient.SendSyncMessage(30000, 2, mtSync_Var, recBuf, recSize, recBuf, recSize);
  if (resType=mtSync_Var) then
  begin
    Writeln('SendSyncMessage 2 Return ('+IntToStr(resType)+'):'+#13#10+
          IntToStr(recBuf.Top)+'-'+IntToStr(recBuf.Left)+'-'+IntToStr(recBuf.Bottom)+'-'+IntToStr(recBuf.Right));
  end;
end;

procedure TTestSyncIPCClientApp.btIntClick;
Var
   recSize, recBuf, msg:Longint;
   resType:TMessageType;

begin
  msg:=$1BCDEF23;
  Writeln('SendSyncMessage 3 (mtSync_Integer):'+IntToHex(msg));
  resType :=CommsClient.SendSyncMessage(30000, 3, mtSync_Integer, msg, 0, recBuf, recSize);
  if (resType=mtSync_Integer) then
  begin
    Writeln('SendSyncMessage 3 Return ('+IntToStr(resType)+'):'+IntToHex(recBuf)+'-'+IntToStr(recSize));
  end;
end;

procedure TTestSyncIPCClientApp.btStreamClick;
Var
   recSize:Integer;
   recBuf:TMemoryStream;
   res:TMemoryStream=nil;
   resType:TMessageType;
   retStr:String;

begin
  recBuf:=TMemoryStream.Create;
  recBuf.WriteAnsiString('SyncMessage 4 as Stream25');
  Writeln('SendSyncMessage 4 (mtSync_Stream): "SyncMessage 4 as Stream25"');
  recSize:=recBuf.Size;
  (*  //Test with Result on a new Stream
  resType :=CommsClient.SendSyncMessage(30000, 4, mtSync_Stream, recBuf, 0, res, recSize);
  if (resType=mtSync_Stream) then
  begin
    res.Position:=0;
    retStr:=res.ReadAnsiString;
    Writeln('SendSyncMessage 4 Return ('+IntToStr(resType)+' - '+IntToStr(recSize)+'):'+retStr+' - '+IntToStr(Integer(res.Size)));
  end;
  *)
  //Test with Result on the same stream
  resType :=CommsClient.SendSyncMessage(30000, 4, mtSync_Stream, recBuf, 0, recBuf, recSize);
  if (resType=mtSync_Stream) then
  begin
    retStr:=recBuf.ReadAnsiString;
    retStr:=recBuf.ReadAnsiString;
    Writeln('SendSyncMessage 4 Return ('+IntToStr(resType)+' - '+IntToStr(recSize)+'):'+retStr+' - '+IntToStr(Integer(recBuf.Size)));
  end;
  recBuf.Free;
  if res<>nil then res.Free;
end;

procedure TTestSyncIPCClientApp.btPRectClick;
Var
   recBuf:^TRect;
   recSize, msg:Integer;
   resType:TMessageType;

begin
  GetMem(recBuf, SizeOf(TRect));
  recBuf^.Top:=666;
  recBuf^.Left:=999;
  recBuf^.Bottom:=789;
  recBuf^.Right:=456;
  recSize:=sizeof(TRect);
  Writeln('SendSyncMessage 5 (mtSync_Pointer):'+#13#10+
        IntToStr(recBuf^.Top)+'-'+IntToStr(recBuf^.Left)+'-'+IntToStr(recBuf^.Bottom)+'-'+IntToStr(recBuf^.Right));
  resType :=CommsClient.SendSyncMessage(30000, 5, mtSync_Pointer, recBuf, recSize, recBuf, recSize);
  if (resType=mtSync_Pointer) then
  begin
    Writeln('SendSyncMessage 5 Return ('+IntToStr(resType)+'):'+#13#10+
          IntToStr(recBuf^.Top)+'-'+IntToStr(recBuf^.Left)+'-'+IntToStr(recBuf^.Bottom)+'-'+IntToStr(recBuf^.Right));
  end;
  FreeMem(recBuf, recSize);
end;


constructor TTestSyncIPCClientApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TTestSyncIPCClientApp.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TTestSyncIPCClientApp;

begin
  Application:=TTestSyncIPCClientApp.Create(nil);
  Application.Title:='Test SyncIPC Client';
  Application.Run;
  Application.Free;
end.

