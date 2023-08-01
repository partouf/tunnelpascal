program testcommands;

uses
  redis;

const
  MapKey    = 'key';
  MapValue  = 'value';
  ListKey   = 'listkey';
  ListValue = 'v';
var
  GRedis: PRedis;
  GRESP: PRESP;
  i: Integer;
begin
  GRedis := Redis.Connect(redis.DefaultHost, redis.DefaultPort);

  GRESP := Redis.SendCommand(GRedis, ['SET', MapKey, MapValue]);
  if GRESP^.RESPType = rtError then begin
    WriteLn(StdErr, GRESP^.ErrorType + ': ' + GRESP^.StrValue);
    Halt(1);
  end else begin
    WriteLn(GRESP^.StrValue);
  end;
  DisposeRESP(GRESP);

  GRESP := Redis.SendCommand(GRedis, ['GET', MapKey]);
  if GRESP^.RESPType = rtError then begin
    WriteLn(StdErr, GRESP^.ErrorType + ': ' + GRESP^.StrValue);
    Halt(1);
  end else begin
    WriteLn(GRESP^.StrValue);
  end;
  DisposeRESP(GRESP);

  for i := 1 to 3 do begin
    GRESP := Redis.SendCommand(GRedis, ['LPUSH', ListKey, ListValue]);
    if GRESP^.RESPType = rtError then begin
      WriteLn(StdErr, GRESP^.ErrorType + ': ' + GRESP^.StrValue);
      Halt(1);
    end else begin
      WriteLn(GRESP^.IntValue);
    end;
    DisposeRESP(GRESP);
  end;

  GRESP := Redis.SendCommand(GRedis, ['LRANGE', ListKey, '0', '-1']);
  if GRESP^.RESPType = rtError then begin
    WriteLn(StdErr, GRESP^.ErrorType + ': ' + GRESP^.StrValue);
    Halt(1);
  end else begin
    for i := 0 to Length(GRESP^.Elements) - 1 do begin
      if i > 0 then Write(', ');
      Write(GRESP^.Elements[i]^.StrValue);
    end;
    WriteLn;
  end;
  DisposeRESP(GRESP);

  for i := 1 to 3 do begin
    GRESP := Redis.SendCommand(GRedis, ['RPOP', ListKey]);
    if GRESP^.RESPType = rtError then begin
      WriteLn(StdErr, GRESP^.ErrorType + ': ' + GRESP^.StrValue);
      Halt(1);
    end else begin
      WriteLn(GRESP^.StrValue);
    end;
    DisposeRESP(GRESP);
  end;

  GRESP := Redis.SendCommand(GRedis, ['DEL', MapKey]);
  if GRESP^.RESPType = rtError then begin
    WriteLn(StdErr, GRESP^.ErrorType + ': ' + GRESP^.StrValue);
    Halt(1);
  end else begin
    WriteLn(GRESP^.IntValue);
  end;
  DisposeRESP(GRESP);

  GRESP := Redis.SendCommand(GRedis, ['DEL', ListKey]);
  if GRESP^.RESPType = rtError then begin
    WriteLn(StdErr, GRESP^.ErrorType + ': ' + GRESP^.StrValue);
    Halt(1);
  end else begin
    WriteLn(GRESP^.IntValue);
  end;
  DisposeRESP(GRESP);

  Redis.Disconnect(GRedis);
end.
