program testcommands;

uses
  redis;

const
  MapKey    = 'key';
  MapValue  = 'value';
  ListKey   = 'listkey';
  ListValue = 'v';
var
  GRedis: TRedis;
  GRESP: TRESP;
  i: Integer;
begin
  GRedis := TRedis.Create(redis.DefaultHost, redis.DefaultPort);

  GRESP := GRedis.SendCommand(['SET', MapKey, MapValue]);
  if GRESP.RESPType = rtError then begin
    WriteLn(StdErr, GRESP.ErrorType + ': ' + GRESP.StrValue);
    Halt(1);
  end else begin
    WriteLn(GRESP.StrValue);
  end;
  GRESP.Free;

  GRESP := GRedis.SendCommand(['GET', MapKey]);
  if GRESP.RESPType = rtError then begin
    WriteLn(StdErr, GRESP.ErrorType + ': ' + GRESP.StrValue);
    Halt(1);
  end else begin
    WriteLn(GRESP.StrValue);
  end;
  GRESP.Free;

  for i := 1 to 3 do begin
    GRESP := GRedis.SendCommand(['LPUSH', ListKey, ListValue]);
    if GRESP.RESPType = rtError then begin
      WriteLn(StdErr, GRESP.ErrorType + ': ' + GRESP.StrValue);
      Halt(1);
    end else begin
      WriteLn(GRESP.IntValue);
    end;
    GRESP.Free;
  end;

  GRESP := GRedis.SendCommand(['LRANGE', ListKey, '0', '-1']);
  if GRESP.RESPType = rtError then begin
    WriteLn(StdErr, GRESP.ErrorType + ': ' + GRESP.StrValue);
    Halt(1);
  end else begin
    for i := 0 to Length(GRESP.Elements) - 1 do begin
      if i > 0 then Write(', ');
      Write(GRESP.Elements[i].StrValue);
    end;
    WriteLn;
  end;
  GRESP.Free;

  for i := 1 to 3 do begin
    GRESP := GRedis.SendCommand(['RPOP', ListKey]);
    if GRESP.RESPType = rtError then begin
      WriteLn(StdErr, GRESP.ErrorType + ': ' + GRESP.StrValue);
      Halt(1);
    end else begin
      WriteLn(GRESP.StrValue);
    end;
    GRESP.Free;
  end;

  GRESP := GRedis.SendCommand(['DEL', MapKey]);
  if GRESP.RESPType = rtError then begin
    WriteLn(StdErr, GRESP.ErrorType + ': ' + GRESP.StrValue);
    Halt(1);
  end else begin
    WriteLn(GRESP.IntValue);
  end;
  GRESP.Free;

  GRESP := GRedis.SendCommand(['DEL', ListKey]);
  if GRESP.RESPType = rtError then begin
    WriteLn(StdErr, GRESP.ErrorType + ': ' + GRESP.StrValue);
    Halt(1);
  end else begin
    WriteLn(GRESP.IntValue);
  end;
  GRESP.Free;

  GRedis.Free;
end.
