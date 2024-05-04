{
    This file is part of the Free Component library.
    Copyright (c) 2023 by Massimo Magnano

    Unit implementing a Message-Result IPC between 2 processes

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit syncipc;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, System.SimpleIpc;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, simpleipc;
{$ENDIF FPC_DOTTEDUNITS}

const
  mtSync_Null = 2;
  mtSync_Integer = 3;
  mtSync_Stream = 4;
  mtSync_String = 5;
  mtSync_Var = 6;
  mtSync_Pointer = 7;

type

  TSyncIPCCallback = function (AElapsedTime:DWord; AMsgID:Integer) :boolean of object;

  { TSyncIPCServer }

  TSyncIPCServer = class(TSimpleIPCServer)
  protected
    rMsgID_Size: Byte;
    resultClient:TSimpleIPCClient;
    rMsgCallback:TSyncIPCCallback;

    procedure InternalMessageRecevied(Sender: TObject);

    //Derived Classes must implement this methods using MessageResult to send back the Result and return True
    //or return False for no Result
    function MessageReceived(AMsgID:Integer):Boolean; virtual; overload;
    function MessageReceived(AMsgID:Integer; AInteger:Integer; IntegerSize:Byte):Boolean; virtual; overload;
    function MessageReceived(AMsgID:Integer; AStream:TStream):Boolean; virtual; overload;
    function MessageReceived(AMsgID:Integer; const Msg: String):Boolean; virtual; overload;
    function MessageReceived(AMsgID:Integer; const Buffer; Count: LongInt):Boolean; virtual; overload;
    function MessageReceived(AMsgID:Integer; const APointer:Pointer; Count: LongInt):Boolean; virtual; overload;

    //Send back Result to Client
    function MessageResult:Boolean; overload;
    function MessageResult(ResultInteger:Integer; IntegerSize:Byte=sizeof(Integer)):Boolean; overload;
    function MessageResult(ResultStream:TStream):Boolean; overload;
    function MessageResult(const ResultString:String):Boolean; overload;
    function MessageResult(const Buffer; Count: LongInt):Boolean; overload;
    function MessageResult(const APointer:Pointer; Count: LongInt):Boolean; overload;

  public
    Constructor Create(AOwner : TComponent); override;

    //This property by default is Sizeof(Integer),
    //the user can specify a fixed size generally the same between client and server.
    //For example in communications between 32bit and 16bit systems it will be equal to 2 (16bit)
    property MsgID_Size:Byte read rMsgID_Size write rMsgID_Size;

    property MsgCallback:TSyncIPCCallback read rMsgCallback write rMsgCallback;
  end;

  { TSyncIPCClient }

  TSyncIPCClient = class(TSimpleIPCClient)
  protected
    rMsgID_Size: Byte;
    resultServer:TSimpleIPCServer;
    rMsgCallback:TSyncIPCCallback;

    function preSendSyncMessage(var MsgStream: TMemoryStream; AMsgID:Integer): Boolean; virtual;
    procedure postSendSyncMessage; virtual;
    function SendSyncMessage(ATimeOut:DWord; AMsgID:Integer;
                             AStream:TStream; ResultStream:TStream;
                             MsgType: TMessageType=mtSync_Stream):TMessageType; overload;

  public
    constructor Create(AOwner : TComponent); override;

// Buffer/AData depends on the type of MsgDataType/ResultType:
//        mtSync_Null    -> No Input/Result Params
//        mtSync_Integer -> An Integer, Count MUST contain the size of Integer or 0 for System size
//        mtSync_Stream  -> A Stream, if AData initially is nil then a new TMemoryStream is returned (user must free it)
//                                    else the result is appended in AData Stream.
//        mtSync_String  -> A String
//        mtSync_Var     -> A Formal Variable
//        mtSync_Pointer -> A Pointer, if AData initially is nil then a new Pointer with Size=ADataSize is allocated
//                                     else the Data is copied in user AData (must be sufficient ADataSize space)
    function SendSyncMessage(ATimeOut:DWord; AMsgID:Integer; MsgDataType:TMessageType;
                             const Buffer; Count: LongInt;
                             var AData; var ADataSize:Longint):TMessageType; overload;

    function SendSyncMessage(ATimeOut:DWord; AMsgID:Integer;
                             const Msg: String; var ResultString:String):TMessageType; overload;

    //This property by default is Sizeof(Integer),
    //the user can specify a fixed size generally the same between client and server.
    //For example in communications between 32bit and 16bit systems it will be equal to 2 (16bit)
    property MsgID_Size:Byte read rMsgID_Size write rMsgID_Size;

    property MsgCallback:TSyncIPCCallback read rMsgCallback write rMsgCallback;
  end;

//Read/Write IntegerSize(1 byte) followed by the AInteger(IntegerSize bytes) so we are platform indipendent
function ReadInt(AStream:TStream; var IntegerSize:Byte):Integer;
procedure WriteInt(AStream:TStream; AInteger:Integer; IntegerSize:Byte=sizeof(Integer));

implementation

function ReadInt(AStream: TStream; var IntegerSize:Byte): Integer;
begin
  //Read Sizeof Integer so we are platform indipendent
  AStream.Read(IntegerSize, 1);
  AStream.Read(Result, IntegerSize);
end;

procedure WriteInt(AStream: TStream; AInteger:Integer; IntegerSize: Byte);
begin
  if (IntegerSize=0) then IntegerSize:=Sizeof(Integer);

  //Write Sizeof Integer so we are platform indipendent
  AStream.Write(IntegerSize, 1);
  AStream.Write(AInteger, IntegerSize);
end;

{ TSyncIPCServer }

procedure TSyncIPCServer.InternalMessageRecevied(Sender: TObject);
var
  curMsgID, msgInteger:Integer;
  msgIDSize:Byte;
  curMsgType:TMessageType;
  resultServerID:String;
  msgStream, resStream:TMemoryStream;
  AResult:Boolean;

begin
  ReadMessage;
  curMsgType :=Self.MsgType;

  //Is it our message?
  if (curMsgType in [mtSync_Null..mtSync_Pointer]) then
  begin
    msgStream:=TMemoryStream(Self.MsgData);
    msgStream.Position:=0;

    //Read from msgStream where to send the Result
    resultServerID:=msgStream.ReadAnsiString;
    curMsgID :=ReadInt(msgStream, msgIDSize);
    { #todo -oMaxM : Test the difference between msgIDSize and rMsgID_Size ? }

    FreeAndNil(resultClient);

    if (resultServerID<>'') then
    try
       //Create resultClient and connect to resultServerID
       resultClient:=TSimpleIPCClient.Create(Nil);
       resultClient.ServerID:=resultServerID;
       resultClient.Connect;

       if resultClient.ServerRunning then
       begin
         //Processes the Received message based on its type
         Case curMsgType of
         mtSync_Null: AResult :=MessageReceived(curMsgID);
         mtSync_Integer: begin
            msgInteger :=ReadInt(msgStream, msgIDSize);
            AResult :=MessageReceived(curMsgID, msgInteger, msgIDSize);
         end;
         mtSync_Stream: try
            //Copy the Message to a new Stream, so there won't be the initial part with the serverid
            resStream :=TMemoryStream.Create;
            resStream.CopyFrom(msgStream, msgStream.Size-msgStream.Position);
            AResult :=MessageReceived(curMsgID, resStream);
         finally
           resStream.Free;
         end;

         mtSync_String: AResult :=MessageReceived(curMsgID, msgStream.ReadAnsiString);
         mtSync_Var: AResult :=MessageReceived(curMsgID, Pointer(msgStream.Memory+msgStream.Position)^, msgStream.Size-msgStream.Position);
         mtSync_Pointer: AResult :=MessageReceived(curMsgID, Pointer(msgStream.Memory+msgStream.Position), msgStream.Size-msgStream.Position);
         end;

         //if MessageReceived has no Result send something to avoid TimeOut
         if not(AResult) then MessageResult(0, 1);
       end;

    finally
       FreeAndNil(resultClient);
    end;
  end;
end;

function TSyncIPCServer.MessageResult: Boolean;
begin
  Result:=False;
end;

function TSyncIPCServer.MessageResult(ResultInteger: Integer; IntegerSize:Byte): Boolean;
var
   curResBuffer:TMemoryStream;

begin
  try
     Result:=False;

     //Create a MemoryStream to send back result and write an Integer
     curResBuffer:=TMemoryStream.Create;
     WriteInt(curResBuffer, ResultInteger, IntegerSize);

     //Send MemoryStream back to client
     resultClient.SendMessage(mtSync_Integer, curResBuffer);
     Result:=True;

  finally
    curResBuffer.Free;
  end;
end;

function TSyncIPCServer.MessageResult(ResultStream: TStream): Boolean;
begin
  try
     Result:=False;
     //Send back ResultStream
     resultClient.SendMessage(mtSync_Stream, ResultStream);
     Result:=True;
  finally
  end;
end;

function TSyncIPCServer.MessageResult(const ResultString: String): Boolean;
var
   curResBuffer:TMemoryStream;

begin
  try
     Result:=False;

     //Create a MemoryStream to send back result and write a String
     curResBuffer:=TMemoryStream.Create;
     curResBuffer.WriteAnsiString(ResultString);

     resultClient.SendMessage(mtSync_String, curResBuffer);
     Result:=True;

  finally
    curResBuffer.Free;
  end;
end;

function TSyncIPCServer.MessageResult(const Buffer; Count: LongInt): Boolean;
var
   curResBuffer:TMemoryStream;

begin
  try
     Result:=False;

     //Create a MemoryStream to send back result and write the Buffer
     curResBuffer:=TMemoryStream.Create;
     curResBuffer.Write(Buffer, Count);


     resultClient.SendMessage(mtSync_Var, curResBuffer);
     Result:=True;

  finally
    curResBuffer.Free;
  end;
end;

function TSyncIPCServer.MessageResult(const APointer: Pointer; Count: LongInt): Boolean;
var
   curResBuffer:TMemoryStream;

begin
  try
     Result:=False;

     //Create a MemoryStream to send back result and write Data pointed by APointer
     curResBuffer:=TMemoryStream.Create;
     curResBuffer.Write(APointer^, Count);

     resultClient.SendMessage(mtSync_Pointer, curResBuffer);
     Result:=True;

  finally
    curResBuffer.Free;
  end;
end;

function TSyncIPCServer.MessageReceived(AMsgID: Integer): Boolean;
begin
  Result :=False;
end;

function TSyncIPCServer.MessageReceived(AMsgID: Integer; AInteger: Integer; IntegerSize:Byte): Boolean;
begin
  //Derived class do something like
  // Case AMsgID of
  // 101: Result :=MessageResult($ABCDEF01);
  // end;

  Result :=False;
end;

function TSyncIPCServer.MessageReceived(AMsgID: Integer; AStream: TStream): Boolean;
begin
  Result :=False;
end;

function TSyncIPCServer.MessageReceived(AMsgID: Integer; const Msg: String): Boolean;
begin
  Result :=False;
end;

function TSyncIPCServer.MessageReceived(AMsgID: Integer; const Buffer; Count: LongInt): Boolean;
begin
  Result :=False;
end;

function TSyncIPCServer.MessageReceived(AMsgID: Integer; const APointer: Pointer; Count: LongInt): Boolean;
begin
  Result :=False;
end;

constructor TSyncIPCServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  rMsgID_Size:=Sizeof(Integer);
  resultClient:=nil;
  Global:=True;
  Self.OnMessageQueued:=@InternalMessageRecevied;
end;

{ TSyncIPCClient }

function TSyncIPCClient.preSendSyncMessage(var MsgStream: TMemoryStream; AMsgID: Integer): Boolean;
var
   myID:TGUID;

   function randCreateGuid:TGUID;
   var
      i:Integer;
      P : PByte;

   begin
     //In Case the system has no CreateGUID we create a random string (Copied from SysUtils)
     Randomize;
     P:=@Result;
     for i:=0 to SizeOf(TGuid)-1 do P[i]:=Random(256);
     Result.clock_seq_hi_and_reserved:=(Result.clock_seq_hi_and_reserved and $3F) + 64;
     Result.time_hi_and_version      :=(Result.time_hi_and_version and $0FFF)+ $4000;
   end;

begin
  Result :=False;

  //Create a Server where to receive the Result and give it a unique name
  resultServer  :=TSimpleIPCServer.Create(Nil);
  if (CreateGUID(myID)=0)
  then resultServer.ServerID:=GUIDToString(myID)
  else resultServer.ServerID:=GUIDToString(randCreateGuid);
  resultServer.Global:=True;
  resultServer.StartServer(False);

  if resultServer.Active then
  begin
    Connect;
    if ServerRunning then
    begin
      //Write at the beginning the name of the server where to send the result, followed by AMsgID
      MsgStream:=TMemoryStream.Create;
      MsgStream.WriteAnsiString(resultServer.ServerID);
      WriteInt(MsgStream, AMsgID, rMsgID_Size);
      Result:=True;
    end;
  end;
end;

procedure TSyncIPCClient.postSendSyncMessage;
begin
  FreeAndNil(resultServer);
end;

function TSyncIPCClient.SendSyncMessage(ATimeOut: DWord; AMsgID: Integer;
                                        AStream: TStream; ResultStream: TStream;
                                        MsgType: TMessageType): TMessageType;
var
   myTickStart, curTick:QWord;
   MsgStream:TMemoryStream=nil;
   aborted:Boolean;

begin
  try
     Result :=mtUnknown;
     aborted :=False;

     //Prepare the resultServer and the Message Incipit
     if preSendSyncMessage(MsgStream, AMsgID) then
     begin
       //Append User Message to MsgStream and send it
       MsgStream.CopyFrom(AStream, 0);
       SendMessage(MsgType, MsgStream);

       if assigned(rMsgCallback) then aborted :=rMsgCallback(0, AMsgID);

       myTickStart :=GetTickCount64; curTick :=myTickStart;

       //Wait (Max for ATimeOut ms) for an Answer in resultServer
       while not(aborted) and ((curTick-myTickStart)<=ATimeOut) do
       begin
         CheckSynchronize;          //Application.ProcessMessages;

         if resultServer.PeekMessage(0, True) then
         begin
           Result:=resultServer.MsgType;
           resultServer.GetMessageData(ResultStream);

           break;
         end;

         curTick :=GetTickCount64;

         if assigned(rMsgCallback) then aborted :=rMsgCallback(ATimeOut, AMsgID);

       end;

       if assigned(rMsgCallback) then rMsgCallback(ATimeOut, AMsgID);
     end;

  finally
    MsgStream.Free;
    postSendSyncMessage;
  end;
end;

constructor TSyncIPCClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  rMsgID_Size:=Sizeof(Integer);
end;

function TSyncIPCClient.SendSyncMessage(ATimeOut: DWord; AMsgID: Integer; MsgDataType: TMessageType;
                                        const Buffer; Count: LongInt;
                                        var AData; var ADataSize: Longint): TMessageType;
var
   msgStream, resStream:TMemoryStream;
   resIntSize:Byte;

begin
  try
     Result :=mtUnknown;

     //We will always use a stream to send the message to the Server,
     //The server will always use a stream to send the results back to us
     msgStream:=nil;
     resStream:=TMemoryStream.Create;

     Case MsgDataType of
     mtSync_Null:begin
        msgStream:=TMemoryStream.Create;
        Result :=SendSyncMessage(ATimeOut, AMsgID, msgStream, resStream, MsgDataType);
     end;
     mtSync_Integer: begin
        msgStream:=TMemoryStream.Create;
        WriteInt(msgStream, Integer(Buffer), Count);
        Result :=SendSyncMessage(ATimeOut, AMsgID, msgStream, resStream, MsgDataType);
     end;
                                                                //use directly the user Stream
     mtSync_Stream : Result :=SendSyncMessage(ATimeOut, AMsgID, TStream(Buffer), resStream, MsgDataType);

     mtSync_String : begin
        msgStream:=TMemoryStream.Create;
        msgStream.WriteAnsiString(String(Buffer));
        Result :=SendSyncMessage(ATimeOut, AMsgID, msgStream, resStream, MsgDataType);
     end;
     mtSync_Var : begin
        msgStream:=TMemoryStream.Create;
        msgStream.Write(Buffer, Count);
        Result :=SendSyncMessage(ATimeOut, AMsgID, msgStream, resStream, MsgDataType);
     end;
     mtSync_Pointer: begin
        msgStream:=TMemoryStream.Create;
        msgStream.Write(Pointer(Buffer)^, Count);
        Result :=SendSyncMessage(ATimeOut, AMsgID, msgStream, resStream, MsgDataType);
     end;
     end;

     resStream.Position:=0;

     //Depending on the type of result fill AData
     Case Result of
     mtSync_Null:begin
        ADataSize:=0;
        resStream.Free;
     end;
     mtSync_Integer: begin
        Integer(AData) :=ReadInt(resStream, resIntSize);
        ADataSize:=resIntSize;
        resStream.Free;
     end;
     mtSync_Stream : begin
        if (TStream(AData)=nil)
        then begin
               //Return resStream directly and do not free it
               TStream(AData) :=resStream;
               ADataSize :=resStream.Size;
             end
        else try
               ADataSize :=TStream(AData).CopyFrom(resStream, 0);

               //if Buffer and AData are the same stream reposition to the beginning of the result
               if (TStream(AData)=TStream(Buffer))
               then TStream(AData).Position:=TStream(AData).Size-resStream.Size;
             finally
               resStream.Free;
             end;
     end;
     mtSync_String : begin
        String(AData) :=resStream.ReadAnsiString;
        resStream.Free;
     end;
     mtSync_Var : begin
        ADataSize:=resStream.Size;
        ADataSize :=resStream.Read(AData, ADataSize);
        resStream.Free;
     end;
     mtSync_Pointer:  begin
        ADataSize:=resStream.Size;

        if (Pointer(AData)=nil)
        then GetMem(Pointer(AData), ADataSize);

        ADataSize :=resStream.Read(Pointer(AData)^, ADataSize);
        resStream.Free;
     end;
     end;

  finally
     if (msgStream<>nil) then msgStream.Free;
  end;
end;

//A simplified version that Send a String and receive a String
function TSyncIPCClient.SendSyncMessage(ATimeOut: DWord; AMsgID: Integer;
                                        const Msg: String; var ResultString: String): TMessageType;
var
   msgStream, resStream:TMemoryStream;

begin
  try
     Result :=mtUnknown;
     resStream:=TMemoryStream.Create;
     msgStream:=TMemoryStream.Create;
     msgStream.WriteAnsiString(Msg);
     Result :=SendSyncMessage(ATimeOut, AMsgID, msgStream, resStream, mtSync_String);

     if (Result=mtSync_String)
     then begin
            resStream.Position:=0;
            ResultString :=resStream.ReadAnsiString;
          end
     else ResultString:='';

  finally
    msgStream.Free;
    resStream.Free;
  end;
end;


end.

