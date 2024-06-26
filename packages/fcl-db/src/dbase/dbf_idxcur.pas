{$IFNDEF FPC_DOTTEDUNITS}
unit dbf_idxcur;
{$ENDIF FPC_DOTTEDUNITS}
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2022 by Pascal Ganaye,Micha Nelissen and other members of the
    Free Pascal development team

    DBF index cursor support

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
interface

{$I dbf_common.inc}

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils,
  System.Classes,
  Data.Db,
  Data.Dbf.Cursor,
  Data.Dbf.Idxfile,
  Data.Dbf.Prsdef,
{$ifndef Windows}
  Data.Dbf.Wtil,
{$endif}
  Data.Dbf.Common;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils,
  Classes,
  db,
  dbf_cursor,
  dbf_idxfile,
  dbf_prsdef,
{$ifndef WINDOWS}
  dbf_wtil,
{$endif}
  dbf_common;
{$ENDIF FPC_DOTTEDUNITS}

type

//====================================================================
//=== Index support
//====================================================================
  TIndexCursor = class(TVirtualCursor)
  private
    FIndexFile: TIndexFile;
  protected
    function  GetPhysicalRecNo: Integer; override;
    function  GetSequentialRecNo: Integer; override;
    function  GetSequentialRecordCount: Integer; override;
    procedure SetPhysicalRecNo(RecNo: Integer); override;
    procedure SetSequentialRecNo(RecNo: Integer); override;

    procedure VariantStrToBuffer(Key: Variant; ABuffer: TRecordBuffer);
  public
    constructor Create(DbfIndexFile: TIndexFile);
    destructor Destroy; override;

    function  Next: Boolean; override;
    function  Prev: Boolean; override;
    procedure First; override;
    procedure Last; override;

    procedure Insert(RecNo: Integer; Buffer: TRecordBuffer);
    procedure Update(RecNo: Integer; PrevBuffer, NewBuffer: TRecordBuffer);

{$ifdef SUPPORT_VARIANTS}
    function  VariantToBuffer(Key: Variant; ABuffer: TRecordBuffer): TExpressionType;
{$endif}
    function  CheckUserKey(Key: PAnsiChar; StringBuf: PAnsiChar): PAnsiChar;

    property IndexFile: TIndexFile read FIndexFile;
  end;

//====================================================================
//  TIndexCursor = class;
//====================================================================
  PIndexPosInfo = ^TIndexPage;

//====================================================================
implementation

{$ifdef WINDOWS}
{$IFDEF FPC_DOTTEDUNITS}
uses
  WinApi.Windows;
{$ELSE FPC_DOTTEDUNITS}
uses
  Windows;
{$ENDIF FPC_DOTTEDUNITS}
{$endif}

//==========================================================
//============ TIndexCursor
//==========================================================
constructor TIndexCursor.Create(DbfIndexFile: TIndexFile);
begin
  inherited Create(DbfIndexFile);

  FIndexFile := DbfIndexFile;
end;

destructor TIndexCursor.Destroy; {override;}
begin
  inherited Destroy;
end;

procedure TIndexCursor.Insert(RecNo: Integer; Buffer: TRecordBuffer);
begin
  TIndexFile(PagedFile).Insert(RecNo,Buffer);
  // TODO SET RecNo and Key
end;

procedure TIndexCursor.Update(RecNo: Integer; PrevBuffer, NewBuffer: TRecordBuffer);
begin
  TIndexFile(PagedFile).Update(RecNo, PrevBuffer, NewBuffer);
end;

procedure TIndexCursor.First;
begin
  TIndexFile(PagedFile).First;
end;

procedure TIndexCursor.Last;
begin
  TIndexFile(PagedFile).Last;
end;

function TIndexCursor.Prev: Boolean;
begin
  Result := TIndexFile(PagedFile).Prev;
end;

function TIndexCursor.Next: Boolean;
begin
  Result := TIndexFile(PagedFile).Next;
end;

function TIndexCursor.GetPhysicalRecNo: Integer;
begin
  Result := TIndexFile(PagedFile).PhysicalRecNo;
end;

procedure TIndexCursor.SetPhysicalRecNo(RecNo: Integer);
begin
  TIndexFile(PagedFile).PhysicalRecNo := RecNo;
end;

function TIndexCursor.GetSequentialRecordCount: Integer;
begin
  Result := TIndexFile(PagedFile).SequentialRecordCount;
end;

function TIndexCursor.GetSequentialRecNo: Integer;
begin
  Result := TIndexFile(PagedFile).SequentialRecNo;
end;

procedure TIndexCursor.SetSequentialRecNo(RecNo: Integer);
begin
  TIndexFile(PagedFile).SequentialRecNo := RecNo;
end;

{$ifdef SUPPORT_VARIANTS}

procedure TIndexCursor.VariantStrToBuffer(Key: Variant; ABuffer: TRecordBuffer);
var
  currLen: Integer;
  StrKey: string;
begin
  StrKey := Key;
  currLen := TranslateString(GetACP, FIndexFile.CodePage, PAnsiChar(StrKey), PAnsiChar(ABuffer), -1);
  // we have null-terminated string, pad with spaces if string too short
  FillChar(ABuffer[currLen], TIndexFile(PagedFile).KeyLen-currLen, ' ');
end;

function TIndexCursor.VariantToBuffer(Key: Variant; ABuffer: TRecordBuffer): TExpressionType;
// assumes ABuffer is large enough ie. at least max key size
begin
  if (TIndexFile(PagedFile).KeyType='N') then
  begin
    PDouble(ABuffer)^ := Key;
    if (TIndexFile(PagedFile).IndexVersion <> xBaseIII) then
    begin
      // make copy of userbcd to buffer
      Move(TIndexFile(PagedFile).PrepareKey(ABuffer, etFloat)[0], ABuffer[0], 11);
    end;
    Result := etInteger;
  end else begin
    VariantStrToBuffer(Key, ABuffer);
    Result := etString;
  end;
end;

{$endif}

function TIndexCursor.CheckUserKey(Key: PAnsiChar; StringBuf: PAnsiChar): PAnsiChar;
var
  keyLen, userLen: Integer;
begin
  // default is to use key
  Result := Key;
  // if key is double, then no check
  if (TIndexFile(PagedFile).KeyType = 'N') then
  begin
    // nothing needs to be done
  end else begin
    // check if string long enough then no copying needed
    userLen := StrLen(Key);
    keyLen := TIndexFile(PagedFile).KeyLen;
    if userLen < keyLen then
    begin
      // copy string
      Move(Key^, StringBuf[0], userLen);
      // add spaces to searchstring
      FillChar(StringBuf[userLen], keyLen - userLen, ' ');
      // set buffer to temporary buffer
      Result := StringBuf;
    end;
  end;
end;

end.

