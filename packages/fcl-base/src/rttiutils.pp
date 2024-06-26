{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 by the Free Pascal development team

    Some RTTI utils, based on RX rtti utils.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ **********************************************************************

  Based on the rttiutils unit that comes with RXLib.
  Adapted to work with FCL, free of VCL dependencies.
  Fixed some errors along the way as well. MVC.

  To make it work across the 'Root Component' (Form/Datamodule etc),
  you MUST set the FindGlobalComponentCallBack event handler.

  Original copyright:
         Delphi VCL Extensions (RX)
         Copyright (c) 1995, 1996 AO ROSNO
         Copyright (c) 1997 Master-Bank
  **********************************************************************}

{$mode objfpc}
{$H+}
{$IFNDEF FPC_DOTTEDUNITS}
unit RttiUtils;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, System.Classes, {Graphics, MacOsApi.Controls, Forms,} System.TypInfo, System.StrUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, Classes, {Graphics, Controls, Forms,} TypInfo, StrUtils;
{$ENDIF FPC_DOTTEDUNITS}

type

{ TPropInfoList }

  TPropInfoList = class(TObject)
  private
    FList: PPropList;
    FCount: Integer;
    FSize: Integer;
    function Get(Index: Integer): PPropInfo;
  public
    constructor Create(AObject: TObject; Filter: TTypeKinds; Sorted: Boolean = True);
    destructor Destroy; override;
    function Contains(P: PPropInfo): Boolean;
    function Find(const AName: string): PPropInfo;
    procedure Delete(Index: Integer);
    procedure Intersect(List: TPropInfoList);
    property Count: Integer read FCount;
    property Items[Index: Integer]: PPropInfo read Get; default;
  end;

{ TPropsStorage }

  TReadStrEvent = function(const ASection, Item, Default: string): string of object;
  TWriteStrEvent = procedure(const ASection, Item, Value: string) of object;
  TEraseSectEvent = procedure(const ASection: string) of object;
  TPropStorageOption = (psoAlwaysStoreStringsCount);
  TPropStorageOptions = set of TPropStorageOption;
  
  TPropsStorage = class(TObject)
  private
    FObject: TObject;
    FOwner: TComponent;
    FPrefix: string;
    FSection: string;
    FOptions : TPropStorageOptions;
    FOnReadString: TReadStrEvent;
    FOnWriteString: TWriteStrEvent;
    FOnEraseSection: TEraseSectEvent;
    function StoreIntegerProperty(PropInfo: PPropInfo): string;
    function StoreCharProperty(PropInfo: PPropInfo): string;
    function StoreEnumProperty(PropInfo: PPropInfo): string;
    function StoreFloatProperty(PropInfo: PPropInfo): string;
    function StoreStringProperty(PropInfo: PPropInfo): string;
    function StoreSetProperty(PropInfo: PPropInfo): string;
    function StoreClassProperty(PropInfo: PPropInfo): string;
    function StoreStringsProperty(PropInfo: PPropInfo): string;
    function StoreComponentProperty(PropInfo: PPropInfo): string;
    function StoreLStringProperty(PropInfo: PPropInfo): string;
    function StoreWCharProperty(PropInfo: PPropInfo): string;
    function StoreVariantProperty(PropInfo: PPropInfo): string;
    procedure LoadLStringProperty(const S: string; PropInfo: PPropInfo);
    procedure LoadWCharProperty(const S: string; PropInfo: PPropInfo);
    procedure LoadVariantProperty(const S: string; PropInfo: PPropInfo);
    function StoreInt64Property(PropInfo: PPropInfo): string;
    procedure LoadInt64Property(const S: string; PropInfo: PPropInfo);
    procedure LoadIntegerProperty(const S: string; PropInfo: PPropInfo);
    procedure LoadCharProperty(const S: string; PropInfo: PPropInfo);
    procedure LoadEnumProperty(const S: string; PropInfo: PPropInfo);
    procedure LoadFloatProperty(const S: string; PropInfo: PPropInfo);
    procedure LoadStringProperty(const S: string; PropInfo: PPropInfo);
    procedure LoadSetProperty(const S: string; PropInfo: PPropInfo);
    procedure LoadClassProperty(const S: string; PropInfo: PPropInfo);
    procedure LoadStringsProperty(const S: string; PropInfo: PPropInfo);
    procedure LoadComponentProperty(const S: string; PropInfo: PPropInfo);
    function CreateInfoList(AComponent: TComponent; StoredList: TStrings): TStrings;
    procedure FreeInfoLists(Info: TStrings);
  protected
    function ReadString(const ASection, Item, Default: string): string; virtual;
    procedure WriteString(const ASection, Item, Value: string); virtual;
    procedure EraseSection(const ASection: string); virtual;
    function GetItemName(const APropName: string): string; virtual;
    function CreateStorage: TPropsStorage; virtual;
  public
    procedure StoreAnyProperty(PropInfo: PPropInfo);
    procedure LoadAnyProperty(PropInfo: PPropInfo);
    procedure StoreProperties(PropList: TStrings);
    procedure LoadProperties(PropList: TStrings);
    procedure LoadObjectsProps(AComponent: TComponent; StoredList: TStrings);
    procedure StoreObjectsProps(AComponent: TComponent; StoredList: TStrings);
    Property Options : TPropStorageOptions Read FOptions Write FOptions; 
    property AObject: TObject read FObject write FObject;
    property Prefix: string read FPrefix write FPrefix;
    property Section: string read FSection write FSection;
    property OnReadString: TReadStrEvent read FOnReadString write FOnReadString;
    property OnWriteString: TWriteStrEvent read FOnWriteString write FOnWriteString;
    property OnEraseSection: TEraseSectEvent read FOnEraseSection write FOnEraseSection;
  end;

{ Utility routines }

procedure UpdateStoredList(AComponent: TComponent; AStoredList: TStrings; FromForm: Boolean);
function CreateStoredItem(const CompName, PropName: string): string;
function ParseStoredItem(const Item: string; var CompName, PropName: string): Boolean;

const
  sPropNameDelimiter: string = '_';

Type
  TFindComponentEvent = Function (Const Name : String) : TComponent;

Var
  FindGlobalComponentCallBack : TFindComponentEvent;

implementation

const
  sCount = 'Count';
  sItem = 'Item%d';
  sNull = '(null)';

type
  TCardinalSet = set of 0..SizeOf(Cardinal) * 8 - 1;

function GetPropType(PropInfo: PPropInfo): PTypeInfo;
begin
  Result := PropInfo^.PropType;
end;

{ TPropInfoList }

constructor TPropInfoList.Create(AObject: TObject; Filter: TTypeKinds; Sorted: Boolean);
begin
  if AObject <> nil then
    begin
    FCount := GetPropList(AObject.ClassInfo, Filter, nil, Sorted);
    FSize := FCount * SizeOf(Pointer);
    GetMem(FList, FSize);
    GetPropList(AObject.ClassInfo, Filter, FList, Sorted);
    end
  else
    begin
    FCount := 0;
    FList := nil;
    end;
end;

destructor TPropInfoList.Destroy;
begin
  if FList <> nil then FreeMem(FList, FSize);
end;

function TPropInfoList.Contains(P: PPropInfo): Boolean;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    with FList^[I]^ do
      if (PropType = P^.PropType) and (CompareText(Name, P^.Name) = 0) then
      begin
        Result := True;
        Exit;
      end;
  Result := False;
end;

function TPropInfoList.Find(const AName: string): PPropInfo;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    with FList^[I]^ do
      if (CompareText(Name, AName) = 0) then
      begin
        Result := FList^[I];
        Exit;
      end;
  Result := nil;
end;

procedure TPropInfoList.Delete(Index: Integer);
begin
  Dec(FCount);
  if Index < FCount then Move(FList^[Index + 1], FList^[Index],
    (FCount - Index) * SizeOf(Pointer));
end;

function TPropInfoList.Get(Index: Integer): PPropInfo;
begin
  Result := FList^[Index];
end;

procedure TPropInfoList.Intersect(List: TPropInfoList);
var
  I: Integer;
begin
  for I := FCount - 1 downto 0 do
    if not List.Contains(FList^[I]) then Delete(I);
end;

{ Utility routines }

function CreateStoredItem(const CompName, PropName: string): string;
begin
  Result := '';
  if (CompName <> '') and (PropName <> '') then
    Result := CompName + '.' + PropName;
end;

function ParseStoredItem(const Item: string; var CompName, PropName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  if Length(Item) = 0 then Exit;
  I := Pos('.', Item);
  if I > 0 then begin
    CompName := Trim(Copy(Item, 1, I - 1));
    PropName := Trim(Copy(Item, I + 1, MaxInt));
    Result := (Length(CompName) > 0) and (Length(PropName) > 0);
  end;
end;

function ReplaceComponentName(const Item, CompName: string): string;
var
  ACompName, APropName: string;
begin
  Result := '';
  if ParseStoredItem(Item, ACompName, APropName) then
    Result := CreateStoredItem(CompName, APropName);
end;

procedure UpdateStoredList(AComponent: TComponent; AStoredList: TStrings; FromForm: Boolean);

var
  I: Integer;
  Component: TComponent;
  CompName, PropName: string;

begin
  if (AStoredList = nil) or (AComponent = nil) then
    Exit;
  for I := AStoredList.Count - 1 downto 0 do
    begin
    if ParseStoredItem(AStoredList[I], CompName, PropName) then
      begin
      if FromForm then
        begin
        Component := AComponent.FindComponent(CompName);
        if Component = nil then
          AStoredList.Delete(I)
        else
          AStoredList.Objects[I]:=Component;
        end
      else
        begin
        Component := TComponent(AStoredList.Objects[I]);
        if Component <> nil then
          AStoredList[I] := ReplaceComponentName(AStoredList[I], Component.Name)
        else
          AStoredList.Delete(I);
        end;
      end
    else
      AStoredList.Delete(I);
  end;
end;

function FindGlobalComponent(const Name: string): TComponent;

begin
  Result:=Nil;
  If Assigned(FindGlobalComponentCallBack) then
    Result:=FindGlobalComponentCallBack(Name);
end;

{ TPropsStorage }

function TPropsStorage.GetItemName(const APropName: string): string;
begin
  Result := Prefix + APropName;
end;

procedure TPropsStorage.LoadAnyProperty(PropInfo: PPropInfo);
var
  S, Def: string;
begin
  try
    if PropInfo <> nil then
      begin
      case PropInfo^.PropType^.Kind of
        tkBool,
        tkInteger: Def := StoreIntegerProperty(PropInfo);
        tkChar: Def := StoreCharProperty(PropInfo);
        tkEnumeration: Def := StoreEnumProperty(PropInfo);
        tkFloat: Def := StoreFloatProperty(PropInfo);
        tkWChar: Def := StoreWCharProperty(PropInfo);
        tkAstring,
        tkLString: Def := StoreLStringProperty(PropInfo);
        tkWString: Def := StoreLStringProperty(PropInfo);
        tkVariant: Def := StoreVariantProperty(PropInfo);
        tkInt64: Def := StoreInt64Property(PropInfo);
        tkString: Def := StoreStringProperty(PropInfo);
        tkSet: Def := StoreSetProperty(PropInfo);
        tkClass: Def := '';
      else
        Exit;
      end;
      if (Def <> '') or (PropInfo^.PropType^.Kind in [tkString, tkClass])
        or (PropInfo^.PropType^.Kind in [tkAString,tkLString, tkWString, tkWChar]) then
        S := Trim(ReadString(Section, GetItemName(PropInfo^.Name), Def))
      else
        S := '';
      case PropInfo^.PropType^.Kind of
        tkBool:LoadIntegerProperty(S,PropInfo);
        tkInteger: LoadIntegerProperty(S, PropInfo);
        tkChar: LoadCharProperty(S, PropInfo);
        tkEnumeration: LoadEnumProperty(S, PropInfo);
        tkFloat: LoadFloatProperty(S, PropInfo);
        tkWChar: LoadWCharProperty(S, PropInfo);
        tkAString,
        tkLString: LoadLStringProperty(S, PropInfo);
        tkWString: LoadLStringProperty(S, PropInfo);
        tkVariant: LoadVariantProperty(S, PropInfo);
        tkInt64: LoadInt64Property(S, PropInfo);
        tkString: LoadStringProperty(S, PropInfo);
        tkSet: LoadSetProperty(S, PropInfo);
        tkClass: LoadClassProperty(S, PropInfo);
      else
        Exit;
      end;
    end;
  except
    { ignore any exception }
  end;
end;

procedure TPropsStorage.StoreAnyProperty(PropInfo: PPropInfo);
var
  S: string;
begin
  if PropInfo <> nil then
    begin
    case PropInfo^.PropType^.Kind of
      tkInteger: S := StoreIntegerProperty(PropInfo);
      tkChar: S := StoreCharProperty(PropInfo);
      tkEnumeration: S := StoreEnumProperty(PropInfo);
      tkFloat: S := StoreFloatProperty(PropInfo);
      tkAstring: S := StoreLStringProperty(PropInfo);
      tkWString: S := StoreLStringProperty(PropInfo);
      tkWChar: S := StoreWCharProperty(PropInfo);
      tkVariant: S := StoreVariantProperty(PropInfo);
      tkInt64: S := StoreInt64Property(PropInfo);
      tkString: S := StoreStringProperty(PropInfo);
      tkSet: S := StoreSetProperty(PropInfo);
      tkClass: S := StoreClassProperty(PropInfo);
      tkBool: S:=StoreIntegerProperty(PropInfo);
    else
      Exit;
    end;
    if (S <> '') or (PropInfo^.PropType^.Kind in [tkString
      , tkLString, tkAString, tkWString, tkWChar ]) then
      WriteString(Section, GetItemName(PropInfo^.Name), Trim(S));
  end;
end;

function TPropsStorage.StoreIntegerProperty(PropInfo: PPropInfo): string;
begin
  Result := IntToStr(GetOrdProp(FObject, PropInfo));
end;

function TPropsStorage.StoreCharProperty(PropInfo: PPropInfo): string;
begin
  Result := Char(GetOrdProp(FObject, PropInfo));
end;

function TPropsStorage.StoreEnumProperty(PropInfo: PPropInfo): string;
begin
  Result := GetEnumName(GetPropType(PropInfo), GetOrdProp(FObject, PropInfo));
end;

function TPropsStorage.StoreFloatProperty(PropInfo: PPropInfo): string;
const
  Precisions: array[TFloatType] of Integer = (7, 15, 18, 18, 19);

begin
  Result := StringReplace(FloatToStrF(GetFloatProp(FObject, PropInfo), ffGeneral,
    Precisions[GetTypeData(GetPropType(PropInfo))^.FloatType], 0),
    DecimalSeparator, '.',[rfReplaceAll]);
end;

function TPropsStorage.StoreStringProperty(PropInfo: PPropInfo): string;
begin
  Result := GetStrProp(FObject, PropInfo);
end;

function TPropsStorage.StoreLStringProperty(PropInfo: PPropInfo): string;
begin
  Result := GetStrProp(FObject, PropInfo);
end;

function TPropsStorage.StoreWCharProperty(PropInfo: PPropInfo): string;
begin
  Result := Char(GetOrdProp(FObject, PropInfo));
end;

function TPropsStorage.StoreVariantProperty(PropInfo: PPropInfo): string;
begin
  Result := GetVariantProp(FObject, PropInfo);
end;

function TPropsStorage.StoreInt64Property(PropInfo: PPropInfo): string;
begin
  Result := IntToStr(GetInt64Prop(FObject, PropInfo));
end;

function TPropsStorage.StoreSetProperty(PropInfo: PPropInfo): string;
var
  TypeInfo: PTypeInfo;
  W: Cardinal;
  I: Integer;
begin
  Result := '[';
  W := GetOrdProp(FObject, PropInfo);
  TypeInfo := GetTypeData(GetPropType(PropInfo))^.CompType;
  for I := 0 to SizeOf(TCardinalSet) * 8 - 1 do
    if I in TCardinalSet(W) then begin
      if Length(Result) <> 1 then Result := Result + ',';
      Result := Result + GetEnumName(TypeInfo, I);
    end;
  Result := Result + ']';
end;

function TPropsStorage.StoreStringsProperty(PropInfo: PPropInfo): string;
var
  List: TObject;
  I: Integer;
  SectName: string;
begin
  Result := '';
  List := TObject(GetObjectProp(Self.FObject, PropInfo));
  SectName := Format('%s.%s', [Section, GetItemName(PropInfo^.Name)]);
  EraseSection(SectName);
  if (List is TStrings) 
     and ((TStrings(List).Count > 0) or (psoAlwaysStoreStringsCount in Options)) then 
    begin
    WriteString(SectName, sCount, IntToStr(TStrings(List).Count));
    for I := 0 to TStrings(List).Count - 1 do
      WriteString(SectName, Format(sItem, [I]), TStrings(List)[I]);
    end;
end;

function TPropsStorage.StoreComponentProperty(PropInfo: PPropInfo): string;
var
  Comp: TComponent;
  RootName: string;
begin
  Comp := TComponent(GetObjectProp(FObject, PropInfo));
  if Comp <> nil then begin
    Result := Comp.Name;
    if (Comp.Owner <> nil) and (Comp.Owner <> FOwner) then begin
      RootName := Comp.Owner.Name;
      if RootName = '' then begin
        RootName := Comp.Owner.ClassName;
        if (RootName <> '') and (UpCase(RootName[1]) = 'T') then
          Delete(RootName, 1, 1);
      end;
      Result := Format('%s.%s', [RootName, Result]);
    end;
  end
  else Result := sNull;
end;

function TPropsStorage.StoreClassProperty(PropInfo: PPropInfo): string;
var
  Saver: TPropsStorage;
  I: Integer;
  Obj: TObject;

  procedure StoreObjectProps(Obj: TObject; const APrefix, ASection: string);
  var
    I: Integer;
    Props: TPropInfoList;
  begin
    with Saver do begin
      AObject := Obj;
      Prefix := APrefix;
      Section := ASection;
      FOnWriteString := Self.FOnWriteString;
      FOnEraseSection := Self.FOnEraseSection;
      Props := TPropInfoList.Create(AObject, tkProperties);
      try
        for I := 0 to Props.Count - 1 do StoreAnyProperty(Props.Items[I]);
      finally
        Props.Free;
      end;
    end;
  end;

begin
  Result := '';
  Obj := TObject(GetObjectProp(Self.FObject, PropInfo));
  if (Obj <> nil) then begin
    if Obj is TStrings then StoreStringsProperty(PropInfo)
    else if Obj is TCollection then begin
      EraseSection(Format('%s.%s', [Section, Prefix + PropInfo^.Name]));
      Saver := CreateStorage;
      try
        WriteString(Section, Format('%s.%s', [Prefix + PropInfo^.Name, sCount]),
          IntToStr(TCollection(Obj).Count));
        for I := 0 to TCollection(Obj).Count - 1 do begin
          StoreObjectProps(TCollection(Obj).Items[I],
            Format(sItem, [I]) + sPropNameDelimiter,
            Format('%s.%s', [Section, Prefix + PropInfo^.Name]));
        end;
      finally
        Saver.Free;
      end;
    end
    else if Obj is TComponent then begin
      Result := StoreComponentProperty(PropInfo);
      Exit;
    end;
  end;
  Saver := CreateStorage;
  try
    with Saver do begin
      StoreObjectProps(Obj, Self.Prefix + PropInfo^.Name, Self.Section);
    end;
  finally
    Saver.Free;
  end;
end;

procedure TPropsStorage.LoadIntegerProperty(const S: string; PropInfo: PPropInfo);
begin
  SetOrdProp(FObject, PropInfo, StrToIntDef(S, 0));
end;

procedure TPropsStorage.LoadCharProperty(const S: string; PropInfo: PPropInfo);
begin
  SetOrdProp(FObject, PropInfo, Integer(S[1]));
end;

procedure TPropsStorage.LoadEnumProperty(const S: string; PropInfo: PPropInfo);
var
  I: Integer;
  EnumType: PTypeInfo;
begin
  EnumType := GetPropType(PropInfo);
  with GetTypeData(EnumType)^ do
    for I := MinValue to MaxValue do
      if CompareText(GetEnumName(EnumType, I), S) = 0 then
      begin
        SetOrdProp(FObject, PropInfo, I);
        Exit;
      end;
end;

procedure TPropsStorage.LoadFloatProperty(const S: string; PropInfo: PPropInfo);
begin
  SetFloatProp(FObject, PropInfo, StrToFloat(StringReplace(S, '.',
    DecimalSeparator,[rfReplaceAll])));
end;

procedure TPropsStorage.LoadInt64Property(const S: string; PropInfo: PPropInfo);
begin
  SetInt64Prop(FObject, PropInfo, StrToInt64Def(S, 0));
end;

procedure TPropsStorage.LoadLStringProperty(const S: string; PropInfo: PPropInfo);
begin
  SetStrProp(FObject, PropInfo, S);
end;

procedure TPropsStorage.LoadWCharProperty(const S: string; PropInfo: PPropInfo);
begin
  SetOrdProp(FObject, PropInfo, Longint(S[1]));
end;

procedure TPropsStorage.LoadVariantProperty(const S: string; PropInfo: PPropInfo);
begin
  SetVariantProp(FObject, PropInfo, S);
end;

procedure TPropsStorage.LoadStringProperty(const S: string; PropInfo: PPropInfo);
begin
  SetStrProp(FObject, PropInfo, S);
end;

procedure TPropsStorage.LoadSetProperty(const S: string; PropInfo: PPropInfo);
const
  Delims = [' ', ',', '[', ']'];
var
  TypeInfo: PTypeInfo;
  W: Cardinal;
  I, N: Integer;
  Count: Integer;
  EnumName: string;
begin
  W := 0;
  TypeInfo := GetTypeData(GetPropType(PropInfo))^.CompType;
  Count := WordCount(S, Delims);
  for N := 1 to Count do begin
    EnumName := ExtractWord(N, S, Delims);
    try
      I := GetEnumValue(TypeInfo, EnumName);
      if I >= 0 then Include(TCardinalSet(W), I);
    except
    end;
  end;
  SetOrdProp(FObject, PropInfo, W);
end;

procedure TPropsStorage.LoadStringsProperty(const S: string; PropInfo: PPropInfo);
var
  List: TObject;
  Temp: TStrings;
  I, Cnt: Integer;
  SectName: string;
begin
  List := TObject(GetObjectProp(Self.FObject, PropInfo));
  if (List is TStrings) then begin
    SectName := Format('%s.%s', [Section, GetItemName(PropInfo^.Name)]);
    Cnt := StrToIntDef(Trim(ReadString(SectName, sCount, '0')), 0);
    if Cnt > 0 then begin
      Temp := TStringList.Create;
      try
        for I := 0 to Cnt - 1 do
          Temp.Add(ReadString(SectName, Format(sItem, [I]), ''));
        TStrings(List).Assign(Temp);
      finally
        Temp.Free;
      end;
    end;
  end;
end;

procedure TPropsStorage.LoadComponentProperty(const S: string; PropInfo: PPropInfo);
var
  RootName, Name: string;
  Root: TComponent;
  P: Integer;
begin
  if Trim(S) = '' then Exit;
  if CompareText(SNull, Trim(S)) = 0 then begin
    SetOrdProp(FObject, PropInfo, Longint(nil));
    Exit;
  end;
  P := Pos('.', S);
  if P > 0 then begin
    RootName := Trim(Copy(S, 1, P - 1));
    Name := Trim(Copy(S, P + 1, MaxInt));
  end
  else begin
    RootName := '';
    Name := Trim(S);
  end;
  if RootName <> '' then Root := FindGlobalComponent(RootName)
  else Root := FOwner;
  if (Root <> nil) then
    SetObjectProp(FObject, PropInfo, Root.FindComponent(Name));
end;

procedure TPropsStorage.LoadClassProperty(const S: string; PropInfo: PPropInfo);
var
  Loader: TPropsStorage;
  I: Integer;
  Cnt: Integer;
  Recreate: Boolean;
  Obj: TObject;

  procedure LoadObjectProps(Obj: TObject; const APrefix, ASection: string);
  var
    I: Integer;
    Props: TPropInfoList;
  begin
    with Loader do begin
      AObject := Obj;
      Prefix := APrefix;
      Section := ASection;
      FOnReadString := Self.FOnReadString;
      Props := TPropInfoList.Create(AObject, tkProperties);
      try
        for I := 0 to Props.Count - 1 do LoadAnyProperty(Props.Items[I]);
      finally
        Props.Free;
      end;
    end;
  end;

begin
  Obj := TObject(GetObjectProp(Self.FObject, PropInfo));
  if (Obj <> nil) then begin
    if Obj is TStrings then LoadStringsProperty(S, PropInfo)
    else if Obj is TCollection then begin
      Loader := CreateStorage;
      try
        Cnt := TCollection(Obj).Count;
        Cnt := StrToIntDef(ReadString(Section, Format('%s.%s',
          [Prefix + PropInfo^.Name, sCount]), IntToStr(Cnt)), Cnt);
        Recreate := TCollection(Obj).Count <> Cnt;
        TCollection(Obj).BeginUpdate;
        try
          if Recreate then TCollection(Obj).Clear;
          for I := 0 to Cnt - 1 do begin
            if Recreate then TCollection(Obj).Add;
            LoadObjectProps(TCollection(Obj).Items[I],
              Format(sItem, [I]) + sPropNameDelimiter,
              Format('%s.%s', [Section, Prefix + PropInfo^.Name]));
          end;
        finally
          TCollection(Obj).EndUpdate;
        end;
      finally
        Loader.Free;
      end;
    end
    else if Obj is TComponent then begin
      LoadComponentProperty(S, PropInfo);
      Exit;
    end;
  end;
  Loader := CreateStorage;
  try
    LoadObjectProps(Obj, Self.Prefix + PropInfo^.Name, Self.Section);
  finally
    Loader.Free;
  end;
end;

procedure TPropsStorage.StoreProperties(PropList: TStrings);
var
  I: Integer;
  Props: TPropInfoList;
begin
  Props := TPropInfoList.Create(AObject, tkProperties);
  try
    for I := 0 to PropList.Count - 1 do
      StoreAnyProperty(Props.Find(PropList[I]));
  finally
    Props.Free;
  end;
end;

procedure TPropsStorage.LoadProperties(PropList: TStrings);
var
  I: Integer;
  Props: TPropInfoList;
begin
  Props := TPropInfoList.Create(AObject, tkProperties);
  try
    for I := 0 to PropList.Count - 1 do
      LoadAnyProperty(Props.Find(PropList[I]));
  finally
    Props.Free;
  end;
end;

function TPropsStorage.CreateInfoList(AComponent: TComponent; StoredList: TStrings): TStrings;
var
  I: Integer;
  Obj: TComponent;
  Props: TPropInfoList;
begin
  UpdateStoredList(AComponent, StoredList, False);
  Result := TStringList.Create;
  try
    TStringList(Result).Sorted := True;
    for I := 0 to StoredList.Count - 1 do
      begin
      Obj := TComponent(StoredList.Objects[I]);
      if Result.IndexOf(Obj.Name) < 0 then
        begin
        Props := TPropInfoList.Create(Obj, tkProperties);
        try
          Result.AddObject(Obj.Name, Props);
        except
          Props.Free;
          raise;
        end;
        end;
      end;
  except
    On E : Exception do
      begin
      Result.Free;
      Result := nil;
      end;
  end;
end;

procedure TPropsStorage.FreeInfoLists(Info: TStrings);
var
  I: Integer;
begin
  for I := Info.Count - 1 downto 0 do Info.Objects[I].Free;
  Info.Free;
end;

procedure TPropsStorage.LoadObjectsProps(AComponent: TComponent; StoredList: TStrings);
var
  Info: TStrings;
  I, Idx: Integer;
  Props: TPropInfoList;
  CompName, PropName: string;
begin
  Info := CreateInfoList(AComponent, StoredList);
  if Info <> nil then
  try
    FOwner := AComponent;
    for I := 0 to StoredList.Count - 1 do
      begin
      if ParseStoredItem(StoredList[I], CompName, PropName) then
        begin
        AObject := StoredList.Objects[I];
        Prefix := TComponent(AObject).Name;
        Idx := Info.IndexOf(Prefix);
        if Idx >= 0 then
          begin
          Prefix := Prefix + sPropNameDelimiter;
          Props := TPropInfoList(Info.Objects[Idx]);
          if Props <> nil then
            LoadAnyProperty(Props.Find(PropName));
          end;
        end;
    end;
  finally
    FOwner := nil;
    FreeInfoLists(Info);
  end;
end;

procedure TPropsStorage.StoreObjectsProps(AComponent: TComponent; StoredList: TStrings);
var
  Info: TStrings;
  I, Idx: Integer;
  Props: TPropInfoList;
  CompName, PropName: string;
begin
  Info := CreateInfoList(AComponent, StoredList);
  if Info <> nil then
    try
      FOwner := AComponent;
      for I := 0 to StoredList.Count - 1 do
        begin
        if ParseStoredItem(StoredList[I], CompName, PropName) then
          begin
          AObject := StoredList.Objects[I];
          Prefix := TComponent(AObject).Name;
          Idx := Info.IndexOf(Prefix);
          if Idx >= 0 then
            begin
            Prefix := Prefix + sPropNameDelimiter;
            Props := TPropInfoList(Info.Objects[Idx]);
            if Props <> nil then
              StoreAnyProperty(Props.Find(PropName));
            end;
          end;
        end;
    finally
      FOwner := nil;
      FreeInfoLists(Info);
    end;
end;

function TPropsStorage.CreateStorage: TPropsStorage;
begin
  Result := TPropsStorage.Create;
end;

function TPropsStorage.ReadString(const ASection, Item, Default: string): string;
begin
  if Assigned(FOnReadString) then Result := FOnReadString(ASection, Item, Default)
  else Result := '';
end;

procedure TPropsStorage.WriteString(const ASection, Item, Value: string);
begin
  if Assigned(FOnWriteString) then FOnWriteString(ASection, Item, Value);
end;

procedure TPropsStorage.EraseSection(const ASection: string);
begin
  if Assigned(FOnEraseSection) then FOnEraseSection(ASection);
end;

end.
