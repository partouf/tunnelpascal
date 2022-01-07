{
    Copyright (c) 1998-2002 by Florian Klaempfl and Peter Vreman

    This module provides some basic classes

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit cclasses;

{$i fpcdefs.inc}

{$define CCLASSESINLINE}

interface

    uses
{$IFNDEF USE_FAKE_SYSUTILS}
      SysUtils,
{$ELSE}
      fksysutl,
{$ENDIF}
      globtype,
      CUtils,CStreams;

{********************************************
                TMemDebug
********************************************}

    type
       tmemdebug = class
       private
          totalmem,
          startmem : int64;
          infostr  : string[40];
       public
          constructor Create(const s:string);
          destructor  Destroy;override;
          procedure show;
          procedure start;
          procedure stop;
       end;

{*******************************************************
      TFPList (From rtl/objpas/classes/classesh.inc)
********************************************************}

const
   SListIndexError = 'List index exceeds bounds (%d)';
   SListCapacityError = 'The maximum list capacity is reached (%d)';
   SListCapacityPower2Error = 'The capacity has to be a power of 2, but is set to %d';
   SListCountError = 'List count too large (%d)';
type
   EListError = class(Exception);

const
  MaxListSize = Maxint div 16;
type
  TListSortCompare = function (Item1, Item2: Pointer): Integer;
  TListCallback = procedure(data,arg:pointer) of object;
  TListStaticCallback = procedure(data,arg:pointer);
  TDynStringArray = Array Of String;
  TDirection = (FromBeginning,FromEnd);
  TFPList = class(TObject)
  private
    FList: PPointer;
    FCount: Integer;
    FCapacity: Integer;
  protected
    function Get(Index: Integer): Pointer;
    procedure Put(Index: Integer; Item: Pointer);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    Procedure RaiseIndexError(Index : Integer);
    property List: PPointer read FList;
  public
    destructor Destroy; override;
    function Add(Item: Pointer): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    class procedure Error(const Msg: string; Data: PtrInt);
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TFPList;
    function Extract(item: Pointer): Pointer;
    function First: Pointer;
    function IndexOf(Item: Pointer): Integer;
    function IndexOfItem(Item: Pointer; Direction: TDirection): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
    function Last: Pointer;
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Assign(Obj:TFPList);
    function Remove(Item: Pointer): Integer;
    procedure Pack;
    procedure Sort(Compare: TListSortCompare);
    procedure ForEachCall(proc2call:TListCallback;arg:pointer);
    procedure ForEachCall(proc2call:TListStaticCallback;arg:pointer);
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: Pointer read Get write Put; default;
  end;


{*******************************************************
        TFPObjectList (From fcl/inc/contnrs.pp)
********************************************************}

  TObjectListCallback = procedure(data:TObject;arg:pointer) of object;
  TObjectListStaticCallback = procedure(data:TObject;arg:pointer);

  TFPObjectList = class(TObject)
  private
    FFreeObjects : Boolean;
    FList: TFPList;
    function GetCount: integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure SetCount(const AValue: integer);
  protected
    function GetItem(Index: Integer): TObject; {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure SetItem(Index: Integer; AObject: TObject);
    procedure SetCapacity(NewCapacity: Integer); {$ifdef CCLASSESINLINE}inline;{$endif}
    function GetCapacity: integer; {$ifdef CCLASSESINLINE}inline;{$endif}
  public
    constructor Create;
    constructor Create(FreeObjects : Boolean);
    destructor Destroy; override;
    procedure Clear;
    function Add(AObject: TObject): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer); {$ifdef CCLASSESINLINE}inline;{$endif}
    function Expand: TFPObjectList;{$ifdef CCLASSESINLINE}inline;{$endif}
    function Extract(Item: TObject): TObject; {$ifdef CCLASSESINLINE}inline;{$endif}
    function Remove(AObject: TObject): Integer;
    function IndexOf(AObject: TObject): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    function IndexOfItem(AObject: TObject; Direction: TDirection): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    function FindInstanceOf(AClass: TClass; AExact: Boolean; AStartAt: Integer): Integer;
    procedure Insert(Index: Integer; AObject: TObject); {$ifdef CCLASSESINLINE}inline;{$endif}
    function First: TObject; {$ifdef CCLASSESINLINE}inline;{$endif}
    function Last: TObject; {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure Move(CurIndex, NewIndex: Integer); {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure Assign(Obj:TFPObjectList);
    procedure ConcatListCopy(Obj:TFPObjectList);
    procedure Pack; {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure Sort(Compare: TListSortCompare); {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure ForEachCall(proc2call:TObjectListCallback;arg:pointer); {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure ForEachCall(proc2call:TObjectListStaticCallback;arg:pointer); {$ifdef CCLASSESINLINE}inline;{$endif}
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property OwnsObjects: Boolean read FFreeObjects write FFreeObjects;
    property Items[Index: Integer]: TObject read GetItem write SetItem; default;
    property List: TFPList read FList;
  end;

{ Memory region that allocates chunks with .Push and frees them all at once with .Done, useful for storing shortstrings.
  Alignment of the sizes is the user's responsibility, but shortstrings are composed of bytes and unaffected,
  and, in general, objects of the same nature will have same alignment and be sized as its multiple,
  not to mention using such a region exclusively for arrays of the same type, for example. }

  PMemoryRegionNode = ^TMemoryRegionNode;
  TMemoryRegionNode = record
    n, alloc: uint32;
    next: PMemoryRegionNode;
    data: array[0 .. 0] of byte; { variable-sized; and aligned to pointer. }
  end;

const
  MinMemoryRegionNodeSize=64;

type
  TMemoryRegion = object
    procedure Init(preallocate: SizeUint=0);
    procedure Done; {$ifdef CCLASSESINLINE}inline;{$endif}
    function Push(n: SizeUint): pointer;
    procedure Clear;
    function CalcSumSize: SizeUint; { don't want to store it as its retrieval is logarithmic. }
  private
    FTop: PMemoryRegionNode;
    class function AllocateNode(n, alloc: SizeUint): PMemoryRegionNode; static;
    function PushNewNode(n: SizeUint): pointer;
  end;

{ "Vi" stands for variable-sized indices.
  Variable-sized indices use less space and reduce the size of a region with potentially chaotic accesses (FHash). }
type
  TViTypeEnum = (vi_u8, vi_u16, vi_u24, vi_u32);
  TViGetter = function(p: pointer; index: SizeUint): SizeUint;
  TViSetter = procedure(p: pointer; index: SizeUint; const value: SizeUint);
  uint24 = packed record
{$if defined(endian_little)}
    lo16: uint16;
    hi8: uint8;
{$elseif defined(endian_big)}
    hi8: uint8;
    lo16: uint16;
{$else} {$error unknown endianness} {$endif}
  end;

  function vi_u8_get(p: pointer; index: SizeUint): SizeUint;
  procedure vi_u8_set(p: pointer; index: SizeUint; const value: SizeUint);
  function vi_u16_get(p: pointer; index: SizeUint): SizeUint;
  procedure vi_u16_set(p: pointer; index: SizeUint; const value: SizeUint);
{ Assumes FOUR bytes are available. Hence ViTypes[u24].overallocate = 1. }
  function vi_u24_get(p: pointer; index: SizeUint): SizeUint;
  procedure vi_u24_set(p: pointer; index: SizeUint; const value: SizeUint);
  function vi_u32_get(p: pointer; index: SizeUint): SizeUint;
  procedure vi_u32_set(p: pointer; index: SizeUint; const value: SizeUint);

type
  PViTypeDesc = ^TViTypeDesc;
  TViTypeDesc = record
    size, overallocate: uint8;
    lim: uint32;
    get: TViGetter;
    &set: TViSetter;
  end;

const
  ViTypes: array[TViTypeEnum] of TViTypeDesc =
  (
    (size: sizeof(uint8);  overallocate: 0; lim: High(uint8);  get: @vi_u8_get;  &set: @vi_u8_set),
    (size: sizeof(uint16); overallocate: 0; lim: High(uint16); get: @vi_u16_get; &set: @vi_u16_set),
    (size: 3;              overallocate: 1; lim: 1 shl 24 - 1; get: @vi_u24_get; &set: @vi_u24_set),
    (size: sizeof(uint32); overallocate: 0; lim: High(uint32); get: @vi_u32_get; &set: @vi_u32_set)
  );

  function ChooseViType(fitValue: SizeUint): PViTypeDesc;
  function ViTypeFromGetter(get: TViGetter): PViTypeDesc; { To avoid storing PViTypeDesc if rarely used. }
  function ViDataSize(ty: PViTypeDesc; n: SizeUint): SizeUint;

const
  ViEmpty = 0;
  ViRealIndexOffset = 1;

type
  PViHashListItem = ^TViHashListItem;
  TViHashListItem = record
    HashValue: uint32;
    Next: int32;
    Str: {$ifdef symansistr} TSymStr {$else} PSymStr {$endif};
    Data: Pointer;
  end;

  TViRehashMode = (vi_Auto, vi_Tight, vi_Pack);

  TViHashList = class(TObject)
  private
    { When not special "empty list", that is, when Assigned(FItems), FHash is a memory region containing FHash + FItems. }
    FHash: Pointer; { Hash table. ViEmpty means empty cell, ViRealIndexOffset+i references FItems[i]. }
    FItems: PViHashListItem;
    FGetIndex: TViGetter; { Accessors for FHash. }
    FSetIndex: TViSetter;
    FHashMask: uint32; { Count of indices in FHash is always "FHashMask + 1" and is always a power of two. }
    FCount: int32;
    FCapacity: uint32; { Allocation size of FItems. Generally speaking, can be arbitrary, without any relation to "FHashMask + 1". }
{$ifndef symansistr}
    FShortstringRegion: TMemoryRegion;
{$endif}
    function Get(Index: SizeInt): Pointer;
    procedure Put(Index: SizeInt; Item: Pointer);
    class procedure RaiseIndexError(Index: SizeInt); static;
    procedure SetupEmptyTable;
    procedure Rehash(ForItems: SizeUint; mode: TViRehashMode=vi_Auto);
{$ifndef symansistr}
    function AddStrToRegion(const s: TSymStr): PSymStr;
{$endif}
    procedure Shrink;
    procedure AddToHashTable(Item: PViHashListItem; Index: SizeUint);
    function InternalFind(AHash:LongWord;const AName:TSymStr;out PrevIndex:SizeInt):SizeInt;
    procedure RemoveFromHashTable(AHash:LongWord;Index, PrevIndex: SizeInt);
    procedure SetCapacity(NewCapacity: uint32);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const AName:TSymStr;Item: Pointer): SizeInt;
    procedure Clear;
    function NameOfIndex(Index: SizeInt): TSymStr;
    function HashOfIndex(Index: SizeInt): LongWord;
    function GetNextCollision(Index: SizeInt): SizeInt; {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure Delete(Index: SizeInt);
    function Extract(item: Pointer): Pointer;
    function IndexOf(Item: Pointer): SizeInt;
    function Find(const AName:TSymStr): Pointer; {$ifdef CCLASSESINLINE}inline;{$endif}
    function FindIndexOf(const AName:TSymStr): SizeInt; {$ifdef CCLASSESINLINE}inline;{$endif}
    function FindWithHash(const AName:TSymStr;AHash:LongWord): Pointer;
    function Rename(const AOldName,ANewName:TSymStr): SizeInt;
    function Remove(Item: Pointer): SizeInt;
    procedure Pack;
    procedure ShowStatistics;
    procedure ForEachCall(proc2call:TListCallback;arg:pointer);
    procedure ForEachCall(proc2call:TListStaticCallback;arg:pointer);
    property Count: int32 read FCount;
    property Capacity: uint32 read FCapacity write SetCapacity;
    property Items[Index: SizeInt]: Pointer read Get write Put; default;
    property List: PViHashListItem read FItems;
  end;

  TFPHashList=TViHashList;

const
  MaxHashListSize = Maxint div 16;


{*******************************************************
        TFPHashObjectList (From fcl/inc/contnrs.pp)
********************************************************}

type
  TFPHashObjectList = class;

  { TFPHashObject }

  TFPHashObject = class
  private
    FOwner     : TFPHashObjectList;
    FStr       : {$ifdef symansistr} TSymStr {$else} PSymStr {$endif};
    FHash      : LongWord;
    procedure InternalChangeOwner(HashObjectList:TFPHashObjectList;const s:TSymStr);
  protected
    function GetName:TSymStr;virtual;
    function GetHash:Longword;virtual;
  public
    constructor CreateNotOwned;
    constructor Create(HashObjectList:TFPHashObjectList;const s:TSymStr);
    procedure ChangeOwner(HashObjectList:TFPHashObjectList);
    procedure ChangeOwnerAndName(HashObjectList:TFPHashObjectList;const s:TSymStr); {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure Rename(const ANewName:TSymStr);
    property Name:TSymStr read GetName;
    property Hash:Longword read GetHash;
    property OwnerList: TFPHashObjectList read FOwner;
  end;

  TFPHashObjectList = class(TObject)
  private
    FFreeObjects : Boolean;
    FHashList: TFPHashList;
    function GetCount: integer; {$ifdef CCLASSESINLINE}inline;{$endif}
  protected
    function GetItem(Index: Integer): TObject; {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure SetItem(Index: Integer; AObject: TObject);
    procedure SetCapacity(NewCapacity: Integer); {$ifdef CCLASSESINLINE}inline;{$endif}
    function GetCapacity: integer; {$ifdef CCLASSESINLINE}inline;{$endif}
  public
    constructor Create(FreeObjects : boolean = True);
    destructor Destroy; override;
    procedure Clear;
    function Add(const AName:TSymStr;AObject: TObject): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    function NameOfIndex(Index: Integer): TSymStr; {$ifdef CCLASSESINLINE}inline;{$endif}
    function HashOfIndex(Index: Integer): LongWord; {$ifdef CCLASSESINLINE}inline;{$endif}
    function GetNextCollision(Index: Integer): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure Delete(Index: Integer);
    function Extract(Item: TObject): TObject; {$ifdef CCLASSESINLINE}inline;{$endif}
    function Remove(AObject: TObject): Integer;
    function IndexOf(AObject: TObject): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    function Find(const s:TSymStr): TObject; {$ifdef CCLASSESINLINE}inline;{$endif}
    function FindIndexOf(const s:TSymStr): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    function FindWithHash(const AName:TSymStr;AHash:LongWord): Pointer; {$ifdef CCLASSESINLINE}inline;{$endif}
    function Rename(const AOldName,ANewName:TSymStr): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    function FindInstanceOf(AClass: TClass; AExact: Boolean; AStartAt: Integer): Integer;
    procedure Pack; {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure ShowStatistics; {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure ForEachCall(proc2call:TObjectListCallback;arg:pointer); {$ifdef CCLASSESINLINE}inline;{$endif}
    procedure ForEachCall(proc2call:TObjectListStaticCallback;arg:pointer); {$ifdef CCLASSESINLINE}inline;{$endif}
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property OwnsObjects: Boolean read FFreeObjects write FFreeObjects;
    property Items[Index: Integer]: TObject read GetItem write SetItem; default;
    property List: TFPHashList read FHashList;
  end;


{********************************************
                TLinkedList
********************************************}

    type
       TLinkedListItem = class
       public
          Previous,
          Next : TLinkedListItem;
          Constructor Create;
          Destructor Destroy;override;
          Function GetCopy:TLinkedListItem;virtual;
       end;

       TLinkedListItemClass = class of TLinkedListItem;

       TLinkedList = class
       private
          FCount : integer;
          FFirst,
          FLast  : TLinkedListItem;
          FNoClear : boolean;
       public
          constructor Create;
          destructor  Destroy;override;
          { true when the List is empty }
          function  Empty:boolean; {$ifdef CCLASSESINLINE}inline;{$endif}
          { deletes all Items }
          procedure Clear;
          { inserts an Item }
          procedure Insert(Item:TLinkedListItem);
          { inserts an Item before Loc }
          procedure InsertBefore(Item,Loc : TLinkedListItem);
          { inserts an Item after Loc }
          procedure InsertAfter(Item,Loc : TLinkedListItem);virtual;
          { concats an Item }
          procedure Concat(Item:TLinkedListItem);
          { deletes an Item }
          procedure Remove(Item:TLinkedListItem);
          { Gets First Item }
          function  GetFirst:TLinkedListItem;
          { Gets last Item }
          function  GetLast:TLinkedListItem;
          { inserts another List at the begin and make this List empty }
          procedure insertList(p : TLinkedList); virtual;
          { inserts another List before the provided item and make this List empty }
          procedure insertListBefore(Item:TLinkedListItem;p : TLinkedList); virtual;
          { inserts another List after the provided item and make this List empty }
          procedure insertListAfter(Item:TLinkedListItem;p : TLinkedList); virtual;
          { concats another List at the end and make this List empty }
          procedure concatList(p : TLinkedList); virtual;
          { concats another List at the start and makes a copy
            the list is ordered in reverse.
          }
          procedure insertListcopy(p : TLinkedList); virtual;
          { concats another List at the end and makes a copy }
          procedure concatListcopy(p : TLinkedList); virtual;
          { removes all items from the list, the items are not freed }
          procedure RemoveAll; virtual;
          property First:TLinkedListItem read FFirst;
          property Last:TLinkedListItem read FLast;
          property Count:Integer read FCount;
          property NoClear:boolean write FNoClear;
       end;

{********************************************
                TCmdStrList
********************************************}

       { string containerItem }
       TCmdStrListItem = class(TLinkedListItem)
          FPStr : TCmdStr;
       public
          constructor Create(const s:TCmdStr);
          destructor  Destroy;override;
          function GetCopy:TLinkedListItem;override;
          property Str: TCmdStr read FPStr;
       end;

       { string container }
       TCmdStrList = class(TLinkedList)
       private
          FDoubles : boolean;  { if this is set to true, doubles (case insensitive!) are allowed }
       public
          constructor Create;
          constructor Create_No_Double;
          { inserts an Item }
          procedure Insert(const s:TCmdStr);
          { concats an Item }
          procedure Concat(const s:TCmdStr);
          { deletes an Item }
          procedure Remove(const s:TCmdStr);
          { Gets First Item }
          function  GetFirst:TCmdStr;
          { Gets last Item }
          function  GetLast:TCmdStr;
          { true if string is in the container, compare case sensitive }
          function FindCase(const s:TCmdStr):TCmdStrListItem;
          { true if string is in the container }
          function Find(const s:TCmdStr):TCmdStrListItem;
          { inserts an item }
          procedure InsertItem(item:TCmdStrListItem);
          { concats an item }
          procedure ConcatItem(item:TCmdStrListItem);
          property Doubles:boolean read FDoubles write FDoubles;
       end;


{********************************************
              DynamicArray
********************************************}

     type
       { can't use sizeof(integer) because it crashes gdb }
       tdynamicblockdata=array[0..1024*1024-1] of byte;

       pdynamicblock = ^tdynamicblock;
       tdynamicblock = record
         pos,
         size,
         used : longword;
         Next : pdynamicblock;
         data : tdynamicblockdata;
       end;

     const
       dynamicblockbasesize = sizeof(tdynamicblock)-sizeof(tdynamicblockdata);
       mindynamicblocksize = 8*sizeof(pointer);

     type
       tdynamicarray = class
       private
         FPosn       : longword;
         FPosnblock  : pdynamicblock;
         FCurrBlocksize,
         FMaxBlocksize  : longword;
         FFirstblock,
         FLastblock  : pdynamicblock;
         procedure grow;
       public
         constructor Create(Ablocksize:longword);
         destructor  Destroy;override;
         procedure reset;
         function  size:longword; {$ifdef CCLASSESINLINE}inline;{$endif}
         procedure align(i:longword);
         procedure seek(i:longword);
         function  read(var d;len:longword):longword;
         procedure write(const d;len:longword);
         procedure writestr(const s:string); {$ifdef CCLASSESINLINE}inline;{$endif}
         procedure readstream(f:TCStream;maxlen:longword);
         procedure writestream(f:TCStream);
         function  equal(other:tdynamicarray):boolean;
         property  CurrBlockSize : longword read FCurrBlocksize;
         property  FirstBlock : PDynamicBlock read FFirstBlock;
         property  Pos : longword read FPosn;
       end;


{******************************************************************
   THashSet (keys not limited to ShortString, no indexed access)
*******************************************************************}

       PPHashSetItem = ^PHashSetItem;
       PHashSetItem = ^THashSetItem;
       THashSetItem = record
         Next: PHashSetItem;
         Key: Pointer; { With FOwnKeys, item and its key are allocated at once, and Key points inside. }
         KeyLength: Integer;
         HashValue: LongWord;
         Data: TObject;
       end;

       THashSet = class(TObject)
       private
         FCount: LongWord;
         FOwnsObjects: Boolean;
         FOwnsKeys: Boolean;
         function Lookup(Key: Pointer; KeyLen: Integer; var Found: Boolean;
           CanCreate: Boolean): PHashSetItem;
         procedure Resize(NewCapacity: LongWord);
       protected
         FBucket: PPHashSetItem;
         FBucketCount: LongWord;
         class procedure FreeItem(item:PHashSetItem); virtual;
         class function SizeOfItem: Integer; virtual;
         function CreateItem(Key: Pointer; KeyLen: Integer; HashValue: LongWord): PHashSetItem;
       public
         constructor Create(InitSize: Integer; OwnKeys, OwnObjects: Boolean);
         destructor Destroy; override;
         procedure Clear;
         { finds an entry by key }
         function Find(Key: Pointer; KeyLen: Integer): PHashSetItem;virtual;
         { finds an entry, creates one if not exists }
         function FindOrAdd(Key: Pointer; KeyLen: Integer;
           var Found: Boolean): PHashSetItem;virtual;
         { finds an entry, creates one if not exists }
         function FindOrAdd(Key: Pointer; KeyLen: Integer): PHashSetItem;virtual;
         { returns Data by given Key }
         function Get(Key: Pointer; KeyLen: Integer): TObject;virtual;
         { removes an entry, returns False if entry wasn't there }
         function Remove(Entry: PHashSetItem): Boolean;
         property Count: LongWord read FCount;
       end;

{******************************************************************
                             TTagHasSet
*******************************************************************}
       PPTagHashSetItem = ^PTagHashSetItem;
       PTagHashSetItem = ^TTagHashSetItem;
       TTagHashSetItem = record
         Item: THashSetItem;
         Tag: LongWord;
       end;

       TTagHashSet = class(THashSet)
       private
         function Lookup(Key: Pointer; KeyLen: Integer; Tag: LongWord; var Found: Boolean;
           CanCreate: Boolean): PTagHashSetItem;
       protected
         class function SizeOfItem: Integer; override;
       public
         { finds an entry by key }
         function Find(Key: Pointer; KeyLen: Integer; Tag: LongWord): PTagHashSetItem; reintroduce;
         { finds an entry, creates one if not exists }
         function FindOrAdd(Key: Pointer; KeyLen: Integer; Tag: LongWord;
           var Found: Boolean): PTagHashSetItem; reintroduce;
         { finds an entry, creates one if not exists }
         function FindOrAdd(Key: Pointer; KeyLen: Integer; Tag: LongWord): PTagHashSetItem; reintroduce;
         { returns Data by given Key }
         function Get(Key: Pointer; KeyLen: Integer; Tag: LongWord): TObject; reintroduce;
       end;


{******************************************************************
                             tbitset
*******************************************************************}

       tbitset = class
       private
         fdata: pbyte;
         fdatasize: longint;
       public
         constructor create(initsize: longint);
         constructor create_bytesize(bytesize: longint);
         destructor destroy; override;
         procedure clear; {$ifdef CCLASSESINLINE}inline;{$endif}
         procedure grow(nsize: longint);
         { sets a bit }
         procedure include(index: longint);
         { clears a bit }
         procedure exclude(index: longint);
         { finds an entry, creates one if not exists }
         function isset(index: longint): boolean;

         procedure addset(aset: tbitset);
         procedure subset(aset: tbitset);

         property data: pbyte read fdata;
         property datasize: longint read fdatasize;
      end;


    function FPHash(P: PChar; Len: Integer; Tag: LongWord): LongWord;
    function FPHash(P: PChar; Len: Integer): LongWord; inline;
    function FPHash(const s:shortstring):LongWord; inline;
    function FPHash(const a:ansistring):LongWord; inline;

    function ExtractStrings(Separators, WhiteSpace: TSysCharSet; Content: PChar; var Strings: TDynStringArray; AddEmptyStrings : Boolean = False): Integer;

implementation


{*****************************************************************************
                                    Memory debug
*****************************************************************************}
    function ExtractStrings(Separators, WhiteSpace: TSysCharSet; Content: PChar; var Strings: TDynStringArray; AddEmptyStrings : Boolean = False): Integer;
    var
      b, c : pchar;

      procedure SkipWhitespace;
        begin
          while (c^ in Whitespace) do
            inc (c);
        end;

      procedure AddString;
        var
          l : integer;
          s : string;
        begin
          l := c-b;
          s:='';
          if (l > 0) or AddEmptyStrings then
            begin
              setlength(s, l);
              if l>0 then
                move (b^, s[1],l*SizeOf(char));
              l:=length(Strings);
              setlength(Strings,l+1);
              Strings[l]:=S;
              inc (result);
            end;
        end;

    var
      quoted : char;
    begin
      result := 0;
      c := Content;
      Quoted := #0;
      Separators := Separators + [#13, #10] - ['''','"'];
      SkipWhitespace;
      b := c;
      while (c^ <> #0) do
        begin
          if (c^ = Quoted) then
            begin
              if ((c+1)^ = Quoted) then
                inc (c)
              else
                Quoted := #0
            end
          else if (Quoted = #0) and (c^ in ['''','"']) then
            Quoted := c^;
          if (Quoted = #0) and (c^ in Separators) then
            begin
              AddString;
              inc (c);
              SkipWhitespace;
              b := c;
            end
          else
            inc (c);
        end;
      if (c <> b) then
        AddString;
    end;

    constructor tmemdebug.create(const s:string);
      begin
        infostr:=s;
        totalmem:=0;
        Start;
      end;


    procedure tmemdebug.start;

      var
        status : TFPCHeapStatus;

      begin
        status:=GetFPCHeapStatus;
        startmem:=status.CurrHeapUsed;
      end;


    procedure tmemdebug.stop;
      var
        status : TFPCHeapStatus;
      begin
        if startmem<>0 then
         begin
           status:=GetFPCHeapStatus;
           inc(TotalMem,startmem-status.CurrHeapUsed);
           startmem:=0;
         end;
      end;


    destructor tmemdebug.destroy;
      begin
        Stop;
        show;
      end;


    procedure tmemdebug.show;
      begin
        write('memory [',infostr,'] ');
        if TotalMem>0 then
         writeln(DStr(TotalMem shr 10),' Kb released')
        else
         writeln(DStr((-TotalMem) shr 10),' Kb allocated');
      end;


{*****************************************************************************
               TFPObjectList (Copied from rtl/objpas/classes/lists.inc)
*****************************************************************************}

procedure TFPList.RaiseIndexError(Index : Integer);
begin
  Error(SListIndexError, Index);
end;

function TFPList.Get(Index: Integer): Pointer;
begin
  If (Index < 0) or (Index >= FCount) then
    RaiseIndexError(Index);
  Result:=FList[Index];
end;

procedure TFPList.Put(Index: Integer; Item: Pointer);
begin
  if (Index < 0) or (Index >= FCount) then
    RaiseIndexError(Index);
  Flist[Index] := Item;
end;

function TFPList.Extract(item: Pointer): Pointer;
var
  i : Integer;
begin
  result := nil;
  i := IndexOf(item);
  if i >= 0 then
   begin
     Result := item;
     FList[i] := nil;
     Delete(i);
   end;
end;

procedure TFPList.SetCapacity(NewCapacity: Integer);
begin
  If (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
     Error (SListCapacityError, NewCapacity);
  if NewCapacity = FCapacity then
    exit;
  ReallocMem(FList, SizeOf(Pointer)*NewCapacity);
  FCapacity := NewCapacity;
end;

procedure TFPList.SetCount(NewCount: Integer);
begin
  if (NewCount < 0) or (NewCount > MaxListSize)then
    Error(SListCountError, NewCount);
  If NewCount > FCount then
    begin
    If NewCount > FCapacity then
      SetCapacity(NewCount);
    If FCount < NewCount then
      FillChar(Flist[FCount], (NewCount-FCount) *  sizeof(Pointer), 0);
    end;
  FCount := Newcount;
end;

destructor TFPList.Destroy;
begin
  Self.Clear;
  inherited Destroy;
end;

function TFPList.Add(Item: Pointer): Integer;
begin
  if FCount = FCapacity then
    Self.Expand;
  FList[FCount] := Item;
  Result := FCount;
  inc(FCount);
end;

procedure TFPList.Clear;
begin
  if Assigned(FList) then
  begin
    SetCount(0);
    SetCapacity(0);
    FList := nil;
  end;
end;

procedure TFPList.Delete(Index: Integer);
begin
  If (Index<0) or (Index>=FCount) then
    Error (SListIndexError, Index);
  dec(FCount);
  System.Move (FList[Index+1], FList[Index], (FCount - Index) * SizeOf(Pointer));
  { Shrink the list if appropriate }
  if (FCapacity > 256) and (FCount < FCapacity shr 2) then
  begin
    FCapacity := FCapacity shr 1;
    ReallocMem(FList, SizeOf(Pointer) * FCapacity);
  end;
end;

class procedure TFPList.Error(const Msg: string; Data: PtrInt);
begin
  Raise EListError.CreateFmt(Msg,[Data]) at get_caller_addr(get_frame), get_caller_frame(get_frame);
end;

procedure TFPList.Exchange(Index1, Index2: Integer);
var
  Temp : Pointer;
begin
  If ((Index1 >= FCount) or (Index1 < 0)) then
    Error(SListIndexError, Index1);
  If ((Index2 >= FCount) or (Index2 < 0)) then
    Error(SListIndexError, Index2);
  Temp := FList[Index1];
  FList[Index1] := FList[Index2];
  FList[Index2] := Temp;
end;

function TFPList.Expand: TFPList;
var
  IncSize : Longint;
begin
  Result := Self;
  if FCount < FCapacity then
    exit;
  IncSize := sizeof(ptrint)*2;
  if FCapacity > 127 then
    Inc(IncSize, FCapacity shr 2)
  else if FCapacity > sizeof(ptrint)*4 then
    Inc(IncSize, FCapacity shr 1)
  else if FCapacity >= sizeof(ptrint) then
    inc(IncSize,sizeof(ptrint));
  SetCapacity(FCapacity + IncSize);
end;

function TFPList.First: Pointer;
begin
  If FCount<>0 then
    Result := Items[0]
  else
    Result := Nil;
end;

function TFPList.IndexOf(Item: Pointer): Integer;
begin
  Result:=
{$if sizeof(pointer)=sizeof(dword)}
    IndexDWord
{$elseif sizeof(pointer)=sizeof(qword)}
    IndexQWord
{$else}
  {$error unknown pointer size}
{$endif}
      (FList^, FCount, PtrUint(Item));
end;

function TFPList.IndexOfItem(Item: Pointer; Direction: TDirection): Integer;
var
  psrc  : PPointer;
  Index : Integer;
begin
  if Direction=FromBeginning then
    Result:=IndexOf(Item)
  else
    begin
      Result:=-1;
      if FCount>0 then
        begin
          psrc:=@FList[FCount-1];
          For Index:=FCount-1 downto 0 Do
            begin
              if psrc^=Item then
                begin
                  Result:=Index;
                  exit;
                end;
              dec(psrc);
            end;
        end;
    end;
end;

procedure TFPList.Insert(Index: Integer; Item: Pointer);
begin
  if (Index < 0) or (Index > FCount )then
    Error(SlistIndexError, Index);
  iF FCount = FCapacity then Self.Expand;
  if Index<FCount then
    System.Move(Flist[Index], Flist[Index+1], (FCount - Index) * SizeOf(Pointer));
  FList[Index] := Item;
  FCount := FCount + 1;
end;

function TFPList.Last: Pointer;
begin
  If FCount<>0 then
    Result := Items[FCount - 1]
  else
    Result := nil
end;

procedure TFPList.Move(CurIndex, NewIndex: Integer);
var
  Temp : Pointer;
begin
  if ((CurIndex < 0) or (CurIndex > Count - 1)) then
    Error(SListIndexError, CurIndex);
  if (NewINdex < 0) then
    Error(SlistIndexError, NewIndex);
  Temp := FList[CurIndex];
  FList[CurIndex] := nil;
  Self.Delete(CurIndex);
  Self.Insert(NewIndex, nil);
  FList[NewIndex] := Temp;
end;

function TFPList.Remove(Item: Pointer): Integer;
begin
  Result := IndexOf(Item);
  If Result <> -1 then
    Self.Delete(Result);
end;

procedure TFPList.Pack;
var
  NewCount,
  i : integer;
  pdest,
  psrc : PPointer;
begin
  NewCount:=0;
  psrc:=@FList[0];
  pdest:=psrc;
  For I:=0 To FCount-1 Do
    begin
      if assigned(psrc^) then
        begin
          pdest^:=psrc^;
          inc(pdest);
          inc(NewCount);
        end;
      inc(psrc);
    end;
  FCount:=NewCount;
end;


Procedure QuickSort(FList: PPointer; L, R : Longint;Compare: TListSortCompare);
var
  I, J, P: Longint;
  PItem, Q : Pointer;
begin
 repeat
   I := L;
   J := R;
   P := (L + R) div 2;
   repeat
     PItem := FList[P];
     while Compare(PItem, FList[i]) > 0 do
       I := I + 1;
     while Compare(PItem, FList[J]) < 0 do
       J := J - 1;
     If I <= J then
     begin
       Q := FList[I];
       Flist[I] := FList[J];
       FList[J] := Q;
       if P = I then
        P := J
       else if P = J then
        P := I;
       I := I + 1;
       J := J - 1;
     end;
   until I > J;
   if L < J then
     QuickSort(FList, L, J, Compare);
   L := I;
 until I >= R;
end;

procedure TFPList.Sort(Compare: TListSortCompare);
begin
  if Not Assigned(FList) or (FCount < 2) then exit;
  QuickSort(Flist, 0, FCount-1, Compare);
end;

procedure TFPList.Assign(Obj: TFPList);
var
  i: Integer;
begin
  Clear;
  for I := 0 to Obj.Count - 1 do
    Add(Obj[i]);
end;


procedure TFPList.ForEachCall(proc2call:TListCallback;arg:pointer);
var
  i : integer;
  p : pointer;
begin
  For I:=0 To Count-1 Do
    begin
      p:=FList[i];
      if assigned(p) then
        proc2call(p,arg);
    end;
end;


procedure TFPList.ForEachCall(proc2call:TListStaticCallback;arg:pointer);
var
  i : integer;
  p : pointer;
begin
  For I:=0 To Count-1 Do
    begin
      p:=FList[i];
      if assigned(p) then
        proc2call(p,arg);
    end;
end;


{*****************************************************************************
            TFPObjectList (Copied from rtl/objpas/classes/lists.inc)
*****************************************************************************}

constructor TFPObjectList.Create(FreeObjects : boolean);
begin
  Create;
  FFreeObjects := Freeobjects;
end;

destructor TFPObjectList.Destroy;
begin
  if (FList <> nil) then
  begin
    Clear;
    FList.Destroy;
    FList:=nil;
  end;
  inherited Destroy;
end;

procedure TFPObjectList.Clear;
var
  i: integer;
begin
  if FFreeObjects then
    for i := 0 to FList.Count - 1 do
      TObject(FList[i]).Free;
  FList.Clear;
end;

constructor TFPObjectList.Create;
begin
  inherited Create;
  FList := TFPList.Create;
  FFreeObjects := True;
end;

function TFPObjectList.IndexOf(AObject: TObject): Integer;
begin
  Result := FList.IndexOf(Pointer(AObject));
end;

function TFPObjectList.IndexOfItem(AObject: TObject; Direction: TDirection): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
begin
  Result := FList.IndexOfItem(Pointer(AObject),Direction);
end;

function TFPObjectList.GetCount: integer;
begin
  Result := FList.Count;
end;

procedure TFPObjectList.SetCount(const AValue: integer);
begin
  if FList.Count <> AValue then
    FList.Count := AValue;
end;

function TFPObjectList.GetItem(Index: Integer): TObject;
begin
  Result := TObject(FList[Index]);
end;

procedure TFPObjectList.SetItem(Index: Integer; AObject: TObject);
begin
  if OwnsObjects then
    TObject(FList[Index]).Free;
  FList[index] := AObject;
end;

procedure TFPObjectList.SetCapacity(NewCapacity: Integer);
begin
  FList.Capacity := NewCapacity;
end;

function TFPObjectList.GetCapacity: integer;
begin
  Result := FList.Capacity;
end;

function TFPObjectList.Add(AObject: TObject): Integer;
begin
  Result := FList.Add(AObject);
end;

procedure TFPObjectList.Delete(Index: Integer);
begin
  if OwnsObjects then
    TObject(FList[Index]).Free;
  FList.Delete(Index);
end;

procedure TFPObjectList.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

function TFPObjectList.Expand: TFPObjectList;
begin
  FList.Expand;
  Result := Self;
end;

function TFPObjectList.Extract(Item: TObject): TObject;
begin
  Result := TObject(FList.Extract(Item));
end;

function TFPObjectList.Remove(AObject: TObject): Integer;
begin
  Result := IndexOf(AObject);
  if (Result <> -1) then
  begin
    if OwnsObjects then
      TObject(FList[Result]).Free;
    FList.Delete(Result);
  end;
end;

function TFPObjectList.FindInstanceOf(AClass: TClass; AExact: Boolean; AStartAt : Integer): Integer;
var
  I : Integer;
begin
  I:=AStartAt;
  Result:=-1;
  If AExact then
    while (I<Count) and (Result=-1) do
      If Items[i].ClassType=AClass then
        Result:=I
      else
        Inc(I)
  else
    while (I<Count) and (Result=-1) do
      If Items[i].InheritsFrom(AClass) then
        Result:=I
      else
        Inc(I);
end;

procedure TFPObjectList.Insert(Index: Integer; AObject: TObject);
begin
  FList.Insert(Index, Pointer(AObject));
end;

procedure TFPObjectList.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
end;

procedure TFPObjectList.Assign(Obj: TFPObjectList);
begin
  Clear;
  ConcatListCopy(Obj);
end;

procedure TFPObjectList.ConcatListCopy(Obj: TFPObjectList);
var
  i: Integer;
begin
  for I := 0 to Obj.Count - 1 do
    Add(Obj[i]);
end;

procedure TFPObjectList.Pack;
begin
  FList.Pack;
end;

procedure TFPObjectList.Sort(Compare: TListSortCompare);
begin
  FList.Sort(Compare);
end;

function TFPObjectList.First: TObject;
begin
  Result := TObject(FList.First);
end;

function TFPObjectList.Last: TObject;
begin
  Result := TObject(FList.Last);
end;

procedure TFPObjectList.ForEachCall(proc2call:TObjectListCallback;arg:pointer);
begin
  FList.ForEachCall(TListCallBack(proc2call),arg);
end;

procedure TFPObjectList.ForEachCall(proc2call:TObjectListStaticCallback;arg:pointer);
begin
  FList.ForEachCall(TListStaticCallBack(proc2call),arg);
end;


{*****************************************************************************
                            TFPHashList
*****************************************************************************}

// MurmurHash3_32
function FPHash(P: PChar; Len: Integer; Tag: LongWord): LongWord;
const
  C1 = uint32($cc9e2d51);
  C2 = uint32($1b873593);
var
  h, tail: uint32;
  e4: pChar;
  len4, nTail: SizeUint;
begin
{$push}
{$q-,r-}
  h := tag;

  len4 := len and not integer(sizeof(uint32) - 1); { len div sizeof(uint32) * sizeof(uint32) }
  e4 := p + len4;
  nTail := len - len4;
  while p < e4 do
    begin
      { If independence on endianness is desired, unaligned(pUint32(p)^) can be replaced with LEtoN(unaligned(pUint32(p)^)). }
      h := RolDWord(h xor (RolDWord(unaligned(pUint32(p)^) * C1, 15) * C2), 13) * 5 + $e6546b64;
      p := p + sizeof(uint32);
    end;

  if nTail > 0 then
    begin
      { tail is 1 to 3 bytes }
      case nTail of
        3: tail := unaligned(pUint16(p)^) or uint32(p[2]) shl 16; { unaligned(pUint16(p^)) can be LEtoNed for portability }
        2: tail := unaligned(pUint16(p)^); { unaligned(pUint16(p^)) can be LEtoNed for portability }
        {1:} else tail := uint32(p^);
      end;
      h := h xor (RolDWord(tail * C1, 15) * C2);
    end;

  h := h xor uint32(len);
  h := (h xor (h shr 16)) * $85ebca6b;
  h := (h xor (h shr 13)) * $c2b2ae35;
  result := h xor (h shr 16);
{$pop}
end;

function FPHash(P: PChar; Len: Integer): LongWord; inline;
begin
  result:=fphash(P,Len, 0);
end;


function FPHash(const s: shortstring): LongWord; inline;
begin
  result:=fphash(pchar(@s[1]),length(s));
end;


function FPHash(const a: ansistring): LongWord; inline;
begin
  result:=fphash(pchar(a),length(a));
end;


procedure TMemoryRegion.Init(preallocate: SizeUint=0);
begin
  FTop:=nil;
  if preallocate>MinMemoryRegionNodeSize then
    FTop:=AllocateNode(0, preallocate);
end;


procedure TMemoryRegion.Done;
begin
  Clear;
end;


function TMemoryRegion.Push(n: SizeUint): pointer;
var
  top: PMemoryRegionNode;
  start: SizeUint;
begin
  top:=FTop;
  if Assigned(top) then
    begin
      start:=top^.n;
      if n<=SizeUint(top^.alloc-start) then
        begin
          top^.n:=start+n;
          exit(PByte(top^.data)+start);
        end;
     end;
  result:=PushNewNode(n);
end;


procedure TMemoryRegion.Clear;
var
  cur, next: PMemoryRegionNode;
begin
  cur:=FTop;
  FTop:=nil;
  while Assigned(cur) do
    begin
      next:=cur^.next;
      FreeMem(cur);
      cur:=next;
    end;
end;


function TMemoryRegion.CalcSumSize: SizeUint;
var
  n: PMemoryRegionNode;
begin
  result:=0;
  n:=FTop;
  while Assigned(n) do
    begin
      result:=result+n^.n;
      n:=n^.next;
    end;
end;


class function TMemoryRegion.AllocateNode(n, alloc: SizeUint): PMemoryRegionNode;
begin
  result:=GetMem(sizeof(TMemoryRegionNode)-sizeof(TMemoryRegionNode.data)+sizeof(TMemoryRegionNode.data[0])*alloc);
  result^.n:=n;
  result^.alloc:=alloc;
  result^.next:=nil;
end;


function TMemoryRegion.PushNewNode(n: SizeUint): pointer;
var
  alloc, sumSize: SizeUint;
  newNode: PMemoryRegionNode;
begin
  { The absolute minimum to allocate is the required contiguous n. }
  sumSize:=CalcSumSize;
  alloc:=MinMemoryRegionNodeSize+n+sumSize div 4+sumSize div 8; { const+n+37,5%. }

  newNode:=AllocateNode(n, alloc);
  newNode^.next:=FTop;
  FTop:=newNode;
  result:=PByte(newNode^.data);
end;


function vi_u8_get(p: pointer; index: SizeUint): SizeUint;
begin
  result:=PUint8(p)[index];
end;


procedure vi_u8_set(p: pointer; index: SizeUint; const value: SizeUint);
begin
  PUint8(p)[index]:=value;
end;


function vi_u16_get(p: pointer; index: SizeUint): SizeUint;
begin
  result:=PUint16(p)[index];
end;


procedure vi_u16_set(p: pointer; index: SizeUint; const value: SizeUint);
begin
  PUint16(p)[index]:=value;
end;


function vi_u24_get(p: pointer; index: SizeUint): SizeUint;
begin
  result:=unaligned(PUint32(p+3*index)^)
{$if defined(endian_little)}
    and $FFFFFF
{$elseif defined(endian_big)}
    shr 8
{$else} {$error unknown endianness} {$endif};
end;


procedure vi_u24_set(p: pointer; index: SizeUint; const value: SizeUint);
begin
  p:=p+3*index;
  uint24(p^).lo16:=uint16(value);
  uint24(p^).hi8:=value shr 16;
end;


function vi_u32_get(p: pointer; index: SizeUint): SizeUint;
begin
  result:=PUint32(p)[index];
end;


procedure vi_u32_set(p: pointer; index: SizeUint; const value: SizeUint);
begin
  PUint32(p)[index]:=value;
end;


function ChooseViType(fitValue: SizeUint): PViTypeDesc;
var
  typeEnum: TViTypeEnum;
begin
  for typeEnum in TViTypeEnum do
    begin
      result:=@ViTypes[typeEnum];
      if fitValue<=result^.lim then
        exit;
    end;
  internalerrorproc(2021122601);
end;


function ViTypeFromGetter(get: TViGetter): PViTypeDesc;
var
  typeEnum: TViTypeEnum;
begin
  for typeEnum in TViTypeEnum do
    begin
      result:=@ViTypes[typeEnum];
      if result^.get=get then
        exit;
    end;
  internalerrorproc(2021122607);
end;


function ViDataSize(ty: PViTypeDesc; n: SizeUint): SizeUint;
begin
  result:=ty^.size*n+ty^.overallocate;
end;


function TViHashList.Get(Index: SizeInt): Pointer;
begin
  If SizeUint(Index)>=SizeUint(FCount) then
    RaiseIndexError(Index);
  Result:=FItems[Index].Data;
end;


procedure TViHashList.Put(Index: SizeInt; Item: Pointer);
begin
  If SizeUint(Index)>=SizeUint(FCount) then
    RaiseIndexError(Index);
  FItems[Index].Data:=Item;
end;


class procedure TViHashList.RaiseIndexError(Index: SizeInt);
begin
  TFPList.Error(SListIndexError, Index);
end;


procedure TViHashList.SetupEmptyTable;
begin
  { PChar('') is a pointer to #0 and is reinterpreted as a pointer to 1-element uint8 array containing one zero, which is ViEmpty.
    Any searches will answer "not found", and any additions will instantly rehash. }
  FHash:=PUint8(PChar(''));
  FItems:=nil;
  FHashMask:=0;
  FCapacity:=0;
  FGetIndex:=@vi_u8_get;
  FSetIndex:=@vi_u8_set;
end;


procedure TViHashList.Rehash(ForItems: SizeUint; mode: TViRehashMode=vi_Auto);
var
  newCapacity, fitCapacity, newHashMask, itemsOffset, regionSize: SizeUint;
  i: SizeInt;
  newIndexType: PViTypeDesc;
  newHash: pointer;
  newItems: PViHashListItem;
  shortcutReAdd: boolean;
  newSetIndex: TViSetter;
begin
  if ForItems=0 then
    begin
      Clear;
      exit;
    end;
  if ForItems>MaxHashListSize then
    TFPList.Error(SListCapacityError, ForItems);

  newCapacity:=ForItems;
  fitCapacity:=ForItems;
  if mode<>vi_Tight then
    begin
      { Reserve some space. }
      newCapacity:=8+newCapacity+newCapacity div 4+newCapacity div 8; { 137.5% }
      { Reserving 260 items when 240 is enough will switch to 16-bit indices without good enough reason, so allow some recoil.
        Subtracting 1/8 here means that the base reserve of 137% is allowed to reduce this way to 137%*7/8≈120%. }
      fitCapacity:=newCapacity-newCapacity div 8;
    end;

  { Max index for "capacity" items is "ViRealIndexOffset + (capacity - 1)", which can be rewritten as "capacity + (ViRealIndexOffset - 1)". }
  newIndexType:=ChooseViType(fitCapacity+(ViRealIndexOffset-1));

  { Index type is usually chosen against deliberately lowered fitCapacity instead of newCapacity.
    If it does not fit newCapacity, re-deduce newCapacity from its limit, realizing the recoil mentioned above.
    Neither allocating 240 indices is a good decision because 1-byte index limit being 255 is very close to it.
    Adding 1/8 here means that the base reserve of 137% is allowed to increase this way to 137*9/8≈154%. }
  fitCapacity:=newIndexType^.lim-(ViRealIndexOffset-1);
  if newCapacity+newCapacity div 8>fitCapacity then
    newCapacity:=fitCapacity;

  { Take item list capacity rounded up to power of two. This can give 50% to 100% load factor (Capacity/(1+HashMask)).
    If it gives more than 3/4, double the hash capacity again. After that, possible load factors will range from 37.5% to 75%.
    Even load factors greater than 100% will work though. Low factors are just slightly faster, at the expense of memory. }
  newHashMask:=SizeUint(1) shl (1+BsrDWord((newCapacity-1) or 1))-1; { UpToPow2(newCapacity)-1 }
  if newHashMask div 4*3<newCapacity then
    newHashMask:=newHashMask*2+1;

  { Allocating and marking up the region for FHash + FItems. }
  itemsOffset:=Align(ViDataSize(newIndexType,newHashMask+1), SizeUint(sizeof(pointer)));
  regionSize:=itemsOffset+sizeof(TViHashListItem)*newCapacity;
  newHash:=GetMem(regionSize);
  newItems:=pointer(newHash)+itemsOffset;

  { If hash mask hasn't changed (this is possible because of arbitrariness of FCapacity),
    items re-adding can be, and is, shortcutted.
    .Pack corrupts indices and expects from .Rehash to recalculate them, so is incompatible with this. }
  shortcutReAdd:=(FHashMask=newHashMask) and (mode<>vi_Pack);
  if shortcutReAdd then
    begin
      { If even index type hasn't changed, just copy FHash. Else convert. }
      if newIndexType=ViTypeFromGetter(FGetIndex) then
        Move(FHash^, newHash^, ViDataSize(newIndexType,newHashMask+1))
      else
        begin
          newSetIndex:=newIndexType^.&set;
          for i:=0 to newHashMask do
            newSetIndex(newHash, i, FGetIndex(FHash, i));
        end;
    end
  else
    { Otherwise set all indices to ViEmpty. }
    FillChar(newHash^, ViDataSize(newIndexType,newHashMask+1), 0);

  { Move items as raw memory, even managed (old area is then deallocated without finalizing). }
  Move(FItems^, newItems^, FCount*sizeof(TViHashListItem));

  { Free the old table. "Assigned(FItems)" means that the table was not the fake table set up by SetupEmptyTable.
    Items were just moved into a new place so shouldn't be finalized. }
  if Assigned(FItems) then
    FreeMem(FHash);

  FHash:=newHash;
  FItems:=newItems;
  FGetIndex:=newIndexType^.get;
  FSetIndex:=newIndexType^.&set;
  FHashMask:=newHashMask;
  FCapacity:=newCapacity;

  { Re-add items if re-adding was not shortcutted before. }
  if not shortcutReAdd then
    for i:=0 to FCount-1 do
      AddToHashTable(FItems+i, i);
end;


{$ifndef symansistr}
function TViHashList.AddStrToRegion(const s: TSymStr): PSymStr;
var
  size: SizeUint;
begin
  size:=1+length(s);
  result:=FShortstringRegion.Push(size);
  System.Move(s[0],result^,size);
end;
{$endif}


procedure TViHashList.Shrink;
begin
  if (FCapacity >= 64) and (uint32(FCount) < FCapacity div 4) then
    Rehash(FCount);
end;


procedure TViHashList.AddToHashTable(Item: PViHashListItem; Index: SizeUint);
var
  HashIndex: SizeUint;
begin
  if not Assigned(Item^.Data) then
    exit;
  HashIndex:=Item^.HashValue and FHashMask;
  FItems[Index].Next:=SizeInt(FGetIndex(FHash, HashIndex))-ViRealIndexOffset;
  FSetIndex(FHash, HashIndex, ViRealIndexOffset+Index);
end;


function TViHashList.InternalFind(AHash:LongWord;const AName:TSymStr;out PrevIndex:SizeInt):SizeInt;
var
  it: PViHashListItem;
begin
  Result:=SizeInt(FGetIndex(FHash, AHash and FHashMask))-ViRealIndexOffset;
  PrevIndex:=-1;
  repeat
    if Result<0 then
      exit;
    it:=FItems+Result;
    if Assigned(it^.Data) and (AHash=it^.HashValue) and (AName=it^.Str {$ifndef symansistr} ^ {$endif}) then
      exit;
    PrevIndex:=Result;
    Result:=FItems[Result].Next;
  until false;
end;


procedure TViHashList.RemoveFromHashTable(AHash:LongWord;Index, PrevIndex: SizeInt);
var
  next: SizeInt;
begin
  next:=SizeInt(FItems[Index].Next);
  if PrevIndex<0 then
    FSetIndex(FHash, AHash and FHashMask, ViRealIndexOffset+next)
  else
    FItems[PrevIndex].Next:=next;
end;


procedure TViHashList.SetCapacity(NewCapacity: uint32);
begin
  if NewCapacity < uint32(FCount) then internalerrorproc(2021122605);
  Rehash(NewCapacity, vi_Tight);
end;


constructor TViHashList.Create;
begin
  inherited Create;
{$ifndef symansistr}
  FShortstringRegion.Init;
{$endif}
  SetupEmptyTable;
end;


destructor TViHashList.Destroy;
begin
  Clear;
{$ifndef symansistr}
  FShortstringRegion.Done;
{$endif}
  inherited Destroy;
end;


function TViHashList.Add(const AName:TSymStr;Item: Pointer): SizeInt;
var
  it: PViHashListItem;
begin
  result:=FCount;
  if uint32(result)=FCapacity then
    Rehash(result+1);

  it:=FItems+result;
  Initialize(it^);
  it^.HashValue:=FPHash(AName);
  it^.Data:=Item;
{$ifdef symansistr}
  it^.Str:=AName;
{$else}
  it^.Str:=AddStrToRegion(AName);
{$endif}

  AddToHashTable(it, result);
  FCount:=result+1;
end;


procedure TViHashList.Clear;
begin
  if Assigned(FItems) then
    begin
      Finalize(FItems^, FCount);
      FreeMem(FHash);
      SetupEmptyTable;
      FCount:=0;
{$ifndef symansistr}
      FShortstringRegion.Clear;
{$endif}
    end;
end;


function TViHashList.NameOfIndex(Index: SizeInt): TSymStr;
begin
  if SizeUint(Index)>=SizeUint(FCount) then
    RaiseIndexError(Index);
  result:=FItems[Index].Str {$ifndef symansistr} ^ {$endif};
end;


function TViHashList.HashOfIndex(Index: SizeInt): LongWord;
begin
  if SizeUint(Index)>=SizeUint(FCount) then
    RaiseIndexError(Index);
  result:=FItems[Index].HashValue;
end;


function TViHashList.GetNextCollision(Index: SizeInt): SizeInt;
begin
  Result:=FItems[Index].Next;
end;


procedure TViHashList.Delete(Index: SizeInt);
var
  it: PViHashListItem;
  prev, i: SizeInt;
begin
  If SizeUint(Index)>=SizeUint(FCount) then
    RaiseIndexError(Index);

  { Remove from array, shifting items above. }
  Finalize(FItems[Index]);
  Move(FItems[Index+1], FItems[Index], (FCount-Index-1)*sizeof(TViHashListItem));
  dec(FCount);

  { Rebuild the table. This is much faster than trying to fix up indices. :( }
  FillChar(FHash^, ViDataSize(ViTypeFromGetter(FGetIndex),FHashMask+1), 0);
  for i:=0 to FCount-1 do
    AddToHashTable(FItems+i, i);
  Shrink;
end;


function TViHashList.Extract(item: Pointer): Pointer;
var
  i : SizeInt;
begin
  result:=nil;
  i:=IndexOf(item);
  if i>=0 then
   begin
     Result:=item;
     Delete(i);
   end;
end;


function TViHashList.IndexOf(Item: Pointer): SizeInt;
var
  itemp, iteme: PViHashListItem;
begin
  Result:=0;
  itemp:=FItems;
  iteme:=itemp+FCount;
  while itemp<iteme do
    begin
      if itemp^.Data=Item then
        exit;
      inc(itemp);
      inc(Result);
    end;
  Result:=-1;
end;


function TViHashList.Find(const AName:TSymStr): Pointer;
begin
  Result:=FindWithHash(AName, FPHash(ANAme));
end;


function TViHashList.FindIndexOf(const AName:TSymStr): SizeInt;
var
  PrevIndex : SizeInt;
begin
  Result:=InternalFind(FPHash(AName),AName,PrevIndex);
end;


function TViHashList.FindWithHash(const AName:TSymStr;AHash:LongWord): Pointer;
var
  Index,
  PrevIndex : SizeInt;
begin
  Result:=nil;
  Index:=InternalFind(AHash,AName,PrevIndex);
  if Index>=0 then
    Result:=FItems[Index].Data;
end;


function TViHashList.Rename(const AOldName,ANewName:TSymStr): SizeInt;
var
  PrevIndex : SizeInt;
  OldHash : LongWord;
  it: PViHashListItem;
begin
  OldHash:=FPHash(AOldName);
  result:=InternalFind(OldHash,AOldName,PrevIndex);
  if result<0 then
    exit;
  RemoveFromHashTable(OldHash, result, PrevIndex);
  it:=FItems+result;
  it^.HashValue:=FPHash(ANewName);
{$ifdef symansistr}
  it^.Str:=ANewName;
{$else}
  it^.Str:=AddStrToRegion(ANewName);
{$endif}
  AddToHashTable(it, result);
end;


function TViHashList.Remove(Item: Pointer): SizeInt;
begin
  Result:=IndexOf(Item);
  if Result>=0 then
    Delete(Result);
end;


procedure TViHashList.Pack;
var
  itemp, iteme, target: PViHashListItem;
  removed: SizeUint;
begin
  itemp:=FItems;
  iteme:=itemp+FCount;
  while itemp<iteme do
    if Assigned(itemp^.Data) then
      inc(itemp)
    else
      break;
  if itemp<iteme then
    begin
      target:=itemp;
      inc(itemp);
      while itemp<iteme do
        begin
          if Assigned(itemp^.data) then
            begin
              target^:=itemp^;
              inc(target);
            end;
          inc(itemp);
        end;
      removed:=SizeUint(pointer(iteme)-pointer(target)) div sizeof(TViHashListItem);
      Finalize(target^, removed);
      FCount:=FCount-removed;
    end;
  if uint32(FCount)<>FCapacity then
    Rehash(FCount, vi_Pack);
end;


procedure TViHashList.ShowStatistics;
var
  HashMean,
  HashStdDev : Double;
  Index,
  i,j : SizeInt;
begin
  { Calculate Mean and StdDev }
  HashMean:=0;
  HashStdDev:=0;
  for i:=0 to FHashMask do
    begin
      j:=0;
      Index:=SizeInt(FGetIndex(FHash, i))-ViRealIndexOffset;
      while Index>=0 do
        begin
          inc(j);
          Index:=FItems[Index].Next;
        end;
      HashMean:=HashMean+j;
      HashStdDev:=HashStdDev+Sqr(j);
    end;
  HashMean:=HashMean/(FHashMask+1);
  HashStdDev:=(HashStdDev-(FHashMask+1)*Sqr(HashMean));
  If FHashMask>0 then
    HashStdDev:=Sqrt(HashStdDev/FHashMask)
  else
    HashStdDev:=0;
  { Print info to stdout }
  Writeln('HashSize   : ',FHashMask+1);
  Writeln('HashMean   : ',HashMean:1:4);
  Writeln('HashStdDev : ',HashStdDev:1:4);
  Writeln('ListSize   : ',FCount,'/',FCapacity);
{$ifndef symansistr}
  Writeln('StringSize : ',FShortstringRegion.CalcSumSize);
{$endif}
end;


procedure TViHashList.ForEachCall(proc2call:TListCallback;arg:pointer);
var
  itemp, iteme: PViHashListItem;
  p: pointer;
begin
  itemp:=FItems;
  iteme:=itemp+FCount;
  while itemp<iteme do
    begin
      p:=itemp^.Data;
      if assigned(p) then
        proc2call(p,arg);
      itemp:=itemp+1;
    end;
end;


procedure TViHashList.ForEachCall(proc2call:TListStaticCallback;arg:pointer);
var
  itemp, iteme: PViHashListItem;
  p: pointer;
begin
  itemp:=FItems;
  iteme:=itemp+FCount;
  while itemp<iteme do
    begin
      p:=itemp^.Data;
      if assigned(p) then
        proc2call(p,arg);
      itemp:=itemp+1;
    end;
end;


{*****************************************************************************
            TFPHashObjectList (Copied from rtl/objpas/classes/lists.inc)
*****************************************************************************}

constructor TFPHashObjectList.Create(FreeObjects : boolean = True);
begin
  inherited Create;
  FHashList := TFPHashList.Create;
  FFreeObjects := Freeobjects;
end;

destructor TFPHashObjectList.Destroy;
begin
  if (FHashList <> nil) then
    begin
      Clear;
      FHashList.Destroy;
      FHashList:=nil;
    end;
  inherited Destroy;
end;

procedure TFPHashObjectList.Clear;
var
  i: integer;
begin
  if FFreeObjects then
    for i := 0 to FHashList.Count - 1 do
      TObject(FHashList[i]).Free;
  FHashList.Clear;
end;

function TFPHashObjectList.IndexOf(AObject: TObject): Integer;
begin
  Result := FHashList.IndexOf(Pointer(AObject));
end;

function TFPHashObjectList.GetCount: integer;
begin
  Result := FHashList.Count;
end;

function TFPHashObjectList.GetItem(Index: Integer): TObject;
begin
  Result := TObject(FHashList[Index]);
end;

procedure TFPHashObjectList.SetItem(Index: Integer; AObject: TObject);
begin
  if OwnsObjects then
    TObject(FHashList[Index]).Free;
  FHashList[index] := AObject;
end;

procedure TFPHashObjectList.SetCapacity(NewCapacity: Integer);
begin
  FHashList.Capacity := NewCapacity;
end;

function TFPHashObjectList.GetCapacity: integer;
begin
  Result := FHashList.Capacity;
end;

function TFPHashObjectList.Add(const AName:TSymStr;AObject: TObject): Integer;
begin
  Result := FHashList.Add(AName,AObject);
end;

function TFPHashObjectList.NameOfIndex(Index: Integer): TSymStr;
begin
  Result := FHashList.NameOfIndex(Index);
end;

function TFPHashObjectList.HashOfIndex(Index: Integer): LongWord;
begin
  Result := FHashList.HashOfIndex(Index);
end;

function TFPHashObjectList.GetNextCollision(Index: Integer): Integer;
begin
  Result := FHashList.GetNextCollision(Index);
end;

procedure TFPHashObjectList.Delete(Index: Integer);
begin
  if OwnsObjects then
    TObject(FHashList[Index]).Free;
  FHashList.Delete(Index);
end;

function TFPHashObjectList.Extract(Item: TObject): TObject;
begin
  Result := TObject(FHashList.Extract(Item));
end;

function TFPHashObjectList.Remove(AObject: TObject): Integer;
begin
  Result := IndexOf(AObject);
  if (Result <> -1) then
    begin
      if OwnsObjects then
        TObject(FHashList[Result]).Free;
      FHashList.Delete(Result);
    end;
end;

function TFPHashObjectList.Find(const s:TSymStr): TObject;
begin
  result:=TObject(FHashList.Find(s));
end;


function TFPHashObjectList.FindIndexOf(const s:TSymStr): Integer;
begin
  result:=FHashList.FindIndexOf(s);
end;


function TFPHashObjectList.FindWithHash(const AName:TSymStr;AHash:LongWord): Pointer;
begin
  Result:=TObject(FHashList.FindWithHash(AName,AHash));
end;


function TFPHashObjectList.Rename(const AOldName,ANewName:TSymStr): Integer;
begin
  Result:=FHashList.Rename(AOldName,ANewName);
end;


function TFPHashObjectList.FindInstanceOf(AClass: TClass; AExact: Boolean; AStartAt : Integer): Integer;
var
  I : Integer;
begin
  I:=AStartAt;
  Result:=-1;
  If AExact then
    while (I<Count) and (Result=-1) do
      If Items[i].ClassType=AClass then
        Result:=I
      else
        Inc(I)
  else
    while (I<Count) and (Result=-1) do
      If Items[i].InheritsFrom(AClass) then
        Result:=I
      else
        Inc(I);
end;


procedure TFPHashObjectList.Pack;
begin
  FHashList.Pack;
end;


procedure TFPHashObjectList.ShowStatistics;
begin
  FHashList.ShowStatistics;
end;


procedure TFPHashObjectList.ForEachCall(proc2call:TObjectListCallback;arg:pointer);
begin
  FHashList.ForEachCall(TListCallBack(proc2call),arg);
end;


procedure TFPHashObjectList.ForEachCall(proc2call:TObjectListStaticCallback;arg:pointer);
begin
  FHashList.ForEachCall(TListStaticCallBack(proc2call),arg);
end;


{*****************************************************************************
                               TFPHashObject
*****************************************************************************}

procedure TFPHashObject.InternalChangeOwner(HashObjectList:TFPHashObjectList;const s:TSymStr);
var
  Index : SizeInt;
  it : PViHashListItem;
begin
  FOwner:=HashObjectList;
  Index:=HashObjectList.Add(s,Self);
  it:=HashObjectList.List.List+Index;
{$ifdef symansistr}
  FStr:=s;
{$else}
  FStr:=it^.Str;
{$endif}
  FHash:=it^.HashValue;
end;


constructor TFPHashObject.CreateNotOwned;
{$ifndef symansistr}
const
  EmptyString: string[1] = '';
{$endif}
begin
{$ifdef symansistr}
  FStr:='';
{$else}
  FStr:=@EmptyString;
{$endif}
  int32(FHash):=-1;
end;


constructor TFPHashObject.Create(HashObjectList:TFPHashObjectList;const s:TSymStr);
begin
  InternalChangeOwner(HashObjectList,s);
end;


procedure TFPHashObject.ChangeOwner(HashObjectList:TFPHashObjectList);
begin
  InternalChangeOwner(HashObjectList, FStr {$ifndef symansistr} ^ {$endif});
end;


procedure TFPHashObject.ChangeOwnerAndName(HashObjectList:TFPHashObjectList;const s:TSymStr);
begin
  InternalChangeOwner(HashObjectList,s);
end;


procedure TFPHashObject.Rename(const ANewName:TSymStr);
var
  Index : integer;
  it : PViHashListItem;
begin
  Index:=FOwner.Rename(FStr {$ifndef symansistr} ^ {$endif},ANewName);
  if Index>=0 then
    begin
      it:=FOwner.List.List+Index;
{$ifdef symansistr}
      FStr:=ANewName;
{$else}
      FStr:=it^.Str;
{$endif}
      FHash:=it^.HashValue;
    end;
end;


function TFPHashObject.GetName:TSymStr;
begin
  Result:=FStr {$ifndef symansistr} ^ {$endif};
end;


function TFPHashObject.GetHash:Longword;
begin
  Result:=FHash;
end;


{****************************************************************************
                             TLinkedListItem
 ****************************************************************************}

    constructor TLinkedListItem.Create;
      begin
        Previous:=nil;
        Next:=nil;
      end;


    destructor TLinkedListItem.Destroy;
      begin
      end;


    function TLinkedListItem.GetCopy:TLinkedListItem;
      var
        p : TLinkedListItem;
        l : integer;
      begin
        p:=TLinkedListItemClass(ClassType).Create;
        l:=InstanceSize;
        Move(pointer(self)^,pointer(p)^,l);
        Result:=p;
      end;


{****************************************************************************
                                   TLinkedList
 ****************************************************************************}

    constructor TLinkedList.Create;
      begin
        FFirst:=nil;
        Flast:=nil;
        FCount:=0;
        FNoClear:=False;
      end;


    destructor TLinkedList.destroy;
      begin
        if not FNoClear then
         Clear;
      end;


    function TLinkedList.empty:boolean;
      begin
        Empty:=(FFirst=nil);
      end;


    procedure TLinkedList.Insert(Item:TLinkedListItem);
      begin
        if FFirst=nil then
         begin
           FLast:=Item;
           Item.Previous:=nil;
           Item.Next:=nil;
         end
        else
         begin
           FFirst.Previous:=Item;
           Item.Previous:=nil;
           Item.Next:=FFirst;
         end;
        FFirst:=Item;
        inc(FCount);
      end;


    procedure TLinkedList.InsertBefore(Item,Loc : TLinkedListItem);
      begin
         Item.Previous:=Loc.Previous;
         Item.Next:=Loc;
         Loc.Previous:=Item;
         if assigned(Item.Previous) then
           Item.Previous.Next:=Item
         else
           { if we've no next item, we've to adjust FFist }
           FFirst:=Item;
         inc(FCount);
      end;


    procedure TLinkedList.InsertAfter(Item,Loc : TLinkedListItem);
      begin
         Item.Next:=Loc.Next;
         Loc.Next:=Item;
         Item.Previous:=Loc;
         if assigned(Item.Next) then
           Item.Next.Previous:=Item
         else
           { if we've no next item, we've to adjust FLast }
           FLast:=Item;
         inc(FCount);
      end;


    procedure TLinkedList.Concat(Item:TLinkedListItem);
      begin
        if FFirst=nil then
         begin
           FFirst:=Item;
           Item.Previous:=nil;
           Item.Next:=nil;
         end
        else
         begin
           Flast.Next:=Item;
           Item.Previous:=Flast;
           Item.Next:=nil;
         end;
        Flast:=Item;
        inc(FCount);
      end;


    procedure TLinkedList.remove(Item:TLinkedListItem);
      begin
         if Item=nil then
           exit;
         if (FFirst=Item) and (Flast=Item) then
           begin
              FFirst:=nil;
              Flast:=nil;
           end
         else if FFirst=Item then
           begin
              FFirst:=Item.Next;
              if assigned(FFirst) then
                FFirst.Previous:=nil;
           end
         else if Flast=Item then
           begin
              Flast:=Flast.Previous;
              if assigned(Flast) then
                Flast.Next:=nil;
           end
         else
           begin
              Item.Previous.Next:=Item.Next;
              Item.Next.Previous:=Item.Previous;
           end;
         Item.Next:=nil;
         Item.Previous:=nil;
         dec(FCount);
      end;


    procedure TLinkedList.clear;
      var
        NewNode, Next : TLinkedListItem;
      begin
        NewNode:=FFirst;
        while assigned(NewNode) do
         begin
           Next:=NewNode.Next;
           prefetch(pointer(Next)^);
           NewNode.Free;
           NewNode:=Next;
          end;
        FLast:=nil;
        FFirst:=nil;
        FCount:=0;
      end;


    function TLinkedList.GetFirst:TLinkedListItem;
      begin
         if FFirst=nil then
          GetFirst:=nil
         else
          begin
            GetFirst:=FFirst;
            if FFirst=FLast then
             FLast:=nil;
            FFirst:=FFirst.Next;
            dec(FCount);
          end;
      end;


    function TLinkedList.GetLast:TLinkedListItem;
      begin
         if FLast=nil then
          Getlast:=nil
         else
          begin
            Getlast:=FLast;
            if FLast=FFirst then
             FFirst:=nil;
            FLast:=FLast.Previous;
            dec(FCount);
          end;
      end;


    procedure TLinkedList.insertList(p : TLinkedList);
      begin
         { empty List ? }
         if (p.FFirst=nil) then
           exit;
         p.Flast.Next:=FFirst;
         { we have a double Linked List }
         if assigned(FFirst) then
           FFirst.Previous:=p.Flast;
         FFirst:=p.FFirst;
         if (FLast=nil) then
           Flast:=p.Flast;
         inc(FCount,p.FCount);
         { p becomes empty }
         p.FFirst:=nil;
         p.Flast:=nil;
         p.FCount:=0;
      end;


    procedure TLinkedList.insertListBefore(Item:TLinkedListItem;p : TLinkedList);
      begin
         { empty List ? }
         if (p.FFirst=nil) then
           exit;
         if (Item=nil) then
           begin
             { Insert at begin }
             InsertList(p);
             exit;
           end
         else
           begin
             p.FLast.Next:=Item;
             p.FFirst.Previous:=Item.Previous;
             if assigned(Item.Previous) then
               Item.Previous.Next:=p.FFirst
             else
               FFirst:=p.FFirst;
             Item.Previous:=p.FLast;
             inc(FCount,p.FCount);
           end;
         { p becomes empty }
         p.FFirst:=nil;
         p.Flast:=nil;
         p.FCount:=0;
      end;


    procedure TLinkedList.insertListAfter(Item:TLinkedListItem;p : TLinkedList);
      begin
         { empty List ? }
         if (p.FFirst=nil) then
           exit;
         if (Item=nil) then
           begin
             { Insert at begin }
             InsertList(p);
             exit;
           end
         else
           begin
             p.FFirst.Previous:=Item;
             p.FLast.Next:=Item.Next;
             if assigned(Item.Next) then
               Item.Next.Previous:=p.FLast
             else
               FLast:=p.FLast;
             Item.Next:=p.FFirst;
             inc(FCount,p.FCount);
           end;
         { p becomes empty }
         p.FFirst:=nil;
         p.Flast:=nil;
         p.FCount:=0;
      end;


    procedure TLinkedList.concatList(p : TLinkedList);
      begin
        if (p.FFirst=nil) then
         exit;
        if FFirst=nil then
         FFirst:=p.FFirst
        else
         begin
           FLast.Next:=p.FFirst;
           p.FFirst.Previous:=Flast;
         end;
        Flast:=p.Flast;
        inc(FCount,p.FCount);
        { make p empty }
        p.Flast:=nil;
        p.FFirst:=nil;
        p.FCount:=0;
      end;


    procedure TLinkedList.insertListcopy(p : TLinkedList);
      var
        NewNode,NewNode2 : TLinkedListItem;
      begin
        NewNode:=p.Last;
        while assigned(NewNode) do
         begin
           NewNode2:=NewNode.Getcopy;
           if assigned(NewNode2) then
            Insert(NewNode2);
           NewNode:=NewNode.Previous;
         end;
      end;


    procedure TLinkedList.concatListcopy(p : TLinkedList);
      var
        NewNode,NewNode2 : TLinkedListItem;
      begin
        NewNode:=p.First;
        while assigned(NewNode) do
         begin
           NewNode2:=NewNode.Getcopy;
           if assigned(NewNode2) then
            Concat(NewNode2);
           NewNode:=NewNode.Next;
         end;
      end;


    procedure TLinkedList.RemoveAll;
      begin
        FFirst:=nil;
        FLast:=nil;
        FCount:=0;
      end;


{****************************************************************************
                             TCmdStrListItem
 ****************************************************************************}

    constructor TCmdStrListItem.Create(const s:TCmdStr);
      begin
        inherited Create;
        FPStr:=s;
      end;


    destructor TCmdStrListItem.Destroy;
      begin
        FPStr:='';
      end;


    function TCmdStrListItem.GetCopy:TLinkedListItem;
      begin
        Result:=(inherited GetCopy);
        { TLinkedListItem.GetCopy performs a "move" to copy all data -> reinit
          the ansistring, so the refcount is properly increased }
        Initialize(TCmdStrListItem(Result).FPStr);
        TCmdStrListItem(Result).FPStr:=FPstr;
      end;


{****************************************************************************
                           TCmdStrList
 ****************************************************************************}

    constructor TCmdStrList.Create;
      begin
         inherited Create;
         FDoubles:=true;
      end;


    constructor TCmdStrList.Create_no_double;
      begin
         inherited Create;
         FDoubles:=false;
      end;


    procedure TCmdStrList.insert(const s : TCmdStr);
      begin
         if (s='') or
            ((not FDoubles) and (findcase(s)<>nil)) then
          exit;
         inherited insert(TCmdStrListItem.create(s));
      end;


    procedure TCmdStrList.concat(const s : TCmdStr);
      begin
         if (s='') or
            ((not FDoubles) and (findcase(s)<>nil)) then
          exit;
         inherited concat(TCmdStrListItem.create(s));
      end;


    procedure TCmdStrList.remove(const s : TCmdStr);
      var
        p : TCmdStrListItem;
      begin
        if s='' then
         exit;
        p:=findcase(s);
        if assigned(p) then
         begin
           inherited Remove(p);
           p.Free;
         end;
      end;


    function TCmdStrList.GetFirst : TCmdStr;
      var
         p : TCmdStrListItem;
      begin
         p:=TCmdStrListItem(inherited GetFirst);
         if p=nil then
          GetFirst:=''
         else
          begin
            GetFirst:=p.FPStr;
            p.free;
          end;
      end;


    function TCmdStrList.Getlast : TCmdStr;
      var
         p : TCmdStrListItem;
      begin
         p:=TCmdStrListItem(inherited Getlast);
         if p=nil then
          Getlast:=''
         else
          begin
            Getlast:=p.FPStr;
            p.free;
          end;
      end;


    function TCmdStrList.FindCase(const s:TCmdStr):TCmdStrListItem;
      var
        NewNode : TCmdStrListItem;
      begin
        result:=nil;
        if s='' then
         exit;
        NewNode:=TCmdStrListItem(FFirst);
        while assigned(NewNode) do
         begin
           if NewNode.FPStr=s then
            begin
              result:=NewNode;
              exit;
            end;
           NewNode:=TCmdStrListItem(NewNode.Next);
         end;
      end;


    function TCmdStrList.Find(const s:TCmdStr):TCmdStrListItem;
      var
        NewNode : TCmdStrListItem;
      begin
        result:=nil;
        if s='' then
         exit;
        NewNode:=TCmdStrListItem(FFirst);
        while assigned(NewNode) do
         begin
           if SysUtils.CompareText(s, NewNode.FPStr)=0 then
            begin
              result:=NewNode;
              exit;
            end;
           NewNode:=TCmdStrListItem(NewNode.Next);
         end;
      end;


    procedure TCmdStrList.InsertItem(item:TCmdStrListItem);
      begin
        inherited Insert(item);
      end;


    procedure TCmdStrList.ConcatItem(item:TCmdStrListItem);
      begin
        inherited Concat(item);
      end;


{****************************************************************************
                                tdynamicarray
****************************************************************************}

    constructor tdynamicarray.create(Ablocksize:longword);
      begin
        FPosn:=0;
        FPosnblock:=nil;
        FFirstblock:=nil;
        FLastblock:=nil;
        FCurrBlockSize:=0;
        { Every block needs at least a header and alignment slack,
          therefore its size cannot be arbitrarily small. However,
          the blocksize argument is often confused with data size.
          See e.g. Mantis #20929. }
        if Ablocksize<mindynamicblocksize then
          Ablocksize:=mindynamicblocksize;
        FMaxBlockSize:=Ablocksize;
        grow;
      end;


    destructor tdynamicarray.destroy;
      var
        hp : pdynamicblock;
      begin
        while assigned(FFirstblock) do
         begin
           hp:=FFirstblock;
           FFirstblock:=FFirstblock^.Next;
           Freemem(hp);
         end;
      end;


    function  tdynamicarray.size:longword;
      begin
        if assigned(FLastblock) then
         size:=FLastblock^.pos+FLastblock^.used
        else
         size:=0;
      end;


    procedure tdynamicarray.reset;
      var
        hp : pdynamicblock;
      begin
        while assigned(FFirstblock) do
         begin
           hp:=FFirstblock;
           FFirstblock:=FFirstblock^.Next;
           Freemem(hp);
         end;
        FPosn:=0;
        FPosnblock:=nil;
        FFirstblock:=nil;
        FLastblock:=nil;
        grow;
      end;


    procedure tdynamicarray.grow;
      var
        nblock  : pdynamicblock;
        OptBlockSize,
        IncSize : integer;
      begin
        if CurrBlockSize<FMaxBlocksize then
          begin
            IncSize := mindynamicblocksize;
            if FCurrBlockSize > 255 then
              Inc(IncSize, FCurrBlockSize shr 2);
            inc(FCurrBlockSize,IncSize);
          end;
        if CurrBlockSize>FMaxBlocksize then
          FCurrBlockSize:=FMaxBlocksize;
        { Calculate the most optimal size so there is no alignment overhead
          lost in the heap manager }
        OptBlockSize:=cutils.Align(CurrBlockSize+dynamicblockbasesize,16)-dynamicblockbasesize-sizeof(ptrint);
        Getmem(nblock,OptBlockSize+dynamicblockbasesize);
        if not assigned(FFirstblock) then
         begin
           FFirstblock:=nblock;
           FPosnblock:=nblock;
           nblock^.pos:=0;
         end
        else
         begin
           FLastblock^.Next:=nblock;
           nblock^.pos:=FLastblock^.pos+FLastblock^.size;
         end;
        nblock^.used:=0;
        nblock^.size:=OptBlockSize;
        nblock^.Next:=nil;
        fillchar(nblock^.data,nblock^.size,0);
        FLastblock:=nblock;
      end;


    procedure tdynamicarray.align(i:longword);
      var
        j : longword;
      begin
        j:=(FPosn mod i);
        if j<>0 then
         begin
           j:=i-j;
           if FPosnblock^.used+j>FPosnblock^.size then
            begin
              dec(j,FPosnblock^.size-FPosnblock^.used);
              FPosnblock^.used:=FPosnblock^.size;
              grow;
              FPosnblock:=FLastblock;
            end;
           inc(FPosnblock^.used,j);
           inc(FPosn,j);
         end;
      end;


    procedure tdynamicarray.seek(i:longword);
      begin
        if (i<FPosnblock^.pos) or (i>=FPosnblock^.pos+FPosnblock^.size) then
         begin
           { set FPosnblock correct if the size is bigger then
             the current block }
           if FPosnblock^.pos>i then
            FPosnblock:=FFirstblock;
           while assigned(FPosnblock) do
            begin
              if FPosnblock^.pos+FPosnblock^.size>i then
               break;
              FPosnblock:=FPosnblock^.Next;
            end;
           { not found ? then increase blocks }
           if not assigned(FPosnblock) then
            begin
              repeat
                { the current FLastblock is now also fully used }
                FLastblock^.used:=FLastblock^.size;
                grow;
                FPosnblock:=FLastblock;
              until FPosnblock^.pos+FPosnblock^.size>=i;
            end;
         end;
        FPosn:=i;
        if FPosn-FPosnblock^.pos>FPosnblock^.used then
         FPosnblock^.used:=FPosn-FPosnblock^.pos;
      end;


    procedure tdynamicarray.write(const d;len:longword);
      var
        p : pchar;
        i,j : longword;
      begin
        p:=pchar(@d);
        while (len>0) do
         begin
           i:=FPosn-FPosnblock^.pos;
           if i+len>=FPosnblock^.size then
            begin
              j:=FPosnblock^.size-i;
              move(p^,FPosnblock^.data[i],j);
              inc(p,j);
              inc(FPosn,j);
              dec(len,j);
              FPosnblock^.used:=FPosnblock^.size;
              if assigned(FPosnblock^.Next) then
               FPosnblock:=FPosnblock^.Next
              else
               begin
                 grow;
                 FPosnblock:=FLastblock;
               end;
            end
           else
            begin
              move(p^,FPosnblock^.data[i],len);
              inc(p,len);
              inc(FPosn,len);
              i:=FPosn-FPosnblock^.pos;
              if i>FPosnblock^.used then
               FPosnblock^.used:=i;
              len:=0;
            end;
         end;
      end;


    procedure tdynamicarray.writestr(const s:string);
      begin
        write(s[1],length(s));
      end;


    function tdynamicarray.read(var d;len:longword):longword;
      var
        p : pchar;
        i,j,res : longword;
      begin
        res:=0;
        p:=pchar(@d);
        while (len>0) do
         begin
           i:=FPosn-FPosnblock^.pos;
           if i+len>=FPosnblock^.used then
            begin
              j:=FPosnblock^.used-i;
              move(FPosnblock^.data[i],p^,j);
              inc(p,j);
              inc(FPosn,j);
              inc(res,j);
              dec(len,j);
              if assigned(FPosnblock^.Next) then
               FPosnblock:=FPosnblock^.Next
              else
               break;
            end
           else
            begin
              move(FPosnblock^.data[i],p^,len);
              inc(p,len);
              inc(FPosn,len);
              inc(res,len);
              len:=0;
            end;
         end;
        read:=res;
      end;


    procedure tdynamicarray.readstream(f:TCStream;maxlen:longword);
      var
        i,left : longword;
      begin
        repeat
          left:=FPosnblock^.size-FPosnblock^.used;
          if left>maxlen then
           left:=maxlen;
          i:=f.Read(FPosnblock^.data[FPosnblock^.used],left);
          dec(maxlen,i);
          inc(FPosnblock^.used,i);
          if FPosnblock^.used=FPosnblock^.size then
           begin
             if assigned(FPosnblock^.Next) then
              FPosnblock:=FPosnblock^.Next
             else
              begin
                grow;
                FPosnblock:=FLastblock;
              end;
           end;
        until (i<left) or (maxlen=0);
      end;


    procedure tdynamicarray.writestream(f:TCStream);
      var
        hp : pdynamicblock;
      begin
        hp:=FFirstblock;
        while assigned(hp) do
         begin
           f.Write(hp^.data,hp^.used);
           hp:=hp^.Next;
         end;
      end;


    function tdynamicarray.equal(other:tdynamicarray):boolean;
      var
        ofsthis,
        ofsother,
        remthis,
        remother,
        len : sizeint;
        blockthis,
        blockother : pdynamicblock;
      begin
        if not assigned(other) then
          exit(false);
        if size<>other.size then
          exit(false);
        blockthis:=Firstblock;
        blockother:=other.FirstBlock;
        ofsthis:=0;
        ofsother:=0;

        while assigned(blockthis) and assigned(blockother) do
          begin
            remthis:=blockthis^.used-ofsthis;
            remother:=blockother^.used-ofsother;
            len:=min(remthis,remother);
            if not CompareMem(@blockthis^.data[ofsthis],@blockother^.data[ofsother],len) then
              exit(false);
            inc(ofsthis,len);
            inc(ofsother,len);
            if ofsthis=blockthis^.used then
              begin
                blockthis:=blockthis^.next;
                ofsthis:=0;
              end;
            if ofsother=blockother^.used then
              begin
                blockother:=blockother^.next;
                ofsother:=0;
              end;
          end;

        if assigned(blockthis) and not assigned(blockother) then
          result:=blockthis^.used=0
        else if assigned(blockother) and not assigned(blockthis) then
          result:=blockother^.used=0
        else
          result:=true;
      end;


{****************************************************************************
                                thashset
****************************************************************************}

    constructor THashSet.Create(InitSize: Integer; OwnKeys, OwnObjects: Boolean);
      var
        I: Integer;
      begin
        inherited Create;
        FOwnsObjects := OwnObjects;
        FOwnsKeys := OwnKeys;
        I := 64;
        while I < InitSize do I := I shl 1;
        FBucketCount := I;
        FBucket := AllocMem(I * sizeof(PHashSetItem));
      end;


    destructor THashSet.Destroy;
      begin
        Clear;
        FreeMem(FBucket);
        inherited Destroy;
      end;


    procedure THashSet.Clear;
      var
        I: Integer;
        item, next: PHashSetItem;
      begin
        for I := 0 to FBucketCount-1 do
        begin
          item := FBucket[I];
          while Assigned(item) do
          begin
            next := item^.Next;
            if FOwnsObjects then
              item^.Data.Free;
            FreeItem(item);
            item := next;
          end;
        end;
        FillChar(FBucket^, FBucketCount * sizeof(PHashSetItem), 0);
      end;


    function THashSet.Find(Key: Pointer; KeyLen: Integer): PHashSetItem;
      var
        Dummy: Boolean;
      begin
        Result := Lookup(Key, KeyLen, Dummy, False);
      end;


    function THashSet.FindOrAdd(Key: Pointer; KeyLen: Integer;
        var Found: Boolean): PHashSetItem;
      begin
        Result := Lookup(Key, KeyLen, Found, True);
      end;


    function THashSet.FindOrAdd(Key: Pointer; KeyLen: Integer): PHashSetItem;
      var
        Dummy: Boolean;
      begin
        Result := Lookup(Key, KeyLen, Dummy, True);
      end;


    function THashSet.Get(Key: Pointer; KeyLen: Integer): TObject;
      var
        e: PHashSetItem;
        Dummy: Boolean;
      begin
        e := Lookup(Key, KeyLen, Dummy, False);
        if Assigned(e) then
          Result := e^.Data
        else
          Result := nil;
      end;


    function THashSet.Lookup(Key: Pointer; KeyLen: Integer;
      var Found: Boolean; CanCreate: Boolean): PHashSetItem;
      var
        EntryPtr: PPHashSetItem;
        Entry: PHashSetItem;
        h: LongWord;
      begin
        h := FPHash(Key, KeyLen);
        EntryPtr := @FBucket[h and (FBucketCount-1)];
        Entry := EntryPtr^;
        while Assigned(Entry) and
          not ((Entry^.HashValue = h) and (Entry^.KeyLength = KeyLen) and
            (CompareByte(Entry^.Key^, Key^, KeyLen) = 0)) do
              begin
                EntryPtr := @Entry^.Next;
                Entry := EntryPtr^;
              end;
        Found := Assigned(Entry);
        if Found or (not CanCreate) then
          begin
            Result := Entry;
            Exit;
          end;
        if FCount > FBucketCount then  { arbitrary limit, probably too high }
          begin
            { rehash and repeat search }
            Resize(FBucketCount * 2);
            Result := Lookup(Key, KeyLen, Found, CanCreate);
          end
        else
          begin
            Result := CreateItem(Key, KeyLen, h);
            Inc(FCount);
            EntryPtr^ := Result;
          end;
        end;


    procedure THashSet.Resize(NewCapacity: LongWord);
      var
        p, chain: PPHashSetItem;
        i: Integer;
        e, n: PHashSetItem;
      begin
        p := AllocMem(NewCapacity * SizeOf(PHashSetItem));
        for i := 0 to FBucketCount-1 do
          begin
            e := FBucket[i];
            while Assigned(e) do
            begin
              chain := @p[e^.HashValue and (NewCapacity-1)];
              n := e^.Next;
              e^.Next := chain^;
              chain^ := e;
              e := n;
            end;
          end;
        FBucketCount := NewCapacity;
        FreeMem(FBucket);
        FBucket := p;
      end;

    class procedure THashSet.FreeItem(item: PHashSetItem);
      begin
        Dispose(item);
      end;

    class function THashSet.SizeOfItem: Integer;
      begin
        Result := SizeOf(THashSetItem);
      end;

    function THashSet.CreateItem(Key: Pointer; KeyLen: Integer; HashValue: LongWord): PHashSetItem;
      var
        itemSize, keyOfs: SizeUint;
      begin
        itemSize := SizeOfItem;
        if FOwnsKeys then
          begin
            keyOfs := itemSize;
            Result := GetMem(keyOfs + SizeUint(KeyLen));
            Result^.Key := Pointer(Result) + keyOfs;
            Move(Key^, Result^.Key^, KeyLen);
          end
        else
          begin
            Result := GetMem(itemSize);
            Result^.Key := Key;
          end;
        Result^.Next := nil;
        Result^.KeyLength := KeyLen;
        Result^.HashValue := HashValue;
        Result^.Data := nil;
      end;

    function THashSet.Remove(Entry: PHashSetItem): Boolean;
      var
        chain: PPHashSetItem;
      begin
        chain := @FBucket[Entry^.HashValue mod FBucketCount];
        while Assigned(chain^) do
          begin
            if chain^ = Entry then
              begin
                chain^ := Entry^.Next;
                if FOwnsObjects then
                  Entry^.Data.Free;
                FreeItem(Entry);
                Dec(FCount);
                Result := True;
                Exit;
              end;
            chain := @chain^^.Next;
          end;
        Result := False;
      end;


{****************************************************************************
                                ttaghashset
****************************************************************************}

    function TTagHashSet.Lookup(Key: Pointer; KeyLen: Integer;
      Tag: LongWord; var Found: Boolean; CanCreate: Boolean): PTagHashSetItem;
      var
        EntryPtr: PPTagHashSetItem;
        Entry: PTagHashSetItem;
        h: LongWord;
      begin
        h := FPHash(Key, KeyLen, Tag);
        EntryPtr := @PPTagHashSetItem(FBucket)[h and (FBucketCount-1)];
        Entry := EntryPtr^;
        while Assigned(Entry) and
          not ((Entry^.Item.HashValue = h) and (Entry^.Item.KeyLength = KeyLen) and
            (Entry^.Tag = Tag) and (CompareByte(Entry^.Item.Key^, Key^, KeyLen) = 0)) do
              begin
                EntryPtr := @Entry^.Item.Next;
                Entry := EntryPtr^;
              end;
        Found := Assigned(Entry);
        if Found or (not CanCreate) then
          begin
            Result := Entry;
            Exit;
          end;
        if FCount > FBucketCount then  { arbitrary limit, probably too high }
          begin
            { rehash and repeat search }
            Resize(FBucketCount * 2);
            Result := Lookup(Key, KeyLen, Tag, Found, CanCreate);
          end
        else
          begin
            Result := PTagHashSetItem(CreateItem(Key, KeyLen, h));
            Result^.Tag := Tag;
            Inc(FCount);
            EntryPtr^ := Result;
          end;
      end;

    class function TTagHashSet.SizeOfItem: Integer;
      begin
        Result := SizeOf(TTagHashSetItem);
      end;

    function TTagHashSet.Find(Key: Pointer; KeyLen: Integer; Tag: LongWord): PTagHashSetItem;
      var
        Dummy: Boolean;
      begin
        Result := Lookup(Key, KeyLen, Tag, Dummy, False);
      end;

    function TTagHashSet.FindOrAdd(Key: Pointer; KeyLen: Integer; Tag: LongWord;
      var Found: Boolean): PTagHashSetItem;
      begin
        Result := Lookup(Key, KeyLen, Tag, Found, True);
      end;

    function TTagHashSet.FindOrAdd(Key: Pointer; KeyLen: Integer; Tag: LongWord): PTagHashSetItem;
      var
        Dummy: Boolean;
      begin
        Result := Lookup(Key, KeyLen, Tag, Dummy, True);
      end;

    function TTagHashSet.Get(Key: Pointer; KeyLen: Integer; Tag: LongWord): TObject;
      var
        e: PTagHashSetItem;
        Dummy: Boolean;
      begin
        e := Lookup(Key, KeyLen, Tag, Dummy, False);
        if Assigned(e) then
          Result := e^.Item.Data
        else
          Result := nil;
      end;

{****************************************************************************
                                tbitset
****************************************************************************}

    constructor tbitset.create(initsize: longint);
      begin
        create_bytesize((initsize+7) div 8);
      end;


    constructor tbitset.create_bytesize(bytesize: longint);
      begin
        fdatasize:=bytesize;
        getmem(fdata,fdataSize);
        clear;
      end;


    destructor tbitset.destroy;
      begin
        freemem(fdata,fdatasize);
        inherited destroy;
      end;


    procedure tbitset.clear;
      begin
        fillchar(fdata^,fdatasize,0);
      end;


    procedure tbitset.grow(nsize: longint);
      begin
        reallocmem(fdata,nsize);
        fillchar(fdata[fdatasize],nsize-fdatasize,0);
        fdatasize:=nsize;
      end;


    procedure tbitset.include(index: longint);
      var
        dataindex: longint;
      begin
        { don't use bitpacked array, not endian-safe }
        dataindex:=index shr 3;
        if (dataindex>=datasize) then
          grow(dataindex+16);
        fdata[dataindex]:=fdata[dataindex] or (1 shl (index and 7));
      end;


    procedure tbitset.exclude(index: longint);
      var
        dataindex: longint;
      begin
        dataindex:=index shr 3;
        if (dataindex>=datasize) then
          exit;
        fdata[dataindex]:=fdata[dataindex] and not(1 shl (index and 7));
      end;


    function tbitset.isset(index: longint): boolean;
      var
        dataindex: longint;
      begin
        dataindex:=index shr 3;
        result:=
          (dataindex<datasize) and
          (((fdata[dataindex] shr (index and 7)) and 1)<>0);
      end;


    procedure tbitset.addset(aset: tbitset);
      var
        i: longint;
      begin
        if (aset.datasize>datasize) then
          grow(aset.datasize);
        for i:=0 to aset.datasize-1 do
          fdata[i]:=fdata[i] or aset.data[i];
      end;


    procedure tbitset.subset(aset: tbitset);
      var
        i: longint;
      begin
        for i:=0 to min(datasize,aset.datasize)-1 do
          fdata[i]:=fdata[i] and not(aset.data[i]);
      end;


end.
