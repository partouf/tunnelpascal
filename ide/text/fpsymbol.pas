{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Symbol browse support routines for the IDE

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FPSymbol;

interface

uses Objects,Drivers,Views,Dialogs,Outline,
     BrowCol,
     FPViews;

const
      { Browser tab constants }
      btScope       = 0;
      btReferences  = 1;
      btInheritance = 2;
      btMemInfo     = 3;
      btBreakWatch  = 4;

type
    PSymbolView = ^TSymbolView;
    TSymbolView = object(TListBox)
      constructor  Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar);
      procedure    HandleEvent(var Event: TEvent); virtual;
      procedure    GotoItem(Item: sw_integer); virtual;
      procedure    TrackItem(Item: sw_integer); virtual;
      function     GetPalette: PPalette; virtual;
    private
      function     TrackReference(R: PReference): boolean; virtual;
      function     GotoReference(R: PReference): boolean; virtual;
    end;

    PSymbolScopeView = ^TSymbolScopeView;
    TSymbolScopeView = object(TSymbolView)
      constructor Init(var Bounds: TRect; ASymbols: PSymbolCollection; AHScrollBar, AVScrollBar: PScrollBar);
      function    GetText(Item,MaxLen: Sw_Integer): String; virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      procedure   Draw; virtual;
      procedure   LookUp(S: string); virtual;
      procedure   GotoItem(Item: sw_integer); virtual;
      procedure   TrackItem(Item: sw_integer); virtual;
    private
      Symbols: PSymbolCollection;
      LookupStr: string;
    end;

    PSymbolReferenceView = ^TSymbolReferenceView;
    TSymbolReferenceView = object(TSymbolView)
      constructor Init(var Bounds: TRect; AReferences: PReferenceCollection; AHScrollBar, AVScrollBar: PScrollBar);
      procedure   HandleEvent(var Event: TEvent); virtual;
      function    GetText(Item,MaxLen: Sw_Integer): String; virtual;
      procedure   SelectItem(Item: Sw_Integer); virtual;
      procedure   GotoItem(Item: sw_integer); virtual;
      procedure   TrackItem(Item: sw_integer); virtual;
    private
      References: PReferenceCollection;
    end;

    PSymbolMemInfoView = ^TSymbolMemInfoView;
    TSymbolMemInfoView = object(TStaticText)
      constructor  Init(var Bounds: TRect; AMemInfo: PSymbolMemInfo);
      procedure    GetText(var S: String); virtual;
      function     GetPalette: PPalette; virtual;
    private
      MemInfo: PSymbolMemInfo;
    end;

    PSymbolInheritanceView = ^TSymbolInheritanceView;
    TSymbolInheritanceView = object(TOutlineViewer)
      constructor  Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar; ARoot: PObjectSymbol);
      function     GetRoot: Pointer; virtual;
      function     HasChildren(Node: Pointer): Boolean; virtual;
      function     GetChild(Node: Pointer; I: Integer): Pointer; virtual;
      function     GetNumChildren(Node: Pointer): Integer; virtual;
      function     GetText(Node: Pointer): String; virtual;
      procedure    Adjust(Node: Pointer; Expand: Boolean); virtual;
      function     IsExpanded(Node: Pointer): Boolean; virtual;
      procedure    Selected(I: Integer); virtual;
      function     GetPalette: PPalette; virtual;
    private
      Root: PObjectSymbol;
    end;

    PBrowserTabItem = ^TBrowserTabItem;
    TBrowserTabItem = record
      Sign  : char;
      Link  : PView;
      Next  : PBrowserTabItem;
    end;

    PBrowserTab = ^TBrowserTab;
    TBrowserTab = object(TView)
      Items: PBrowserTabItem;
      constructor Init(var Bounds: TRect; AItems: PBrowserTabItem);
      function    GetItemCount: sw_integer; virtual;
      function    GetItem(Index: sw_integer): PBrowserTabItem; virtual;
      procedure   SetParams(AFlags: word; ACurrent: Sw_integer); virtual;
      procedure   SelectItem(Index: Sw_integer); virtual;
      procedure   Draw; virtual;
      function    GetPalette: PPalette; virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      destructor  Done; virtual;
    private
      Flags   : word;
      Current : Sw_integer;
    end;

    PBrowserWindow = ^TBrowserWindow;
    TBrowserWindow = object(TFPWindow)
      constructor Init(var Bounds: TRect; ATitle: TTitleStr; ANumber: Sw_Integer;ASym : PSymbol;
                    const AName: string; ASymbols: PSymbolCollection; AReferences: PReferenceCollection;
                    AInheritance: PObjectSymbol; AMemInfo: PSymbolMemInfo);
      procedure   HandleEvent(var Event: TEvent); virtual;
      procedure   SetState(AState: Word; Enable: Boolean); virtual;
      procedure   Close; virtual;
      procedure   SelectTab(BrowserTab: Sw_integer); virtual;
      function    GetPalette: PPalette; virtual;
    private
      PageTab       : PBrowserTab;
      Sym           : PSymbol;
      ScopeView     : PSymbolScopeView;
      ReferenceView : PSymbolReferenceView;
      InheritanceView: PSymbolInheritanceView;
      MemInfoView   : PSymbolMemInfoView;
    end;

procedure OpenSymbolBrowser(X,Y: Sw_integer;const Name,Line: string;S : PSymbol;
            Symbols: PSymbolCollection; References: PReferenceCollection;
            Inheritance: PObjectSymbol; MemInfo: PSymbolMemInfo);

function IsSymbolInfoAvailable: boolean;

procedure OpenOneSymbolBrowser(Name : String);

implementation

uses Commands,App,
     WEditor,WViews,
     FPConst,FPUtils,FPVars,{$ifndef FPDEBUG}FPDebug{$endif};

function NewBrowserTabItem(ASign: char; ALink: PView; ANext: PBrowserTabItem): PBrowserTabItem;
var P: PBrowserTabItem;
begin
  New(P); FillChar(P^,SizeOf(P^),0);
  with P^ do begin Sign:=ASign; Link:=ALink; Next:=ANext; end;
  NewBrowserTabItem:=P;
end;

procedure DisposeBrowserTabItem(P: PBrowserTabItem);
begin
  if P<>nil then Dispose(P);
end;

procedure DisposeBrowserTabList(P: PBrowserTabItem);
begin
  if P<>nil then
  begin
    if P^.Next<>nil then DisposeBrowserTabList(P^.Next);
    DisposeBrowserTabItem(P);
  end;
end;

function IsSymbolInfoAvailable: boolean;
begin
  IsSymbolInfoAvailable:=BrowCol.Modules<>nil;
end;

procedure OpenOneSymbolBrowser(Name : String);

var Index : sw_integer;
    PS : PSymbol;
    P : Pstring;

  function Search(P : PSymbol) : boolean;
  begin
    Search:=UpcaseStr(P^.Items^.LookUp(Name,Index))=Name;
  end;

begin
   Name:=UpcaseStr(Name);
   If BrowCol.Modules<>nil then
     begin
       PS:=BrowCol.Modules^.FirstThat(@Search);
       If assigned(PS) then
         OpenSymbolBrowser(0,20,
                PS^.Items^.At(Index)^.GetName,'',PS^.Items^.At(Index),
                PS^.Items^.At(Index)^.Items,PS^.Items^.At(Index)^.References,nil,PS^.MemInfo)
       else
         begin
           P:=@Name;
           ErrorBox(#3'Symbol %s not found',@P);
         end;
     end
   else
     ErrorBox('No Browser info available',nil);
end;

(*procedure ReadBrowseLog(FileName: string);
var f: text;
    IOOK,EndOfFile: boolean;
    Line: string;
procedure NextLine;
begin
  readln(f,Line);
  EndOfFile:=Eof(f);
end;
var Level: integer;
procedure ProcessSymTable(Indent: integer; Owner: PSymbolCollection);
var IndentS,S,Source: string;
    Sym: PSymbol;
    Ref: PSymbolReference;
    P: byte;
    PX: TPoint;
    PS: PString;
    PCount: integer;
    Params: array[0..30] of PString;
    Typ: tsymtyp;
    ExitBack: boolean;
begin
  Inc(Level);
  IndentS:=CharStr(' ',Indent); ExitBack:=false;
  Sym:=nil;
  repeat
    if copy(Line,1,length(IndentS))<>IndentS then ExitBack:=true else
    if copy(Line,Indent+1,3)='***' then
      { new symbol }
      begin
        S:=copy(Line,Indent+1+3,255);
        P:=Pos('***',S); if P=0 then P:=length(S)+1;
        S:=Trim(copy(S,1,P-1));
        if (copy(S,1,1)='_') and (Pos('$$',S)>0) then
          begin
            repeat
              P:=Pos('$$',S);
              if P>0 then Delete(S,1,P+1);
            until P=0;
            P:=Pos('$',S);
            Delete(S,1,P);
            PCount:=0;
            repeat
              P:=Pos('$',S); if P=0 then P:=length(S)+1;
              Params[PCount]:=TypeNames^.Add(copy(S,1,P-1));
              Inc(PCount);
              Delete(S,1,P);
            until S='';
            Sym^.Typ:=procsym;
            Sym^.SetParams(PCount,@Params);
          end
        else
          New(Sym, Init(S, varsym, 0, nil));
        Owner^.Insert(Sym);
        NextLine;
      end else
    if copy(Line,Indent+1,3)='---' then
      { child symtable }
      begin
        S:=Trim(copy(Line,Indent+1+12,255));
        if Level=1 then Typ:=unitsym else
          Typ:=typesym;
        if (Sym<>nil) and (Sym^.GetName=S) then
        else
          begin
            New(Sym, Init(S, Typ, 0, nil));
            Owner^.Insert(Sym);
          end;
        Sym^.Typ:=Typ;
        NextLine;
        New(Sym^.Items, Init(0,50));
        ProcessSymTable(Indent+2,Sym^.Items);
      end else
{    if Sym<>nil then}
    if copy(Line,Indent+1,1)=' ' then
      { reference }
      begin
        S:=copy(Line,Indent+1+2,255);
        P:=Pos('(',S); if P=0 then P:=length(S)+1;
        Source:=Trim(copy(S,1,P-1)); Delete(S,1,P);
        P:=Pos(',',S); if P=0 then P:=length(S)+1;
        PX.Y:=StrToInt(copy(S,1,P-1)); Delete(S,1,P);
        P:=Pos(')',S); if P=0 then P:=length(S)+1;
        PX.X:=StrToInt(copy(S,1,P-1)); Delete(S,1,P);
        PS:=ModuleNames^.Add(Source);
        New(Ref, Init(PS, PX));
        if Sym^.References=nil then
          New(Sym^.References, Init(10,50));
        Sym^.References^.Insert(Ref);
      end;
    if ExitBack=false then
      NextLine;
  until EndOfFile or ExitBack;
  Dec(Level);
end;
begin
  DoneSymbolBrowser;
  InitSymbolBrowser;

{$I-}
  Assign(f,FileName);
  Reset(f);
  Level:=0;
  NextLine;
  while (IOResult=0) and (EndOfFile=false) do
    ProcessSymTable(0,Modules);
  Close(f);
  EatIO;
{$I+}
end;*)


{****************************************************************************
                               TSymbolView
****************************************************************************}

constructor TSymbolView.Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar);
begin
  inherited Init(Bounds,1,AVScrollBar);
  HScrollBar:=AHScrollBar;
  if assigned(HScrollBar) then
    HScrollBar^.SetRange(1,80);
  Options:=Options or (ofSelectable+ofTopSelect);
end;

procedure TSymbolView.HandleEvent(var Event: TEvent);
var DontClear: boolean;
begin
  case Event.What of
    evKeyDown :
      begin
        DontClear:=false;
        case Event.KeyCode of
          kbEnter :
            GotoItem(Focused);
          kbSpaceBar :
            TrackItem(Focused);
          kbRight,kbLeft :
            if HScrollBar<>nil then
              HScrollBar^.HandleEvent(Event);
        else DontClear:=true;
        end;
        if DontClear=false then ClearEvent(Event);
      end;
    evMouseDown :
      if Event.double then
        GotoItem(Focused);
  end;
  inherited HandleEvent(Event);
end;

function TSymbolView.GetPalette: PPalette;
const
  P: string[length(CBrowserListBox)] = CBrowserListBox;
begin
  GetPalette:=@P;
end;

procedure TSymbolView.GotoItem(Item: sw_integer);
begin
  SelectItem(Item);
end;

procedure TSymbolView.TrackItem(Item: sw_integer);
begin
  SelectItem(Item);
end;

function LastBrowserWindow: PBrowserWindow;
var BW: PBrowserWindow;
procedure IsBW(P: PView); {$ifndef FPC}far;{$endif}
begin
  if (P^.HelpCtx=hcBrowserWindow) then
    BW:=pointer(P);
end;
begin
  BW:=nil;
  Desktop^.ForEach(@IsBW);
  LastBrowserWindow:=BW;
end;

function TSymbolView.TrackReference(R: PReference): boolean;
var W: PSourceWindow;
    BW: PBrowserWindow;
    P: TPoint;
begin
  Message(Desktop,evBroadcast,cmClearLineHighlights,nil);
  Desktop^.Lock;
  P.X:=R^.Position.X-1; P.Y:=R^.Position.Y-1;
  W:=TryToOpenFile(nil,R^.GetFileName,P.X,P.Y,true);
  if W<>nil then
  begin
    BW:=LastBrowserWindow;
    if BW=nil then
      W^.Select
    else
      begin
        Desktop^.Delete(W);
        Desktop^.InsertBefore(W,BW^.NextView);
      end;
    W^.Editor^.SetHighlightRow(P.Y);
  end;
  Desktop^.UnLock;
  TrackReference:=W<>nil;
end;

function TSymbolView.GotoReference(R: PReference): boolean;
var W: PSourceWindow;
begin
  Desktop^.Lock;
  W:=TryToOpenFile(nil,R^.GetFileName,R^.Position.X-1,R^.Position.Y-1,true);
  if W<>nil then W^.Select;
  Desktop^.UnLock;
  GotoReference:=W<>nil;
end;

{****************************************************************************
                               TSymbolScopeView
****************************************************************************}

constructor TSymbolScopeView.Init(var Bounds: TRect; ASymbols: PSymbolCollection; AHScrollBar, AVScrollBar: PScrollBar);
begin
  inherited Init(Bounds,AHScrollBar, AVScrollBar);
  Symbols:=ASymbols;
  NewList(ASymbols);
  SetRange(Symbols^.Count);
end;

procedure TSymbolScopeView.HandleEvent(var Event: TEvent);
var OldFocus: sw_integer;
begin
  case Event.What of
    evKeyDown :
      case Event.KeyCode of
        kbBack :
          begin
            LookUp(copy(LookUpStr,1,length(LookUpStr)-1));
            ClearEvent(Event);
          end;
      else
        if Event.CharCode in[#33..#255] then
          begin
            LookUp(LookUpStr+Event.CharCode);
            ClearEvent(Event);
          end;
      end;
  end;
  OldFocus:=Focused;
  inherited HandleEvent(Event);
  if OldFocus<>Focused then
    Lookup('');
end;

procedure TSymbolScopeView.Draw;
begin
  inherited Draw;
  SetCursor(2+SymbolTypLen+length(LookUpStr),Focused-TopItem);
end;

procedure TSymbolScopeView.LookUp(S: string);
var Idx: Sw_integer;
    NS: string;
begin
  NS:=LookUpStr;
  if (Symbols=nil) or (S='') then NS:='' else
    begin
      S:=Symbols^.LookUp(S,Idx);
      if Idx<>-1 then
        begin
          NS:=S;
          FocusItem(Idx);
        end;
    end;
  LookUpStr:=NS;
  SetState(sfCursorVis,LookUpStr<>'');
  DrawView;
end;

procedure TSymbolScopeView.GotoItem(Item: sw_integer);
begin
  SelectItem(Item);
end;

procedure TSymbolScopeView.TrackItem(Item: sw_integer);
var S: PSymbol;
begin
  if Range=0 then Exit;
  S:=List^.At(Focused);
  if (S^.References<>nil) and (S^.References^.Count>0) then
    TrackReference(S^.References^.At(0));
end;

function TSymbolScopeView.GetText(Item,MaxLen: Sw_Integer): String;
var S: string;
begin
  S:=Symbols^.At(Item)^.GetText;
  GetText:=copy(S,1,MaxLen);
end;


{****************************************************************************
                             TSymbolReferenceView
****************************************************************************}

constructor TSymbolReferenceView.Init(var Bounds: TRect; AReferences: PReferenceCollection;
              AHScrollBar, AVScrollBar: PScrollBar);
begin
  inherited Init(Bounds,AHScrollBar, AVScrollBar);
  References:=AReferences;
  NewList(AReferences);
  SetRange(References^.Count);
end;

procedure TSymbolReferenceView.HandleEvent(var Event: TEvent);
var OldFocus: sw_integer;
begin
  OldFocus:=Focused;
  inherited HandleEvent(Event);
  if OldFocus<>Focused then
    Message(Desktop,evBroadcast,cmClearLineHighlights,nil);
end;

function TSymbolReferenceView.GetText(Item,MaxLen: Sw_Integer): String;
var S: string;
    P: PReference;
begin
  P:=References^.At(Item);
  S:=P^.GetFileName+'('+IntToStr(P^.Position.Y)+','+IntToStr(P^.Position.X)+')';
  GetText:=copy(S,1,MaxLen);
end;

procedure TSymbolReferenceView.GotoItem(Item: sw_integer);
begin
  if Range=0 then Exit;
  GotoReference(List^.At(Item));
end;

procedure TSymbolReferenceView.TrackItem(Item: sw_integer);
begin
  if Range=0 then Exit;
  TrackReference(List^.At(Item));
end;

procedure TSymbolReferenceView.SelectItem(Item: Sw_Integer);
begin
  GotoItem(Item);
end;


constructor TSymbolMemInfoView.Init(var Bounds: TRect; AMemInfo: PSymbolMemInfo);
begin
  inherited Init(Bounds,'');
  Options:=Options or (ofSelectable+ofTopSelect);
  MemInfo:=AMemInfo;
end;

procedure TSymbolMemInfoView.GetText(var S: String);
function SizeStr(Size: longint): string;
var S: string[40];
begin
  S:=IntToStrL(Size,7);
  S:=S+' byte';
  if Size>0 then S:=S+'s';
  SizeStr:=S;
end;
function AddrStr(Addr: longint): string;
type TLongint = record LoW,HiW: word; end;
begin
  with TLongint(Addr) do
  AddrStr:='$'+IntToHexL(HiW,4)+IntToHexL(HiW,4);
end;
begin
  S:=
   #13+
{  ' Memory location: '+AddrStr(MemInfo^.Addr)+#13+
  '   Local address: '+AddrStr(MemInfo^.LocalAddr)+#13+}

  { ??? internal linker ??? }

  '  Size in memory: '+SizeStr(MemInfo^.Size)+#13+
  '   Size on stack: '+SizeStr(MemInfo^.PushSize)+#13+
  ''
  ;
end;

function TSymbolMemInfoView.GetPalette: PPalette;
begin
  GetPalette:=inherited GetPalette;
end;

{****************************************************************************
                          TSymbolInheritanceView
****************************************************************************}

constructor TSymbolInheritanceView.Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar; ARoot: PObjectSymbol);
begin
  inherited Init(Bounds,AHScrollBar,AVScrollBar);
  Options:=Options or (ofSelectable+ofTopSelect);
  Root:=ARoot;
  ExpandAll(GetRoot); Update;
end;

function TSymbolInheritanceView.GetRoot: Pointer;
begin
  GetRoot:=Root;
end;

function TSymbolInheritanceView.HasChildren(Node: Pointer): Boolean;
begin
  HasChildren:=GetNumChildren(Node)>0;
end;

function TSymbolInheritanceView.GetChild(Node: Pointer; I: Integer): Pointer;
begin
  GetChild:=PObjectSymbol(Node)^.GetDescendant(I);
end;

function TSymbolInheritanceView.GetNumChildren(Node: Pointer): Integer;
begin
  GetNumChildren:=PObjectSymbol(Node)^.GetDescendantCount;
end;

function TSymbolInheritanceView.GetText(Node: Pointer): String;
begin
  GetText:=PObjectSymbol(Node)^.GetName;
end;

procedure TSymbolInheritanceView.Adjust(Node: Pointer; Expand: Boolean);
begin
  PObjectSymbol(Node)^.Expanded:=Expand;
end;

function TSymbolInheritanceView.IsExpanded(Node: Pointer): Boolean;
begin
  IsExpanded:=PObjectSymbol(Node)^.Expanded;
end;

function TSymbolInheritanceView.GetPalette: PPalette;
const P: string[length(CBrowserOutline)] = CBrowserOutline;
begin
  GetPalette:=@P;
end;

procedure TSymbolInheritanceView.Selected(I: Integer);
var P: pointer;
    S: PSymbol;
    Anc: PObjectSymbol;
begin
  P:=GetNode(I);
  if P=nil then Exit;

  S:=PObjectSymbol(P)^.Symbol;
  
  { this happens for the top objects view (PM) }
  if S=nil then exit;
  
  if S^.Ancestor=nil then Anc:=nil else
    Anc:=SearchObjectForSymbol(S^.Ancestor);
  OpenSymbolBrowser(Origin.X-1,FOC-Delta.Y+1,
    S^.GetName,
    S^.GetText,S,
    S^.Items,S^.References,Anc,S^.MemInfo);
end;


{****************************************************************************
                               TBrowserTab
****************************************************************************}

constructor TBrowserTab.Init(var Bounds: TRect; AItems: PBrowserTabItem);
begin
  inherited Init(Bounds);
  Options:=Options or ofPreProcess;
  Items:=AItems;
  SetParams(0,0);
end;

procedure TBrowserTab.SetParams(AFlags: word; ACurrent: Sw_integer);
begin
  Flags:=AFlags;
  SelectItem(ACurrent);
end;

procedure TBrowserTab.SelectItem(Index: Sw_integer);
var P: PBrowserTabItem;
begin
  Current:=Index;
  P:=GetItem(Current);
  if (P<>nil) and (P^.Link<>nil) then
    P^.Link^.Focus;
  DrawView;
end;

function TBrowserTab.GetItemCount: sw_integer;
var Count: integer;
    P: PBrowserTabItem;
begin
  Count:=0; P:=Items;
  while (P<>nil) do
    begin
      Inc(Count);
      P:=P^.Next;
    end;
  GetItemCount:=Count;
end;

function TBrowserTab.GetItem(Index: sw_integer): PBrowserTabItem;
var Counter: integer;
    P: PBrowserTabItem;
begin
  P:=Items; Counter:=0;
  while (P<>nil) and (Counter<Index) do
    begin
      P:=P^.Next;
      Inc(Counter);
    end;
  GetItem:=P;
end;

procedure TBrowserTab.Draw;
var B: TDrawBuffer;
    SelColor, NormColor, C: word;
    I,CurX,Count: Sw_integer;
function Names(Idx: integer): char;
begin
  Names:=GetItem(Idx)^.Sign;
end;
begin
  NormColor:=GetColor(1); SelColor:=GetColor(2);
  MoveChar(B,'�',SelColor,Size.X);
  CurX:=0; Count:=0;
  for I:=0 to GetItemCount-1 do
    if (Flags and (1 shl I))<>0 then
    begin
      Inc(Count);
      if Current=I then C:=SelColor
                   else C:=NormColor;
      if Count=1 then MoveChar(B[CurX],'�',SelColor,1)
                 else MoveChar(B[CurX],'�',SelColor,1);
      MoveCStr(B[CurX+1],' '+Names(I)+' ',C);
      Inc(CurX,4);
    end;
  if Count>0 then
    MoveChar(B[CurX],'�',SelColor,1);
  WriteLine(0,0,Size.X,Size.Y,B);
end;

procedure TBrowserTab.HandleEvent(var Event: TEvent);
var I,Idx: integer;
    DontClear: boolean;
    P: TPoint;
function GetItemForCoord(X: integer): integer;
var I,CurX,Idx: integer;
begin
  CurX:=0; Idx:=-1;
  for I:=0 to GetItemCount-1 do
    if (Flags and (1 shl I))<>0 then
    begin
      if (CurX+1<=X) and (X<=CurX+3) then
        begin Idx:=I; Break; end;
      Inc(CurX,4);
    end;
  GetItemForCoord:=Idx;
end;
begin
  case Event.What of
    evMouseDown :
      if MouseInView(Event.Where) then
        begin
          repeat
            MakeLocal(Event.Where,P);
            Idx:=GetItemForCoord(P.X);
            if Idx<>-1 then
              SelectItem(Idx);
          until not MouseEvent(Event, evMouseMove);
          ClearEvent(Event);
        end;
    evKeyDown :
      begin
        DontClear:=false; Idx:=-1;
        for I:=0 to GetItemCount-1 do
          if GetCtrlCode(GetItem(I)^.Sign)=Event.KeyCode then
            begin
              Idx:=I;
              Break;
            end;
        if Idx=-1 then
          DontClear:=true
        else
          SelectItem(Idx);
        if DontClear=false then ClearEvent(Event);
      end;
  end;
  inherited HandleEvent(Event);
end;

function TBrowserTab.GetPalette: PPalette;
const P: string[length(CBrowserTab)] = CBrowserTab;
begin
  GetPalette:=@P;
end;

destructor TBrowserTab.Done;
begin
  inherited Done;
  if Items<>nil then DisposeBrowserTabList(Items);
end;

constructor TBrowserWindow.Init(var Bounds: TRect; ATitle: TTitleStr; ANumber: Sw_Integer;ASym : PSymbol;
             const AName: string; ASymbols: PSymbolCollection; AReferences: PReferenceCollection;
             AInheritance: PObjectSymbol; AMemInfo: PSymbolMemINfo);
var R: TRect;
    ST: PStaticText;
    HSB,VSB: PScrollBar;
function CreateVSB(R: TRect): PScrollBar;
var R2: TRect;
    SB: PScrollBar;
begin
  Sym:=ASym;
  R2.Copy(R); R2.Move(1,0); R2.A.X:=R2.B.X-1;
  New(SB, Init(R2)); SB^.GrowMode:=gfGrowLoX+gfGrowHiX+gfGrowHiY;
  CreateVSB:=SB;
end;
function CreateHSB(R: TRect): PScrollBar;
var R2: TRect;
    SB: PScrollBar;
begin
  R2.Copy(R); R2.Move(0,1); R2.A.Y:=R2.B.Y-1;
  New(SB, Init(R2)); SB^.GrowMode:=gfGrowLoY+gfGrowHiX+gfGrowHiY;
  CreateHSB:=SB;
end;
begin
  inherited Init(Bounds, ATitle, ANumber);
  HelpCtx:=hcBrowserWindow;

  GetExtent(R); R.Grow(-1,-1); R.B.Y:=R.A.Y+1;
  New(ST, Init(R, ' '+AName)); ST^.GrowMode:=gfGrowHiX;
  Insert(ST);

  GetExtent(R); R.Grow(-1,-1); Inc(R.A.Y,2);
  if assigned(ASymbols) and (ASymbols^.Count>0) then
    begin
      HSB:=CreateHSB(R); Insert(HSB);
      VSB:=CreateVSB(R); Insert(VSB);
      New(ScopeView, Init(R, ASymbols, HSB, VSB));
      ScopeView^.GrowMode:=gfGrowHiX+gfGrowHiY;
      Insert(ScopeView);
    end;
  if assigned(AReferences) and (AReferences^.Count>0) then
    begin
      HSB:=CreateHSB(R); Insert(HSB);
      VSB:=CreateVSB(R); Insert(VSB);
      New(ReferenceView, Init(R, AReferences, HSB, VSB));
      ReferenceView^.GrowMode:=gfGrowHiX+gfGrowHiY;
      Insert(ReferenceView);
    end;
  if assigned(AInheritance) then
    begin
      New(InheritanceView, Init(R, nil,nil, AInheritance));
      InheritanceView^.GrowMode:=gfGrowHiX+gfGrowHiY;
      Insert(InheritanceView);
    end;
  if assigned(AMemInfo) then
    begin
      New(MemInfoView, Init(R, AMemInfo));
      MemInfoView^.GrowMode:=gfGrowHiX+gfGrowHiY;
      Insert(MemInfoView);
    end;

  GetExtent(R); R.Grow(-1,-1); R.Move(0,1); R.B.Y:=R.A.Y+1;
  New(PageTab, Init(R,
    NewBrowserTabItem('S',ScopeView,
    NewBrowserTabItem('R',ReferenceView,
    NewBrowserTabItem('I',InheritanceView,
    NewBrowserTabItem('M',MemInfoView,
    nil))
    ))));
  PageTab^.GrowMode:=gfGrowHiX;
  Insert(PageTab);

  if assigned(ScopeView) then
   SelectTab(btScope)
  else
   if assigned(ReferenceView) then
    SelectTab(btReferences)
  else
   if assigned(InheritanceView) then
    SelectTab(btInheritance);
end;

procedure TBrowserWindow.HandleEvent(var Event: TEvent);
var DontClear: boolean;
    S: PSymbol;
    Anc: PObjectSymbol;
    P: TPoint;
begin
  case Event.What of
    evBroadcast :
      case Event.Command of
        cmSearchWindow :
          ClearEvent(Event);
        cmListItemSelected :
          if Event.InfoPtr=ScopeView then
            begin
              S:=ScopeView^.Symbols^.At(ScopeView^.Focused);
              MakeGlobal(ScopeView^.Origin,P);
              Desktop^.MakeLocal(P,P); Inc(P.Y,ScopeView^.Focused-ScopeView^.TopItem);
              Inc(P.Y);
              if S^.Ancestor=nil then Anc:=nil else
                Anc:=SearchObjectForSymbol(S^.Ancestor);
              if (S^.GetReferenceCount>0) or (S^.GetItemCount>0) or (Anc<>nil) then
               OpenSymbolBrowser(Origin.X-1,P.Y,
                 S^.GetName,
                 ScopeView^.GetText(ScopeView^.Focused,255),S,
                 S^.Items,S^.References,Anc,S^.MemInfo);
            end;
      end;
{    evCommand :
      begin
        DontClear:=false;
        case Event.Command of
        cmGotoSymbol :
          if Event.InfoPtr=ScopeView then
           if ReferenceView<>nil then
            if ReferenceView^.Range>0 then
              ReferenceView^.GotoItem(0);
        cmTrackSymbol :
          if Event.InfoPtr=ScopeView then
            if (ScopeView<>nil) and (ScopeView^.Range>0) then
              begin
                S:=ScopeView^.At(ScopeView^.Focused);
                if (S^.References<>nil) and (S^.References^.Count>0) then
                  TrackItem(S^.References^.At(0));
        else DontClear:=true;
        end;
        if DontClear=false then ClearEvent(Event);
      end;}
    evKeyDown :
      begin
        DontClear:=false;
        case Event.KeyCode of
          kbEsc :
            Close;
        else DontClear:=true;
        end;
        if DontClear=false then ClearEvent(Event);
      end;
  end;
  inherited HandleEvent(Event);
end;

procedure TBrowserWindow.SetState(AState: Word; Enable: Boolean);
var OldState: word;
begin
  OldState:=State;
  inherited SetState(AState,Enable);
  if ((State xor OldState) and sfActive)<>0 then
    if GetState(sfActive)=false then
      Message(Desktop,evBroadcast,cmClearLineHighlights,nil);
end;

procedure TBrowserWindow.Close;
begin
  Message(Desktop,evBroadcast,cmClearLineHighlights,nil);
  inherited Close;
end;

procedure TBrowserWindow.SelectTab(BrowserTab: Sw_integer);
var Tabs: Sw_integer;
{    PB : PBreakpoint;
    PS :PString;
    l : longint; }
begin
(*  case BrowserTab of
    btScope :
      if assigned(ScopeView) then
        ScopeView^.Select;
    btReferences :
      if assigned(ReferenceView) then
        ReferenceView^.Select;
    btBreakWatch :
      begin
        if Assigned(Sym) then
          begin
            if Pos('proc',Sym^.GetText)>0 then
          { insert function breakpoint }
            begin
               { make it visible }
               PS:=Sym^.Name;
               l:=Length(PS^);
               If PS^[l]='*' then
                 begin
                   PB:=BreakpointCollection^.GetType(bt_function,copy(GetStr(PS),1,l-1));
                   If Assigned(PB) then
                     BreakpointCollection^.Delete(PB);
                   Sym^.Name:=NewStr(copy(GetStr(PS),1,l-1));
                   DrawView;
                   DisposeStr(PS);
                 end
               else
                 begin
                   Sym^.Name:=NewStr(GetStr(PS)+'*');
                   DrawView;
                   New(PB,init_function(GetStr(PS)));
                   DisposeStr(PS);
                   BreakpointCollection^.Insert(PB);
                   BreakpointCollection^.Update;
                 end;
            end
          else if pos('var',Sym^.GetText)>0 then
            { insert watch point }
            begin
               { make it visible }
               PS:=Sym^.Name;
               l:=Length(PS^);
               If PS^[l]='*' then
                 begin
                   PB:=BreakpointCollection^.GetType(bt_awatch,copy(PS^,1,l-1));
                   If Assigned(PB) then
                     BreakpointCollection^.Delete(PB);
                   Sym^.Name:=NewStr(copy(PS^,1,l-1));
                   DrawView;
                   DisposeStr(PS);
                 end
               else
                 begin
                   Sym^.Name:=NewStr(GetStr(PS)+'*');
                   DrawView;
                   New(PB,init_type(bt_awatch,GetStr(PS)));
                   DisposeStr(PS);
                   BreakpointCollection^.Insert(PB);
                   BreakpointCollection^.Update;
                 end;
            end;
        end;
      end;

  end;*)
  Tabs:=0;
  if assigned(ScopeView) then
    Tabs:=Tabs or (1 shl btScope);
  if assigned(ReferenceView) then
    Tabs:=Tabs or (1 shl btReferences);
  if assigned(InheritanceView) then
    Tabs:=Tabs or (1 shl btInheritance);
  if assigned(MemInfoView) then
    Tabs:=Tabs or (1 shl btMemInfo);
  if Assigned(Sym) then
    if (Pos('proc',Sym^.GetText)>0) or (Pos('var',Sym^.GetText)>0) then
      Tabs:=Tabs or (1 shl btBreakWatch);
  if PageTab<>nil then PageTab^.SetParams(Tabs,BrowserTab);
end;

function TBrowserWindow.GetPalette: PPalette;
const S: string[length(CBrowserWindow)] = CBrowserWindow;
begin
  GetPalette:=@S;
end;

procedure OpenSymbolBrowser(X,Y: Sw_integer;const Name,Line: string;S : PSymbol;
            Symbols: PSymbolCollection; References: PReferenceCollection;
            Inheritance: PObjectSymbol; MemInfo: PSymbolMemInfo);
var R: TRect;
begin
  if X=0 then X:=Desktop^.Size.X-35;
  R.A.X:=X; R.A.Y:=Y;
  R.B.X:=R.A.X+35; R.B.Y:=R.A.Y+15;
  while (R.B.Y>Desktop^.Size.Y) do R.Move(0,-1);
  Desktop^.Insert(New(PBrowserWindow, Init(R,
    'Browse: '+Name,SearchFreeWindowNo,S,Line,Symbols,References,Inheritance,MemInfo)));
end;

END.
{
  $Log$
  Revision 1.16  1999-06-17 23:44:01  pierre
   * problem with Inheritance list

  Revision 1.15  1999/04/15 08:58:06  peter
    * syntax highlight fixes
    * browser updates

  Revision 1.14  1999/04/07 21:55:53  peter
    + object support for browser
    * html help fixes
    * more desktop saving things
    * NODEBUG directive to exclude debugger

  Revision 1.13  1999/03/16 00:44:44  peter
    * forgotten in last commit :(

  Revision 1.12  1999/03/01 15:42:02  peter
    + Added dummy entries for functions not yet implemented
    * MenuBar didn't update itself automatically on command-set changes
    * Fixed Debugging/Profiling options dialog
    * TCodeEditor converts spaces to tabs at save only if efUseTabChars is
 set
    * efBackSpaceUnindents works correctly
    + 'Messages' window implemented
    + Added '$CAP MSG()' and '$CAP EDIT' to available tool-macros
    + Added TP message-filter support (for ex. you can call GREP thru
      GREP2MSG and view the result in the messages window - just like in TP)
    * A 'var' was missing from the param-list of THelpFacility.TopicSearch,
      so topic search didn't work...
    * In FPHELP.PAS there were still context-variables defined as word instead
      of THelpCtx
    * StdStatusKeys() was missing from the statusdef for help windows
    + Topic-title for index-table can be specified when adding a HTML-files

  Revision 1.11  1999/02/22 11:51:38  peter
    * browser updates from gabor

  Revision 1.9  1999/02/18 13:44:34  peter
    * search fixed
    + backward search
    * help fixes
    * browser updates

  Revision 1.7  1999/02/16 12:44:20  pierre
   * DoubleClick works now

  Revision 1.6  1999/02/10 09:44:59  pierre
    + added B tab for functions and vars for break/watch
      TBrowserWindow also stores the symbol itself for break/watchpoints

  Revision 1.5  1999/02/04 17:53:47  pierre
   + OpenOneSymbolBrowser

  Revision 1.4  1999/02/04 13:16:14  pierre
   + column info added

  Revision 1.3  1999/01/21 11:54:23  peter
    + tools menu
    + speedsearch in symbolbrowser
    * working run command

  Revision 1.2  1999/01/14 21:42:24  peter
    * source tracking from Gabor

  Revision 1.1  1999/01/12 14:29:40  peter
    + Implemented still missing 'switch' entries in Options menu
    + Pressing Ctrl-B sets ASCII mode in editor, after which keypresses (even
      ones with ASCII < 32 ; entered with Alt+<###>) are interpreted always as
      ASCII chars and inserted directly in the text.
    + Added symbol browser
    * splitted fp.pas to fpide.pas

  Revision 1.0  1999/01/09 11:49:41  gabor
     Original implementation
}
