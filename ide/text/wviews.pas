{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit WViews;

interface

uses Objects,Drivers,Views,Menus,Dialogs;

const
      evIdle                 = $8000;

      cmLocalMenu            = 54100;
      cmUpdate               = 54101;
      cmListFocusChanged     = 54102;

type
    PCenterDialog = ^TCenterDialog;
    TCenterDialog = object(TDialog)
      constructor Init(var Bounds: TRect; ATitle: TTitleStr);
    end;

    PAdvancedMenuBox = ^TAdvancedMenuBox;
    TAdvancedMenuBox = object(TMenuBox)
      function NewSubView(var Bounds: TRect; AMenu: PMenu;
                 AParentMenu: PMenuView): PMenuView; virtual;
      function Execute: Word; virtual;
    end;

    PAdvancedMenuPopUp = ^TAdvancedMenuPopup;
    TAdvancedMenuPopUp = object(TMenuPopup)
      function NewSubView(var Bounds: TRect; AMenu: PMenu;
                 AParentMenu: PMenuView): PMenuView; virtual;
      function Execute: Word; virtual;
    end;

    PAdvancedMenuBar = ^TAdvancedMenuBar;
    TAdvancedMenuBar = object(TMenuBar)
      constructor Init(var Bounds: TRect; AMenu: PMenu);
      function  NewSubView(var Bounds: TRect; AMenu: PMenu;
                  AParentMenu: PMenuView): PMenuView; virtual;
      procedure Update; virtual;
      procedure HandleEvent(var Event: TEvent); virtual;
      function  Execute: Word; virtual;
    end;

    PAdvancedStaticText = ^TAdvancedStaticText;
    TAdvancedStaticText = object(TStaticText)
      procedure SetText(S: string); virtual;
    end;

    PAdvancedListBox = ^TAdvancedListBox;
    TAdvancedListBox = object(TListBox)
      Default: boolean;
      procedure FocusItem(Item: sw_integer); virtual;
      procedure HandleEvent(var Event: TEvent); virtual;
    end;

    TLocalMenuListBox = object(TAdvancedListBox)
      procedure   HandleEvent(var Event: TEvent); virtual;
      procedure   LocalMenu(P: TPoint); virtual;
      function    GetLocalMenu: PMenu; virtual;
      function    GetCommandTarget: PView; virtual;
    private
      LastLocalCmd: word;
    end;

    PColorStaticText = ^TColorStaticText;
    TColorStaticText = object(TAdvancedStaticText)
      Color: word;
      DontWrap: boolean;
      Delta: TPoint;
      constructor Init(var Bounds: TRect; AText: String; AColor: word);
      procedure   Draw; virtual;
    end;

    PHSListBox = ^THSListBox;
    THSListBox = object(TLocalMenuListBox)
      constructor Init(var Bounds: TRect; ANumCols: Word; AHScrollBar, AVScrollBar: PScrollBar);
    end;

    PDlgWindow = ^TDlgWindow;
    TDlgWindow = object(TDialog)
      constructor Init(var Bounds: TRect; ATitle: TTitleStr; ANumber: Integer);
    end;

    PAdvancedStatusLine = ^TAdvancedStatusLine;
    TAdvancedStatusLine = object(TStatusLine)
      StatusText: PString;
      function  GetStatusText: string; virtual;
      procedure SetStatusText(const S: string); virtual;
      procedure ClearStatusText; virtual;
      procedure Draw; virtual;
    end;

procedure InsertOK(ADialog: PDialog);
procedure InsertButtons(ADialog: PDialog);

procedure ErrorBox(const S: string; Params: pointer);
procedure WarningBox(const S: string; Params: pointer);
procedure InformationBox(const S: string; Params: pointer);
function  ConfirmBox(const S: string; Params: pointer; CanCancel: boolean): word;

procedure ShowMessage(Msg: string);
procedure HideMessage;

function  SearchMenuItem(Menu: PMenu; Cmd: word): PMenuItem;
procedure SetMenuItemParam(Menu: PMenuItem; Param: string);
function  IsSubMenu(P: PMenuItem): boolean;
function  IsSeparator(P: PMenuItem): boolean;
function  UpdateMenu(M: PMenu): boolean;
function  SearchSubMenu(M: PMenu; Index: integer): PMenuItem;
procedure AppendMenuItem(M: PMenu; I: PMenuItem);
procedure RemoveMenuItem(Menu: PMenu; I: PMenuItem);
function  GetMenuItemBefore(Menu:PMenu; BeforeOf: PMenuItem): PMenuItem;

procedure NotImplemented;

implementation

uses Commands,App,MsgBox;

const
  MessageDialog  : PCenterDialog = nil;

{*****************************************************************************
                              TCenterDialog
*****************************************************************************}

constructor TCenterDialog.Init(var Bounds: TRect; ATitle: TTitleStr);
begin
  inherited Init(Bounds,ATitle);
  Options:=Options or ofCentered;
end;

function TAdvancedMenuBox.NewSubView(var Bounds: TRect; AMenu: PMenu;
  AParentMenu: PMenuView): PMenuView;
begin
  NewSubView := New(PAdvancedMenuBox, Init(Bounds, AMenu, AParentMenu));
end;

function TAdvancedMenuBox.Execute: word;
type
  MenuAction = (DoNothing, DoSelect, DoReturn);
var
  AutoSelect: Boolean;
  Action: MenuAction;
  Ch: Char;
  Result: Word;
  ItemShown, P: PMenuItem;
  Target: PMenuView;
  R: TRect;
  E: TEvent;
  MouseActive: Boolean;
function IsDisabled(Item: PMenuItem): boolean;
var Found: boolean;
begin
  Found:=Item^.Disabled or IsSeparator(Item);
  if (Found=false) and (IsSubMenu(Item)=false) then
     Found:=CommandEnabled(Item^.Command)=false;
  IsDisabled:=Found;
end;

procedure TrackMouse;
var
  Mouse: TPoint;
  R: TRect;
  OldC: PMenuItem;
begin
  MakeLocal(E.Where, Mouse);
  OldC:=Current;
  Current := Menu^.Items;
  while Current <> nil do
  begin
    GetItemRect(Current, R);
    if R.Contains(Mouse) then
    begin
      MouseActive := True;
      Break;
    end;
    Current := Current^.Next;
  end;
  if (Current<>nil) and IsDisabled(Current) then
  begin
     Current:={OldC}nil;
     MouseActive:=false;
  end;
end;

procedure TrackKey(FindNext: Boolean);

procedure NextItem;
begin
  Current := Current^.Next;
  if Current = nil then Current := Menu^.Items;
end;

procedure PrevItem;
var
  P: PMenuItem;
begin
  P := Current;
  if P = Menu^.Items then P := nil;
  repeat NextItem until Current^.Next = P;
end;

begin
  if Current <> nil then
    repeat
      if FindNext then NextItem else PrevItem;
    until (Current^.Name <> nil) and (IsDisabled(Current)=false);
end;

function MouseInOwner: Boolean;
var
  Mouse: TPoint;
  R: TRect;
begin
  MouseInOwner := False;
  if (ParentMenu <> nil) and (ParentMenu^.Size.Y = 1) then
  begin
    ParentMenu^.MakeLocal(E.Where, Mouse);
    ParentMenu^.GetItemRect(ParentMenu^.Current, R);
    MouseInOwner := R.Contains(Mouse);
  end;
end;

function MouseInMenus: Boolean;
var
  P: PMenuView;
begin
  P := ParentMenu;
  while (P <> nil) and (P^.MouseInView(E.Where)=false) do
        P := P^.ParentMenu;
  MouseInMenus := P <> nil;
end;

function TopMenu: PMenuView;
var
  P: PMenuView;
begin
  P := @Self;
  while P^.ParentMenu <> nil do P := P^.ParentMenu;
  TopMenu := P;
end;

begin
  AutoSelect := False; E.What:=evNothing;
  Result := 0;
  ItemShown := nil;
  Current := Menu^.Default;
  MouseActive := False;
  if UpdateMenu(Menu) then
 begin
  if Current<>nil then
    if Current^.Disabled then
       TrackKey(true);
  repeat
    Action := DoNothing;
    GetEvent(E);
    case E.What of
      evMouseDown:
        if MouseInView(E.Where) or MouseInOwner then
        begin
          TrackMouse;
          if Size.Y = 1 then AutoSelect := True;
        end else Action := DoReturn;
      evMouseUp:
        begin
          TrackMouse;
          if MouseInOwner then
            Current := Menu^.Default
          else
            if (Current <> nil) and (Current^.Name <> nil) then
              Action := DoSelect
            else
              if MouseActive or MouseInView(E.Where) then Action := DoReturn
              else
              begin
                Current := Menu^.Default;
                if Current = nil then Current := Menu^.Items;
                Action := DoNothing;
              end;
        end;
      evMouseMove:
        if E.Buttons <> 0 then
        begin
          TrackMouse;
          if not (MouseInView(E.Where) or MouseInOwner) and
            MouseInMenus then Action := DoReturn;
        end;
      evKeyDown:
        case CtrlToArrow(E.KeyCode) of
          kbUp, kbDown:
            if Size.Y <> 1 then
              TrackKey(CtrlToArrow(E.KeyCode) = kbDown) else
              if E.KeyCode = kbDown then AutoSelect := True;
          kbLeft, kbRight:
            if ParentMenu = nil then
              TrackKey(CtrlToArrow(E.KeyCode) = kbRight) else
              Action := DoReturn;
          kbHome, kbEnd:
            if Size.Y <> 1 then
            begin
              Current := Menu^.Items;
              if E.KeyCode = kbEnd then TrackKey(False);
            end;
          kbEnter:
            begin
              if Size.Y = 1 then AutoSelect := True;
              Action := DoSelect;
            end;
          kbEsc:
            begin
              Action := DoReturn;
              if (ParentMenu = nil) or (ParentMenu^.Size.Y <> 1) then
                ClearEvent(E);
            end;
        else
          Target := @Self;
          Ch := GetAltChar(E.KeyCode);
          if Ch = #0 then Ch := E.CharCode else Target := TopMenu;
          P := Target^.FindItem(Ch);
          if P = nil then
          begin
            P := TopMenu^.HotKey(E.KeyCode);
            if (P <> nil) and CommandEnabled(P^.Command) then
            begin
              Result := P^.Command;
              Action := DoReturn;
            end
          end else
            if Target = @Self then
            begin
              if Size.Y = 1 then AutoSelect := True;
              Action := DoSelect;
              Current := P;
            end else
              if (ParentMenu <> Target) or (ParentMenu^.Current <> P) then
                Action := DoReturn;
        end;
      evCommand:
        if E.Command = cmMenu then
        begin
          AutoSelect := False;
          if ParentMenu <> nil then Action := DoReturn;
        end else Action := DoReturn;
    end;
    if ItemShown <> Current then
    begin
      ItemShown := Current;
      DrawView;
    end;
    if (Action = DoSelect) or ((Action = DoNothing) and AutoSelect) then
      if Current <> nil then with Current^ do if Name <> nil then
        if Command = 0 then
        begin
          if E.What and (evMouseDown + evMouseMove) <> 0 then PutEvent(E);
          GetItemRect(Current, R);
          R.A.X := R.A.X + Origin.X;
          R.A.Y := R.B.Y + Origin.Y;
          R.B := Owner^.Size;
          if Size.Y = 1 then Dec(R.A.X);
          Target := TopMenu^.NewSubView(R, SubMenu, @Self);
          Result := Owner^.ExecView(Target);
          Dispose(Target, Done);
        end else if Action = DoSelect then Result := Command;
    if (Result <> 0) and CommandEnabled(Result) then
    begin
      Action := DoReturn;
      ClearEvent(E);
    end
    else
      Result := 0;
  until Action = DoReturn;
 end;
  if E.What <> evNothing then
    if (ParentMenu <> nil) or (E.What = evCommand) then PutEvent(E);
  if Current <> nil then
  begin
    Menu^.Default := Current;
    Current := nil;
    DrawView;
  end;
  Execute := Result;
end;

function TAdvancedMenuPopup.NewSubView(var Bounds: TRect; AMenu: PMenu;
  AParentMenu: PMenuView): PMenuView;
begin
  NewSubView := New(PAdvancedMenuBox, Init(Bounds, AMenu, AParentMenu));
end;

function TAdvancedMenuPopup.Execute: word;
type
  MenuAction = (DoNothing, DoSelect, DoReturn);
var
  AutoSelect: Boolean;
  Action: MenuAction;
  Ch: Char;
  Result: Word;
  ItemShown, P: PMenuItem;
  Target: PMenuView;
  R: TRect;
  E: TEvent;
  MouseActive: Boolean;
function IsDisabled(Item: PMenuItem): boolean;
var Found: boolean;
begin
  Found:=Item^.Disabled or IsSeparator(Item);
  if (Found=false) and (IsSubMenu(Item)=false) then
     Found:=CommandEnabled(Item^.Command)=false;
  IsDisabled:=Found;
end;

procedure TrackMouse;
var
  Mouse: TPoint;
  R: TRect;
  OldC: PMenuItem;
begin
  MakeLocal(E.Where, Mouse);
  OldC:=Current;
  Current := Menu^.Items;
  while Current <> nil do
  begin
    GetItemRect(Current, R);
    if R.Contains(Mouse) then
    begin
      MouseActive := True;
      Break;
    end;
    Current := Current^.Next;
  end;
  if (Current<>nil) and IsDisabled(Current) then
  begin
     Current:={OldC}nil;
     MouseActive:=false;
  end;
end;

procedure TrackKey(FindNext: Boolean);

procedure NextItem;
begin
  Current := Current^.Next;
  if Current = nil then Current := Menu^.Items;
end;

procedure PrevItem;
var
  P: PMenuItem;
begin
  P := Current;
  if P = Menu^.Items then P := nil;
  repeat NextItem until Current^.Next = P;
end;

begin
  if Current <> nil then
    repeat
      if FindNext then NextItem else PrevItem;
    until (Current^.Name <> nil) and (IsDisabled(Current)=false);
end;

function MouseInOwner: Boolean;
var
  Mouse: TPoint;
  R: TRect;
begin
  MouseInOwner := False;
  if (ParentMenu <> nil) and (ParentMenu^.Size.Y = 1) then
  begin
    ParentMenu^.MakeLocal(E.Where, Mouse);
    ParentMenu^.GetItemRect(ParentMenu^.Current, R);
    MouseInOwner := R.Contains(Mouse);
  end;
end;

function MouseInMenus: Boolean;
var
  P: PMenuView;
begin
  P := ParentMenu;
  while (P <> nil) and (P^.MouseInView(E.Where)=false) do
        P := P^.ParentMenu;
  MouseInMenus := P <> nil;
end;

function TopMenu: PMenuView;
var
  P: PMenuView;
begin
  P := @Self;
  while P^.ParentMenu <> nil do P := P^.ParentMenu;
  TopMenu := P;
end;

begin
  AutoSelect := False; E.What:=evNothing;
  Result := 0;
  ItemShown := nil;
  Current := Menu^.Default;
  MouseActive := False;
  if UpdateMenu(Menu) then
 begin
  if Current<>nil then
    if Current^.Disabled then
       TrackKey(true);
  repeat
    Action := DoNothing;
    GetEvent(E);
    case E.What of
      evMouseDown:
        if MouseInView(E.Where) or MouseInOwner then
        begin
          TrackMouse;
          if Size.Y = 1 then AutoSelect := True;
        end else Action := DoReturn;
      evMouseUp:
        begin
          TrackMouse;
          if MouseInOwner then
            Current := Menu^.Default
          else
            if (Current <> nil) and (Current^.Name <> nil) then
              Action := DoSelect
            else
              if MouseActive or MouseInView(E.Where) then Action := DoReturn
              else
              begin
                Current := Menu^.Default;
                if Current = nil then Current := Menu^.Items;
                Action := DoNothing;
              end;
        end;
      evMouseMove:
        if E.Buttons <> 0 then
        begin
          TrackMouse;
          if not (MouseInView(E.Where) or MouseInOwner) and
            MouseInMenus then Action := DoReturn;
        end;
      evKeyDown:
        case CtrlToArrow(E.KeyCode) of
          kbUp, kbDown:
            if Size.Y <> 1 then
              TrackKey(CtrlToArrow(E.KeyCode) = kbDown) else
              if E.KeyCode = kbDown then AutoSelect := True;
          kbLeft, kbRight:
            if ParentMenu = nil then
              TrackKey(CtrlToArrow(E.KeyCode) = kbRight) else
              Action := DoReturn;
          kbHome, kbEnd:
            if Size.Y <> 1 then
            begin
              Current := Menu^.Items;
              if E.KeyCode = kbEnd then TrackKey(False);
            end;
          kbEnter:
            begin
              if Size.Y = 1 then AutoSelect := True;
              Action := DoSelect;
            end;
          kbEsc:
            begin
              Action := DoReturn;
              if (ParentMenu = nil) or (ParentMenu^.Size.Y <> 1) then
                ClearEvent(E);
            end;
        else
          Target := @Self;
          Ch := GetAltChar(E.KeyCode);
          if Ch = #0 then Ch := E.CharCode else Target := TopMenu;
          P := Target^.FindItem(Ch);
          if P = nil then
          begin
            P := TopMenu^.HotKey(E.KeyCode);
            if (P <> nil) and CommandEnabled(P^.Command) then
            begin
              Result := P^.Command;
              Action := DoReturn;
            end
          end else
            if Target = @Self then
            begin
              if Size.Y = 1 then AutoSelect := True;
              Action := DoSelect;
              Current := P;
            end else
              if (ParentMenu <> Target) or (ParentMenu^.Current <> P) then
                Action := DoReturn;
        end;
      evCommand:
        if E.Command = cmMenu then
        begin
          AutoSelect := False;
          if ParentMenu <> nil then Action := DoReturn;
        end else Action := DoReturn;
    end;
    if ItemShown <> Current then
    begin
      ItemShown := Current;
      DrawView;
    end;
    if (Action = DoSelect) or ((Action = DoNothing) and AutoSelect) then
      if Current <> nil then with Current^ do if Name <> nil then
        if Command = 0 then
        begin
          if E.What and (evMouseDown + evMouseMove) <> 0 then PutEvent(E);
          GetItemRect(Current, R);
          R.A.X := R.A.X + Origin.X;
          R.A.Y := R.B.Y + Origin.Y;
          R.B := Owner^.Size;
          if Size.Y = 1 then Dec(R.A.X);
          Target := TopMenu^.NewSubView(R, SubMenu, @Self);
          Result := Owner^.ExecView(Target);
          Dispose(Target, Done);
        end else if Action = DoSelect then Result := Command;
    if (Result <> 0) and CommandEnabled(Result) then
    begin
      Action := DoReturn;
      ClearEvent(E);
    end
    else
      Result := 0;
  until Action = DoReturn;
 end;
  if E.What <> evNothing then
    if (ParentMenu <> nil) or (E.What = evCommand) then PutEvent(E);
  if Current <> nil then
  begin
    Menu^.Default := Current;
    Current := nil;
    DrawView;
  end;
  Execute := Result;
end;

constructor TAdvancedMenuBar.Init(var Bounds: TRect; AMenu: PMenu);
begin
  inherited Init(Bounds, AMenu);
  EventMask:=EventMask or evBroadcast;
end;

function TAdvancedMenuBar.NewSubView(var Bounds: TRect; AMenu: PMenu;
  AParentMenu: PMenuView): PMenuView;
begin
  NewSubView := New(PAdvancedMenuBox, Init(Bounds, AMenu, AParentMenu));
end;

procedure TAdvancedMenuBar.Update;
begin
  UpdateMenu(Menu);
  DrawView;
end;

procedure TAdvancedMenuBar.HandleEvent(var Event: TEvent);
begin
  case Event.What of
    evBroadcast :
      case Event.Command of
        cmCommandSetChanged : Update;
        cmUpdate            : Update;
      end;
  end;
  inherited HandleEvent(Event);
end;

function TAdvancedMenuBar.Execute: word;
type
  MenuAction = (DoNothing, DoSelect, DoReturn);
var
  AutoSelect: Boolean;
  Action: MenuAction;
  Ch: Char;
  Result: Word;
  ItemShown, P: PMenuItem;
  Target: PMenuView;
  R: TRect;
  E: TEvent;
  MouseActive: Boolean;
function IsDisabled(Item: PMenuItem): boolean;
var Dis : boolean;
begin
  Dis:=Item^.Disabled or IsSeparator(Item);
  if (Dis=false) and (IsSubMenu(Item)=false) then
     Dis:=CommandEnabled(Item^.Command)=false;
  IsDisabled:=Dis;
end;

procedure TrackMouse;
var
  Mouse: TPoint;
  R: TRect;
  OldC: PMenuItem;
begin
  MakeLocal(E.Where, Mouse);
  OldC:=Current;
  Current := Menu^.Items;
  while Current <> nil do
  begin
    GetItemRect(Current, R);
    if R.Contains(Mouse) then
    begin
      MouseActive := True;
      Break;
    end;
    Current := Current^.Next;
  end;
  if (Current<>nil) and IsDisabled(Current) then
    Current:=nil;
end;

procedure TrackKey(FindNext: Boolean);

procedure NextItem;
begin
  Current := Current^.Next;
  if Current = nil then Current := Menu^.Items;
end;

procedure PrevItem;
var
  P: PMenuItem;
begin
  P := Current;
  if P = Menu^.Items then P := nil;
  repeat NextItem until Current^.Next = P;
end;

begin
  if Current <> nil then
    repeat
      if FindNext then NextItem else PrevItem;
    until (Current^.Name <> nil) and (IsDisabled(Current)=false);
end;

function MouseInOwner: Boolean;
var
  Mouse: TPoint;
  R: TRect;
begin
  MouseInOwner := False;
  if (ParentMenu <> nil) and (ParentMenu^.Size.Y = 1) then
  begin
    ParentMenu^.MakeLocal(E.Where, Mouse);
    ParentMenu^.GetItemRect(ParentMenu^.Current, R);
    MouseInOwner := R.Contains(Mouse);
  end;
end;

function MouseInMenus: Boolean;
var
  P: PMenuView;
begin
  P := ParentMenu;
  while (P <> nil) and not P^.MouseInView(E.Where) do P := P^.ParentMenu;
  MouseInMenus := P <> nil;
end;

function TopMenu: PMenuView;
var
  P: PMenuView;
begin
  P := @Self;
  while P^.ParentMenu <> nil do P := P^.ParentMenu;
  TopMenu := P;
end;

begin
  AutoSelect := False; E.What:=evNothing;
  Result := 0;
  ItemShown := nil;
  Current := Menu^.Default;
  MouseActive := False;
  if UpdateMenu(Menu) then
 begin
  if Current<>nil then
    if Current^.Disabled then
       TrackKey(true);
  repeat
    Action := DoNothing;
    GetEvent(E);
    case E.What of
      evMouseDown:
        if MouseInView(E.Where) or MouseInOwner then
        begin
          TrackMouse;
          if Size.Y = 1 then AutoSelect := True;
        end else Action := DoReturn;
      evMouseUp:
        begin
          TrackMouse;
          if MouseInOwner then
            Current := Menu^.Default
          else
            if (Current <> nil) and (Current^.Name <> nil) then
              Action := DoSelect
            else
              if MouseActive or MouseInView(E.Where) then Action := DoReturn
              else
              begin
                Current := Menu^.Default;
                if Current = nil then Current := Menu^.Items;
                Action := DoNothing;
              end;
        end;
      evMouseMove:
        if E.Buttons <> 0 then
        begin
          TrackMouse;
          if not (MouseInView(E.Where) or MouseInOwner) and
            MouseInMenus then Action := DoReturn;
        end;
      evKeyDown:
        case CtrlToArrow(E.KeyCode) of
          kbUp, kbDown:
            if Size.Y <> 1 then
              TrackKey(CtrlToArrow(E.KeyCode) = kbDown) else
              if E.KeyCode = kbDown then AutoSelect := True;
          kbLeft, kbRight:
            if ParentMenu = nil then
              TrackKey(CtrlToArrow(E.KeyCode) = kbRight) else
              Action := DoReturn;
          kbHome, kbEnd:
            if Size.Y <> 1 then
            begin
              Current := Menu^.Items;
              if E.KeyCode = kbEnd then TrackKey(False);
            end;
          kbEnter:
            begin
              if Size.Y = 1 then AutoSelect := True;
              Action := DoSelect;
            end;
          kbEsc:
            begin
              Action := DoReturn;
              if (ParentMenu = nil) or (ParentMenu^.Size.Y <> 1) then
                ClearEvent(E);
            end;
        else
          Target := @Self;
          Ch := GetAltChar(E.KeyCode);
          if Ch = #0 then Ch := E.CharCode else Target := TopMenu;
          P := Target^.FindItem(Ch);
          if P = nil then
          begin
            P := TopMenu^.HotKey(E.KeyCode);
            if (P <> nil) and CommandEnabled(P^.Command) then
            begin
              Result := P^.Command;
              Action := DoReturn;
            end
          end else
            if Target = @Self then
            begin
              if Size.Y = 1 then AutoSelect := True;
              Action := DoSelect;
              Current := P;
            end else
              if (ParentMenu <> Target) or (ParentMenu^.Current <> P) then
                Action := DoReturn;
        end;
      evCommand:
        if E.Command = cmMenu then
        begin
          AutoSelect := False;
          if ParentMenu <> nil then Action := DoReturn;
        end else Action := DoReturn;
    end;
    if ItemShown <> Current then
    begin
      ItemShown := Current;
      DrawView;
    end;
    if (Action = DoSelect) or ((Action = DoNothing) and AutoSelect) then
      if Current <> nil then with Current^ do if Name <> nil then
        if Command = 0 then
        begin
          if E.What and (evMouseDown + evMouseMove) <> 0 then PutEvent(E);
          GetItemRect(Current, R);
          R.A.X := R.A.X + Origin.X;
          R.A.Y := R.B.Y + Origin.Y;
          R.B := Owner^.Size;
          if Size.Y = 1 then Dec(R.A.X);
          Target := TopMenu^.NewSubView(R, SubMenu, @Self);
          Result := Owner^.ExecView(Target);
          Dispose(Target, Done);
        end else if Action = DoSelect then Result := Command;
    if (Result <> 0) and CommandEnabled(Result) then
    begin
      Action := DoReturn;
      ClearEvent(E);
    end
    else
      Result := 0;
  until Action = DoReturn;
 end;
  if E.What <> evNothing then
    if (ParentMenu <> nil) or (E.What = evCommand) then PutEvent(E);
  if Current <> nil then
  begin
    Menu^.Default := Current;
    Current := nil;
    DrawView;
  end;
  Execute := Result;
end;

procedure TAdvancedStaticText.SetText(S: string);
begin
  if Text<>nil then DisposeStr(Text);
  Text:=NewStr(S);
  DrawView;
end;

procedure TAdvancedListBox.FocusItem(Item: sw_integer);
begin
  inherited FocusItem(Item);
  Message(Owner,evBroadcast,cmListFocusChanged,@Self);
end;

procedure TAdvancedListBox.HandleEvent(var Event: TEvent);
begin
  case Event.What of
    evMouseDown :
      if MouseInView(Event.Where) and (Event.Double) then
      begin
        inherited HandleEvent(Event);
        if Range>Focused then SelectItem(Focused);
      end;
    evBroadcast :
      case Event.Command of
        cmListItemSelected :
          Message(Owner,evBroadcast,cmDefault,nil);
      end;
  end;
  inherited HandleEvent(Event);
end;

constructor TColorStaticText.Init(var Bounds: TRect; AText: String; AColor: word);
begin
  inherited Init(Bounds,AText);
  Color:=AColor;
end;

procedure TColorStaticText.Draw;
var
  C: word;
  Center: Boolean;
  I, J, L, P, Y: Integer;
  B: TDrawBuffer;
  S: String;
  T: string;
  CurS: string;
  TildeCount,Po: integer;
  TempS: string;
begin
  if Size.X=0 then Exit;
  if DontWrap=false then
 begin
  C:=Color;
  GetText(S);
  L := Length(S);
  P := 1;
  Y := 0;
  Center := False;
  while Y < Size.Y do
  begin
    MoveChar(B, ' ', Lo(C), Size.X);
    if P <= L then
    begin
      if S[P] = #3 then
      begin
        Center := True;
        Inc(P);
      end;
      I := P;
      repeat
        J := P;
        while (P <= L) and (S[P] = ' ') do Inc(P);
        while (P <= L) and (S[P] <> ' ') and (S[P] <> #13) do Inc(P);
      until (P > L) or (P >= I + Size.X) or (S[P] = #13);
      TildeCount:=0; TempS:=copy(S,I,P-I);
      repeat
        Po:=Pos('~',TempS);
        if Po>0 then begin Inc(TildeCount); Delete(TempS,1,Po); end;
      until Po=0;
      if P > I + Size.X + TildeCount then
        if J > I then P := J else P := I + Size.X;
      T:=copy(S,I,P-I);
      if Center then J := (Size.X - {P + I}CStrLen(T)) div 2 else J := 0;
      MoveCStr(B[J],T,C);
      while (P <= L) and (S[P] = ' ') do Inc(P);
      if (P <= L) and (S[P] = #13) then
      begin
        Center := False;
        Inc(P);
        if (P <= L) and (S[P] = #10) then Inc(P);
      end;
    end;
    WriteLine(0, Y, Size.X, 1, B);
    Inc(Y);
  end;
 end { Wrap=false } else
 begin
  C := Color;
  GetText(S);
  I:=1;
  for Y:=0 to Size.Y-1 do
  begin
    MoveChar(B, ' ', Lo(C), Size.X);
    CurS:='';
    if S<>'' then
    begin
    P:=Pos(#13,S);
    if P=0 then P:=length(S)+1;
    CurS:=copy(S,1,P-1);
    CurS:=copy(CurS,Delta.X+1,255);
    CurS:=copy(CurS,1,MaxViewWidth);
    Delete(S,1,P);
    end;
    if CurS<>'' then MoveCStr(B,CurS,C);
    WriteLine(0,Y,Size.X,1,B);
  end;
 end;
end;

constructor THSListBox.Init(var Bounds: TRect; ANumCols: Word; AHScrollBar, AVScrollBar: PScrollBar);
begin
  inherited Init(Bounds,ANumCols,AVScrollBar);
  HScrollBar:=AHScrollBar;
end;

constructor TDlgWindow.Init(var Bounds: TRect; ATitle: TTitleStr; ANumber: Integer);
begin
  inherited Init(Bounds,ATitle);
  Number:=ANumber;
  Flags:=Flags or (wfMove + wfGrow + wfClose + wfZoom);
end;

procedure TLocalMenuListBox.LocalMenu(P: TPoint);
var M: PMenu;
    MV: PAdvancedMenuPopUp;
    R: TRect;
    Re: word;
begin
  M:=GetLocalMenu;
  if M=nil then Exit;
  if LastLocalCmd<>0 then
     M^.Default:=SearchMenuItem(M,LastLocalCmd);
  Desktop^.GetExtent(R);
  MakeGlobal(P,R.A); {Desktop^.MakeLocal(R.A,R.A);}
  New(MV, Init(R, M));
  Re:=Application^.ExecView(MV);
  if M^.Default=nil then LastLocalCmd:=0
     else LastLocalCmd:=M^.Default^.Command;
  Dispose(MV, Done);
  if Re<>0 then
    Message(GetCommandTarget,evCommand,Re,@Self);
end;

function TLocalMenuListBox.GetLocalMenu: PMenu;
begin
  GetLocalMenu:=nil;
  Abstract;
end;

function TLocalMenuListBox.GetCommandTarget: PView;
begin
  GetCommandTarget:=@Self;
end;

procedure TLocalMenuListBox.HandleEvent(var Event: TEvent);
var DontClear: boolean;
    P: TPoint;
begin
  case Event.What of
    evMouseDown :
      if MouseInView(Event.Where) and (Event.Buttons=mbRightButton) then
        begin
          MakeLocal(Event.Where,P); Inc(P.X); Inc(P.Y);
          LocalMenu(P);
          ClearEvent(Event);
        end;
    evKeyDown :
      begin
        DontClear:=false;
        case Event.KeyCode of
          kbAltF10 : Message(@Self,evCommand,cmLocalMenu,@Self);
        else DontClear:=true;
        end;
        if DontClear=false then ClearEvent(Event);
      end;
    evCommand :
      begin
        DontClear:=false;
        case Event.Command of
          cmLocalMenu :
            begin
              P:=Cursor; Inc(P.X); Inc(P.Y);
              LocalMenu(P);
            end;
        else DontClear:=true;
        end;
        if not DontClear then ClearEvent(Event);
      end;
  end;
  inherited HandleEvent(Event);
end;

function TAdvancedStatusLine.GetStatusText: string;
var S: string;
begin
  if StatusText=nil then S:='' else S:=StatusText^;
  GetStatusText:=S;
end;

procedure TAdvancedStatusLine.SetStatusText(const S: string);
begin
  if StatusText<>nil then DisposeStr(StatusText);
  StatusText:=NewStr(S);
  DrawView;
end;

procedure TAdvancedStatusLine.ClearStatusText;
begin
  SetStatusText('');
end;

procedure TAdvancedStatusLine.Draw;
var B: TDrawBuffer;
    C: word;
    S: string;
begin
  S:=GetStatusText;
  if S='' then inherited Draw else
  begin
    C:=GetColor(1);
    MoveChar(B,' ',C,Size.X);
    MoveStr(B[1],S,C);
    WriteLine(0,0,Size.X,Size.Y,B);
  end;
end;


procedure ErrorBox(const S: string; Params: pointer);
begin
  MessageBox(S,Params,mfError+mfInsertInApp+mfOKButton);
end;

procedure WarningBox(const S: string; Params: pointer);
begin
  MessageBox(S,Params,mfWarning+mfInsertInApp+mfOKButton);
end;

procedure InformationBox(const S: string; Params: pointer);
begin
  MessageBox(S,Params,mfInformation+mfInsertInApp+mfOKButton);
end;

function ConfirmBox(const S: string; Params: pointer; CanCancel: boolean): word;
begin
  ConfirmBox:=MessageBox(S,Params,mfConfirmation+mfInsertInApp+mfYesButton+mfNoButton+integer(CanCancel)*mfCancelButton);
end;

function IsSeparator(P: PMenuItem): boolean;
begin
  IsSeparator:=(P<>nil) and (P^.Name=nil) and (P^.HelpCtx=hcNoContext);
end;

function IsSubMenu(P: PMenuItem): boolean;
begin
  IsSubMenu:=(P<>nil) and (P^.Name<>nil) and (P^.Command=0) and (P^.SubMenu<>nil);
end;

function SearchMenuItem(Menu: PMenu; Cmd: word): PMenuItem;
var P,I: PMenuItem;
begin
  I:=nil;
  if Menu=nil then P:=nil else P:=Menu^.Items;
  while (P<>nil) and (I=nil) do
  begin
    if IsSubMenu(P) then
       I:=SearchMenuItem(P^.SubMenu,Cmd);
    if I=nil then
    if P^.Command=Cmd then I:=P else
    P:=P^.Next;
  end;
  SearchMenuItem:=I;
end;

procedure SetMenuItemParam(Menu: PMenuItem; Param: string);
begin
  if Menu=nil then Exit;
  if Menu^.Param<>nil then DisposeStr(Menu^.Param);
  Menu^.Param:=NewStr(Param);
end;

function UpdateMenu(M: PMenu): boolean;
var P: PMenuItem;
    IsEnabled: boolean;
begin
  if M=nil then begin UpdateMenu:=false; Exit; end;
  P:=M^.Items; IsEnabled:=false;
  while (P<>nil) do
  begin
    if IsSubMenu(P) then
       P^.Disabled:=not UpdateMenu(P^.SubMenu);
    if (IsSeparator(P)=false) and (P^.Disabled=false) and (Application^.CommandEnabled(P^.Command)=true) then
       IsEnabled:=true;
    P:=P^.Next;
  end;
  UpdateMenu:=IsEnabled;
end;

function SearchSubMenu(M: PMenu; Index: integer): PMenuItem;
var P,C: PMenuItem;
    Count: integer;
begin
  P:=nil; Count:=-1;
  if M<>nil then C:=M^.Items else C:=nil;
  while (C<>nil) and (P=nil) do
  begin
    if IsSubMenu(C) then
     begin
       Inc(Count);
       if Count=Index then P:=C;
     end;
    C:=C^.Next;
  end;
  SearchSubMenu:=P;
end;

procedure AppendMenuItem(M: PMenu; I: PMenuItem);
var P: PMenuItem;
begin
  if (M=nil) or (I=nil) then Exit;
  I^.Next:=nil;
  if M^.Items=nil then M^.Items:=I else
  begin
    P:=M^.Items;
    while (P^.Next<>nil) do P:=P^.Next;
    P^.Next:=I;
  end;
end;

procedure DisposeMenuItem(P: PMenuItem);
begin
  if P<>nil then
  begin
    if IsSubMenu(P) then DisposeMenu(P^.SubMenu) else
      if IsSeparator(P)=false then
       if P^.Param<>nil then DisposeStr(P^.Param);
    if P^.Name<>nil then DisposeStr(P^.Name);
    Dispose(P);
  end;
end;

procedure RemoveMenuItem(Menu: PMenu; I: PMenuItem);
var P,PrevP: PMenuItem;
begin
  if (Menu=nil) or (I=nil) then Exit;
  P:=Menu^.Items; PrevP:=nil;
  while (P<>nil) do
  begin
    if P=I then
      begin
        if Menu^.Items<>I then PrevP^.Next:=P^.Next
                          else Menu^.Items:=P^.Next;
        DisposeMenuItem(P);
        Break;
      end;
    PrevP:=P; P:=P^.Next;
  end;
end;

function GetMenuItemBefore(Menu: PMenu; BeforeOf: PMenuItem): PMenuItem;
var P,C: PMenuItem;
begin
  P:=nil;
  if Menu<>nil then C:=Menu^.Items else C:=nil;
  while (C<>nil) do
    begin
      if C^.Next=BeforeOf then begin P:=C; Break; end;
      C:=C^.Next;
    end;
  GetMenuItemBefore:=P;
end;

procedure NotImplemented;
begin
  InformationBox('This function is not yet implemented...',nil);
end;

procedure InsertButtons(ADialog: PDialog);
var R   : TRect;
    W,H : integer;
    X   : integer;
    X1,X2: Sw_integer;
begin
  with ADialog^ do
  begin
    GetExtent(R);
    W:=R.B.X-R.A.X; H:=(R.B.Y-R.A.Y);
    R.Assign(0,0,W,H+3); ChangeBounds(R);
    X:=W div 2; X1:=X div 2+1; X2:=X+X1-1;
    R.Assign(X1-3,H,X1+7,H+2);
    Insert(New(PButton, Init(R, 'O~K~', cmOK, bfDefault)));
    R.Assign(X2-7,H,X2+3,H+2);
    Insert(New(PButton, Init(R, 'Cancel', cmCancel, bfNormal)));
    SelectNext(true);
  end;
end;

procedure InsertOK(ADialog: PDialog);
var BW: Sw_integer;
    R: TRect;
begin
  with ADialog^ do
  begin
    GetBounds(R); R.Grow(0,1); Inc(R.B.Y);
    ChangeBounds(R);
    BW:=10;
    R.A.Y:=R.B.Y-2; R.B.Y:=R.A.Y+2;
    R.A.X:=R.A.X+(R.B.X-R.A.X-BW) div 2; R.B.X:=R.A.X+BW;
    Insert(New(PButton, Init(R, 'O~K~', cmOK, bfDefault)));
    SelectNext(true);
  end;
end;

procedure ShowMessage(Msg: string);
var R: TRect;
    Width: integer;
begin
  Width:=length(Msg)+4*2;
  if Width<(Desktop^.Size.X div 2) then Width:=(Desktop^.Size.X div 2);
  R.Assign(0,0,Width,5);
  New(MessageDialog, Init(R, ''));
  with MessageDialog^ do
  begin
    Flags:=0;
    GetExtent(R); R.Grow(-4,-2);
    if copy(Msg,1,1)<>^C then Msg:=^C+Msg;
    Insert(New(PStaticText, Init(R, Msg)));
  end;
  Application^.Insert(MessageDialog);
end;

procedure HideMessage;
begin
  if MessageDialog<>nil then
    begin
      Application^.Delete(MessageDialog);
      Dispose(MessageDialog, Done);
      MessageDialog:=nil;
    end;
end;



END.
{
  $Log$
  Revision 1.1  1999-03-01 15:51:43  peter
    + Log

}
