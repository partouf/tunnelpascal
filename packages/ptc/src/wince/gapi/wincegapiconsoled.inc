{
    This file is part of the PTCPas framebuffer library
    Copyright (C) 2007, 2009-2012  Nikolay Nikolov (nickysn@users.sourceforge.net)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version
    with the following modification:

    As a special exception, the copyright holders of this library give you
    permission to link this library with independent modules to produce an
    executable, regardless of the license terms of these independent modules,and
    to copy and distribute the resulting executable under terms of your choice,
    provided that you also meet, for each linked independent module, the terms
    and conditions of the license of that module. An independent module is a
    module which is not derived from or based on this library. If you modify
    this library, you may extend this exception to your version of the library,
    but you are not obligated to do so. If you do not wish to do so, delete this
    exception statement from your version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
}

type
  TWinCEGAPIConsole = class(TPTCOpenGLLessConsole)
  private
    FWindow: TWinCEWindow;
    FKeyboard: TWinCEKeyboard;
    FMouse: TWinCEMouse;

    FGXDisplayProperties: GXDisplayProperties;

    FCopy: TPTCCopy;
    FClear: TPTCClear;
    FArea: IPTCArea;
    FClip: IPTCArea;
    FEventQueue: TEventQueue;
    FModes: array of IPTCMode;

    FOpen: Boolean;
    FLocked: Boolean;

    FGXDisplayIsOpen: Boolean;

    FTitle: AnsiString;

    FDisplayWidth: Integer;
    FDisplayHeight: Integer;
    FDisplayPitch: Integer;
    FDisplayFormat: IPTCFormat;

    function WndProc(Ahwnd: HWND; AuMsg: UINT; AwParam: WPARAM; AlParam: LPARAM): LRESULT;

    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function GetPitch: Integer; override;
    function GetArea: IPTCArea; override;
    function GetFormat: IPTCFormat; override;
    function GetPages: Integer; override;
    function GetName: AnsiString; override;
    function GetTitle: AnsiString; override;
    function GetInformation: AnsiString; override;

    procedure CheckOpen(    AMessage: AnsiString);
    procedure CheckUnlocked(AMessage: AnsiString);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Open(const ATitle: AnsiString; APages: Integer = 0); overload; override;
    procedure Open(const ATitle: AnsiString; AFormat: IPTCFormat;
                   APages: Integer = 0); overload; override;
    procedure Open(const ATitle: AnsiString; AWidth, AHeight: Integer;
                   AFormat: IPTCFormat; APages: Integer = 0); overload; override;
    procedure Open(const ATitle: AnsiString; AMode: IPTCMode;
                   APages: Integer = 0); overload; override;
    procedure Close; override;

    procedure Copy(ASurface: IPTCSurface); override;
    procedure Copy(ASurface: IPTCSurface;
                   ASource, ADestination: IPTCArea); override;

    procedure Load(const APixels: Pointer;
                   AWidth, AHeight, APitch: Integer;
                   AFormat: IPTCFormat;
                   APalette: IPTCPalette); override;
    procedure Load(const APixels: Pointer;
                   AWidth, AHeight, APitch: Integer;
                   AFormat: IPTCFormat;
                   APalette: IPTCPalette;
                   ASource, ADestination: IPTCArea); override;
    procedure Save(APixels: Pointer;
                   AWidth, AHeight, APitch: Integer;
                   AFormat: IPTCFormat;
                   APalette: IPTCPalette); override;
    procedure Save(APixels: Pointer;
                   AWidth, AHeight, APitch: Integer;
                   AFormat: IPTCFormat;
                   APalette: IPTCPalette;
                   ASource, ADestination: IPTCArea); override;

    function Lock: Pointer; override;
    procedure Unlock; override;

    procedure Clear; override;
    procedure Clear(AColor: IPTCColor); override;
    procedure Clear(AColor: IPTCColor;
                    AArea: IPTCArea); override;

    procedure Configure(const AFileName: AnsiString); override;
    function Option(const AOption: AnsiString): Boolean; override;

    procedure Palette(APalette: IPTCPalette); override;
    procedure Clip(AArea: IPTCArea); override;
    function Clip: IPTCArea; override;
    function Palette: IPTCPalette; override;
    function Modes: TPTCModeList; override;

    procedure Flush; override;
    procedure Finish; override;
    procedure Update; override;
    procedure Update(AArea: IPTCArea); override;

    function NextEvent(out AEvent: IPTCEvent; AWait: Boolean; const AEventMask: TPTCEventMask): Boolean; override;
    function PeekEvent(AWait: Boolean; const AEventMask: TPTCEventMask): IPTCEvent; override;
  end;
