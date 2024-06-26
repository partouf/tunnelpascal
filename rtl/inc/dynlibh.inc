{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2015 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifdef FPC_SYSTEM_HAS_SYSDLH}
{$i sysdlh.inc}
{$else FPC_SYSTEM_HAS_SYSDLH}
{ These are fallback declarations }
type
  TLibHandle = THandle;
  HINST = TLibHandle;
  
const
  NilHandle = TLibHandle(0);
{$endif FPC_SYSTEM_HAS_SYSDLH}

{ ---------------------------------------------------------------------
  OS - Independent declarations.
  ---------------------------------------------------------------------}
type
{$if not declared(TOrdinalEntry)}
  TOrdinalEntry = SizeUInt;
{$endif not declared(TOrdinalEntry)}

  TLoadLibraryUHandler = function(const Name: UnicodeString): TLibHandle;
  TLoadLibraryAHandler = function(const Name: RawByteString): TLibHandle;
  TGetProcAddressHandler = function(Lib: TLibHandle; const ProcName: AnsiString): {$ifdef cpui8086}FarPointer{$else}Pointer{$endif};
  TGetProcAddressOrdinalHandler = function(Lib: TLibHandle; Ordinal: TOrdinalEntry): {$ifdef cpui8086}FarPointer{$else}Pointer{$endif};
  TUnloadLibraryHandler = function(Lib: TLibHandle): Boolean;
  TGetLoadErrorStrHandler = function: AnsiString;

  TDynLibsManager = record
    LoadLibraryU: TLoadLibraryUHandler;
    LoadLibraryA: TLoadLibraryAHandler;
    GetProcAddress: TGetProcAddressHandler;
    GetProcAddressOrdinal: TGetProcAddressOrdinalHandler;
    UnloadLibrary: TUnloadLibraryHandler;
    GetLoadErrorStr: TGetLoadErrorStrHandler;
  end;

Function SafeLoadLibrary(const Name : RawByteString) : TLibHandle;
Function LoadLibrary(const Name : RawByteString) : TLibHandle;
Function SafeLoadLibrary(const Name : UnicodeString) : TLibHandle;
Function LoadLibrary(const Name : UnicodeString) : TLibHandle;

Function GetProcedureAddress(Lib : TlibHandle; const ProcName : AnsiString) : {$ifdef cpui8086}FarPointer{$else}Pointer{$endif};
Function GetProcedureAddress(Lib : TLibHandle; Ordinal: TOrdinalEntry) : {$ifdef cpui8086}FarPointer{$else}Pointer{$endif};
Function UnloadLibrary(Lib : TLibHandle) : Boolean;
Function GetLoadErrorStr: AnsiString;

// Kylix/Delphi compability

Function FreeLibrary(Lib : TLibHandle) : Boolean; inline;
Function GetProcAddress(Lib : TlibHandle; const ProcName : AnsiString) : {$ifdef cpui8086}FarPointer{$else}Pointer{$endif}; inline;

Procedure GetDynLibsManager (Out Manager : TDynLibsManager);
Procedure SetDynLibsManager (Const New : TDynLibsManager);
Procedure SetDynLibsManager (Const New : TDynLibsManager; out Old: TDynLibsManager);

// Delphi compatibility, needs to be expanded when packages arrive.
Type
  TEnumModuleFuncLW = function (HInstance: THandle; Data: Pointer): Boolean;
  
procedure EnumModules(Func: TEnumModuleFuncLW; Data: Pointer); 

  
  
