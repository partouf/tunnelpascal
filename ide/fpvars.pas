{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Global variables for the IDE

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$i globdir.inc}

unit FPVars;

interface

uses Objects,Views,App,
     WUtils,WEditor,
     FPConst,
     FPDebug,FPRegs,
     FPUtils,FPViews,FPCalc;

type
    TRecentFileEntry = record
      FileName  : string;
      LastPos   : TPoint;
    end;

    TCompPhase = (cpNothing,cpCompiling,cpLinking,
                  cpAborted,cpFailed,cpDone);

const ClipboardWindow  : PClipboardWindow = nil;
      CalcWindow       : PCalculator = nil;
      RecentFileCount  : integer = 0;
      LastCompileTime  : cardinal = 0;
      OpenExts         : string = '*.pas;*.pp;*.inc';
      HighlightExts    : string = '*.pas;*.pp;*.inc';
      TabsPattern      : string = 'make*;make*.*;fpcmake.loc';
      SourceDirs       : string = '';
      StandardUnits    : string = '';
      UseStandardUnitsInCodeComplete : boolean = false;
      UseAllUnitsInCodeComplete : boolean = true;
      ShowOnlyUnique   : boolean = true;
      PrimaryFile      : string = '';
      PrimaryFileMain  : string = '';
      PrimaryFileSwitches : string = '';
      PrimaryFilePara  : string = '';
      GDBOutputFile    : string = GDBOutputFileName;
      IsEXECompiled    : boolean = false;
      { LinkAfter        : boolean = true; changed into a function }
      MainHasDebugInfo : boolean = false;
      UseMouse         : boolean = true;
      MainFile         : string = '';
      PrevMainFile     : string = '';
      EXEFile          : string = '';
      CompilationPhase : TCompPhase = cpNothing;
      GDBWindow        : PGDBWindow = nil;
      DisassemblyWindow : PDisassemblyWindow = nil;
      BreakpointsWindow : PBreakpointsWindow = nil;
      WatchesWindow    : PWatchesWindow = nil;
      UserScreenWindow : PScreenWindow = nil;
      StackWindow      : PStackWindow = nil;
      RegistersWindow  : PRegistersWindow = nil;
      FPUWindow        : PFPUWindow = nil;

      HeapView         : PFPHeapView = nil;
      ClockView        : PFPClockView = nil;
      HelpFiles        : WUtils.PUnsortedStringCollection = nil;
      ShowStatusOnError: boolean = true;
      StartupDir       : string = '.'+DirSep;
      IDEDir           : string = '.'+DirSep;
{$ifdef Unix}
      SystemIDEDir     : string = '';
{$endif Unix}
      INIFileName      : string = ININame;
      SwitchesPath     : string = SwitchesName;
      CtrlMouseAction  : integer = acTopicSearch;
      AltMouseAction   : integer = acBrowseSymbol;
      StartupOptions   : longint = 0;
      LastExitCode     : integer = 0;
      ASCIIChart       : PFPASCIIChart = nil;
      BackgroundPath   : string = BackgroundName;
      DesktopPath      : string = DesktopName;
      DesktopFileFlags : longint = dfHistoryLists+dfOpenWindows+
                                   dfCodeCompleteWords+dfCodeTemplates;
      DesktopLocation  : byte    = dlConfigFileDir;
      AutoSaveOptions  : longint = asEnvironment+asDesktop;
      MiscOptions      : longint = moChangeDirOnOpen+moCloseOnGotoSource;
      EditorModified   : boolean = false;
      IniCenterDebuggerRow : boolean = true;
      SleepTimeOut     : longint = trunc(10*18.2);
{$ifdef USE_EXTERNAL_COMPILER}
      UseExternalCompiler : boolean = true;
      ExternalCompilerExe : string = 'ppc386'+ExeExt;
{$endif USE_EXTERNAL_COMPILER}
      ShowReadme       : boolean = true;
      AskRecompileIfModifiedFlag : boolean = true;

{$ifdef SUPPORT_REMOTE}
     RemoteMachine : string = '';
     RemotePort : string = '2345';
     RemoteConfig : string = '';
     RemoteIdent : string = '';
     RemoteDir : string = '';
     RemoteSendCommand : string = 'scp $CONFIG $IDENT $LOCALFILE $REMOTEMACHINE:$REMOTEDIR';
{$endif SUPPORT_REMOTE}

     DebuggeeTTY : string = '';

      ActionCommands   : array[acFirstAction..acLastAction] of word =
        (cmHelpTopicSearch,cmGotoCursor,cmToggleBreakpoint,
         cmEvaluate,cmAddWatch,cmBrowseAtCursor);

      AppPalette       : string = CIDEAppColor;

var   RecentFiles      : array[1..MaxRecentFileCount] of TRecentFileEntry;

implementation

END.
{
  $Log$
  Revision 1.9  2004-11-08 20:28:28  peter
    * Breakpoints are now deleted when removed from source, disabling is
      still possible from the breakpoint list
    * COMPILER_1_0, FVISION, GABOR defines removed, only support new
      FV and 1.9.x compilers
    * Run directory added to Run menu
    * Useless programinfo window removed

  Revision 1.8  2002/12/12 00:06:41  pierre
   Use fpregs unit

  Revision 1.7  2002/11/28 12:53:10  pierre
   + global vars used for remote debugging

  Revision 1.6  2002/09/10 12:19:14  pierre
   * use faster method for loading files by default

  Revision 1.5  2002/09/07 15:40:46  peter
    * old logs removed and tabs fixed

  Revision 1.4  2002/09/04 14:06:36  pierre
   + new variables for Unit symbols code complete

  Revision 1.3  2002/05/29 22:33:23  pierre
   Asciitab now in fvision

}
