{ %INTERACTIVE }
{******************************************}
{  Used to check the DOS unit              }
{------------------------------------------}
{  Requirements for this unit can be       }
{  found in testdos.htm                    }
{******************************************}
Program TestDos;

Uses Dos;

{----------------------------------------------------------------------}
{ The following routines are not portable, and therefore have not been }
{ added in this test unit:                                             }
{----------------------------------------------------------------------}
{ o GetIntVec                                                          }
{ o SetIntVec                                                          }
{ o Intr                                                               }
{ o Keep                                                               }
{ o MSDOS                                                              }
{ o Swapvectors (can't really be tested)                               }
{----------------------------------------------------------------------}
{ ROUTINES LEFT TO DO:
DosExitCode | Func | Returns the exit code of a subprocess.
Exec        | Proc | Executes a specified program with a specified command
            |      | line.
FExpand     | Func | Expands a file name into a fully-qualified file name.
FSearch     | Func | Searches for a file.
GetEnv      | Func | Returns the value of a specified environment variable.
}
{**********************************************************************}
{ Some specific OS verifications : }
{ Mainly for file attributes:      }
{ Read-Only                        }
{ Hidden                           }
{ System File                      }
{ only work on Win32, OS/2 and DOS }



{$IFDEF MSDOS}
        {$DEFINE EXTATTR}
{$ENDIF}
{$IFDEF OS2}
        {$DEFINE EXTATTR}
{$ENDIF}
{$IFDEF WIN32}
        {$DEFINE EXTATTR}
{$ENDIF}
{$IFDEF TOS}
        {$DEFINE EXTATTR}
{$ENDIF}



{$IFNDEF UNIX}
{$IFDEF LINUX}
        {$DEFINE UNIX}
{$ENDIF}
{$IFDEF QNX}
        {$DEFINE UNIX}
{$ENDIF}
{$IFDEF SOLARIS}
        {$DEFINE UNIX}
{$ENDIF}
{$IFDEF FREEBSD}
        {$DEFINE UNIX}
{$ENDIF}
{$ENDIF}
const
{ what is the root path }
{$IFDEF EXTATTR}
  RootPath = 'C:\';
{$ENDIF}
{$IFDEF UNIX}
  RootPath := '/';
{$ENDIF}
{**********************************************************************}



CONST
 Week:Array[0..6] of String =
 ('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');

 TestFName = 'TESTDOS.DAT';  { CASE SENSITIVE DON'T TOUCH! }
 TestFName1 = 'TESTFILE';    { CASE SENSITIVE DON'T TOUCH! }
 TestDir = 'MYDIR';          { CASE SENSITIVE DON'T TOUCH! }
 TestExt   = 'DAT';


Procedure PauseScreen;
var
 ch: char;
Begin
 WriteLn('-- Press any key --');
 ReadLn;
end;

{ verifies that the DOSError variable is equal to }
{ the value requested.                            }
Procedure CheckDosError(err: Integer);
 var
  x : integer;
  s :string;
 Begin
  Write('Verifying value of DOS Error...');
  x := DosError;
  case x of
  0 : s := '(0): No Error.';
  2 : s := '(2): File not found.';
  3 : s := '(3): Path not found.';
  5 : s := '(5): Access Denied.';
  6 : s := '(6): Invalid File Handle.';
  8 : s := '(8): Not enough memory.';
  10 : s := '(10) : Invalid Environment.';
  11 : s := '(11) : Invalid format.';
  18 : s := '(18) : No more files.';
  else
    s := 'INVALID DOSERROR';
  end;
  if err <> x then
    Begin
      WriteLn('FAILURE. (Value should be ',err,' '+s+')');
    end
  else
    WriteLn('Success.');
 end;


Procedure TestdiskSize;
Var
 i : Integer;
Begin
 WriteLn('----------------------------------------------------------------------');
 WriteLn('                          DISKSIZE/DISKFREE                           ');
 WriteLn('----------------------------------------------------------------------');
 WriteLn(' Note: Should return -1 on both functions if device is not ready.     ');
 WriteLn('----------------------------------------------------------------------');
 CheckDosError(0);
{ Check Disksize / DiskFree routines }
 for I:=0 to 20 do
 Begin
   Write('Disk unit ',i:2,' free size : ',DiskFree(i):10, ' Total Size: ',DiskSize(i):10);
   WriteLn(' bytes.');
 end;
 CheckDosError(0);
 PauseScreen;
end;

Procedure TestDosVersion;
Begin
 WriteLn('----------------------------------------------------------------------');
 WriteLn('                          DOSVERSION                                  ');
 WriteLn('----------------------------------------------------------------------');
 WriteLn(' Note: Number should be major version followed by minor version.      ');
 WriteLn('----------------------------------------------------------------------');
 CheckDosError(0);
 {*------------------------- NOTE -------------------------------------*}
 {* This is OS specific. LO -> Major revision, HI -> Minor Revision    *}
 {*--------------------------------------------------------------------*}
 WriteLn('Operating system Version :',Lo(DosVersion),'.',Hi(DosVersion));
 CheckDosError(0);
 PauseScreen;
end;

Procedure TestEnvCount;
Var
 I: Integer;
Begin
 WriteLn('----------------------------------------------------------------------');
 WriteLn('                       ENVCOUNT/ENVSTR                                ');
 WriteLn('----------------------------------------------------------------------');
 WriteLn(' Note: Environment variables should be of the form VAR=VALUE          ');
 WriteLn(' Note: Non valid indexes should return empty strings.                 ');
 WriteLn(' Note: Index 0 points to an empty string                              ');
 WriteLn('----------------------------------------------------------------------');
 CheckDosError(0);
 PauseScreen;
 {*------------------------- NOTE -------------------------------------*}
 {* Variables should be of the form VAR=VALUE                          *}
 {*--------------------------------------------------------------------*}
 WriteLn('CURRENT ENVIRONMENT');
 For I:=1 to EnvCount do
  WriteLn(EnvStr(i));
 CheckDosError(0);
 WriteLn('----------------------------------------------------------------------');
 WriteLn(' Note: The next few lines should be empty strings, as they are        ');
 WriteLn('       invalid environment indexes.                                   ');
 WriteLn('----------------------------------------------------------------------');
 For i:=-5 to 0 do
  WriteLn(EnvStr(i));
 CheckDosError(0);
 For i:=20000 to 20002 do
  WriteLn(EnvStr(i));
 CheckDosError(0);
 PauseScreen;
end;

Procedure TestVerify;
Var
 B: Boolean;
 s: string;
Begin
 WriteLn('----------------------------------------------------------------------');
 WriteLn('                       GETVERIFY/SETVERIFY                            ');
 WriteLn('----------------------------------------------------------------------');
 CheckDosError(0);
 s:='Testing GetVerify...';
 SetVerify(TRUE);
 CheckDosError(0);
 GetVerify(b);
 CheckDosError(0);
 if b then
   WriteLn(s+'Success.')
 else
  Begin
    WriteLn(s+'FAILURE.');
{    Halt;}
  end;
 s:='Testing GetVerify...';
 SetVerify(FALSE);
 CheckDosError(0);
 GetVerify(b);
 CheckDosError(0);
 if NOT b then
   WriteLn(s+'Success.')
 else
  Begin
    WriteLn(s+'FAILURE.');
  end;
 PauseScreen;
end;

Procedure TestCBreak;
Var
 B: Boolean;
 s: string;
Begin
 WriteLn('----------------------------------------------------------------------');
 WriteLn('                       GETCBREAK/SETCBREAK                            ');
 WriteLn('----------------------------------------------------------------------');
 CheckDosError(0);
 s:='Testing GetCBreak...';
 SetCBreak(TRUE);
 CheckDosError(0);
 GetCBreak(b);
 CheckDosError(0);
 if b then
   WriteLn(s+'Success.')
 else
  Begin
    WriteLn(s+'FAILURE.');
  end;
 s:='Testing GetCBreak...';
 SetCBreak(FALSE);
 CheckDosError(0);
 GetCBreak(b);
 CheckDosError(0);
 if NOT b then
   WriteLn(s+'Success.')
 else
  Begin
    WriteLn(s+'FAILURE.');
  end;
 PauseScreen;
end;


Procedure TestSystemDate;
var
 Year,Month, DayOfWeek, Day: Word;
 Year1,Month1, DayOfWeek1, Day1: Word;
 s: string;
Begin
 WriteLn('----------------------------------------------------------------------');
 WriteLn('                            GETDATE                                   ');
 WriteLn('----------------------------------------------------------------------');
 WriteLn(' Note: Number of week should be consistent (0 = Sunday)               ');
 WriteLn(' Note: Year should contain full four digits.                          ');
 WriteLn('----------------------------------------------------------------------');
 CheckDosError(0);
 Month:=0;
 Day:=0;
 DayOfWeek:=0;
 Year:=0;
 GetDate(Year,Month,Day,DayOfWeek);
 CheckDosError(0);
 Write('DD-MM-YYYY : ',Day,'-',Month,'-',Year);
 WriteLn(' (',Week[DayOfWeek],')');
 PauseScreen;
 WriteLn('----------------------------------------------------------------------');
 WriteLn('                            SETDATE                                   ');
 WriteLn('----------------------------------------------------------------------');
{ WriteLn(' Note: GetDate should return the same value as previous test.         ');
 WriteLn('----------------------------------------------------------------------');}
 { We'll change each field to an invalid field separately }
 s:='Testing with invalid year....';
 SetDate(2200,Month,Day);
 CheckDosError(0);
 GetDate(Year1,Month1,Day1,DayOfWeek1);
 CheckDosError(0);
 if (Year1 <> Year) or (Month1 <> month) or (Day1 <> Day) then
  Begin
     WriteLn(s+'FAILURE.');
  end
 else
  WriteLn(s+'Success.');

 s:='Testing with invalid year....';
 SetDate(98,Month,Day);
 CheckDosError(0);
 GetDate(Year1,Month1,Day1,DayOfWeek1);
 CheckDosError(0);
 if (Year1 <> Year) or (Month1 <> month) or (Day1 <> Day) then
  Begin
     WriteLn(s+'FAILURE.');
  end
 else
  WriteLn(s+'Success.');

 SetDate(Year,Month,255);
 CheckDosError(0);
 s:='Testing with invalid day.....';
 GetDate(Year1,Month1,Day1,DayOfWeek1);
 CheckDosError(0);
 if (Year1 <> Year) or (Month1 <> month) or (Day1 <> Day) then
  Begin
     WriteLn(s+'FAILURE.');
  end
 else
  WriteLn(s+'Success.');

 SetDate(Year,13,Day);
 CheckDosError(0);
 s:='Testing with invalid month...';
 GetDate(Year1,Month1,Day1,DayOfWeek1);
 CheckDosError(0);
 if (Year1 <> Year) or (Month1 <> month) or (Day1 <> Day) then
  Begin
     WriteLn(s+'FAILURE.');
  end
 else
  WriteLn(s+'Success.');

 WriteLn('----------------------------------------------------------------------');
 WriteLn(' Note: Date should be 01-01-1998                                      ');
 WriteLn('----------------------------------------------------------------------');
 SetDate(1998,01,01);
 CheckDosError(0);
 GetDate(Year1,Month1,Day1,DayOfWeek1);
 CheckDosError(0);
 WriteLn('DD-MM-YYYY : ',Day1,'-',Month1,'-',Year1);
 SetDate(Year,Month,Day);
 CheckDosError(0);
 WriteLn('----------------------------------------------------------------------');
 WriteLn(' Note: Date should be restored to previous value                      ');
 WriteLn('----------------------------------------------------------------------');
 GetDate(Year1,Month1,Day1,DayOfWeek1);
 CheckDosError(0);
 WriteLn('DD-MM-YYYY : ',Day1,'-',Month1,'-',Year1);
 PauseScreen;
end;

Procedure TestsystemTime;
Var
 Hour, Minute, Second, Sec100: word;
 Hour1, Minute1, Second1, Sec1001: word;
Begin
 WriteLn('----------------------------------------------------------------------');
 WriteLn('                            GETTIME                                   ');
 WriteLn('----------------------------------------------------------------------');
 WriteLn(' Note: Hours should be in military format (0..23), and MSec in 0..100 ');
 WriteLn('----------------------------------------------------------------------');
 CheckDosError(0);
 Hour:=0;
 Minute:=0;
 Second:=0;
 Sec100:=0;
 GetTime(Hour,Minute,Second,Sec100);
 CheckDosError(0);
 WriteLn('HH:MIN:SEC (MS): ',Hour,':',Minute,':',Second,' (',Sec100,')');
 WriteLn('----------------------------------------------------------------------');
 WriteLn('                            SETTIME                                   ');
 WriteLn('----------------------------------------------------------------------');
 WriteLn(' Note: GetTime should return the same value as the previous test.     ');
 WriteLn('----------------------------------------------------------------------');
 SetTime(36,Minute,Second,Sec100);
 CheckDosError(0);
 GetTime(Hour1,Minute1,Second1,Sec1001);
 CheckDosError(0);
 WriteLn('HH:MIN:SEC ',Hour1,':',Minute1,':',Second1);
 SetTime(Hour,32000,Second,Sec100);
 CheckDosError(0);
 GetTime(Hour1,Minute1,Second1,Sec1001);
 CheckDosError(0);
 WriteLn('HH:MIN:SEC ',Hour1,':',Minute1,':',Second1);
 WriteLn('----------------------------------------------------------------------');
 WriteLn(' Note: GetTime should return  0:0:0                                   ');
 WriteLn('----------------------------------------------------------------------');
 SetTime(0,0,0,0);
 CheckDosError(0);
 GetTime(Hour1,Minute1,Second1,Sec1001);
 CheckDosError(0);
 WriteLn('HH:MIN:SEC ',Hour1,':',Minute1,':',Second1);
 WriteLn('----------------------------------------------------------------------');
 WriteLn(' Note: GetTime should return  approximately the original time         ');
 WriteLn('----------------------------------------------------------------------');
 SetTime(Hour,Minute,Second,Sec1001);
 CheckDosError(0);
 GetTime(Hour1,Minute1,Second1,Sec1001);
 CheckDosError(0);
 WriteLn('HH:MIN:SEC ',Hour1,':',Minute1,':',Second1);
end;


Procedure TestFAttr;
Var
 F: File;
 Attr: Word;
 s: string;
Begin
 PauseScreen;
 WriteLn('----------------------------------------------------------------------');
 WriteLn('                         GETFATTR / SETFATTR                          ');
 WriteLn('----------------------------------------------------------------------');
 CheckDosError(0);

 WriteLn('Opening an invalid file...Success.');
 Assign(f,'');
 GetFAttr(f,Attr);
 CheckDosError(3);
 Assign(f,TestFName);
 WriteLn('Trying to open a valid file..Success.');
 GetFAttr(f,Attr);
 CheckDosError(0);
 {----------------------------------------------------------------}
 { This routine causes problems, because it all depends on the    }
 { operating system. It is assumed here that HIDDEN is available  }
 { to all operating systems.                                      }
 {----------------------------------------------------------------}
 s:='Setting read-only attribute on '+TestFName+'...';
 SetFAttr(f,ReadOnly);
 CheckDosError(0);
{$IFDEF EXTATTR}
 GetFAttr(f,Attr);
 CheckDosError(0);
 if Attr and ReadOnly<> 0 then
   WriteLn(s+'Success.')
 else
  Begin
    WriteLn(s+'FAILURE. Read-only attribute not set.');
  end;
 { file should no longer be read only }
 s:='Removing read-only attribute...';
 SetFAttr(f,Archive);
 CheckDosError(0);
 GetFAttr(f,Attr);
 CheckDosError(0);
 if Attr and ReadOnly<> 0 then
  Begin
    WriteLn(s+'FAILURE. Read-only attribute still set.');
  end
 else
   WriteLn(s+'Success.');
{$ENDIF}

 s:='Setting hidden attribute on '+TestFName+'...';
 SetFAttr(f,Hidden);
 CheckDosError(0);
{$IFDEF EXTATTR}
 GetFAttr(f,Attr);
 CheckDosError(0);
 if Attr and Hidden<> 0 then
   WriteLn(s+'Success.')
 else
  Begin
    WriteLn(s+'FAILURE. Hidden attribute not set.');
  end;

 { file should no longer be read only }
 s:='Removing hidden attribute...';
 SetFAttr(f,Archive);
 CheckDosError(0);
 GetFAttr(f,Attr);
 CheckDosError(0);
 if Attr and Hidden<> 0 then
  Begin
    WriteLn(s+'FAILURE. Hidden attribute still set.');
  end
 else
   WriteLn(s+'Success.');
{$ENDIF}

 s:='Setting system attribute on '+TestFName+'...';
 SetFAttr(f,SysFile);
 CheckDosError(0);
{$IFDEF EXTATTR}
 GetFAttr(f,Attr);
 CheckDosError(0);
 if Attr and SysFile<> 0 then
   WriteLn(s+'Success.')
 else
  Begin
    WriteLn(s+'FAILURE. SysFile attribute not set.');
  end;
 { file should no longer be read only }
 s:='Removing read-only attribute...';
 SetFAttr(f,Archive);
 CheckDosError(0);
 GetFAttr(f,Attr);
 CheckDosError(0);
 if Attr and Sysfile<> 0 then
  Begin
    WriteLn(s+'FAILURE. SysFile attribute still set.');
  end
 else
   WriteLn(s+'Success.');
{$ENDIF}

 s:='Setting Directory attribute on '+TestFName+'...';
 SetFAttr(f,Directory);
 CheckDosError(5);
 GetFAttr(f,Attr);
 CheckDosError(0);
 if Attr and Directory<> 0 then
  Begin
    WriteLn(s+'FAILURE. Directory Attribute set.');
  end
 else
   WriteLn(s+'Success.');

 {**********************************************************************}
 {********************** TURBO PASCAL BUG ******************************}
 { The File is not a volume name, and DosError = 0, which is incorrect  }
 { it shoulf not be so in FPC.                                          }
 {**********************************************************************}
 {********************** TURBO PASCAL BUG ******************************}
 s:='Setting Volume attribute on '+TestFName+'...';
 SetFAttr(f,VolumeID);
 CheckDosError(5);
 GetFAttr(f,Attr);
 CheckDosError(0);
 if Attr and VolumeID<> 0 then
  Begin
    WriteLn(s+'FAILURE. Volume Attribute set.');
  end
 else
   WriteLn(s+'Success.');

 PauseScreen;
end;


Procedure TestFTime;
var
 s : string;
 F: File;
 Time: Longint;
 DT: DateTime;
 DT1 : Datetime; { saved values }
Begin
 WriteLn('----------------------------------------------------------------------');
 WriteLn('                         GETFTIME / SETFTIME                          ');
 WriteLn('----------------------------------------------------------------------');
 CheckDosError(0);

 {**********************************************************************}
 {********************** TURBO PASCAL BUG ******************************}
 { The File is not Open and DosError is still zero! THIS SHOULD NOT BE  }
 { SO IN FPC!                                                           }
 {**********************************************************************}
 {********************** TURBO PASCAL BUG ******************************}
 Write('Opening an invalid file...');
 Assign(f,'x');
 GetFTime(f,Time);
 CheckDosError(6);

 Write('Trying to open ',TestFName,'...');
 Assign(f,TestFName);
 Reset(f,1);
 GetFTime(f,Time);
 CheckDosError(0);
 UnpackTime(Time,Dt);
 WriteLn('----------------------------------------------------------------------');
 WriteLn(' Note: Hour should be in military format and year should be a 4 digit ');
 WriteLn('       number.                                                        ');
 WriteLn('----------------------------------------------------------------------');
 WriteLn('DD-MM-YYYY : ',DT.Day,'-',DT.Month,'-',DT.Year);
 WriteLn('HH:MIN:SEC ',DT.Hour,':',DT.Min,':',DT.Sec);

 { SETFTIME / GETFTIME No Range checking is performed so the tests are }
 { very limited.                                                       }
 s:='Setting '+TestFName+' date/time to 01-28-1998:0:0:0...';
 dt1.Year:=1998;
 dt1.Month:=1;
 dt1.Day:=28;
 Dt1.Hour:=0;
 Dt1.Min:=0;
 Dt1.Sec:=0;
 PackTime(DT1,Time);
 CheckDosError(0);
 SetFTime(f,Time);
 CheckDosError(0);
 GetFTime(f,Time);
 CheckDosError(0);
 { Re-initialize the date time file }
 FillChar(Dt1,sizeof(dt1),#0);
 UnpackTime(Time,Dt1);
 if (Dt1.Year <> 1998) or (Dt1.Month<>1) or (Dt1.Day<>28) or
    (Dt1.Hour<>0) or (Dt1.Min <>0) or (Dt1.Sec<>0) then
   Begin
      WriteLn(s+'FAILURE.');
   end
 else
   WriteLn(s+'Success.');

 s:='Restoring old file time stamp...';
 Move(Dt,Dt1,sizeof(Dt));
 PackTime(DT1,Time);
 CheckDosError(0);
 SetFTime(f,Time);
 CheckDosError(0);
 GetFTime(f,Time);
 CheckDosError(0);
 { Re-initialize the date time file }
 FillChar(Dt1,sizeof(dt),#0);
 UnpackTime(Time,Dt1);
 if (Dt1.Year <> Dt.Year) or (Dt1.Month<>Dt.Month) or (Dt1.Day<>Dt.Day) or
    (Dt1.Hour<>Dt.Hour) or (Dt1.Min <> Dt.Min) or (Dt1.Sec<>Dt.Sec) then
   Begin
      WriteLn(s+'FAILURE.');
   end
 else
   WriteLn(s+'Success.');
 Close(f);
end;

Procedure TestFind;
var
 Search: SearchRec;
 DT: Datetime;
 Year, Month, Day, DayOfWeek: Word;
 Failure : Boolean;
 FoundDot, FoundDotDot: boolean;
 FoundDir : boolean;
 s : string;
Begin
 WriteLn('----------------------------------------------------------------------');
 WriteLn('                         FINDFIRST/ FINDNEXT                          ');
 WriteLn('----------------------------------------------------------------------');
 WriteLn(' Note: The full path should NOT be displayed.                         ');
 WriteLn('----------------------------------------------------------------------');
 CheckDosError(0);
 WriteLn('Trying to find an invalid file ('''') with Any Attribute...');
 FindFirst('',AnyFile,Search);
 CheckDosError(3);
{$IFDEF FPC}
 FindClose(Search);
{$ENDIF}

 WriteLn('Trying to find an invalid file ('''') with VolumeID attribute...');
 FindFirst('',VolumeID,Search);
 CheckDosError(3);
{$IFDEF FPC}
 FindClose(Search);
{$ENDIF}

 WriteLn('Trying to find an invalid file (''''zz.dat'''') with Any Attribute...');
 FindFirst('zz.dat',AnyFile,Search);
 CheckDosError(18);
{$IFDEF FPC}
 FindClose(Search);
{$ENDIF}

 WriteLn('Trying to find an invalid file (''''zz.dat'''') with VolumeID attribute...');
 FindFirst('zz.dat',VolumeID,Search);
 CheckDosError(18);
{$IFDEF FPC}
 FindClose(Search);
{$ENDIF}

 WriteLn('Trying to find an invalid file (''''zz.dat'''') with Directory attribute...');
 FindFirst('zz.dat',Directory,Search);
 CheckDosError(18);
{$IFDEF FPC}
 FindClose(Search);
{$ENDIF}

 s:='Looking for '+TestFName +' with Any Attribute...';
 FindFirst('*.DAT',AnyFile,Search);
 { At least testdos.dat should appear }
 if DosError <> 0 then
   WriteLn(s+'FAILURE. ',TestFName,' should be found.')
 else
   WriteLn(s+'Success.');
 if Search.Name <> TestFName then
  Begin
    repeat
      FindNext(Search);
    until (DosError <> 0) OR (Search.Name = TestFName);
  end;
{$IFDEF FPC}
 FindClose(Search);
{$ENDIF}

 { In addition to normal files          }
 { directory files should also be found }
 s:='Looking for '+TestFName +' with Directory Attribute...';
 FindFirst('*.DAT',Directory,Search);
 if DosError<> 0 then
   WriteLn(s+'FAILURE. ',TestFName,' should be found.')
 else
   WriteLn(s+'Success.');
 if Search.Name <> TestFName then
  Begin
    repeat
      FindNext(Search);
    until (DosError <> 0) OR (Search.Name = TestFName);
  end;
{$IFDEF FPC}
 FindClose(Search);
{$ENDIF}


 Write('Checking file stats of ',TestFName,'...');
 UnpackTime(Search.Time,DT);
 GetDate(Year, Month, Day, DayOfWeek);
 if (Search.Size <> Sizeof(week)) OR (DT.Year <> Year) OR (DT.Month <> Month)
    OR (DT.Day <> Day)
 then
  Begin
    WriteLn('FAILURE. Size/Date is different.')
  end
 else
   WriteLn('Success.');
 Write('Looking for ',TestFName,'...');
 FindFirst('*.D??',AnyFile,Search);
 { At least testdos.dat should appear }
 if DosError <> 0 then
   WriteLn('FAILURE. ',Testfname,' should be found.')
 else
   WriteLn('Success.');
 if Search.Name <> TestFName then
  Begin
    repeat
      FindNext(Search);
    until (DosError <> 0) OR (Search.Name = TestFName);
  end;
{$IFDEF FPC}
 FindClose(Search);
{$ENDIF}

 Write('Checking file stats of ',TestFName,'...');
 UnpackTime(Search.Time,DT);
 GetDate(Year, Month, Day, DayOfWeek);
 if (Search.Size <> Sizeof(week)) OR (DT.Year <> Year) OR (DT.Month <> Month)
    OR (DT.Day <> Day)
 then
  Begin
    WriteLn('FAILURE. Size/Date is different.')
  end
 else
   WriteLn('Success.');

 { Should show all possible files }
 FoundDot := False;
 FoundDotDot := False;
 Failure := True;
 FoundDir := False;
 s:='Searching using * wildcard (normal files + directories)...';
 FindFirst('*',Directory,Search);
 WriteLn(#9'Resources found (full path should not be displayed):');
 while DosError = 0 do
 Begin
    If Search.Name = TestDir then
    Begin
      If Search.Attr and Directory <> 0 then
        FoundDir := TRUE;
    end;
    If Search.Name = '.' then
    Begin
      If Search.Attr and Directory <> 0 then
         FoundDot := TRUE;
    End;
    if Search.Name = '..' then
    Begin
      If Search.Attr and Directory <> 0 then
         FoundDotDot := TRUE;
    End;
    { check for both . and .. special files }
    If Search.Name = TestFName1 then
      Failure := FALSE;
    WriteLn(#9+Search.Name);
    FindNext(Search);
 end;
{$IFDEF FPC}
 FindClose(Search);
{$ENDIF}
 if not FoundDir then
   WriteLn(s+'FAILURE. Did not find '+TestDir+' directory')
 else
 if not FoundDot then
   WriteLn(s+'FAILURE. Did not find special ''''.'''' directory')
 else
 if not FoundDotDot then
   WriteLn(s+'FAILURE. Did not find special ''''..'''' directory')
 else
 if Failure then
   WriteLn(s+'FAILURE. Did not find special '+TestFName1+' directory')
 else
   WriteLn(s+'Success.');

{$IFDEF FPC}
 FindClose(Search);
{$ENDIF}

 s:='Searching using ??? wildcard (normal files + all special files)...';
 FindFirst('???',AnyFile,Search);
 FoundDot := False;
 FoundDotDot := False;
 WriteLn(#9'Resources found (full path should not be displayed):');
 while DosError = 0 do
 Begin
    If Search.Name = '.' then
    Begin
      If Search.Attr and Directory <> 0 then
         FoundDot := TRUE;
    End;
    if Search.Name = '..' then
    Begin
      If Search.Attr and Directory <> 0 then
         FoundDotDot := TRUE;
    End;
    WriteLn(#9+Search.Name);
    FindNext(Search);
 end;
 if not FoundDot then
   WriteLn(s+'FAILURE. Did not find special ''''.'''' directory')
 else
 if not FoundDotDot then
   WriteLn(s+'FAILURE. Did not find special ''''..'''' directory')
 else
   WriteLn(s+'Success.');
{$IFDEF FPC}
  FindClose(Search);
{$ENDIF}

 { search for volume ID }
 s:='Searching using * wildcard in ROOT (normal files + volume ID)...';
 FindFirst(RootPath+'*',VolumeID,Search);
 Failure := TRUE;
 WriteLn(#9'Resources found (full path should not be displayed):');
 while DosError = 0 do
 Begin
    If Search.Attr and VolumeID <> 0 then
    Begin
      Failure := FALSE;
      WriteLn(#9+Search.Name);
    End;
    FindNext(Search);
 end;
 If Failure then
   WriteLn(s+'FAILURE. Did not find volume name')
 else
   WriteLn(s+'Success.');
{$IFDEF FPC}
  FindClose(Search);
{$ENDIF}


end;


Procedure TestSplit;
var
 P: PathStr;
 D: DirStr;
 N: NameStr;
 E: ExtStr;
 temp : string;
Begin
 WriteLn('----------------------------------------------------------------------');
 WriteLn('                                FSPLIT                                ');
 WriteLn('----------------------------------------------------------------------');
 Write('Testing invalid filename...');
 { Initialize names ot invalid values! }
 D:='Garbage';
 N:='Garbage';
 E:='GAR';
 { This is the path to be split }
 P:='';
 FSPlit(P,D,N,E);
 IF (length(D) <> 0) OR (length(N) <>0) OR (length(E) <> 0) THEN
   WriteLn('FAILURE. Same length as PATH (now length 0) should be returned.')
 else
   WriteLn('Success.');
 Write('Testing paramstr(0)...');
 { Initialize names ot invalid values! }
 D:='Garbage';
 N:='Garbage';
 E:='GAR';
 { This is the path to be split }
 P:=paramstr(0);
 FSPlit(P,D,N,E);
 IF length(p) <> (length(d)+length(n)+length(e)) then
   WriteLn('FAILURE. Same length as PATH should be returned.')
 else
   WriteLn('Success.');
 temp:=d+n+e;
 Write('Testing paramstr(0)...');
 if temp <> p then
   WriteLn('FAILURE. Concatenated string should be the same.')
 else
   WriteLn('Success.');
 WriteLn('PARAMSTR(0) = ', ParamStr(0));
 WriteLn('DRIVE + NAME + EXT = ',d+n+e);

 Write('Testing invalid path (..)...');
 P:='..';
 FSPlit(P,D,N,E);
 IF (length(D) <> 0) OR (length(N) <>0) OR (E <> P) THEN
   WriteLn('FAILURE. Length of drive and name should be zero and Ext should return Path')
 else
   WriteLn('Success.');
 Write('Testing invalid path (*)...');
 P:='*';
 FSPlit(P,D,N,E);
 IF (length(D) <> 0) OR (length(e) <>0) OR (N <> P) THEN
   WriteLn('FAILURE. Length of drive and name should be zero and Name should return Path')
 else
   WriteLn('Success.');
end;



var
 F: File;
 Attr : Word;
Begin
{ ClrScr;}
 { TestdiskSize; }
 TestDosVersion;
 TestEnvCount;
 TestVerify;
 TestSystemDate;
 TestSystemTime;

 { Now the file I/O functions                  }
 { Let us create a file that we will play with }
 Assign(f,TestFName);
 Rewrite(f,1);
 BlockWrite(f,Week,sizeof(Week));
 Close(f);
 Assign(f,TestFName1);
 Rewrite(f,1);
 Close(F);
 MkDir(TestDir);
 TestFAttr;
 TestFTime;
 TestCBreak;
 TestFind;
 PauseScreen;
 TestSplit;
 RmDir(TestDir);
 PauseScreen;
end.

{
DosExitCode | Func | Returns the exit code of a subprocess.
Exec        | Proc | Executes a specified program with a specified command
            |      | line.
FExpand     | Func | Expands a file name into a fully-qualified file name.
FSearch     | Func | Searches for a file.
GetEnv      | Func | Returns the value of a specified environment variable.
}

{
  $Log$
  Revision 1.1  2001-04-02 02:34:13  carl
  + initial version of complete test for dos unit


}