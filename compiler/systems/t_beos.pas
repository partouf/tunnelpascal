{
    $Id$
    Copyright (c) 1998-2002 by Peter Vreman

    This unit implements support import,export,link routines
    for the (i386) BeOS target.

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
unit t_beos;

{$i fpcdefs.inc}

interface

  uses
    symsym,symdef,
    import,export,link;

  type
    timportlibbeos=class(timportlib)
      procedure preparelib(const s:string);override;
      procedure importprocedure(aprocdef:tprocdef;const module:string;index:longint;const name:string);override;
      procedure importvariable(vs:tvarsym;const name,module:string);override;
      procedure generatelib;override;
    end;

    texportlibbeos=class(texportlib)
      procedure preparelib(const s : string);override;
      procedure exportprocedure(hp : texported_item);override;
      procedure exportvar(hp : texported_item);override;
      procedure generatelib;override;
    end;

    tlinkerbeos=class(texternallinker)
    private
      Function  WriteResponseFile(isdll:boolean;makelib:boolean) : Boolean;
    public
      constructor Create;override;
      procedure SetDefaultInfo;override;
      function  MakeExecutable:boolean;override;
      function  MakeSharedLibrary:boolean;override;
    end;


implementation

  uses
{$ifdef delphi}
    dmisc,
{$else}
    dos,
{$endif}
    cutils,cclasses,
    verbose,systems,globtype,globals,
    symconst,script,
    fmodule,aasmbase,aasmtai,aasmcpu,cpubase,i_beos;

{*****************************************************************************
                               TIMPORTLIBBEOS
*****************************************************************************}

procedure timportlibbeos.preparelib(const s : string);
begin
end;


procedure timportlibbeos.importprocedure(aprocdef:tprocdef;const module:string;index:longint;const name:string);
begin
  { insert sharedlibrary }
  current_module.linkothersharedlibs.add(SplitName(module),link_allways);
  { do nothing with the procedure, only set the mangledname }
  if name<>'' then
    aprocdef.setmangledname(name)
  else
    message(parser_e_empty_import_name);
end;


procedure timportlibbeos.importvariable(vs:tvarsym;const name,module:string);
begin
  { insert sharedlibrary }
  current_module.linkothersharedlibs.add(SplitName(module),link_allways);
  { reset the mangledname and turn off the dll_var option }
  vs.set_mangledname(name);
  exclude(vs.varoptions,vo_is_dll_var);
end;


procedure timportlibbeos.generatelib;
begin
end;


{*****************************************************************************
                               TEXPORTLIBBEOS
*****************************************************************************}

procedure texportlibbeos.preparelib(const s:string);
begin
end;


procedure texportlibbeos.exportprocedure(hp : texported_item);
var
  hp2 : texported_item;
begin
  { first test the index value }
  if (hp.options and eo_index)<>0 then
   begin
     Message1(parser_e_no_export_with_index_for_target,'beos');
     exit;
   end;
  { now place in correct order }
  hp2:=texported_item(current_module._exports.first);
  while assigned(hp2) and
     (hp.name^>hp2.name^) do
    hp2:=texported_item(hp2.next);
  { insert hp there !! }
  if assigned(hp2) and (hp2.name^=hp.name^) then
    begin
      { this is not allowed !! }
      Message1(parser_e_export_name_double,hp.name^);
      exit;
    end;
  if hp2=texported_item(current_module._exports.first) then
    current_module._exports.concat(hp)
  else if assigned(hp2) then
    begin
       hp.next:=hp2;
       hp.previous:=hp2.previous;
       if assigned(hp2.previous) then
         hp2.previous.next:=hp;
       hp2.previous:=hp;
    end
  else
    current_module._exports.concat(hp);
end;


procedure texportlibbeos.exportvar(hp : texported_item);
begin
  hp.is_var:=true;
  exportprocedure(hp);
end;


procedure texportlibbeos.generatelib;
var
  hp2 : texported_item;
begin
  hp2:=texported_item(current_module._exports.first);
  while assigned(hp2) do
   begin
     if (not hp2.is_var) and
        (hp2.sym.typ=procsym) then
      begin
        { the manglednames can already be the same when the procedure
          is declared with cdecl }
        if tprocsym(hp2.sym).first_procdef.mangledname<>hp2.name^ then
         begin
{$ifdef i386}
           { place jump in codesegment }
           codesegment.concat(Tai_align.Create_op(4,$90));
           codeSegment.concat(Tai_symbol.Createname_global(hp2.name^,AT_FUNCTION,0));
           codeSegment.concat(Taicpu.Op_sym(A_JMP,S_NO,objectlibrary.newasmsymbol(tprocsym(hp2.sym).first_procdef.mangledname,AB_EXTERNAL,AT_FUNCTION)));
           codeSegment.concat(Tai_symbol_end.Createname(hp2.name^));
{$endif i386}
         end;
      end
     else
      Message1(parser_e_no_export_of_variables_for_target,'beos');
     hp2:=texported_item(hp2.next);
   end;
end;


{*****************************************************************************
                                  TLINKERBEOS
*****************************************************************************}

Constructor TLinkerBeos.Create;
var
  s : string;
  i : integer;
begin
  Inherited Create;
  s:=GetEnv('BELIBRARIES');
  { convert to correct format in case under unix system }
  for i:=1 to length(s) do
    if s[i] = ':' then
      s[i] := ';';
  { just in case we have a single path : add the ending ; }
  { since that is what the compiler expects.              }
  if pos(';',s) = 0 then
    s:=s+';';
  LibrarySearchPath.AddPath(s,true); {format:'path1;path2;...'}
end;


procedure TLinkerBeOS.SetDefaultInfo;
begin
  with Info do
   begin
     ExeCmd[1]:='ld $OPT $DYNLINK $STATIC $STRIP -L. -o $EXE `cat $RES`';
     DllCmd[1]:='ld $OPT $INIT $FINI $SONAME -shared -L. -o $EXE `cat $RES`';
     DllCmd[2]:='strip --strip-unneeded $EXE';
     {
     ExeCmd[1]:='sh $RES $EXE $OPT $STATIC $STRIP -L.';
{     ExeCmd[1]:='sh $RES $EXE $OPT $DYNLINK $STATIC $STRIP -L.';}
      DllCmd[1]:='sh $RES $EXE $OPT -L.';

{     DllCmd[1]:='sh $RES $EXE $OPT -L. -g -nostart -soname=$EXE';
 }    DllCmd[2]:='strip --strip-unneeded $EXE';
{     DynamicLinker:='/lib/ld-beos.so.2';}
      }
   end;
end;


function TLinkerBeOS.WriteResponseFile(isdll:boolean;makelib:boolean) : Boolean;
Var
  linkres  : TLinkRes;
  i        : integer;
  cprtobj,
  prtobj   : string[80];
  HPath    : TStringListItem;
  s        : string;
  linklibc : boolean;
begin
  WriteResponseFile:=False;
{ set special options for some targets }
  linklibc:=(SharedLibFiles.Find('root')<>nil);

  prtobj:='prt0';
  cprtobj:='cprt0';
  if (cs_profile in aktmoduleswitches) or
     (not SharedLibFiles.Empty) then
   begin
     AddSharedLibrary('root');
     linklibc:=true;
   end;

  if (not linklibc) and makelib then
   begin
     linklibc:=true;
     cprtobj:='dllprt.o';
   end;

  if linklibc then
   prtobj:=cprtobj;

  { Open link.res file }
  LinkRes:=TLinkRes.Create(outputexedir+Info.ResName);
  {
  if not isdll then
   LinkRes.Add('ld -o $1 $2 $3 $4 $5 $6 $7 $8 $9 \')
  else
   LinkRes.Add('ld -o $1 -e 0 $2 $3 $4 $5 $6 $7 $8 $9\');
  }
  LinkRes.Add('-m elf_i386_be -shared -Bsymbolic');

  { Write path to search libraries }
  HPath:=TStringListItem(current_module.locallibrarysearchpath.First);
  while assigned(HPath) do
   begin
     LinkRes.Add('-L'+HPath.Str);
     HPath:=TStringListItem(HPath.Next);
   end;
  HPath:=TStringListItem(LibrarySearchPath.First);
  while assigned(HPath) do
   begin
     LinkRes.Add('-L'+HPath.Str);
     HPath:=TStringListItem(HPath.Next);
   end;

  { try to add crti and crtbegin if linking to C }
  if linklibc then
   begin
     if librarysearchpath.FindFile('crti.o',s) then
      LinkRes.AddFileName(s);
     if librarysearchpath.FindFile('crtbegin.o',s) then
      LinkRes.AddFileName(s);
{      s:=librarysearchpath.FindFile('start_dyn.o',found)+'start_dyn.o';
     if found then LinkRes.AddFileName(s+' \');}

     if prtobj<>'' then
      LinkRes.AddFileName(FindObjectFile(prtobj,'',false));

     if isdll then
      LinkRes.AddFileName(FindObjectFile('func.o','',false));

     if librarysearchpath.FindFile('init_term_dyn.o',s) then
      LinkRes.AddFileName(s);
   end
  else
   begin
     if prtobj<>'' then
      LinkRes.AddFileName(FindObjectFile(prtobj,'',false));
   end;

  { main objectfiles }
  while not ObjectFiles.Empty do
   begin
     s:=ObjectFiles.GetFirst;
     if s<>'' then
      LinkRes.AddFileName(s);
   end;

{  LinkRes.Add('-lroot \');
  LinkRes.Add('/boot/develop/tools/gnupro/lib/gcc-lib/i586-beos/2.9-beos-991026/crtend.o \');
  LinkRes.Add('/boot/develop/lib/x86/crtn.o \');}

  { Write staticlibraries }
  if not StaticLibFiles.Empty then
   begin
     While not StaticLibFiles.Empty do
      begin
        S:=StaticLibFiles.GetFirst;
        LinkRes.AddFileName(s)
      end;
   end;

  { Write sharedlibraries like -l<lib> }
  if not SharedLibFiles.Empty then
   begin
     While not SharedLibFiles.Empty do
      begin
        S:=SharedLibFiles.GetFirst;
        if s<>'c' then
         begin
           i:=Pos(target_info.sharedlibext,S);
           if i>0 then
            Delete(S,i,255);
           LinkRes.Add('-l'+s);
         end
        else
         begin
           linklibc:=true;
         end;
      end;
     { be sure that libc is the last lib }
{     if linklibc then
       LinkRes.Add('-lroot');}
{     if linkdynamic and (Info.DynamicLinker<>'') then
       LinkRes.AddFileName(Info.DynamicLinker);}
   end;
  if isdll then
   LinkRes.Add('-lroot');

  { objects which must be at the end }
  if linklibc then
   begin
     if librarysearchpath.FindFile('crtend.o',s) then
      LinkRes.AddFileName(s);
     if librarysearchpath.FindFile('crtn.o',s) then
      LinkRes.AddFileName(s);
   end;

{ Write and Close response }
  linkres.Add(' ');
  linkres.writetodisk;
  linkres.free;

  WriteResponseFile:=True;
end;


function TLinkerBeOS.MakeExecutable:boolean;
var
  binstr : String;
  cmdstr  : TcmdStr;
  success : boolean;
  DynLinkStr : string[60];
  StaticStr,
  StripStr   : string[40];
begin
  if not(cs_link_extern in aktglobalswitches) then
   Message1(exec_i_linking,current_module.exefilename^);

{ Create some replacements }
  StaticStr:='';
  StripStr:='';
  DynLinkStr:='';
  if (cs_link_staticflag in aktglobalswitches) then
   StaticStr:='-static';
  if (cs_link_strip in aktglobalswitches) then
   StripStr:='-s';
  If (cs_profile in aktmoduleswitches) or
     ((Info.DynamicLinker<>'') and (not SharedLibFiles.Empty)) then
   begin
     DynLinkStr:='-dynamic-linker='+Info.DynamicLinker;
     if cshared Then
       DynLinkStr:='--shared ' + DynLinkStr;
     if rlinkpath<>'' Then
       DynLinkStr:='--rpath-link '+rlinkpath + ' '+ DynLinkStr;
   End;

{ Write used files and libraries }
  WriteResponseFile(false,false);

{ Call linker }
  SplitBinCmd(Info.ExeCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$EXE',current_module.exefilename^);
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  Replace(cmdstr,'$RES',outputexedir+Info.ResName);
  Replace(cmdstr,'$STATIC',StaticStr);
  Replace(cmdstr,'$STRIP',StripStr);
  Replace(cmdstr,'$DYNLINK',DynLinkStr);
  success:=DoExec(FindUtil(utilsprefix+BinStr),CmdStr,true,true);

{ Remove ReponseFile }
  if (success) and not(cs_link_extern in aktglobalswitches) then
   RemoveFile(outputexedir+Info.ResName);

  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;


Function TLinkerBeOS.MakeSharedLibrary:boolean;
var
  binstr : String;
  cmdstr  : TCmdStr;
  success : boolean;
  DynLinkStr : string[60];
  StaticStr,
  StripStr   : string[40];

 begin
  MakeSharedLibrary:=false;
  if not(cs_link_extern in aktglobalswitches) then
   Message1(exec_i_linking,current_module.sharedlibfilename^);

{ Create some replacements }
  StaticStr:='';
  StripStr:='';
  DynLinkStr:='';
  if (cs_link_staticflag in aktglobalswitches) then
   StaticStr:='-static';
  if (cs_link_strip in aktglobalswitches) then
   StripStr:='-s';
  If (cs_profile in aktmoduleswitches) or
     ((Info.DynamicLinker<>'') and (not SharedLibFiles.Empty)) then
   begin
     DynLinkStr:='-dynamic-linker='+Info.DynamicLinker;
     if cshared Then
       DynLinkStr:='--shared ' + DynLinkStr;
     if rlinkpath<>'' Then
       DynLinkStr:='--rpath-link '+rlinkpath + ' '+ DynLinkStr;
   End;
{ Write used files and libraries }
  WriteResponseFile(true,true);

{ Call linker }
  SplitBinCmd(Info.DllCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$EXE',current_module.exefilename^);
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  Replace(cmdstr,'$RES',outputexedir+Info.ResName);
  Replace(cmdstr,'$STATIC',StaticStr);
  Replace(cmdstr,'$STRIP',StripStr);
  Replace(cmdstr,'$DYNLINK',DynLinkStr);
  success:=DoExec(FindUtil(utilsprefix+binstr),cmdstr,true,true);

{ Strip the library ? }
  if success and (cs_link_strip in aktglobalswitches) then
   begin
     SplitBinCmd(Info.DllCmd[2],binstr,cmdstr);
     Replace(cmdstr,'$EXE',current_module.sharedlibfilename^);
     success:=DoExec(FindUtil(utilsprefix+binstr),cmdstr,true,false);
   end;

{ Remove ReponseFile }
  if (success) and not(cs_link_extern in aktglobalswitches) then
   RemoveFile(outputexedir+Info.ResName);

  MakeSharedLibrary:=success;   { otherwise a recursive call to link method }
end;


{*****************************************************************************
                                  Initialize
*****************************************************************************}

initialization
{$ifdef i386}
  RegisterExternalLinker(system_i386_beos_info,TLinkerbeos);
  RegisterImport(system_i386_beos,timportlibbeos);
  RegisterExport(system_i386_beos,texportlibbeos);
  RegisterTarget(system_i386_beos_info);
{$endif i386}
end.
{
  $Log$
  Revision 1.13  2004-10-14 18:16:17  mazen
  * USE_SYSUTILS merged successfully : cycles with and without defines
  * Need to be optimized in performance

  Revision 1.12  2004/09/22 15:25:14  mazen
  * Fix error committing : previous version must be in branch USE_SYSUTILS

  Revision 1.10  2004/06/20 08:55:32  florian
    * logs truncated

  Revision 1.9  2004/03/02 00:36:33  olle
    * big transformation of Tai_[const_]Symbol.Create[data]name*

  Revision 1.8  2004/01/29 23:57:15  florian
    * fixed linker response file handling

  Revision 1.7  2004/01/29 22:50:53  florian
    * tried to fix BeOS linking

}
