{
    $Id$
    Copyright (c) 1998-2002 by Peter Vreman (original Linux)
              (c) 2000      by Marco van de Voort (FreeBSD mods)

    This unit implements support import,export,link routines
    for the (i386)FreeBSD target

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
unit t_bsd;

{$i fpcdefs.inc}

interface


implementation

  uses
{$ifdef gdb}
    gdb,
{$endif gdb}
    cutils,cclasses,
    verbose,systems,globtype,globals,
    symconst,script,
    fmodule,aasmbase,aasmtai,aasmcpu,cpubase,symsym,symdef,
    import,export,link,i_bsd,
    cgobj;

  type
    tdarwinimported_item = class(timported_item)
       procdef : tprocdef;
    end;

    timportlibdarwin=class(timportlib)
      procedure preparelib(const s:string);override;
      procedure importprocedure(aprocdef:tprocdef;const module:string;index:longint;const name:string);override;
      procedure importvariable(vs:tvarsym;const name,module:string);override;
      procedure generatelib;override;
      procedure generatesmartlib;override;
     private
      procedure darwinimportproc(aprocdef:tprocdef;const func,module : string;index : longint;const name : string);
      procedure importvariable_str(const s:string;const name,module:string);
    end;

    timportlibbsd=class(timportlib)
      procedure preparelib(const s:string);override;
      procedure importprocedure(aprocdef:tprocdef;const module:string;index:longint;const name:string);override;
      procedure importvariable(vs:tvarsym;const name,module:string);override;
      procedure generatelib;override;
    end;

    texportlibbsd=class(texportlib)
      procedure preparelib(const s : string);override;
      procedure exportprocedure(hp : texported_item);override;
      procedure exportvar(hp : texported_item);override;
      procedure generatelib;override;
    end;

    tlinkerbsd=class(texternallinker)
    private
      Glibc2,
      Glibc21,
      LdSupportsNoResponseFile : boolean;
      LibrarySuffix : Char;
      Function  WriteResponseFile(isdll:boolean) : Boolean;
    public
      constructor Create;override;
      procedure SetDefaultInfo;override;
      function  MakeExecutable:boolean;override;
      function  MakeSharedLibrary:boolean;override;
    end;



{*****************************************************************************
                             TIMPORTLIBDARWIN
*****************************************************************************}

    procedure timportlibdarwin.preparelib(const s : string);
      begin
         if not(assigned(importssection)) then
           importssection:=TAAsmoutput.create;
      end;


    procedure timportlibdarwin.darwinimportproc(aprocdef:tprocdef;const func,module : string;index : longint;const name : string);
      var
         hp1 : timportlist;
         hp2 : tdarwinimported_item;
      begin
         { force the current mangledname }
         if assigned(aprocdef) then
           aprocdef.has_mangledname:=true;
         { search for the module }
         hp1:=timportlist(current_module.imports.first);
         { generate a new item ? }
         if not(assigned(hp1)) then
           begin
              { we don't need an import section per library }
              hp1:=timportlist.create('imports');
              current_module.imports.concat(hp1);
           end;
         { search for reuse of old import item }
         hp2:=tdarwinimported_item(hp1.imported_items.first);
         while assigned(hp2) do
          begin
            if hp2.func^=func then
             break;
            { there's already another declaration refering to this imported symbol }
            { -> make this declaration refer to that entry as well                 }
            if (hp2.name^ = name) then
              begin
                if not assigned(aprocdef) then
                  internalerror(2004010306);
                if assigned(aprocdef) then
                  aprocdef.setmangledname(hp2.func^);
                break;
              end;
            hp2:=tdarwinimported_item(hp2.next);
          end;
         if not assigned(hp2) then
          begin
            hp2:=tdarwinimported_item.create(func,name,index);
            hp2.procdef:=aprocdef;
            hp1.imported_items.concat(hp2);
          end;
      end;


    procedure timportlibdarwin.importprocedure(aprocdef:tprocdef;const module : string;index : longint;const name : string);
      begin
        darwinimportproc(aprocdef,aprocdef.mangledname,module,index,name);
      end;


    procedure timportlibdarwin.importvariable(vs:tvarsym;const name,module:string);
      begin
        importvariable_str(vs.mangledname,name,module);
      end;


    procedure timportlibdarwin.importvariable_str(const s:string;const name,module:string);
      var
         hp1 : timportlist;
         hp2 : tdarwinimported_item;
      begin
         { search for the module }
         hp1:=timportlist(current_module.imports.first);
         { generate a new item ? }
         if not(assigned(hp1)) then
           begin
              hp1:=timportlist.create('imports');
              current_module.imports.concat(hp1);
           end;
         hp2:=tdarwinimported_item.create_var(s,name);
         hp2.procdef:=nil;
         hp1.imported_items.concat(hp2);
      end;


    procedure timportlibdarwin.generatesmartlib;
      begin
         generatelib;
       end;


    procedure timportlibdarwin.generatelib;
      var
         hp1 : timportlist;
         hp2 : tdarwinimported_item;
         l1  : tasmsymbol;
         symname: string;
         mangledstring : string;
{$ifdef GDB}
         importname : string;
         suffix : integer;
{$endif GDB}
         href : treference;
      begin
         hp1:=timportlist(current_module.imports.first);
         while assigned(hp1) do
           begin
              hp2:=tdarwinimported_item(hp1.imported_items.first);
              while assigned(hp2) do
                begin
                   if not assigned(hp2.name) then
                     internalerror(2004010302);
                   symname := hp2.name^;
                   if assigned(tdarwinimported_item(hp2).procdef) and
                      (tdarwinimported_item(hp2).procdef.proccalloption in [pocall_cdecl,pocall_cppdecl]) then
                     symname := target_info.Cprefix+symname;
                   if not hp2.is_var then
                    begin
{$IfDef GDB}
                      if (cs_debuginfo in aktmoduleswitches) then
                        importssection.concat(tai_stab_function_name.create(nil));
{$EndIf GDB}
                      if not assigned(hp2.procdef) then
                        internalerror(2004010306);
                      mangledstring := hp2.func^;
{$ifdef powerpc}
                      if (po_public in hp2.procdef.procoptions) then
                        begin
                          importsSection.concat(Tai_section.Create(sec_code));
                          importsSection.concat(Tai_symbol.createname_global(mangledstring,0));
                          mangledstring := '_$'+mangledstring;
                          importsSection.concat(taicpu.op_sym(A_B,objectlibrary.newasmsymbol(mangledstring)));
                        end;
{$else powerpc}
                      internalerror(2004010501);
{$endif powerpc}
                      
                      importsSection.concat(Tai_section.Create(sec_data));
                      importsSection.concat(Tai_direct.create(strpnew('.section __TEXT,__symbol_stub1,symbol_stubs,pure_instructions,16')));
                      importsSection.concat(Tai_align.Create(4));
                      importsSection.concat(Tai_symbol.Createname(mangledstring,0));
                      importsSection.concat(Tai_direct.create(strpnew((#9+'.indirect_symbol ')+symname)));
                      l1 := objectlibrary.newasmsymbol(mangledstring+'$lazy_ptr');
                      reference_reset_symbol(href,l1,0);
{$IfDef GDB}
                      if (cs_debuginfo in aktmoduleswitches) and assigned(hp2.procdef) then
                       begin
                         mangledstring:=hp2.procdef.mangledname;
                         hp2.procdef.setmangledname(mangledstring);
                         hp2.procdef.concatstabto(importssection);
                         hp2.procdef.setmangledname(mangledstring);
                       end;
{$EndIf GDB}
{$ifdef powerpc}
                      href.symaddr := refs_ha;
                      importsSection.concat(taicpu.op_reg_ref(A_LIS,NR_R11,href));
                      href.symaddr := refs_l;
                      href.base := NR_R11;
                      importsSection.concat(taicpu.op_reg_ref(A_LWZU,NR_R12,href));
                      importsSection.concat(taicpu.op_reg(A_MTCTR,NR_R12));
                      importsSection.concat(taicpu.op_none(A_BCTR));
{$else powerpc}
                      internalerror(2004010502);
{$endif powerpc}
                      importsSection.concat(Tai_section.Create(sec_data));
                      importsSection.concat(Tai_direct.create(strpnew('.lazy_symbol_pointer')));
                      importsSection.concat(Tai_symbol.Create(l1,0));
                      importsSection.concat(Tai_direct.create(strpnew((#9+'.indirect_symbol ')+symname)));
                      importsSection.concat(tai_const_symbol.createname(strpnew('dyld_stub_binding_helper')));
                     
                    end
                   else
                    begin
                      importsSection.concat(Tai_section.Create(sec_data));
                      importsSection.concat(Tai_direct.create(strpnew('.non_lazy_symbol_pointer')));
                      importsSection.concat(Tai_symbol.Createname(hp2.func^,0));
                      importsSection.concat(Tai_direct.create(strpnew((#9+'.indirect_symbol ')+hp2.name^)));
                      importsSection.concat(Tai_const.create_32bit(0));
                    end;
                   hp2:=tdarwinimported_item(hp2.next);
                end;
              hp1:=timportlist(hp1.next);
           end;
      end;


{*****************************************************************************
                               TIMPORTLIBBSD
*****************************************************************************}

procedure timportlibbsd.preparelib(const s : string);
begin
end;


procedure timportlibbsd.importprocedure(aprocdef:tprocdef;const module:string;index:longint;const name:string);
begin
  { insert sharedlibrary }
  current_module.linkothersharedlibs.add(SplitName(module),link_allways);
  { do nothing with the procedure, only set the mangledname }
  if name<>'' then
   begin
     aprocdef.setmangledname(name);
   end
  else
    message(parser_e_empty_import_name);
end;


procedure timportlibbsd.importvariable(vs:tvarsym;const name,module:string);
begin
  { insert sharedlibrary }
  current_module.linkothersharedlibs.add(SplitName(module),link_allways);
  { reset the mangledname and turn off the dll_var option }
  vs.set_mangledname(name);
  exclude(vs.varoptions,vo_is_dll_var);
end;


procedure timportlibbsd.generatelib;
begin
end;


{*****************************************************************************
                               TEXPORTLIBBSD
*****************************************************************************}

procedure texportlibbsd.preparelib(const s:string);
begin
end;


procedure texportlibbsd.exportprocedure(hp : texported_item);
var
  hp2 : texported_item;
begin
  { first test the index value }
  if (hp.options and eo_index)<>0 then
   begin
     Message1(parser_e_no_export_with_index_for_target,'freebsd');
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


procedure texportlibbsd.exportvar(hp : texported_item);
begin
  hp.is_var:=true;
  exportprocedure(hp);
end;


procedure texportlibbsd.generatelib;
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
           codeSegment.concat(Tai_symbol.Createname_global(hp2.name^,0));
           codeSegment.concat(Taicpu.Op_sym(A_JMP,S_NO,objectlibrary.newasmsymbol(tprocsym(hp2.sym).first_procdef.mangledname)));
           codeSegment.concat(Tai_symbol_end.Createname(hp2.name^));
{$endif i386}
         end;
      end
     else
      Message1(parser_e_no_export_of_variables_for_target,'freebsd');
     hp2:=texported_item(hp2.next);
   end;
end;


{*****************************************************************************
                                  TLINKERLINUX
*****************************************************************************}

Constructor TLinkerBSD.Create;
begin
  Inherited Create;
  if not Dontlinkstdlibpath Then
   if (target_info.system <> system_powerpc_darwin) then
     LibrarySearchPath.AddPath('/lib;/usr/lib;/usr/X11R6/lib',true)
   else
     { Mac OS X doesn't have a /lib }
     LibrarySearchPath.AddPath('/usr/lib',true)
end;


procedure TLinkerBSD.SetDefaultInfo;
{
  This will also detect which libc version will be used
}
begin
  LibrarySuffix:=' ';
  LdSupportsNoResponseFile := (target_info.system in [system_m68k_netbsd,system_powerpc_darwin]);
  with Info do
   begin
     if LdSupportsNoResponseFile then
       begin
         ExeCmd[1]:='ld $OPT $DYNLINK $STATIC $STRIP -L. -o $EXE `cat $RES`';
       end
     else
       ExeCmd[1]:='ld $OPT $DYNLINK $STATIC $STRIP -L. -o $EXE $RES';
     DllCmd[1]:='ld $OPT -shared -L. -o $EXE $RES';
     DllCmd[2]:='strip --strip-unneeded $EXE';
     { first try glibc2 }
{$ifdef GLIBC2} {Keep linux code in place. FBSD might go to a different
                                glibc too once}
     DynamicLinker:='/lib/ld-linux.so.2';
     if FileExists(DynamicLinker) then
      begin
        Glibc2:=true;
        { Check for 2.0 files, else use the glibc 2.1 stub }
        if FileExists('/lib/ld-2.0.*') then
         Glibc21:=false
        else
         Glibc21:=true;
      end
     else
      DynamicLinker:='/lib/ld-linux.so.1';
{$else}
      DynamicLinker:='';
{$endif}
   end;
end;


Function TLinkerBSD.WriteResponseFile(isdll:boolean) : Boolean;
Var
  linkres      : TLinkRes;
  i            : longint;
  cprtobj,
  gprtobj,
  prtobj       : string[80];
  HPath        : TStringListItem;
  s,s1,s2      : string;
  linkdynamic,
  linklibc     : boolean;
  Fl1,Fl2      : Boolean;

begin
  WriteResponseFile:=False;
{ set special options for some targets }
  if target_info.system <> system_powerpc_darwin then
    begin
      linkdynamic:=not(SharedLibFiles.empty);
      linklibc:=(SharedLibFiles.Find('c')<>nil);
      prtobj:='prt0';
      cprtobj:='cprt0';
      gprtobj:='gprt0';
      if glibc21 then
       begin
         cprtobj:='cprt21';
         gprtobj:='gprt21';
       end;
      if cs_profile in aktmoduleswitches then
       begin
         prtobj:=gprtobj;
    {
         if not glibc2 then
          AddSharedLibrary('gmon');
    }
         AddSharedLibrary('c');
         LibrarySuffix:='p';
         linklibc:=true;
       end
      else
       begin
         if linklibc then
          prtobj:=cprtobj;
       end;
    end
  else
    begin
      { for darwin: always link dynamically against libc }
      linklibc := true;
      if not(cs_profile in aktmoduleswitches) then
        prtobj:='/usr/lib/crt1.o'
      else
        prtobj:='/usr/lib/gcrt1.o';
    end;

  { Open link.res file }
  LinkRes:=TLinkRes.Create(outputexedir+Info.ResName);

  { Write path to search libraries }
  HPath:=TStringListItem(current_module.locallibrarysearchpath.First);
  while assigned(HPath) do
   begin
     if LdSupportsNoResponseFile then
       LinkRes.Add('-L'+HPath.Str)
     else
       LinkRes.Add('SEARCH_DIR('+HPath.Str+')');
     HPath:=TStringListItem(HPath.Next);
   end;
  HPath:=TStringListItem(LibrarySearchPath.First);
  while assigned(HPath) do
   begin
     if LdSupportsNoResponseFile then
       LinkRes.Add('-L'+HPath.Str)
     else
       LinkRes.Add('SEARCH_DIR('+HPath.Str+')');
     HPath:=TStringListItem(HPath.Next);
   end;

  if not LdSupportsNoResponseFile then
    LinkRes.Add('INPUT(');
  { add objectfiles, start with prt0 always }
  if prtobj<>'' then
   LinkRes.AddFileName(FindObjectFile(prtobj,'',false));
  { try to add crti and crtbegin if linking to C }
  if linklibc and
     (target_info.system <> system_powerpc_darwin) then
   begin
     if librarysearchpath.FindFile('crtbegin.o',s) then
      LinkRes.AddFileName(s);
     if librarysearchpath.FindFile('crti.o',s) then
      LinkRes.AddFileName(s);
   end;
  { main objectfiles }
  while not ObjectFiles.Empty do
   begin
     s:=ObjectFiles.GetFirst;
     if s<>'' then
      LinkRes.AddFileName(s);
   end;
  if not LdSupportsNoResponseFile then
   LinkRes.Add(')');

  { Write staticlibraries }
  if not StaticLibFiles.Empty then
   begin
     if not LdSupportsNoResponseFile then
       LinkRes.Add('GROUP(');
     While not StaticLibFiles.Empty do
      begin
        S:=StaticLibFiles.GetFirst;
        LinkRes.AddFileName(s)
      end;
     if not LdSupportsNoResponseFile then
       LinkRes.Add(')');
   end;

  { Write sharedlibraries like -l<lib>, also add the needed dynamic linker
    here to be sure that it gets linked this is needed for glibc2 systems (PFV) }
  if not SharedLibFiles.Empty then
   begin
     if not LdSupportsNoResponseFile then
       LinkRes.Add('INPUT(');
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
           linkdynamic:=false; { libc will include the ld-linux for us }
         end;
      end;
     { be sure that libc is the last lib }
     if linklibc then
       Begin
         If LibrarySuffix=' ' Then
          LinkRes.Add('-lc')
         else
          LinkRes.Add('-lc_'+LibrarySuffix)
       end;
     { when we have -static for the linker the we also need libgcc }
     if (cs_link_staticflag in aktglobalswitches) then
      LinkRes.Add('-lgcc');
     if linkdynamic and (Info.DynamicLinker<>'') then
      LinkRes.AddFileName(Info.DynamicLinker);
     if not LdSupportsNoResponseFile then
       LinkRes.Add(')');
   end;
  { objects which must be at the end }
  if linklibc and
     (target_info.system <> system_powerpc_darwin) then
   begin
     Fl1:=librarysearchpath.FindFile('crtend.o',s1);
     Fl2:=librarysearchpath.FindFile('crtn.o',s2);
     if Fl1 or Fl2 then
      begin
        LinkRes.Add('INPUT(');
         If Fl1 Then
        LinkRes.AddFileName(s1);
        If Fl2 Then
         LinkRes.AddFileName(s2);
        LinkRes.Add(')');
      end;
   end;
{ Write and Close response }
  linkres.writetodisk;
  linkres.Free;

  WriteResponseFile:=True;
end;


function TLinkerBSD.MakeExecutable:boolean;
var
  binstr,
  cmdstr  : string;
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
    begin
      if (target_info.system=system_m68k_netbsd) and
         ((cs_link_on_target in aktglobalswitches) or
          (target_info.system=source_info.system)) then
        StaticStr:='-Bstatic'
      else
        StaticStr:='-static';
    end;
  if (cs_link_strip in aktglobalswitches) then
   StripStr:='-s';
  If (cs_profile in aktmoduleswitches) or
     ((Info.DynamicLinker<>'') and (not SharedLibFiles.Empty)) then
   DynLinkStr:='-dynamic-linker='+Info.DynamicLinker;

  if CShared Then
   DynLinKStr:=DynLinkStr+' --shared';
{ Write used files and libraries }
  WriteResponseFile(false);

{ Call linker }
  SplitBinCmd(Info.ExeCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$EXE',current_module.exefilename^);
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  Replace(cmdstr,'$RES',outputexedir+Info.ResName);
  Replace(cmdstr,'$STATIC',StaticStr);
  Replace(cmdstr,'$STRIP',StripStr);
  Replace(cmdstr,'$DYNLINK',DynLinkStr);
  success:=DoExec(FindUtil(utilsprefix+BinStr),CmdStr,true,LdSupportsNoResponseFile);

{ Remove ReponseFile }
  if (success) and not(cs_link_extern in aktglobalswitches) then
   RemoveFile(outputexedir+Info.ResName);

  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;


Function TLinkerBSD.MakeSharedLibrary:boolean;
var
  binstr,
  cmdstr  : string;
  success : boolean;
begin
  MakeSharedLibrary:=false;
  if not(cs_link_extern in aktglobalswitches) then
   Message1(exec_i_linking,current_module.sharedlibfilename^);

{ Write used files and libraries }
  WriteResponseFile(true);

{ Call linker }
  SplitBinCmd(Info.DllCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$EXE',current_module.sharedlibfilename^);
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  Replace(cmdstr,'$RES',outputexedir+Info.ResName);
  success:=DoExec(FindUtil(utilsprefix+binstr),cmdstr,true,false);

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
  RegisterExternalLinker(system_i386_FreeBSD_info,TLinkerBSD);
  RegisterExternalLinker(system_i386_NetBSD_info,TLinkerBSD);
  RegisterExternalLinker(system_i386_OpenBSD_info,TLinkerBSD);
  RegisterImport(system_i386_freebsd,timportlibbsd);
  RegisterExport(system_i386_freebsd,texportlibbsd);
  RegisterTarget(system_i386_freebsd_info);
  RegisterImport(system_i386_netbsd,timportlibbsd);
  RegisterExport(system_i386_netbsd,texportlibbsd);
  RegisterTarget(system_i386_netbsd_info);
  RegisterImport(system_i386_openbsd,timportlibbsd);
  RegisterExport(system_i386_openbsd,texportlibbsd);
  RegisterTarget(system_i386_openbsd_info);
{$endif i386}
{$ifdef m68k}
//  RegisterExternalLinker(system_m68k_FreeBSD_info,TLinkerBSD);
  RegisterExternalLinker(system_m68k_NetBSD_info,TLinkerBSD);
  RegisterImport(system_m68k_netbsd,timportlibbsd);
  RegisterExport(system_m68k_netbsd,texportlibbsd);
  RegisterTarget(system_m68k_netbsd_info);
{$endif m68k}
{$ifdef powerpc}
//  RegisterExternalLinker(system_m68k_FreeBSD_info,TLinkerBSD);
  RegisterExternalLinker(system_powerpc_darwin_info,TLinkerBSD);
  RegisterImport(system_powerpc_darwin,timportlibdarwin);
  RegisterExport(system_powerpc_darwin,texportlibbsd);
  RegisterTarget(system_powerpc_darwin_info);
  RegisterExternalLinker(system_powerpc_netbsd_info,TLinkerBSD);
  RegisterImport(system_powerpc_netbsd,timportlibbsd);
  RegisterExport(system_powerpc_netbsd,texportlibbsd);
  RegisterTarget(system_powerpc_netbsd_info);
{$endif powerpc}
end.
{
  $Log$
  Revision 1.8  2004-01-21 20:53:51  marco
   * Copy and pasted some structures from Net- to OpenBSD (3.4+ ELF!)

  Revision 1.7  2004/01/05 08:13:30  jonas
    * fixed compilation problems under x86

  Revision 1.6  2004/01/04 21:26:31  jonas
    + Darwin support for routines imported from external libraries (not yet
      ideal, we should generate stubs in all files where the routines are
      used -> these are automatically merged by the linker; now we generate
      one global symbol with a jump to a stub in unit where the routine is
      declared)
    + (not yet working) Darwin support for imported variables
    + Darwin support for linking

  Revision 1.5  2003/10/30 18:35:30  marco
   * librarysuffix + profiling

  Revision 1.4  2003/10/11 19:32:04  marco
   * -Xd

  Revision 1.3  2003/10/03 14:16:48  marco
   * -XP<prefix> support

  Revision 1.2  2003/05/25 23:15:04  marco
   * NetBSD target support. OpenBSD reserved in the enum, for future use.

  Revision 1.1  2003/05/20 23:54:00  florian
    + basic darwin support added

  Revision 1.5  2003/04/27 07:29:52  peter
    * aktprocdef cleanup, aktprocdef is now always nil when parsing
      a new procdef declaration
    * aktprocsym removed
    * lexlevel removed, use symtable.symtablelevel instead
    * implicit init/final code uses the normal genentry/genexit
    * funcret state checking updated for new funcret handling

  Revision 1.4  2003/04/26 09:16:08  peter
    * .o files belonging to the unit are first searched in the same dir
      as the .ppu

  Revision 1.3  2003/01/18 16:16:13  marco
   * Small fix for netbsd

  Revision 1.2  2002/09/09 17:34:17  peter
    * tdicationary.replace added to replace and item in a dictionary. This
      is only allowed for the same name
    * varsyms are inserted in symtable before the types are parsed. This
      fixes the long standing "var longint : longint" bug
    - consume_idlist and idstringlist removed. The loops are inserted
      at the callers place and uses the symtable for duplicate id checking

  Revision 1.1  2002/09/06 15:03:51  carl
    * moved files to systems directory

  Revision 1.29  2002/09/03 16:26:28  daniel
    * Make Tprocdef.defs protected

  Revision 1.28  2002/08/12 15:08:44  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.27  2002/08/11 14:32:32  peter
    * renamed current_library to objectlibrary

  Revision 1.26  2002/08/11 13:24:19  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.25  2002/07/26 21:15:45  florian
    * rewrote the system handling

  Revision 1.24  2002/07/24 13:51:34  marco
   * Fixed small error

  Revision 1.23  2002/07/24 13:10:22  marco
   * urgent fix.

  Revision 1.22  2002/07/01 18:46:34  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.21  2002/05/18 13:34:26  peter
    * readded missing revisions

  Revision 1.20  2002/05/16 19:46:53  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.18  2002/04/22 18:19:22  carl
  - remove use_bound_instruction field

  Revision 1.17  2002/04/20 21:43:18  carl
  * fix stack size for some targets
  + add offset to parameters from frame pointer info.
  - remove some unused stuff

  Revision 1.16  2002/04/19 15:46:04  peter
    * mangledname rewrite, tprocdef.mangledname is now created dynamicly
      in most cases and not written to the ppu
    * add mangeledname_prefix() routine to generate the prefix of
      manglednames depending on the current procedure, object and module
    * removed static procprefix since the mangledname is now build only
      on demand from tprocdef.mangledname

  Revision 1.15  2002/04/15 19:16:57  carl
  - remove size_of_pointer field

  Revision 1.14  2002/01/29 21:27:34  peter
    * default alignment changed to 4 bytes for locals and static const,var

}
