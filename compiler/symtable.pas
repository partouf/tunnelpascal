{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl, Pierre Muller

    This unit handles the symbol tables

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
{$ifdef TP}
  {$N+,E+,F+}
{$endif}
unit symtable;

  interface

    uses
{$ifdef TP}
       objects,
{$endif}
       strings,cobjects,
       globtype,globals,tokens,systems,verbose,
       aasm
{$ifdef i386}
  {$ifndef OLDASM}
       ,i386base
  {$else}
       ,i386
  {$endif}
{$endif}
{$ifdef m68k}
       ,m68k
{$endif}
{$ifdef alpha}
       ,alpha
{$endif}
{$ifdef GDB}
       ,gdb
{$endif}
       ;

{$ifdef OLDPPU}
  {define NOLOCALBROWSER if you have problems with -bl option }
{$endif}

{************************************************
           Some internal constants
************************************************}

   const
       hasharraysize    = 256;
{$ifndef OLDPPU}
  {$ifdef TP}
       indexgrowsize    = 256;
  {$else}
       indexgrowsize    = 1024;
  {$endif}
{$else}
       defhasharraysize = 16000;
{$endif}


{************************************************
                Constants
************************************************}

{$i symconst.inc}


{************************************************
            Needed forward pointers
************************************************}

    type
       { needed for owner (table) of symbol }
       psymtable     = ^tsymtable;
       punitsymtable = ^tunitsymtable;

       { needed for names by the definitions }
       ptypesym = ^ttypesym;
       penumsym = ^tenumsym;

       pref = ^tref;
       tref = object
         nextref     : pref;
         posinfo     : tfileposinfo;
         moduleindex : word;
         is_written  : boolean;
         constructor init(ref:pref;pos:pfileposinfo);
         destructor  done; virtual;
       end;

{************************************************
                    TDef
************************************************}

{$i symdefh.inc}

{************************************************
                   TSym
************************************************}

{$i symsymh.inc}

{************************************************
                 TSymtable
************************************************}

       tsymtabletype = (invalidsymtable,withsymtable,staticsymtable,
                        globalsymtable,unitsymtable,
                        objectsymtable,recordsymtable,
                        macrosymtable,localsymtable,
                        parasymtable,inlineparasymtable,
                        inlinelocalsymtable,stt_exceptsymtable,
                        { only used for PPU reading of static part
                          of a unit }
                        staticppusymtable);

       tcallback = procedure(p : psym);

{$ifdef OLDPPU}
       tnamedindexcallback = procedure(p : psym);
{$endif}

       tsearchhasharray = array[0..hasharraysize-1] of psym;
       psearchhasharray = ^tsearchhasharray;

{$ifdef OLDPPU}
       tdefhasharray = array[0..defhasharraysize-1] of pdef;
       pdefhasharray = ^tdefhasharray;
{$endif}

       tsymtable = object
          symtabletype : tsymtabletype;
          unitid    : word;           { each symtable gets a number }
          name      : pstring;
          datasize  : longint;
{$ifndef OLDPPU}
          symindex,
          defindex  : pindexarray;
          symsearch : pdictionary;
{$else}
          searchroot : psym;
          searchhasharray : psearchhasharray;
          lastsym   : psym;
          rootdef   : pdef;
          defhasharraysize : longint;
          defhasharray : pdefhasharray;
{$endif}
          next      : psymtable;
          defowner  : pdef; { for records and objects }
          { alignment used in this symtable }
          alignment : longint;
          { only used for parameter symtable to determine the offset relative }
          { to the frame pointer and for local inline }
          address_fixup : longint;
          { this saves all definition to allow a proper clean up }
          { separate lexlevel from symtable type }
          symtablelevel : byte;
          constructor init(t : tsymtabletype);
          destructor  done;virtual;
          { access }
{$ifdef OLDPPU}
          { indexes all defs from 0 to num and return num + 1 }
          function  number_defs:longint;
          { indexes all symbols from 1 to num and return num }
          function  number_symbols:longint;
{$endif}
          function getdefnr(l : longint) : pdef;
          function getsymnr(l : longint) : psym;
          { load/write }
          constructor load;
          procedure write;
          constructor loadas(typ : tsymtabletype);
          procedure writeas;
          procedure loaddefs;
          procedure loadsyms;
          procedure writedefs;
          procedure writesyms;
{$ifndef OLDPPU}
          procedure deref;
{$endif}
          procedure clear;
          function  rename(const olds,news : stringid):psym;
          procedure foreach(proc2call : tnamedindexcallback);
          function  insert(sym : psym):psym;
          function  search(const s : stringid) : psym;
          function  speedsearch(const s : stringid;speedvalue : longint) : psym;
          procedure registerdef(p : pdef);
          procedure allsymbolsused;
          procedure allunitsused;
          procedure check_forwards;
          procedure checklabels;
          { change alignment for args  only parasymtable }
          procedure set_alignment(_alignment : byte);
          { find arg having offset  only parasymtable }
          function  find_at_offset(l : longint) : pvarsym;
{$ifdef CHAINPROCSYMS}
          procedure chainprocsyms;
{$endif CHAINPROCSYMS}
          procedure load_browser;
          procedure write_browser;
{$ifdef BrowserLog}
          procedure writebrowserlog;
{$endif BrowserLog}
{$ifdef GDB}
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
          function getnewtypecount : word; virtual;
       end;

       tunitsymtable = object(tsymtable)
          unittypecount  : word;
          unitsym        : punitsym;
{$ifdef GDB}
          dbx_count : longint;
          prev_dbx_counter : plongint;
          dbx_count_ok : boolean;
          is_stab_written : boolean;
{$endif GDB}
          constructor init(t : tsymtabletype;const n : string);
          constructor loadasunit;
          destructor done;virtual;
          procedure writeasunit;
{$ifdef GDB}
{$ifdef OLDPPU}
          procedure orderdefs;
{$endif}
          procedure concattypestabto(asmlist : paasmoutput);
{$endif GDB}
          procedure load_symtable_refs;
          function getnewtypecount : word; virtual;
       end;

       pwithsymtable = ^twithsymtable;
       twithsymtable = object(tsymtable)
{$ifndef NODIRECTWITH}
          { used for withsymtable for allowing constructors }
          direct_with : boolean;
          { in fact it is a ptree }
          withnode : pointer;
          { ptree to load of direct with var }
          { already usable before firstwith
            needed for firstpass of function parameters PM }
          withrefnode : pointer;
{$endif def NODIRECTWITH}
          constructor init;
          destructor  done;virtual;
        end;

{****************************************************************************
                              Var / Consts
****************************************************************************}

    const
       systemunit            : punitsymtable = nil; { pointer to the system unit }
       objpasunit            : punitsymtable = nil; { pointer to the objpas unit }
       current_object_option : symprop = sp_public;

    var
       { for STAB debugging }
       globaltypecount  : word;
       pglobaltypecount : pword;

       registerdef : boolean;      { true, when defs should be registered }

       defaultsymtablestack,       { symtablestack after default units
                                     have been loaded }
       symtablestack : psymtable;  { linked list of symtables }

       srsym : psym;               { result of the last search }
       srsymtable : psymtable;
       lastsrsym : psym;           { last sym found in statement }
       lastsrsymtable : psymtable;
       lastsymknown : boolean;

       forwardsallowed : boolean;  { true, wenn forward pointers can be
                                     inserted }

       constsymtable : psymtable;  { symtable were the constants can be
                                     inserted }

       voidpointerdef : ppointerdef; { pointer for Void-Pointerdef      }
       charpointerdef : ppointerdef; { pointer for Char-Pointerdef      }
       voidfarpointerdef : ppointerdef;

       cformaldef : pformaldef;    { unique formal definition          }
       voiddef   : porddef;        { Pointer to Void (procedure)       }
       cchardef  : porddef;        { Pointer to Char                   }
       booldef   : porddef;        { pointer to boolean type           }
       u8bitdef  : porddef;        { Pointer to 8-Bit unsigned         }
       u16bitdef : porddef;        { Pointer to 16-Bit unsigned        }
       u32bitdef : porddef;        { Pointer to 32-Bit unsigned        }
       s32bitdef : porddef;        { Pointer to 32-Bit signed          }

       cu64bitdef : porddef;       { pointer to 64 bit unsigned def }
       cs64bitdef : porddef;       { pointer to 64 bit signed def, }
                                   { calculated by the int unit on i386 }

       s32floatdef : pfloatdef;    { pointer for realconstn            }
       s64floatdef : pfloatdef;    { pointer for realconstn            }
       s80floatdef : pfloatdef;    { pointer to type of temp. floats   }
       s32fixeddef : pfloatdef;    { pointer to type of temp. fixed    }

       cshortstringdef : pstringdef;  { pointer to type of short string const   }
       clongstringdef  : pstringdef;  { pointer to type of long string const   }
       cansistringdef  : pstringdef;  { pointer to type of ansi string const  }
       cwidestringdef  : pstringdef;  { pointer to type of wide string const  }
       openshortstringdef : pstringdef;  { pointer to type of an open shortstring,
                                            needed for readln() }
       openchararraydef : parraydef;     { pointer to type of an open array of char,
                                            needed for readln() }

       cfiledef : pfiledef;       { get the same definition for all file }
                                  { uses for stabs }

       firstglobaldef,         { linked list of all globals defs }
       lastglobaldef : pdef;   { used to reset stabs/ranges }

       class_tobject : pobjectdef; { pointer to the anchestor of all   }
                                   { clases                            }

       aktprocsym : pprocsym;      { pointer to the symbol for the
                                     currently be parsed procedure }

       aktcallprocsym : pprocsym;  { pointer to the symbol for the
                                     currently be called procedure,
                                     only set/unset in firstcall }

       aktvarsym : pvarsym;        { pointer to the symbol for the
                                     currently read var, only used
                                     for variable directives }

       procprefix : string;        { eindeutige Namen bei geschachtel- }
                                   { ten Unterprogrammen erzeugen      }

       lexlevel : longint;         { level of code                     }
                                   { 1 for main procedure              }
                                   { 2 for normal function or proc     }
                                   { higher for locals                 }
    const
       main_program_level = 1;
       unit_init_level = 1;
       normal_function_level = 2;
       in_loading : boolean = false;

{$ifdef i386}
       bestrealdef : ^pfloatdef = @s80floatdef;
{$endif}
{$ifdef m68k}
       bestrealdef : ^pfloatdef = @s64floatdef;
{$endif}

    var

       macros : psymtable;         { pointer for die Symboltabelle mit  }
                                   { Makros                            }

       read_member : boolean;      { true, wenn Members aus einer PPU-  }
                                   { Datei gelesen werden, d.h. ein     }
                                   { varsym seine Adresse einlesen soll }

       generrorsym : psym;         { Jokersymbol, wenn das richtige    }
                                   { Symbol nicht gefunden wird        }

       generrordef : pdef;         { Jokersymbol for eine fehlerhafte  }
                                   { Typdefinition                     }

       aktobjectdef : pobjectdef;  { used for private functions check !! }

    const
       { last operator which can be overloaded }
       first_overloaded = PLUS;
       last_overloaded  = ASSIGNMENT;
    var
       overloaded_operators : array[first_overloaded..last_overloaded] of pprocsym;
       { unequal is not equal}
    const
       overloaded_names : array [first_overloaded..last_overloaded] of string[16] =
         ('plus','minus','star','slash','equal',
          'greater','lower','greater_or_equal',
          'lower_or_equal','as','is','in','sym_diff',
          'starstar','assign');


{****************************************************************************
                             Functions
****************************************************************************}

{*** Misc ***}
    function  globaldef(const s : string) : pdef;
    procedure duplicatesym(sym:psym);

{*** Search ***}
    function  search_a_symtable(const symbol:string;symtabletype:tsymtabletype):Psym;
    procedure getsym(const s : stringid;notfounderror : boolean);
    procedure getsymonlyin(p : psymtable;const s : stringid);

{*** Forwards ***}
    procedure save_forward(ppd : ppointerdef;typesym : ptypesym);
    procedure resolve_forwards;

{*** PPU Write/Loading ***}
    procedure writeunitas(const s : string;unittable : punitsymtable;only_crc : boolean);
    procedure closecurrentppu;
    procedure numberunits;
    procedure load_interface;

{*** GDB ***}
{$ifdef GDB}
    function  typeglobalnumber(const s : string) : string;
{$endif}

{*** Definition ***}
   procedure reset_global_defs;

{*** Object Helpers ***}
    function search_class_member(pd : pobjectdef;const n : string) : psym;
    function search_default_property(pd : pobjectdef) : ppropertysym;

{*** Macro ***}
    procedure def_macro(const s : string);
    procedure set_macro(const s : string;value : string);

{*** symtable stack ***}
    procedure dellexlevel;
{$ifdef DEBUG}
    procedure test_symtablestack;
    procedure list_symtablestack;
{$endif DEBUG}

{*** dispose of a pdefcoll (args of a function) ***}
    procedure disposepdefcoll(var para1 : pdefcoll);

{*** Init / Done ***}
    procedure InitSymtable;
    procedure DoneSymtable;


implementation

  uses
     version,
     types,ppu,
     gendef,files
     ,tree
{$ifdef newcg}
     ,cgbase
{$else}
     ,hcodegen
{$endif}
{$ifdef BrowserLog}
     ,browlog
{$endif BrowserLog}
     ;

  var
     aktrecordsymtable : psymtable; { current record read from ppu symtable }
     aktstaticsymtable : psymtable; { current static for local ppu symtable }
{$ifdef GDB}
     asmoutput : paasmoutput;
{$endif GDB}
{$ifdef TP}
   {$ifndef dpmi}
       symbolstream : temsstream;  { stream which is used to store some info }
   {$else}
       symbolstream : tmemorystream;
   {$endif}
{$endif}

   {to dispose the global symtable of a unit }
  const
     dispose_global : boolean = false;
     memsizeinc = 2048; { for long stabstrings }
     tagtypes : Set of tdeftype =
       [recorddef,enumdef,
       {$IfNDef GDBKnowsStrings}
       stringdef,
       {$EndIf not GDBKnowsStrings}
       {$IfNDef GDBKnowsFiles}
       filedef,
       {$EndIf not GDBKnowsFiles}
       objectdef];

{*****************************************************************************
                             Helper Routines
*****************************************************************************}

    function demangledparas(s : string) : string;
      var
         r : string;
         l : longint;
      begin
         demangledparas:='';
         r:=',';
         { delete leading $$'s }
         l:=pos('$$',s);
         while l<>0 do
           begin
              delete(s,1,l+1);
              l:=pos('$$',s);
           end;
         l:=pos('$',s);
         if l=0 then
           exit;
         delete(s,1,l);
         l:=pos('$',s);
         if l=0 then
           l:=length(s)+1;
         while s<>'' do
           begin
              r:=r+copy(s,1,l-1)+',';
              delete(s,1,l);
           end;
         delete(r,1,1);
         delete(r,length(r),1);
         demangledparas:=r;
      end;


    procedure numberunits;
      var
        counter : longint;
        hp      : pused_unit;
      begin
        counter:=1;
        psymtable(current_module^.globalsymtable)^.unitid:=0;
        hp:=pused_unit(current_module^.used_units.first);
        while assigned(hp) do
         begin
           psymtable(hp^.u^.globalsymtable)^.unitid:=counter;
           inc(counter);
           hp:=pused_unit(hp^.next);
         end;
      end;


   procedure setstring(var p : pchar;const s : string);
     begin
{$ifdef TP}
       if use_big then
        begin
          p:=pchar(symbolstream.getsize);
          symbolstream.seek(longint(p));
          symbolstream.writestr(@s);
        end
       else
{$endif TP}
        p:=strpnew(s);
     end;


     procedure duplicatesym(sym:psym);
       begin
         Message1(sym_e_duplicate_id,sym^.name);
         with sym^.fileinfo do
          Message2(sym_h_duplicate_id_where,current_module^.sourcefiles^.get_file_name(fileindex),tostr(line));
       end;


{****************************************************************************
                               TRef
****************************************************************************}

    constructor tref.init(ref :pref;pos : pfileposinfo);
      begin
        nextref:=nil;
        if pos<>nil then
          posinfo:=pos^;
        if assigned(current_module) then
          moduleindex:=current_module^.unit_index;
        if assigned(ref) then
          ref^.nextref:=@self;
        is_written:=false;
      end;


    destructor tref.done;
      var
         inputfile : pinputfile;
      begin
         inputfile:=get_source_file(moduleindex,posinfo.fileindex);
         if inputfile<>nil then
           dec(inputfile^.ref_count);
         if assigned(nextref) then
          dispose(nextref,done);
         nextref:=nil;
      end;


{*****************************************************************************
                           PPU Reading Writing
*****************************************************************************}

{$I symppu.inc}


{*****************************************************************************
                            Definition Helpers
*****************************************************************************}

    function globaldef(const s : string) : pdef;

      var st : string;
          symt : psymtable;
      begin
         srsym := nil;
         if pos('.',s) > 0 then
           begin
           st := copy(s,1,pos('.',s)-1);
           getsym(st,false);
           st := copy(s,pos('.',s)+1,255);
           if assigned(srsym) then
             begin
             if srsym^.typ = unitsym then
               begin
               symt := punitsym(srsym)^.unitsymtable;
               srsym := symt^.search(st);
               end else srsym := nil;
             end;
           end else st := s;
         if srsym = nil then getsym(st,false);
         if srsym = nil then
           getsymonlyin(systemunit,st);
         if srsym^.typ<>typesym then
           begin
             Message(type_e_type_id_expected);
             exit;
           end;
         globaldef := ptypesym(srsym)^.definition;
      end;

{*****************************************************************************
                        Symbol / Definition Resolving
*****************************************************************************}

const localsymtablestack : psymtable = nil;

    function find_local_symtable(index : word) : psymtable;
    var
       p : psymtable;
      begin
         p:=localsymtablestack;
         while assigned(p) do
           begin
              if p^.unitid=index then break
              else
                p:=p^.next;
           end;
         if (p=nil) then
           comment(v_fatal,'Error in local browser');
         find_local_symtable:=p;
      end;

    procedure resolvesym(var d : psym);
      begin
        if longint(d)=-1 then
          d:=nil
        else
          begin
            if (longint(d) and $ffff)=$ffff then
              d:=aktrecordsymtable^.getsymnr(longint(d) shr 16)
            else
            if (longint(d) and $ffff)=$fffe then
              d:=aktstaticsymtable^.getsymnr(longint(d) shr 16)
            else if (longint(d) and $ffff)>$8000 then
              d:=find_local_symtable(longint(d) and $ffff)^.getsymnr(longint(d) shr 16)
            else
{$ifdef NEWMAP}
              d:=psymtable(current_module^.map^[longint(d) and $ffff]^.globalsymtable)^.getsymnr(longint(d) shr 16);
{$else NEWMAP}
              d:=psymtable(current_module^.map^[longint(d) and $ffff])^.getsymnr(longint(d) shr 16);
{$endif NEWMAP}
          end;
      end;

    procedure resolvedef(var d : pdef);
      begin
        if longint(d)=-1 then
          d:=nil
        else
          begin
            if (longint(d) and $ffff)=$ffff then
              d:=aktrecordsymtable^.getdefnr(longint(d) shr 16)
            else
            if (longint(d) and $ffff)=$fffe then
              d:=aktstaticsymtable^.getdefnr(longint(d) shr 16)
            else if (longint(d) and $ffff)>$8000 then
              d:=find_local_symtable(longint(d) and $ffff)^.getdefnr(longint(d) shr 16)
            else
{$ifdef NEWMAP}
              d:=psymtable(current_module^.map^[longint(d) and $ffff]^.globalsymtable)^.getdefnr(longint(d) shr 16);
{$else NEWMAP}
              d:=psymtable(current_module^.map^[longint(d) and $ffff])^.getdefnr(longint(d) shr 16);
{$endif NEWMAP}
           end;
      end;


{*****************************************************************************
                        Symbol Call Back Functions
*****************************************************************************}

{$ifdef OLDPPU}
    procedure writesym(p : psym);
      begin
         p^.write;
      end;
{$endif}

    procedure derefsym(p : {$ifndef OLDPPU}pnamedindexobject{$else}psym{$endif});
      begin
         psym(p)^.deref;
      end;

    procedure derefsymsdelayed(p : {$ifndef OLDPPU}pnamedindexobject{$else}psym{$endif});
      begin
         if psym(p)^.typ in [absolutesym,propertysym] then
           psym(p)^.deref;
      end;

    procedure check_procsym_forward(sym : {$ifndef OLDPPU}pnamedindexobject{$else}psym{$endif});
      begin
         if psym(sym)^.typ=procsym then
           pprocsym(sym)^.check_forward
         { check also object method table             }
         { we needn't to test the def list            }
         { because each object has to have a type sym }
         else
          if (psym(sym)^.typ=typesym) and
             assigned(ptypesym(sym)^.definition) and
             (ptypesym(sym)^.definition^.deftype=objectdef) then
           pobjectdef(ptypesym(sym)^.definition)^.check_forwards;
      end;

    procedure labeldefined(p : {$ifndef OLDPPU}pnamedindexobject{$else}psym{$endif});
      begin
        if (psym(p)^.typ=labelsym) and
           not(plabelsym(p)^.defined) then
          Message1(sym_w_label_not_defined,p^.name);
      end;

    procedure unitsymbolused(p : {$ifndef OLDPPU}pnamedindexobject{$else}psym{$endif});
      begin
         if (psym(p)^.typ=unitsym) and
            (punitsym(p)^.refs=0) then
           comment(V_info,'Unit '+p^.name+' is not used');
      end;

    procedure varsymbolused(p : {$ifndef OLDPPU}pnamedindexobject{$else}psym{$endif});
      begin
         if (psym(p)^.typ=varsym) and
            ((psym(p)^.owner^.symtabletype in [parasymtable,localsymtable,staticsymtable])) then
           { unused symbol should be reported only if no }
           { error is reported                           }
           { if the symbol is in a register it is used   }
           { also don't count the value parameters which have local copies }
           { also don't claim for high param of open parameters (PM) }
           if (pvarsym(p)^.refs=0) and
              (Errorcount=0) and
              (copy(p^.name,1,3)<>'val') and
              (copy(p^.name,1,4)<>'high') then
             begin
                if (psym(p)^.owner^.symtabletype=parasymtable) or pvarsym(p)^.islocalcopy then
                  MessagePos1(psym(p)^.fileinfo,sym_h_para_identifier_not_used,p^.name)
                else
                  MessagePos1(psym(p)^.fileinfo,sym_n_local_identifier_not_used,p^.name);
             end;
      end;

{$ifdef GDB}
    procedure concatstab(p : {$ifndef OLDPPU}pnamedindexobject{$else}psym{$endif});
      begin
        if psym(p)^.typ <> procsym then
          psym(p)^.concatstabto(asmoutput);
      end;

    procedure concattypestab(p : {$ifndef OLDPPU}pnamedindexobject{$else}psym{$endif});
      begin
        if psym(p)^.typ = typesym then
         begin
           psym(p)^.isstabwritten:=false;
           psym(p)^.concatstabto(asmoutput);
         end;
      end;

    procedure forcestabto(asmlist : paasmoutput; pd : pdef);
      begin
        if not pd^.is_def_stab_written then
         begin
           if assigned(pd^.sym) then
            pd^.sym^.isusedinstab := true;
           pd^.concatstabto(asmlist);
         end;
      end;
{$endif}

{$ifdef CHAINPROCSYMS}
    procedure chainprocsym(p : psym);
      var
         storesymtablestack : psymtable;
      begin
         if p^.typ=procsym then
           begin
              storesymtablestack:=symtablestack;
              symtablestack:=p^.owner^.next;
              while assigned(symtablestack) do
                begin
                  { search for same procsym in other units }
                  getsym(p^.name,false);
                  if assigned(srsym) and (srsym^.typ=procsym) then
                    begin
                       pprocsym(p)^.nextprocsym:=pprocsym(srsym);
                       symtablestack:=storesymtablestack;
                       exit;
                    end
                  else if srsym=nil then
                    symtablestack:=nil
                  else
                    symtablestack:=srsymtable^.next;
                end;
              symtablestack:=storesymtablestack;
           end;
      end;
{$endif}

    procedure write_refs(sym : {$ifndef OLDPPU}pnamedindexobject{$else}psym{$endif});
      begin
         psym(sym)^.write_references;
      end;

{$ifdef BrowserLog}
    procedure add_to_browserlog(p : psym);
      begin
         p^.add_to_browserlog;
      end;
{$endif UseBrowser}


{****************************************************************************
                             Forward Resolving
****************************************************************************}

    type
       presolvelist = ^tresolvelist;
       tresolvelist = record
          p : ppointerdef;
          typ : ptypesym;
          next : presolvelist;
       end;

    var
       sroot : presolvelist;
    procedure save_forward(ppd : ppointerdef;typesym : ptypesym);
      var
         p : presolvelist;
      begin
         new(p);
         p^.next:=sroot;
         p^.p:=ppd;
         ppd^.defsym := typesym;
         p^.typ:=typesym;
         sroot:=p;
      end;


    procedure resolve_forwards;
      var
         p : presolvelist;
      begin
         p:=sroot;
         while p<>nil do
           begin
              sroot:=sroot^.next;
              p^.p^.definition:=p^.typ^.definition;
              dispose(p);
              p:=sroot;
           end;
      end;


{*****************************************************************************
                          Search Symtables for Syms
*****************************************************************************}

    procedure getsym(const s : stringid;notfounderror : boolean);
      var
        speedvalue : longint;
      begin
         speedvalue:=getspeedvalue(s);
         lastsrsym:=nil;
         srsymtable:=symtablestack;
         while assigned(srsymtable) do
           begin
              srsym:=srsymtable^.speedsearch(s,speedvalue);
              if assigned(srsym) then
                exit
              else
                srsymtable:=srsymtable^.next;
           end;
         if forwardsallowed then
           begin
              srsymtable:=symtablestack;
              while (srsymtable^.symtabletype in [objectsymtable,recordsymtable]) do
                   srsymtable:=srsymtable^.next;
              srsym:=new(ptypesym,init(s,nil));
              srsym^.properties:=sp_forwarddef;
              srsymtable^.insert(srsym);
           end
         else if notfounderror then
           begin
              Message1(sym_e_id_not_found,s);
              srsym:=generrorsym;
           end
         else srsym:=nil;
      end;


    procedure getsymonlyin(p : psymtable;const s : stringid);
      begin
         { the caller have to take care if srsym=nil (FK) }
         srsym:=nil;
         if assigned(p) then
           begin
              srsymtable:=p;
              srsym:=srsymtable^.search(s);
              if assigned(srsym) then
                exit
              else
               begin
                  if (punitsymtable(srsymtable)=punitsymtable(current_module^.globalsymtable)) then
                    begin
                       getsymonlyin(psymtable(current_module^.localsymtable),s);
                       if assigned(srsym) then
                         srsymtable:=psymtable(current_module^.localsymtable)
                       else
                         Message1(sym_e_id_not_found,s);
                    end
                  else
                    Message1(sym_e_id_not_found,s);
               end;
           end;
      end;


    function search_a_symtable(const symbol:string;symtabletype:tsymtabletype):Psym;
    {Search for a symbol in a specified symbol table. Returns nil if
     the symtable is not found, and also if the symbol cannot be found
     in the desired symtable }
    var hsymtab:Psymtable;
        res:Psym;
    begin
        res:=nil;
        hsymtab:=symtablestack;
        while (hsymtab<>nil) and (hsymtab^.symtabletype<>symtabletype) do
            hsymtab:=hsymtab^.next;
        if hsymtab<>nil then
            {We found the desired symtable. Now check if the symbol we
             search for is defined in it }
            res:=hsymtab^.search(symbol);
        search_a_symtable:=res;
    end;


{****************************************************************************
                                TSYMTABLE
****************************************************************************}

    constructor tsymtable.init(t : tsymtabletype);
      begin
         symtabletype:=t;
         symtablelevel:=0;
         defowner:=nil;
         unitid:=0;
         next:=nil;
         name:=nil;
         address_fixup:=0;
         datasize:=0;
{$ifndef OLDPPU}
         new(symindex,init(indexgrowsize));
         new(defindex,init(indexgrowsize));
         if symtabletype<>withsymtable then
           begin
              new(symsearch,init);
              symsearch^.noclear:=true;
           end
         else
           symsearch:=nil;
{$else}
         lastsym:=nil;
         rootdef:=nil;
         defhasharray:=nil;
         defhasharraysize:=0;
         searchroot:=nil;
         searchhasharray:=nil;
{$endif}
         alignment:=def_alignment;
      end;


    destructor tsymtable.done;
{$ifdef OLDPPU}
      var
         hp : pdef;
  {$ifdef GDB}
         last : pdef;
  {$endif GDB}
{$endif}
      begin
        stringdispose(name);
{$ifndef OLDPPU}
        dispose(symindex,done);
        dispose(defindex,done);
        { symsearch can already be disposed or set to nil for withsymtable }
        if assigned(symsearch) then
         begin
           dispose(symsearch,done);
           symsearch:=nil;
         end;
{$else}
        if assigned(defhasharray) then
          begin
             freemem(defhasharray,sizeof(pdef)*defhasharraysize);
             defhasharray:=nil;
          end;
      { clear all entries, pprocsyms have still the definitions left }
        clear;
  {$ifdef GDB}
        last := Nil;
  {$endif GDB}
         hp:=rootdef;
         while assigned(hp) do
           begin
  {$ifdef GDB}
              if hp^.owner=@self then
               begin
                 if assigned(last) then
                  last^.next := hp^.next;
  {$endif GDB}
                 rootdef:=hp^.next;
                 dispose(hp,done);
  {$ifdef GDB}
                end
              else
                begin
                  last := hp;
                  rootdef:=hp^.next;
                end;
  {$endif GDB}
              hp:=rootdef;
           end;
{$endif}
      end;


    constructor twithsymtable.init;
      begin
         inherited init(withsymtable);
{$ifndef NODIRECTWITH}
         direct_with:=false;
         withnode:=nil;
         withrefnode:=nil;
{$endif def NODIRECTWITH}
      end;


    destructor twithsymtable.done;
      begin
{$ifndef OLDPPU}
        symsearch:=nil;
{$endif}
        inherited done;
      end;


{***********************************************
                Helpers
***********************************************}

   function tsymtable.getnewtypecount : word;
      begin
         getnewtypecount:=pglobaltypecount^;
         inc(pglobaltypecount^);
      end;

    procedure tsymtable.registerdef(p : pdef);
      begin
{$ifndef OLDPPU}
         defindex^.insert(p);
{$else}
         p^.next:=rootdef;
         rootdef:=p;
{$endif}
         { set def owner and indexnb }
         p^.owner:=@self;
      end;

{$ifndef OLDPPU}

    procedure tsymtable.foreach(proc2call : tnamedindexcallback);
      begin
        symindex^.foreach(proc2call);
      end;

{$else}

    procedure tsymtable.foreach(proc2call : tnamedindexcallback);

        procedure a(p : psym);
        { must be preorder, because it's used by reading in }
        { a PPU file                                        }
        { what does this mean ? I need to index
          so proc2call must be after left and before right !! PM }
        begin
          proc2call(p);
          if assigned(p^.left) then
            a(p^.left);
          if assigned(p^.right) then
            a(p^.right);
        end;

      var
         i : longint;
      begin
        if assigned(searchhasharray) then
         begin
           for i:=0 to hasharraysize-1 do
            if assigned(searchhasharray^[i]) then
             a(searchhasharray^[i]);
         end
        else
         if assigned(searchroot) then
          a(searchroot);
      end;

{$endif}

{$ifdef OLDPPU}

    function tsymtable.number_defs:longint;
      var
         pd : pdef;
         counter : longint;
      begin
         counter:=0;
         pd:=rootdef;
         while assigned(pd) do
           begin
              pd^.indexnb:=counter;
              inc(counter);
              pd:=pd^.next;
           end;
         number_defs:=counter;
      end;


   var symtable_index : longint;

    procedure numbersym(p : psym);

      begin
          p^.indexnb:=symtable_index;
          inc(symtable_index);
      end;


    function tsymtable.number_symbols:longint;
      var old_nr : longint;
      begin
        old_nr:=symtable_index;
        symtable_index:=1;
        {$ifdef tp}
        foreach(numbersym);
        {$else}
        foreach(@numbersym);
        {$endif}
        number_symbols:=symtable_index-1;
        symtable_index:=old_nr;
      end;
{$endif}


{***********************************************
       LOAD / WRITE SYMTABLE FROM PPU
***********************************************}

    procedure tsymtable.loaddefs;
      var
{$ifdef OLDPPU}
        counter : longint;
        last : pdef;
{$endif}
        hp : pdef;
        b  : byte;
      begin
      { load start of definition section, which holds the amount of defs }
         if current_ppu^.readentry<>ibstartdefs then
          Message(unit_f_ppu_read_error);
{$ifdef OLDPPU}
         if symtabletype=unitsymtable then
          begin
            defhasharraysize:=current_ppu^.getlongint;
            getmem(defhasharray,sizeof(pdef)*defhasharraysize);
            fillchar(defhasharray^,sizeof(pdef)*defhasharraysize,0);
          end
         else
{$endif}
           current_ppu^.getlongint;
      { read definitions }
{$ifdef OLDPPU}
         counter:=0;
         rootdef:=nil;
{$endif}
         repeat
           b:=current_ppu^.readentry;
           case b of
              ibpointerdef : hp:=new(ppointerdef,load);
                ibarraydef : hp:=new(parraydef,load);
                  iborddef : hp:=new(porddef,load);
                ibfloatdef : hp:=new(pfloatdef,load);
                 ibprocdef : hp:=new(pprocdef,load);
          ibshortstringdef : hp:=new(pstringdef,shortload);
           iblongstringdef : hp:=new(pstringdef,longload);
           ibansistringdef : hp:=new(pstringdef,ansiload);
           ibwidestringdef : hp:=new(pstringdef,wideload);
               ibrecorddef : hp:=new(precdef,load);
               ibobjectdef : hp:=new(pobjectdef,load);
                 ibenumdef : hp:=new(penumdef,load);
                  ibsetdef : hp:=new(psetdef,load);
              ibprocvardef : hp:=new(pprocvardef,load);
                 ibfiledef : hp:=new(pfiledef,load);
             ibclassrefdef : hp:=new(pclassrefdef,load);
               ibformaldef : hp:=new(pformaldef,load);
                 ibenddefs : break;
                     ibend : Message(unit_f_ppu_read_error);
           else
             Message1(unit_f_ppu_invalid_entry,tostr(b));
           end;
{$ifndef OLDPPU}
           hp^.owner:=@self;
           defindex^.insert(hp);
{$else}
         { each def gets a number }
           hp^.indexnb:=counter;
           if counter=0 then
             begin
                rootdef:=hp;
                last:=hp;
             end
           else
             begin
                last^.next:=hp;
                last:=hp;
             end;
           if assigned(defhasharray) then
             begin
               if counter<defhasharraysize then
                 defhasharray^[counter]:=hp
               else
                 internalerror(10997);
             end;
           inc(counter);
{$endif}
         until false;
{$ifdef OLDPPU}
         number_defs;
{$endif}
      end;


    procedure tsymtable.loadsyms;
      var
        b   : byte;
        sym : psym;
      begin
      { load start of definition section, which holds the amount of defs }
         if current_ppu^.readentry<>ibstartsyms then
          Message(unit_f_ppu_read_error);
         { skip amount of symbols, not used currently }
         current_ppu^.getlongint;
         { load datasize of this symboltable }
         datasize:=current_ppu^.getlongint;
      { now read the symbols }
         repeat
           b:=current_ppu^.readentry;
           case b of
                ibtypesym : sym:=new(ptypesym,load);
                ibprocsym : sym:=new(pprocsym,load);
               ibconstsym : sym:=new(pconstsym,load);
                 ibvarsym : sym:=new(pvarsym,load);
             ibfuncretsym : sym:=new(pfuncretsym,load);
            ibabsolutesym : sym:=new(pabsolutesym,load);
                ibenumsym : sym:=new(penumsym,load);
          ibtypedconstsym : sym:=new(ptypedconstsym,load);
            ibpropertysym : sym:=new(ppropertysym,load);
                ibunitsym : sym:=new(punitsym,load);
               iblabelsym : sym:=new(plabelsym,load);
{$ifndef OLDPPU}
                 ibsyssym : sym:=new(psyssym,load);
{$endif}
                ibendsyms : break;
                    ibend : Message(unit_f_ppu_read_error);
           else
             Message1(unit_f_ppu_invalid_entry,tostr(b));
           end;
{$ifndef OLDPPU}
           sym^.owner:=@self;
           symindex^.insert(sym);
           symsearch^.insert(sym);
{$else}
           if not (symtabletype in [recordsymtable,objectsymtable]) then
            begin
              { don't deref absolute symbols there, because it's possible   }
              { that the var sym which the absolute sym refers, isn't       }
              { loaded                                                      }
              { but syms must be derefered to determine the definition      }
              { because must know the varsym size when inserting the symbol }
              if not(b in [ibabsolutesym,ibpropertysym]) then
                sym^.deref;
            end;
           insert(sym);
{$endif}
         until false;

{$ifdef OLDPPU}
       { symbol numbering for references }
         number_symbols;

         if not (symtabletype in [recordsymtable,objectsymtable]) then
          begin
            {$ifdef tp}
             foreach(derefsymsdelayed);
            {$else}
             foreach(@derefsymsdelayed);
            {$endif}
          end;
{$endif}
      end;


    procedure tsymtable.writedefs;
      var
         pd : pdef;
      begin
      { each definition get a number, write then the amount of defs to the
         ibstartdef entry }
{$ifndef OLDPPU}
         current_ppu^.putlongint(defindex^.count);
{$else}
         current_ppu^.putlongint(number_defs);
{$endif}
         current_ppu^.writeentry(ibstartdefs);
      { now write the definition }
{$ifndef OLDPPU}
         pd:=pdef(defindex^.first);
{$else}
         pd:=rootdef;
{$endif}
         while assigned(pd) do
           begin
              pd^.write;
              pd:=pdef(pd^.next);
           end;
      { write end of definitions }
         current_ppu^.writeentry(ibenddefs);
      end;


    procedure tsymtable.writesyms;
{$ifndef OLDPPU}
      var
        pd : psym;
{$endif}
      begin
       { each definition get a number, write then the amount of syms and the
         datasize to the ibsymdef entry }
{$ifndef OLDPPU}
         current_ppu^.putlongint(symindex^.count);
{$else}
         current_ppu^.putlongint(number_symbols);
{$endif}
         current_ppu^.putlongint(datasize);
         current_ppu^.writeentry(ibstartsyms);
       { foreach is used to write all symbols }
{$ifndef OLDPPU}
         pd:=psym(symindex^.first);
         while assigned(pd) do
           begin
              pd^.write;
              pd:=psym(pd^.next);
           end;
{$else}
         {$ifdef tp}
           foreach(writesym);
         {$else}
           foreach(@writesym);
         {$endif}
{$endif}
       { end of symbols }
         current_ppu^.writeentry(ibendsyms);
      end;


{$ifndef OLDPPU}
    procedure tsymtable.deref;
      var
        hp : pdef;
        hs : psym;
      begin
        hp:=pdef(defindex^.first);
        while assigned(hp) do
         begin
           hp^.deref;
           hp^.symderef;
           hp:=pdef(hp^.next);
         end;

        hs:=psym(symindex^.first);
        while assigned(hs) do
         begin
           hs^.deref;
           hs:=psym(hs^.next);
         end;
      end;
{$endif}


    constructor tsymtable.load;
      var
{$ifdef OLDPPU}
         hp : pdef;
{$endif}
         st_loading : boolean;
      begin
        st_loading:=in_loading;
        in_loading:=true;
{$ifndef NEWMAP}
        current_module^.map^[0]:=@self;
{$else NEWMAP}
        current_module^.globalsymtable:=@self;
{$endif NEWMAP}

        symtabletype:=unitsymtable;
        symtablelevel:=0;

        { unused for units }
        address_fixup:=0;

        datasize:=0;
        defowner:=nil;
        name:=nil;
        unitid:=0;
        defowner:=nil;
{$ifndef OLDPPU}
        new(symindex,init(indexgrowsize));
        new(defindex,init(indexgrowsize));
        new(symsearch,init);
        symsearch^.usehash;
        symsearch^.noclear:=true;
{$else}
        lastsym:=nil;
        next:=nil;
        rootdef:=nil;
        defhasharray:=nil;
        defhasharraysize:=0;
        { reset search arrays }
        searchroot:=nil;
        new(searchhasharray);
        fillchar(searchhasharray^,sizeof(searchhasharray^),0);
{$endif}
        alignment:=def_alignment;

      { load definitions }
        loaddefs;
{$ifdef OLDPPU}
      { solve the references to other definitions for each definition }
        hp:=rootdef;
        while assigned(hp) do
         begin
           hp^.deref;
           { insert also the owner }
           hp^.owner:=@self;
           hp:=pdef(hp^.next);
         end;
{$endif}

      { load symbols }
        loadsyms;

{$ifndef OLDPPU}
        if not(symtabletype in [objectsymtable,recordsymtable]) then
          deref;
{$endif}

{$ifdef NEWMAP}
        { necessary for dependencies }
        current_module^.globalsymtable:=nil;
{$endif NEWMAP}
        in_loading:=st_loading;
      end;


    procedure tsymtable.write;
      begin
      { write definitions }
         writedefs;
      { write symbols }
         writesyms;
      end;


    constructor tsymtable.loadas(typ : tsymtabletype);
      var
         storesymtable : psymtable;
{$ifdef OLDPPU}
         hp : pdef;
{$endif}
         st_loading : boolean;
      begin
         st_loading:=in_loading;
         in_loading:=true;
         symtabletype:=typ;
{$ifndef OLDPPU}
         new(symindex,init(indexgrowsize));
         new(defindex,init(indexgrowsize));
         new(symsearch,init);
         symsearch^.noclear:=true;
{$else}
         lastsym:=nil;
         next:=nil;
         rootdef:=nil;
         defhasharray:=nil;
         defhasharraysize:=0;
         searchroot:=nil;
         searchhasharray:=nil;
{$endif}
         defowner:=nil;
         storesymtable:=aktrecordsymtable;
         if typ in [recordsymtable,objectsymtable,
                    parasymtable,localsymtable] then
           aktrecordsymtable:=@self;
         { used for local browser }
         if typ=staticppusymtable then
           begin
              aktstaticsymtable:=@self;
{$ifndef OLDPPU}
              symsearch^.usehash;
{$else}
              new(searchhasharray);
              fillchar(searchhasharray^,sizeof(searchhasharray^),0);
{$endif}
           end;
         name:=nil;
         alignment:=def_alignment;
         { isn't used there }
         datasize:=0;
         address_fixup:= 0;
         { also unused }
         unitid:=0;

      { load definitions }
      { we need the correct symtable for registering }
         if not (typ in [recordsymtable,objectsymtable]) then
           begin
             next:=symtablestack;
             symtablestack:=@self;
           end;

         loaddefs;

{$ifdef OLDPPU}
       { solve the references of the symbols for each definition }
         hp:=rootdef;
         if not (typ in [recordsymtable,objectsymtable]) then
          while assigned(hp) do
           begin
              hp^.deref;
              { insert also the owner }
              hp^.owner:=@self;
              hp:=pdef(hp^.next);
           end;
{$endif}

      { load symbols }
         loadsyms;

{$ifndef OLDPPU}
         if not (typ in [recordsymtable,objectsymtable]) then
           deref;
{$endif}

         aktrecordsymtable:=storesymtable;
         if not (typ in [recordsymtable,objectsymtable]) then
           begin
             symtablestack:=next;
           end;
        in_loading:=st_loading;
      end;


    procedure tsymtable.writeas;
      var
         oldtyp : byte;
         storesymtable : psymtable;
      begin
         oldtyp:=current_ppu^.entrytyp;
         storesymtable:=aktrecordsymtable;
         if symtabletype in [recordsymtable,objectsymtable,
                    parasymtable,localsymtable] then
           aktrecordsymtable:=@self;
         if (symtabletype in [recordsymtable,objectsymtable]) then
         current_ppu^.entrytyp:=subentryid;
         { write definitions }
         writedefs;
         { write symbols }
         writesyms;
         current_ppu^.entrytyp:=oldtyp;
         aktrecordsymtable:=storesymtable;
      end;


{***********************************************
          Get Symbol / Def by Number
***********************************************}

{$ifndef OLDPPU}

    function tsymtable.getsymnr(l : longint) : psym;
      var
        hp : psym;
      begin
        hp:=psym(symindex^.search(l));
        if hp=nil then
         internalerror(10999);
        getsymnr:=hp;
      end;

    function tsymtable.getdefnr(l : longint) : pdef;
      var
        hp : pdef;
      begin
        hp:=pdef(defindex^.search(l));
        if hp=nil then
         internalerror(10998);
        getdefnr:=hp;
      end;

{$else}

    function tsymtable.getsymnr(l : longint) : psym;
      var
         hp : psym;
         i  : longint;
      begin
          getsymnr:=nil;
          if assigned(searchhasharray) then
            begin
               hp:=nil;
               for i:=0 to hasharraysize-1 do
                 if assigned(searchhasharray^[i]) then
                   if (searchhasharray^[i]^.indexnb>l) then
                     break
                   else
                     hp:=searchhasharray^[i];
            end
          else
            hp:=searchroot;
          { hp has an index that is <= l               }
          { if hp's index = l we found                 }
          { if hp^.right exists and is also <= l       }
          { the sym is in the right branch             }
          { else in the left                           }
          while assigned(hp) do
            begin
               if hp^.indexnb=l then
                 begin
                    getsymnr:=hp;
                    exit;
                 end
               else if assigned(hp^.right) and (hp^.right^.indexnb<=l) then
                 hp:=hp^.right
               else
                 hp:=hp^.left;
            end;
        InternalError(10999);
      end;


    function tsymtable.getdefnr(l : longint) : pdef;
      var
         hp : pdef;
      begin
         if assigned(defhasharray) and
            (l<defhasharraysize) and
            assigned(defhasharray^[l]) and
            (defhasharray^[l]^.indexnb=l) then
           begin
              getdefnr:=defhasharray^[l];
              exit;
           end;
         hp:=rootdef;
         while (assigned(hp)) and (hp^.indexnb<>l) do
           hp:=hp^.next;
         if assigned(defhasharray) and
            (l<defhasharraysize) then
           if not assigned(defhasharray^[l]) then
             defhasharray^[l]:=hp
           else
             begin
{$ifdef debug}
                if (l<defhasharraysize) and
                   (hp<>defhasharray^[l]) then
                  InternalError(10998);
{$endif debug}
             end;
         if assigned(hp) then
           getdefnr:=hp
         else
           InternalError(10998);
      end;

{$endif}

{***********************************************
                Table Access
***********************************************}

{$ifndef OLDPPU}

    procedure tsymtable.clear;
      begin
         { remove no entry from a withsymtable as it is only a pointer to the
         recorddef  or objectdef symtable }
         if symtabletype=withsymtable then
           exit;
         symindex^.clear;
         defindex^.clear;
      end;


    function tsymtable.insert(sym:psym):psym;
      var
         hp : psymtable;
         hsym : psym;
      begin
         { set owner and sym indexnb }
         sym^.owner:=@self;
{$ifdef CHAINPROCSYMS}
         { set the nextprocsym field }
         if sym^.typ=procsym then
           chainprocsym(sym);
{$endif CHAINPROCSYMS}
         { writes the symbol in data segment if required }
         { also sets the datasize of owner               }
         if not in_loading then
           sym^.insert_in_data;
         if (symtabletype in [staticsymtable,globalsymtable]) then
           begin
              hp:=symtablestack;
              while assigned(hp) do
                begin
                   if hp^.symtabletype in [staticsymtable,globalsymtable] then
                    begin
                       hsym:=hp^.search(sym^.name);
                       if (assigned(hsym)) and
                          (hsym^.properties and sp_forwarddef=0) then
                         DuplicateSym(hsym);
                    end;
                  hp:=hp^.next;
                end;
           end;

         { check for duplicate id in local and parsymtable symtable }
         if (symtabletype=localsymtable) then
           { to be on the sure side: }
           begin
              if assigned(next) and
                (next^.symtabletype=parasymtable) then
                begin
                   hsym:=next^.search(sym^.name);
                   if assigned(hsym) then
                     DuplicateSym(hsym);
                end
              else if (current_module^.flags and uf_local_browser)=0 then
                internalerror(43789);
           end;

         { check for duplicate id in local symtable of methods }
         if (symtabletype=localsymtable) and
           assigned(next) and
           assigned(next^.next) and
          { funcretsym is allowed !! }
           (sym^.typ <> funcretsym) and
           (next^.next^.symtabletype=objectsymtable) then
           begin
              hsym:=search_class_member(pobjectdef(next^.next^.defowner),sym^.name);
              { but private ids can be reused }
              if assigned(hsym) and
                ((hsym^.properties<>sp_private) or
                 (hsym^.owner^.defowner^.owner^.symtabletype<>unitsymtable)) then
                DuplicateSym(hsym);
           end;
         { check for duplicate field id in inherited classes }
         if (sym^.typ=varsym) and
            (symtabletype=objectsymtable) and
            assigned(defowner) then
           begin
              hsym:=search_class_member(pobjectdef(defowner),sym^.name);
              { but private ids can be reused }
              if assigned(hsym) and
                ((hsym^.properties<>sp_private) or
                 (hsym^.owner^.defowner^.owner^.symtabletype<>unitsymtable)) then
                DuplicateSym(hsym);
           end;

         if sym^.typ = typesym then
           if assigned(ptypesym(sym)^.definition) then
             begin
             if not assigned(ptypesym(sym)^.definition^.owner) and
                (ptypesym(sym)^.definition^.deftype<>errordef) then
              registerdef(ptypesym(sym)^.definition);
{$ifdef GDB}
             if (cs_debuginfo in aktmoduleswitches) and assigned(debuglist)
                and (symtabletype in [globalsymtable,staticsymtable]) then
                   begin
                   ptypesym(sym)^.isusedinstab := true;
                   sym^.concatstabto(debuglist);
                   end;
{$endif GDB}
             end;
         { insert in index and search hash }
         symindex^.insert(sym);
         symsearch^.insert(sym);
         insert:=sym;
      end;


    function tsymtable.search(const s : stringid) : psym;
      begin
        search:=psym(symsearch^.search(s));
      end;


    function tsymtable.speedsearch(const s : stringid;speedvalue : longint) : psym;
      var
        hp : psym;
      begin
        hp:=psym(symsearch^.speedsearch(s,speedvalue));
        if assigned(hp) then
         begin
           { reject non static members in static procedures,
             be carefull aktprocsym^.definition is not allways
             loaded already (PFV) }
           if (symtabletype=objectsymtable) and
              ((hp^.properties and sp_static)=0) and
              allow_only_static
              {assigned(aktprocsym) and
              assigned(aktprocsym^.definition) and
              ((aktprocsym^.definition^.options and postaticmethod)<>0)} then
                  Message(sym_e_only_static_in_static);
           if (symtabletype=unitsymtable) and
              assigned(punitsymtable(@self)^.unitsym) then
             inc(punitsymtable(@self)^.unitsym^.refs);
           { unitsym are only loaded for browsing PM    }
           { this was buggy anyway because we could use }
           { unitsyms from other units in _USES !!      }
           if (symtabletype=unitsymtable) and (hp^.typ=unitsym) and
              assigned(current_module) and (current_module^.globalsymtable<>@self) then
             hp:=nil;
           if assigned(hp) and
              (cs_browser in aktmoduleswitches) and make_ref then
             begin
                hp^.lastref:=new(pref,init(hp^.lastref,@tokenpos));
                { for symbols that are in tables without
                browser info or syssyms (PM) }
                if hp^.refcount=0 then
                  hp^.defref:=hp^.lastref;
                inc(hp^.refcount);
             end;
         end;
        speedsearch:=hp;
      end;


    function tsymtable.rename(const olds,news : stringid):psym;
      begin
        rename:=psym(symsearch^.rename(olds,news));
      end;

{$else}


    procedure tsymtable.clear;
      var
         w : longint;
      begin
         { remove no entry from a withsymtable as it is only a pointer to the
         recorddef  or objectdef symtable }
         if symtabletype=withsymtable then
           exit;
         { remove all entry from a symbol table }
         if assigned(searchroot) then
           begin
             dispose(searchroot,done);
             searchroot:=nil;
           end;
         if assigned(searchhasharray) then
           begin
              for w:=0 to hasharraysize-1 do
                if assigned(searchhasharray^[w]) then
                  begin
                    dispose(searchhasharray^[w],done);
                    searchhasharray^[w]:=nil;
                  end;
              dispose(searchhasharray);
              searchhasharray:=nil;
           end;
      end;


    function tsymtable.insert(sym:psym):psym;
      var
        ref : pref;

      function _insert(var osym : psym):psym;
      {To prevent TP from allocating temp space for temp strings, we allocate
       some temp strings manually. We can use two temp strings, plus a third
       one that TP adds, where TP alone needs five temp strings!. Storing
       these on the heap saves even more, totally 1016 bytes per recursion!}
        var
          s1,s2:^string;
          lasthfp,hfp : pforwardpointer;
        begin
           if osym=nil then
             begin
               osym:=sym;
               _insert:=osym;
{$ifndef nonextfield}
               if assigned(lastsym) then
                 lastsym^.nextsym:=sym;
               lastsym:=sym;
{$endif}
             end

         { first check speedvalue, to allow a fast insert }
           else
             if osym^.speedvalue>sym^.speedvalue then
               _insert:=_insert(psym(osym^.right))
           else
             if osym^.speedvalue<sym^.speedvalue then
               _insert:=_insert(psym(osym^.left))
           else
             begin
                new(s1);
                new(s2);
                s1^:=osym^.name;
                s2^:=sym^.name;
                if s1^>s2^ then
                  begin
                    dispose(s2);
                    dispose(s1);
                    _insert:=_insert(psym(osym^.right));
                  end
                else
                  if s1^<s2^ then
                    begin
                      dispose(s2);
                      dispose(s1);
                      _insert:=_insert(psym(osym^.left));
                    end
                else
                  begin
                     dispose(s2);
                     dispose(s1);
                     if (osym^.typ=typesym) and (osym^.properties=sp_forwarddef) then
                       begin
                          if (sym^.typ<>typesym) then
                           Message(sym_f_id_already_typed);
                          {
                          if (ptypesym(sym)^.definition^.deftype<>recorddef) and
                             (ptypesym(sym)^.definition^.deftype<>objectdef) then
                             Message(sym_f_type_must_be_rec_or_class);
                          }
                          ptypesym(osym)^.definition:=ptypesym(sym)^.definition;
                          osym^.properties:=sp_public;
                          { resolve the definition right now !! }
                          {forward types have two defref chained
                          the first corresponding to the location
                          of  the
                             ptype = ^ttype;
                          and the second
                          to the line
                             ttype = record }
                          if cs_browser in aktmoduleswitches then
                           begin
                             new(ref,init(nil,@sym^.fileinfo));
                             ref^.nextref:=osym^.defref;
                             osym^.defref:=ref;
                           end;

                          { update all forwardpointers to this definition }
                          hfp:=ptypesym(osym)^.forwardpointer;
                          while assigned(hfp) do
                           begin
                             lasthfp:=hfp;
                             hfp^.def^.definition:=ptypesym(osym)^.definition;
                             hfp:=hfp^.next;
                             dispose(lasthfp);
                           end;

                          if ptypesym(osym)^.definition^.sym = ptypesym(sym) then
                            ptypesym(osym)^.definition^.sym := ptypesym(osym);
{$ifdef GDB}
                         ptypesym(osym)^.isusedinstab := true;
                         if (cs_debuginfo in aktmoduleswitches) and assigned(debuglist) then
                            osym^.concatstabto(debuglist);
{$endif GDB}
                          { don't do a done on sym
                          because it also disposes left and right !!
                           sym is new so it has no left nor right }
                          dispose(sym,done);
                          _insert:=osym;
                       end
                     else
                       begin
                         DuplicateSym(sym);
                         _insert:=osym;
                       end;
                  end;
             end;
        end;

      var
         hp : psymtable;
         hsym : psym;
      begin
         { set owner and sym indexnb }
         sym^.owner:=@self;
{$ifdef CHAINPROCSYMS}
         { set the nextprocsym field }
         if sym^.typ=procsym then
           chainprocsym(sym);
{$endif CHAINPROCSYMS}
         { writes the symbol in data segment if required }
         { also sets the datasize of owner               }
         if not in_loading then
           sym^.insert_in_data;
         if (symtabletype in [staticsymtable,globalsymtable]) then
           begin
              hp:=symtablestack;
              while assigned(hp) do
                begin
                   if hp^.symtabletype in [staticsymtable,globalsymtable] then
                    begin
                       hsym:=hp^.search(sym^.name);
                       if (assigned(hsym)) and
                          (hsym^.properties and sp_forwarddef=0) then
                         DuplicateSym(hsym);
                    end;
                  hp:=hp^.next;
                end;
           end;

         { check for duplicate id in local and parsymtable symtable }
         if (symtabletype=localsymtable) then
           { to be on the sure side: }
           begin
              if assigned(next) and
                (next^.symtabletype=parasymtable) then
                begin
                   hsym:=next^.search(sym^.name);
                   if assigned(hsym) then
                     DuplicateSym(hsym);
                end
              else if (current_module^.flags and uf_local_browser)=0 then
                internalerror(43789);
           end;

         { check for duplicate id in local symtable of methods }
         if (symtabletype=localsymtable) and
           assigned(next) and
           assigned(next^.next) and
          { funcretsym is allowed !! }
           (sym^.typ <> funcretsym) and
           (next^.next^.symtabletype=objectsymtable) then
           begin
              hsym:=search_class_member(pobjectdef(next^.next^.defowner),sym^.name);
              { but private ids can be reused }
              if assigned(hsym) and
                ((hsym^.properties<>sp_private) or
                 (hsym^.owner^.defowner^.owner^.symtabletype<>unitsymtable)) then
                DuplicateSym(hsym);
           end;
         { check for duplicate field id in inherited classes }
         if (sym^.typ=varsym) and
            (symtabletype=objectsymtable) and
            assigned(defowner) then
           begin
              hsym:=search_class_member(pobjectdef(defowner),sym^.name);
              { but private ids can be reused }
              if assigned(hsym) and
                ((hsym^.properties<>sp_private) or
                 (hsym^.owner^.defowner^.owner^.symtabletype<>unitsymtable)) then
                DuplicateSym(hsym);
           end;

         if sym^.typ = typesym then
           if assigned(ptypesym(sym)^.definition) then
             begin
             if not assigned(ptypesym(sym)^.definition^.owner) then
              registerdef(ptypesym(sym)^.definition);
{$ifdef GDB}
             if (cs_debuginfo in aktmoduleswitches) and assigned(debuglist)
                and (symtabletype in [globalsymtable,staticsymtable]) then
                   begin
                   ptypesym(sym)^.isusedinstab := true;
                   sym^.concatstabto(debuglist);
                   end;
{$endif GDB}
             end;
         sym^.speedvalue:=getspeedvalue(sym^.name);
         if assigned(searchhasharray) then
           insert:=_insert(searchhasharray^[sym^.speedvalue mod hasharraysize])
         else
           insert:=_insert(searchroot);
         { store the sym also in the index, must be after the insert the table
           because }
      end;


    function tsymtable.search(const s : stringid) : psym;
      begin
        search:=speedsearch(s,getspeedvalue(s));
      end;


    function tsymtable.speedsearch(const s : stringid;speedvalue : longint) : psym;
      var
         hp : psym;
      begin
         if assigned(searchhasharray) then
           hp:=searchhasharray^[speedvalue mod hasharraysize]
         else
           hp:=searchroot;
         while assigned(hp) do
           begin
              if speedvalue>hp^.speedvalue then
                hp:=hp^.left
              else
                if speedvalue<hp^.speedvalue then
                  hp:=hp^.right
              else
                begin
                   if (hp^.name=s) then
                     begin
                        { reject non static members in static procedures,
                          be carefull aktprocsym^.definition is not allways
                          loaded already (PFV) }
                        if (symtabletype=objectsymtable) and
                           ((hp^.properties and sp_static)=0) and
                           allow_only_static
                           {assigned(aktprocsym) and
                           assigned(aktprocsym^.definition) and
                           ((aktprocsym^.definition^.options and postaticmethod)<>0)} then
                               Message(sym_e_only_static_in_static);
                        if (symtabletype=unitsymtable) and
                           assigned(punitsymtable(@self)^.unitsym) then
                          inc(punitsymtable(@self)^.unitsym^.refs);
                        { unitsym are only loaded for browsing PM    }
                        { this was buggy anyway because we could use }
                        { unitsyms from other units in _USES !!      }
                        if (symtabletype=unitsymtable) and (hp^.typ=unitsym) and
                           assigned(current_module) and (current_module^.globalsymtable<>@self) then
                          hp:=nil;
                        if assigned(hp) and
                           (cs_browser in aktmoduleswitches) and make_ref then
                          begin
                             hp^.lastref:=new(pref,init(hp^.lastref,@tokenpos));
                             { for symbols that are in tables without
                             browser info or syssyms (PM) }
                             if hp^.refcount=0 then
                               hp^.defref:=hp^.lastref;
                             inc(hp^.refcount);
                          end;
                        speedsearch:=hp;
                        exit;
                     end
                   else
                     if s>hp^.name then
                       hp:=hp^.left
                   else
                     hp:=hp^.right;
                end;
           end;
         speedsearch:=nil;
      end;


    function tsymtable.rename(const olds,news : stringid):psym;
      var
        spdval : longint;
        lasthp,
        hp,hp2,hp3 : psym;

        function _insert(var osym:psym):psym;
        var
          s1,s2:^string;
        begin
          if osym=nil then
           begin
             osym:=hp;
             _insert:=osym;
           end
          { first check speedvalue, to allow a fast insert }
          else
           if osym^.speedvalue>hp^.speedvalue then
            _insert:=_insert(osym^.right)
           else
            if osym^.speedvalue<hp^.speedvalue then
             _insert:=_insert(osym^.left)
           else
            begin
              new(s1);
              new(s2);
              s1^:=osym^._name^;
              s2^:=hp^._name^;
              if s1^>s2^ then
               begin
                 dispose(s2);
                 dispose(s1);
                 _insert:=_insert(osym^.right);
               end
              else
               if s1^<s2^ then
                begin
                  dispose(s2);
                  dispose(s1);
                  _insert:=_insert(osym^.left);
                end
               else
                begin
                  dispose(s2);
                  dispose(s1);
                  _insert:=osym;
                end;
            end;
        end;

        procedure inserttree(p:psym);
        begin
          if assigned(p) then
           begin
             inserttree(p^.left);
             inserttree(p^.right);
             _insert(p);
           end;
        end;

      begin
        spdval:=getspeedvalue(olds);
        if assigned(searchhasharray) then
         hp:=searchhasharray^[spdval mod hasharraysize]
        else
         hp:=searchroot;
        lasthp:=nil;
        while assigned(hp) do
          begin
            if spdval>hp^.speedvalue then
             begin
               lasthp:=hp;
               hp:=hp^.left
             end
            else
             if spdval<hp^.speedvalue then
              begin
                lasthp:=hp;
                hp:=hp^.right
              end
            else
             begin
               if (hp^.name=olds) then
                begin
                  { get in hp2 the replacer for the root or hasharr }
                  hp2:=hp^.left;
                  hp3:=hp^.right;
                  if not assigned(hp2) then
                   begin
                     hp2:=hp^.right;
                     hp3:=hp^.left;
                   end;
                  { remove entry from the tree }
                  if assigned(lasthp) then
                   begin
                     if lasthp^.left=hp then
                      lasthp^.left:=hp2
                     else
                      lasthp^.right:=hp2;
                   end
                  else
                   begin
                     if assigned(searchhasharray) then
                      searchhasharray^[spdval mod hasharraysize]:=hp2
                     else
                      searchroot:=hp2;
                   end;
                  { reinsert the hp3 }
                  inserttree(hp3);
                  { reinsert }
                  hp^.setname(news);
                  hp^.speedvalue:=getspeedvalue(news);
                  if assigned(searchhasharray) then
                   rename:=_insert(searchhasharray^[hp^.speedvalue mod hasharraysize])
                  else
                   rename:=_insert(searchroot);
                  exit;
                end
               else
                if olds>hp^.name then
                 begin
                   lasthp:=hp;
                   hp:=hp^.left
                 end
                else
                 begin
                   lasthp:=hp;
                   hp:=hp^.right;
                 end;
             end;
          end;
      end;

{$endif}


{***********************************************
                Browser
***********************************************}

    procedure tsymtable.load_browser;
      var
        b     : byte;
        sym   : psym;
        prdef : pdef;
        oldrecsyms : psymtable;
      begin
         if symtabletype in [recordsymtable,objectsymtable,
                    parasymtable,localsymtable] then
           begin
              oldrecsyms:=aktrecordsymtable;
              aktrecordsymtable:=@self;
           end;
         if symtabletype=staticppusymtable then
           aktstaticsymtable:=@self;
         b:=current_ppu^.readentry;
         if b <> ibbeginsymtablebrowser then
           Message1(unit_f_ppu_invalid_entry,tostr(b));
         repeat
           b:=current_ppu^.readentry;
           case b of
           ibsymref : begin
                        sym:=readsymref;
                        resolvesym(sym);
                        if assigned(sym) then
                          sym^.load_references;
                      end;
           ibdefref : begin
                        prdef:=readdefref;
                        resolvedef(prdef);
                        if assigned(prdef) then
                         begin
                           if prdef^.deftype<>procdef then
                            Message(unit_f_ppu_read_error);
                           pprocdef(prdef)^.load_references;
                         end;
                      end;
            ibendsymtablebrowser : break;
           else
             Message1(unit_f_ppu_invalid_entry,tostr(b));
           end;
         until false;
         if symtabletype in [recordsymtable,objectsymtable,
                    parasymtable,localsymtable] then
           aktrecordsymtable:=oldrecsyms;
      end;


    procedure tsymtable.write_browser;
      var
         oldrecsyms : psymtable;
      begin
         { symbol numbering for references
           should have been done in write PM
         number_symbols;
         number_defs;   }

         if symtabletype in [recordsymtable,objectsymtable,
                    parasymtable,localsymtable] then
           begin
              oldrecsyms:=aktrecordsymtable;
              aktrecordsymtable:=@self;
           end;
         current_ppu^.writeentry(ibbeginsymtablebrowser);
      {$ifdef tp}
         foreach(write_refs);
      {$else}
         foreach(@write_refs);
      {$endif}
         current_ppu^.writeentry(ibendsymtablebrowser);
         if symtabletype in [recordsymtable,objectsymtable,
                    parasymtable,localsymtable] then
           aktrecordsymtable:=oldrecsyms;
      end;


{$ifdef BrowserLog}
    procedure tsymtable.writebrowserlog;
      begin
        if cs_browser in aktmoduleswitches then
         begin
           if assigned(name) then
             Browserlog.AddLog('---Symtable '+name^)
           else
             begin
                if (symtabletype=recordsymtable) and
                  assigned(defowner^.sym) then
                  Browserlog.AddLog('---Symtable '+defowner^.sym^.name)
                else
                  Browserlog.AddLog('---Symtable with no name');
             end;
           Browserlog.Ident;
         {$ifdef tp}
           foreach(add_to_browserlog);
         {$else}
           foreach(@add_to_browserlog);
         {$endif}
           browserlog.Unident;
         end;
      end;
{$endif BrowserLog}


{***********************************************
           Process all entries
***********************************************}

    { checks, if all procsyms and methods are defined }
    procedure tsymtable.check_forwards;
      begin
      {$ifdef tp}
         foreach(check_procsym_forward);
      {$else}
         foreach(@check_procsym_forward);
      {$endif}
      end;

    procedure tsymtable.checklabels;
      begin
      {$ifdef tp}
         foreach(labeldefined);
      {$else}
         foreach(@labeldefined);
      {$endif}
      end;

    procedure tsymtable.set_alignment(_alignment : byte);
      var
         sym : pvarsym;
         l : longint;
      begin
        { this can not be done if there is an
          hasharray ! }
        alignment:=_alignment;
        if (symtabletype<>parasymtable)
{$ifdef OLDPPU}
           or assigned(searchhasharray)
{$endif}
           then
          internalerror(1111);
{$ifndef OLDPPU}
        sym:=pvarsym(symindex^.first);
{$else}
        sym:=pvarsym(searchroot);
{$endif}
        datasize:=0;
        { there can be only varsyms }
        while assigned(sym) do
          begin
             l:=sym^.getpushsize;
             sym^.address:=datasize;
             datasize:=align(datasize+l,alignment);
{$ifndef OLDPPU}
             sym:=pvarsym(sym^.next);
{$else}
             sym:=pvarsym(sym^.nextsym);
{$endif}
          end;
      end;

    function tsymtable.find_at_offset(l : longint) : pvarsym;
      var
         sym : pvarsym;
      begin
        find_at_offset:=nil;
        { this can not be done if there is an
          hasharray ! }
        if (symtabletype<>parasymtable)
{$ifdef OLDPPU}
           or assigned(searchhasharray)
{$endif}
           then
          internalerror(1111);
{$ifndef OLDPPU}
        sym:=pvarsym(symindex^.first);
{$else}
        sym:=pvarsym(searchroot);
{$endif}
        while assigned(sym) do
          begin
             if sym^.address+address_fixup=l then
               begin
                 find_at_offset:=sym;
                 exit;
               end;
{$ifndef OLDPPU}
             sym:=pvarsym(sym^.next);
{$else}
             sym:=pvarsym(sym^.nextsym);
{$endif}
          end;
      end;

    procedure tsymtable.allunitsused;
      begin
      {$ifdef tp}
         foreach(unitsymbolused);
      {$else}
         foreach(@unitsymbolused);
      {$endif}
      end;

    procedure tsymtable.allsymbolsused;
      begin
      {$ifdef tp}
         foreach(varsymbolused);
      {$else}
         foreach(@varsymbolused);
      {$endif}
      end;

{$ifdef CHAINPROCSYMS}
    procedure tsymtable.chainprocsyms;
      begin
      {$ifdef tp}
         foreach(chainprocsym);
      {$else}
         foreach(@chainprocsym);
      {$endif}
      end;
{$endif CHAINPROCSYMS}

{$ifdef GDB}
      procedure tsymtable.concatstabto(asmlist : paasmoutput);
      begin
        asmoutput:=asmlist;
      {$ifdef tp}
        foreach(concatstab);
      {$else}
        foreach(@concatstab);
      {$endif}
      end;
{$endif}


{****************************************************************************
                              TUNITSYMTABLE
****************************************************************************}

    constructor tunitsymtable.init(t : tsymtabletype; const n : string);
      begin
         inherited init(t);
         name:=stringdup(upper(n));
         unitid:=0;
         unitsym:=nil;
{$ifndef OLDPPU}
         symsearch^.usehash;
{$else}
       { create a hasharray }
         new(searchhasharray);
         fillchar(searchhasharray^,sizeof(searchhasharray^),0);
{$endif}
       { reset GDB things }
{$ifdef GDB}
         if t = globalsymtable then
           begin
              prev_dbx_counter := dbx_counter;
              dbx_counter := @dbx_count;
           end;
         is_stab_written:=false;
         if cs_gdb_dbx in aktglobalswitches then
           begin
             dbx_count := 0;
             if (symtabletype=globalsymtable) then
               pglobaltypecount := @unittypecount;
             debuglist^.concat(new(pai_stabs,init(strpnew('"'+name^+'",'+tostr(N_BINCL)+',0,0,0'))));
             unitid:=current_module^.unitcount;
             inc(current_module^.unitcount);
             debuglist^.concat(new(pai_asm_comment,init(strpnew('Global '+name^+' has index '+tostr(unitid)))));
           end;
{$endif GDB}
      end;


    constructor tunitsymtable.loadasunit;
      var
        storeGlobalTypeCount : pword;
        b : byte;
      begin
         unitsym:=nil;
         unitid:=0;
         if (current_module^.flags and uf_has_dbx)<>0 then
           begin
              storeGlobalTypeCount:=PGlobalTypeCount;
              PglobalTypeCount:=@UnitTypeCount;
           end;

       { load symtables }
         inherited load;
       { set the name after because it is set to nil in tsymtable.load !! }
         name:=stringdup(current_module^.modulename^);

       { dbx count }
{$ifdef GDB}
         if (current_module^.flags and uf_has_dbx)<>0 then
           begin
              b := current_ppu^.readentry;
              if b <> ibdbxcount then
               Message(unit_f_ppu_dbx_count_problem)
              else
               dbx_count := readlong;
              dbx_count_ok := true;
              PGlobalTypeCount:=storeGlobalTypeCount;
           end
         else
           dbx_count := 0;
         is_stab_written:=false;
{$endif GDB}

         b:=current_ppu^.readentry;
         if b<>ibendimplementation then
           Message1(unit_f_ppu_invalid_entry,tostr(b));
      end;


     destructor tunitsymtable.done;
       var
          pus : punitsym;
       begin
          pus:=unitsym;
          while assigned(pus) do
            begin
               unitsym:=pus^.prevsym;
               pus^.prevsym:=nil;
               pus^.unitsymtable:=nil;
               pus:=unitsym;
            end;
          inherited done;
       end;
       
       procedure tunitsymtable.load_symtable_refs;
         var
            b : byte;
            unitindex : word;
         begin
{$ifdef OLDPPU}
         number_defs;
         number_symbols;
{$endif}
         if ((current_module^.flags and uf_local_browser)<>0) then
           begin
              current_module^.localsymtable:=new(psymtable,loadas(staticppusymtable));
              psymtable(current_module^.localsymtable)^.name:=
                stringdup('implementation of '+psymtable(current_module^.globalsymtable)^.name^);
           end;
         { load browser }
         if (current_module^.flags and uf_has_browser)<>0 then
           begin
              {if not (cs_browser in aktmoduleswitches) then
                current_ppu^.skipuntilentry(ibendbrowser)
              else }
                begin
                   load_browser;
                   unitindex:=1;
                   while assigned(current_module^.map^[unitindex]) do
                     begin
                        {each unit wrote one browser entry }
                        load_browser;
                        inc(unitindex);
                     end;
                   b:=current_ppu^.readentry;
                   if b<>ibendbrowser then
                     Message1(unit_f_ppu_invalid_entry,tostr(b));
                end;
           end;
         if ((current_module^.flags and uf_local_browser)<>0) then
           psymtable(current_module^.localsymtable)^.load_browser;
         end;


    procedure tunitsymtable.writeasunit;
      var
         pu           : pused_unit;
      begin
      { first the unitname }
        current_ppu^.putstring(name^);
        current_ppu^.writeentry(ibmodulename);

        writesourcefiles;

        writeusedunit;

      { write the objectfiles and libraries that come for this unit,
        preserve the containers becuase they are still needed to load
        the link.res. All doesn't depend on the crc! It doesn't matter
        if a unit is in a .o or .a file }
        current_ppu^.do_crc:=false;
        writecontainer(current_module^.linkunitfiles,iblinkunitfiles,true,true);
        writecontainer(current_module^.linkofiles,iblinkofiles,true,false);
        writecontainer(current_module^.linksharedlibs,iblinksharedlibs,true,true);
        writecontainer(current_module^.linkstaticlibs,iblinkstaticlibs,true,true);
        current_ppu^.do_crc:=true;

        current_ppu^.writeentry(ibendinterface);

      { write the symtable entries }
        inherited write;

      { write dbx count }
{$ifdef GDB}
        if cs_gdb_dbx in aktglobalswitches then
         begin
{$IfDef EXTDEBUG}
           writeln('Writing dbx_count ',dbx_count,' in unit ',name^,'.ppu');
{$ENDIF EXTDEBUG}
           current_ppu^.putlongint(dbx_count);
           current_ppu^.writeentry(ibdbxcount);
         end;
{$endif GDB}

        current_ppu^.writeentry(ibendimplementation);

         { write static symtable
           needed for local debugging of unit functions }
        if (current_module^.flags and uf_local_browser)<>0 then
          psymtable(current_module^.localsymtable)^.write;
      { write all browser section }
        if (current_module^.flags and uf_has_browser)<>0 then
         begin
           current_ppu^.do_crc:=false; { doesn't affect crc }
           write_browser;
           pu:=pused_unit(current_module^.used_units.first);
           while assigned(pu) do
            begin
              psymtable(pu^.u^.globalsymtable)^.write_browser;
              pu:=pused_unit(pu^.next);
            end;
           current_ppu^.writeentry(ibendbrowser);
           current_ppu^.do_crc:=true;
         end;
        if (current_module^.flags and uf_local_browser)<>0 then
          psymtable(current_module^.localsymtable)^.write_browser;

      { the last entry ibend is written automaticly }
      end;


   function tunitsymtable.getnewtypecount : word;

      begin
{$ifdef GDB}
         if not (cs_gdb_dbx in aktglobalswitches) then
           getnewtypecount:=tsymtable.getnewtypecount
         else
{$endif GDB}
           if symtabletype = staticsymtable then
           getnewtypecount:=tsymtable.getnewtypecount
         else
           begin
              getnewtypecount:=unittypecount;
              inc(unittypecount);
           end;
      end;


{$ifdef GDB}
  {$ifdef OLDPPU}
    procedure tunitsymtable.orderdefs;
      var
         firstd, last, nonum, pd, cur, prev, lnext : pdef;

      begin
         pd:=rootdef;
         firstd:=nil;
         last:=nil;
         nonum:=nil;
         while assigned(pd) do
           begin
              lnext:=pd^.next;
              if pd^.globalnb > 0 then
                if firstd = nil then
                  begin
                     firstd:=pd;
                     last:=pd;
                     last^.next:=nil;
                  end
                else
                  begin
                     cur:=firstd;
                     prev:=nil;
                     while assigned(cur) and
                           (prev <> last) and
                           (cur^.globalnb>0) and
                           (cur^.globalnb<pd^.globalnb) do
                       begin
                          prev:=cur;
                          cur:=cur^.next;
                       end;
                     if cur = firstd then
                       begin
                          pd^.next:=firstd;
                          firstd:=pd;
                       end
                     else
                     if prev = last then
                       begin
                          pd^.next:=nil;
                          last^.next:=pd;
                          last:=pd;
                       end
                     else
                       begin
                          pd^.next:=cur;
                          prev^.next:=pd;
                       end;
                  end
                else  { without number }
                  begin
                     pd^.next:=nonum;
                     nonum:=pd;
                  end;
              pd:=lnext;
           end;
         if assigned(firstd) then
           begin
              rootdef:=firstd;
              last^.next:=nonum;
           end else
           rootdef:=nonum;
      end;
  {$endif}

      procedure tunitsymtable.concattypestabto(asmlist : paasmoutput);
        var prev_dbx_count : plongint;
        begin
           if is_stab_written then exit;
           if not assigned(name) then name := stringdup('Main_program');
           if symtabletype = unitsymtable then
             begin
                unitid:=current_module^.unitcount;
                inc(current_module^.unitcount);
             end;
           asmlist^.concat(new(pai_asm_comment,init(strpnew('Begin unit '+name^
                  +' has index '+tostr(unitid)))));
           if cs_gdb_dbx in aktglobalswitches then
             begin
                if dbx_count_ok then
                  begin
                     asmlist^.insert(new(pai_asm_comment,init(strpnew('"repeated" unit '+name^
                              +' has index '+tostr(unitid)))));
                     do_count_dbx:=true;
                     asmlist^.concat(new(pai_stabs,init(strpnew('"'+name^+'",'
                       +tostr(N_EXCL)+',0,0,'+tostr(dbx_count)))));
                     exit;
                  end;
                prev_dbx_count := dbx_counter;
                dbx_counter := nil;
                if symtabletype = unitsymtable then
                  asmlist^.concat(new(pai_stabs,init(strpnew('"'+name^+'",'
                    +tostr(N_BINCL)+',0,0,0'))));
                dbx_counter := @dbx_count;
             end;
           asmoutput:=asmlist;
           {$ifdef tp}
             foreach(concattypestab);
           {$else}
             foreach(@concattypestab);
           {$endif}
           if cs_gdb_dbx in aktglobalswitches then
             begin
                dbx_counter := prev_dbx_count;
                do_count_dbx:=true;
                asmlist^.concat(new(pai_stabs,init(strpnew('"'+name^+'",'
                  +tostr(N_EINCL)+',0,0,0'))));
                dbx_count_ok := true;
             end;
           asmlist^.concat(new(pai_asm_comment,init(strpnew('End unit '+name^
                  +' has index '+tostr(unitid)))));
           is_stab_written:=true;
        end;
{$endif}

{****************************************************************************
                              Definitions
****************************************************************************}

{$I symdef.inc}

{****************************************************************************
                                Symbols
****************************************************************************}

{$I symsym.inc}

{****************************************************************************
                               GDB Helpers
****************************************************************************}

{$ifdef GDB}
    function typeglobalnumber(const s : string) : string;

      var st : string;
          symt : psymtable;
          old_make_ref : boolean;
      begin
         old_make_ref:=make_ref;
         make_ref:=false;
         typeglobalnumber := '0';
         srsym := nil;
         if pos('.',s) > 0 then
           begin
           st := copy(s,1,pos('.',s)-1);
           getsym(st,false);
           st := copy(s,pos('.',s)+1,255);
           if assigned(srsym) then
             begin
             if srsym^.typ = unitsym then
               begin
               symt := punitsym(srsym)^.unitsymtable;
               srsym := symt^.search(st);
               end else srsym := nil;
             end;
           end else st := s;
         if srsym = nil then getsym(st,true);
         if srsym^.typ<>typesym then
           begin
             Message(type_e_type_id_expected);
             exit;
           end;
         typeglobalnumber := ptypesym(srsym)^.definition^.numberstring;
         make_ref:=old_make_ref;
      end;
{$endif GDB}


{****************************************************************************
                           Definition Helpers
****************************************************************************}

   procedure reset_global_defs;
     var
       def     : pdef;
{$ifdef debug}
       prevdef : pdef;
{$endif debug}
     begin
{$ifdef debug}
        prevdef:=nil;
{$endif debug}
{$ifdef GDB}
        pglobaltypecount:=@globaltypecount;
{$endif GDB}
        def:=firstglobaldef;
        while assigned(def) do
          begin
{$ifdef GDB}
            if assigned(def^.sym) then
              def^.sym^.isusedinstab:=false;
            def^.is_def_stab_written:=false;
{$endif GDB}
            {if not current_module^.in_implementation then}
              begin
                { reset rangenr's }
                case def^.deftype of
                  orddef   : porddef(def)^.rangenr:=0;
                  enumdef  : penumdef(def)^.rangenr:=0;
                  arraydef : parraydef(def)^.rangenr:=0;
                end;
                if def^.deftype<>objectdef then
                  def^.has_rtti:=false;
                def^.has_inittable:=false;
              end;
{$ifdef debug}
            prevdef:=def;
{$endif debug}
            def:=def^.nextglobal;
          end;
     end;


{****************************************************************************
                              Object Helpers
****************************************************************************}

    function search_class_member(pd : pobjectdef;const n : string) : psym;
    { searches n in symtable of pd and all anchestors }
      var
         sym : psym;
      begin
         sym:=nil;
         while assigned(pd) do
           begin
              sym:=pd^.publicsyms^.search(n);
              if assigned(sym) then
                break;
              pd:=pd^.childof;
           end;
         { this is needed for static methods in do_member_read pexpr unit PM
           caused bug0214 }
         if assigned(sym) then
           begin
             srsymtable:=pd^.publicsyms;
           end;
         search_class_member:=sym;
      end;

   var
      _defaultprop : ppropertysym;

   procedure testfordefaultproperty(p : {$ifndef OLDPPU}pnamedindexobject{$else}psym{$endif});
     begin
        if (psym(p)^.typ=propertysym) and ((ppropertysym(p)^.options and ppo_defaultproperty)<>0) then
          _defaultprop:=ppropertysym(p);
     end;


   function search_default_property(pd : pobjectdef) : ppropertysym;
   { returns the default property of a class, searches also anchestors }
     begin
        _defaultprop:=nil;
        while assigned(pd) do
          begin
           {$ifdef tp}
             pd^.publicsyms^.foreach(testfordefaultproperty);
           {$else}
             pd^.publicsyms^.foreach(@testfordefaultproperty);
           {$endif}
             if assigned(_defaultprop) then
               break;
             pd:=pd^.childof;
          end;
        search_default_property:=_defaultprop;
     end;

{****************************************************************************
                               Macro's
****************************************************************************}

      procedure def_macro(const s : string);
        var
          mac : pmacrosym;
        begin
           mac:=pmacrosym(macros^.search(s));
           if mac=nil then
             begin
               mac:=new(pmacrosym,init(s));
               Message1(parser_m_macro_defined,mac^.name);
               macros^.insert(mac);
             end;
           mac^.defined:=true;
        end;


      procedure set_macro(const s : string;value : string);
        var
          mac : pmacrosym;
        begin
           mac:=pmacrosym(macros^.search(s));
           if mac=nil then
             begin
               mac:=new(pmacrosym,init(s));
               macros^.insert(mac);
             end
           else
             begin
                if assigned(mac^.buftext) then
                  freemem(mac^.buftext,mac^.buflen);
             end;
           Message2(parser_m_macro_set_to,mac^.name,value);
           mac^.buflen:=length(value);
           getmem(mac^.buftext,mac^.buflen);
           move(value[1],mac^.buftext^,mac^.buflen);
           mac^.defined:=true;
        end;


{****************************************************************************
                            Symtable Stack
****************************************************************************}

    procedure dellexlevel;
      var
         p : psymtable;
      begin
         p:=symtablestack;
         symtablestack:=p^.next;
         { symbol tables of unit interfaces are never disposed }
         { this is handle by the unit unitm                    }
         if not(p^.symtabletype in [unitsymtable,globalsymtable,stt_exceptsymtable]) or dispose_global then
          dispose(p,done);
      end;

{$ifdef DEBUG}
    procedure test_symtablestack;
      var
         p : psymtable;
         i : longint;
      begin
         p:=symtablestack;
         i:=0;
         while assigned(p) do
           begin
              inc(i);
              p:=p^.next;
              if i>500 then
               Message(sym_f_internal_error_in_symtablestack);
           end;
      end;

    procedure list_symtablestack;
      var
         p : psymtable;
         i : longint;
      begin
         p:=symtablestack;
         i:=0;
         while assigned(p) do
           begin
              inc(i);
              writeln(i,' ',p^.name^);
              p:=p^.next;
              if i>500 then
               Message(sym_f_internal_error_in_symtablestack);
           end;
      end;
{$endif DEBUG}


{****************************************************************************
                           Init/Done Symtable
****************************************************************************}

{$ifdef tp}
   procedure do_streamerror;
     begin
       if symbolstream.status=-2 then
        WriteLn('Error: Not enough EMS memory')
       else
        WriteLn('Error: EMS Error ',symbolstream.status);
       halt(1);
     end;
{$endif TP}

   procedure InitSymtable;
     begin
{$ifdef TP}
     { Allocate stream }
        if use_big then
         begin
           streamerror:=@do_streamerror;
         { symbolstream.init('TMPFILE',stcreate,16000); }
         {$ifndef dpmi}
           symbolstream.init(10000,4000000); {using ems streams}
         {$else}
           symbolstream.init(1000000,16000); {using memory streams}
         {$endif}
           if symbolstream.errorinfo=stiniterror then
            do_streamerror;
         { write something, because pos 0 means nil pointer }
           symbolstream.writestr(@inputfile);
         end;
{$endif tp}
      { Reset symbolstack }
        registerdef:=false;
        read_member:=false;
        symtablestack:=nil;
        systemunit:=nil;
        objpasunit:=nil;
        sroot:=nil;
{$ifdef GDB}
        firstglobaldef:=nil;
        lastglobaldef:=nil;
{$endif GDB}
        globaltypecount:=1;
        pglobaltypecount:=@globaltypecount;
     { create error syms and def }
        generrorsym:=new(perrorsym,init);
        generrordef:=new(perrordef,init);
     end;


   procedure DoneSymtable;
      begin
        dispose(generrorsym,done);
        dispose(generrordef,done);
      { unload all symtables
         done with loaded_units
        dispose_global:=true;
        while assigned(symtablestack) do
          dellexlevel;  }
{$ifdef TP}
      { close the stream }
        if use_big then
         symbolstream.done;
{$endif}
     end;

end.
{
  $Log$
  Revision 1.12  1999-05-10 22:34:59  pierre
   * one more unitsym problem fix

  Revision 1.11  1999/05/10 15:02:51  pierre
  unitsym finally problem fixed

  Revision 1.10  1999/05/09 12:46:26  peter
    + hint where a duplicate sym is already defined

  Revision 1.9  1999/05/08 19:52:40  peter
    + MessagePos() which is enhanced Message() function but also gets the
      position info
    * Removed comp warnings

  Revision 1.8  1999/05/06 21:38:38  peter
    * don't register errordef

  Revision 1.7  1999/05/06 09:05:31  peter
    * generic write_float and str_float
    * fixed constant float conversions

  Revision 1.6  1999/05/05 09:19:16  florian
    * more fixes to get it with delphi running

  Revision 1.5  1999/05/01 13:24:43  peter
    * merged nasm compiler
    * old asm moved to oldasm/

  Revision 1.4  1999/04/29 17:25:37  peter
    * small fix for deref

  Revision 1.3  1999/04/26 18:30:03  peter
    * farpointerdef moved into pointerdef.is_far

  Revision 1.151  1999/04/26 13:31:54  peter
    * release storenumber,double_checksum

  Revision 1.150  1999/04/25 17:36:13  peter
    * typo fix for storenumber

  Revision 1.149  1999/04/21 22:05:28  pierre
    + tsymtable.find_at_offset function
      used by ra386att to give arg name from ebp offset with -vz option

  Revision 1.148  1999/04/21 16:31:44  pierre
  ra386att.pas : commit problem !

  Revision 1.147  1999/04/21 09:43:57  peter
    * storenumber works
    * fixed some typos in double_checksum
    + incompatible types type1 and type2 message (with storenumber)

  Revision 1.146  1999/04/19 09:33:14  pierre
    + added tsymtable.set_alignment(longint) function
      to change the offsets of all function args
      if declared as cdecl or stdcall
      (this must be done after because the cdecl is parsed after
      insertion of the function parameterss into parast symboltable)

  Revision 1.145  1999/04/17 13:16:24  peter
    * fixes for storenumber

  Revision 1.144  1999/04/15 10:01:45  peter
    * small update for storenumber

  Revision 1.143  1999/04/14 09:15:04  peter
    * first things to store the symbol/def number in the ppu

  Revision 1.142  1999/04/08 14:54:10  pierre
   * suppression of val para unused warnings

  Revision 1.141  1999/04/07 15:31:09  pierre
    * all formaldefs are now a sinlge definition
      cformaldef (this was necessary for double_checksum)
    + small part of double_checksum code

  Revision 1.140  1999/03/31 13:55:24  peter
    * assembler inlining working for ag386bin

  Revision 1.139  1999/03/24 23:17:30  peter
    * fixed bugs 212,222,225,227,229,231,233

  Revision 1.138  1999/03/21 22:49:11  florian
    * private ids of objects can be reused in child classes
      if they are in another unit

  Revision 1.137  1999/03/17 22:23:20  florian
    * a FPC compiled compiler checks now also in debug mode in assigned
      if a pointer points to the heap
    * when a symtable is loaded, there is no need to check for duplicate
      symbols. This leads to crashes because defowner isn't assigned
      in this case

  Revision 1.136  1999/03/01 13:45:07  pierre
   + added staticppusymtable symtable type for local browsing

  Revision 1.135  1999/02/23 18:29:28  pierre
    * win32 compilation error fix
    + some work for local browser (not cl=omplete yet)

  Revision 1.134  1999/02/22 15:09:42  florian
    * behaviaor of PROTECTED and PRIVATE fixed, works now like TP/Delphi

  Revision 1.133  1999/02/22 13:07:12  pierre
    + -b and -bl options work !
    + cs_local_browser ($L+) is disabled if cs_browser ($Y+)
      is not enabled when quitting global section
    * local vars and procedures are not yet stored into PPU

  Revision 1.132  1999/02/22 02:15:40  peter
    * updates for ag386bin

  Revision 1.131  1999/02/16 00:44:34  peter
    * tp7 fix, assigned() can only be used on vars, not on functions

  Revision 1.130  1999/02/15 13:13:16  pierre
   * fix for bug0216

  Revision 1.129  1999/02/11 09:46:29  pierre
    * fix for normal method calls inside static methods :
      WARNING there were both parser and codegen errors !!
      added static_call boolean to calln tree

  Revision 1.128  1999/02/09 23:03:05  florian
    * check for duplicate field names in inherited classes/objects
    * bug with self from the mailing list solved (the problem
      was that classes were sometimes pushed wrong)

  Revision 1.127  1999/02/08 11:29:06  pierre
   * fix for bug0214
     several problems where combined
     search_class_member did not set srsymtable
     => in do_member_read the call node got a wrong symtable
     in cg386cal the vmt was pushed twice without chacking if it exists
     now %esi is set to zero and pushed if not vmt
     (not very efficient but should work !)

  Revision 1.126  1999/02/05 08:54:31  pierre
    + linkofiles splitted inot linkofiles and linkunitfiles
      because linkofiles must be stored with directory
      to enabled linking of different objects with same name
      in a different directory

  Revision 1.125  1999/02/03 09:44:33  pierre
    * symbol nubering begins with 1 in number_symbols
    * program tmodule has globalsymtable for its staticsymtable
      (to get it displayed in IDE globals list)
    + list of symbol (browcol) greatly improved for IDE

  Revision 1.124  1999/01/27 12:58:33  pierre
   * unused var warning suppressed for high of open arrays

  Revision 1.123  1999/01/21 16:41:03  pierre
   * fix for constructor inside with statements

  Revision 1.122  1999/01/20 10:16:44  peter
    * don't update crc when writing objs,libs and sources

  Revision 1.121  1999/01/14 21:50:00  peter
    * fixed forwardpointer problem with multiple forwards for the same
      typesym. It now uses a linkedlist instead of a single pointer

  Revision 1.120  1999/01/13 14:29:22  daniel
  * nonextfield repaired

  Revision 1.119  1999/01/12 14:25:38  peter
    + BrowserLog for browser.log generation
    + BrowserCol for browser info in TCollections
    * released all other UseBrowser

  Revision 1.118  1999/01/05 08:20:10  florian
    * mainly problem with invalid case ranges fixed (reported by Jonas)

  Revision 1.117  1998/12/30 22:15:57  peter
    + farpointer type
    * absolutesym now also stores if its far

  Revision 1.116  1998/12/30 13:41:16  peter
    * released valuepara

  Revision 1.115  1998/12/11 00:03:48  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.114  1998/12/10 09:47:29  florian
    + basic operations with int64/qord (compiler with -dint64)
    + rtti of enumerations extended: names are now written

  Revision 1.113  1998/12/08 10:18:17  peter
    + -gh for heaptrc unit

  Revision 1.112  1998/12/04 10:18:10  florian
    * some stuff for procedures of object added
    * bug with overridden virtual constructors fixed (reported by Italo Gomes)

  Revision 1.111  1998/11/30 16:34:46  pierre
    * corrected problems with rangecheck
    + added needed code for no rangecheck  in CRC32 functions in ppu unit
    * enumdef lso need its rangenr reset to zero
      when calling reset_global_defs

  Revision 1.110  1998/11/28 16:20:58  peter
    + support for dll variables

  Revision 1.109  1998/11/27 14:50:49  peter
    + open strings, $P switch support

  Revision 1.108  1998/11/24 23:00:32  peter
    * small crash prevention

  Revision 1.107  1998/11/20 15:36:01  florian
    * problems with rtti fixed, hope it works

  Revision 1.106  1998/11/18 15:44:20  peter
    * VALUEPARA for tp7 compatible value parameters

  Revision 1.105  1998/11/17 10:39:18  peter
    * has_rtti,has_inittable reset

  Revision 1.104  1998/11/16 10:13:52  peter
    * label defines are checked at the end of the proc

  Revision 1.103  1998/11/13 15:40:32  pierre
    + added -Se in Makefile cvstest target
    + lexlevel cleanup
      normal_function_level main_program_level and unit_init_level defined
    * tins_cache grown to A_EMMS (gave range check error in asm readers)
      (test added in code !)
    * -Un option was wrong
    * _FAIL and _SELF only keyword inside
      constructors and methods respectively

  Revision 1.102  1998/11/12 16:43:34  florian
    * functions with ansi strings as result didn't work, solved

  Revision 1.101  1998/11/12 12:55:18  pierre
   * fix for bug0176 and bug0177

  Revision 1.100  1998/11/10 10:09:15  peter
    * va_list -> array of const

  Revision 1.99  1998/11/09 11:44:38  peter
    + va_list for printf support

  Revision 1.98  1998/11/05 23:33:35  peter
    * symtable.done sets vars to nil

  Revision 1.97  1998/11/05 12:03:00  peter
    * released useansistring
    * removed -Sv, its now available in fpc modes

  Revision 1.96  1998/10/28 18:26:19  pierre
   * removed some erros after other errors (introduced by useexcept)
   * stabs works again correctly (for how long !)

  Revision 1.95  1998/10/21 08:40:01  florian
    + ansistring operator +
    + $h and string[n] for n>255 added
    * small problem with TP fixed

  Revision 1.94  1998/10/20 08:07:03  pierre
    * several memory corruptions due to double freemem solved
      => never use p^.loc.location:=p^.left^.loc.location;
    + finally I added now by default
      that ra386dir translates global and unit symbols
    + added a first field in tsymtable and
      a nextsym field in tsym
      (this allows to obtain ordered type info for
      records and objects in gdb !)

  Revision 1.93  1998/10/19 08:55:08  pierre
    * wrong stabs info corrected once again !!
    + variable vmt offset with vmt field only if required
      implemented now !!!

  Revision 1.92  1998/10/16 13:12:56  pierre
    * added vmt_offsets in destructors code also !!!
    * vmt_offset code for m68k

  Revision 1.91  1998/10/16 08:48:38  peter
    * fixed some misplaced $endif GDB

  Revision 1.90  1998/10/15 15:13:32  pierre
    + added oo_hasconstructor and oo_hasdestructor
      for objects options

  Revision 1.89  1998/10/14 13:38:25  peter
    * fixed path with staticlib/objects in ppufiles

  Revision 1.88  1998/10/09 16:36:07  pierre
    * some memory leaks specific to usebrowser define fixed
    * removed tmodule.implsymtable (was like tmodule.localsymtable)

  Revision 1.87  1998/10/09 11:47:57  pierre
    * still more memory leaks fixes !!

  Revision 1.86  1998/10/08 17:17:35  pierre
    * current_module old scanner tagged as invalid if unit is recompiled
    + added ppheap for better info on tracegetmem of heaptrc
      (adds line column and file index)
    * several memory leaks removed ith help of heaptrc !!

  Revision 1.85  1998/10/08 13:48:51  peter
    * fixed memory leaks for do nothing source
    * fixed unit interdependency

  Revision 1.84  1998/10/06 17:16:58  pierre
    * some memory leaks fixed (thanks to Peter for heaptrc !)

  Revision 1.83  1998/09/26 17:45:45  peter
    + idtoken and only one token table

  Revision 1.82  1998/09/25 09:52:57  peter
    + store also datasize and # of symbols in ppu
    * # of defs is now also stored in structs

  Revision 1.81  1998/09/24 23:49:21  peter
    + aktmodeswitches

  Revision 1.80  1998/09/23 12:20:51  pierre
    * main program tmodule had no symtable (crashed browser)
    * unit symbols problem fixed !!

  Revision 1.79  1998/09/23 12:03:57  peter
    * overloading fix for array of const

  Revision 1.78  1998/09/22 17:13:54  pierre
    + browsing updated and developed
      records and objects fields are also stored

  Revision 1.77  1998/09/22 15:37:24  peter
    + array of const start

  Revision 1.76  1998/09/21 10:00:08  peter
    * store number of defs in ppu file

  Revision 1.75  1998/09/21 08:58:31  peter
    + speedsearch, which also needs speedvalue as parameter

  Revision 1.74  1998/09/21 08:45:25  pierre
    + added vmt_offset in tobjectdef.write for fututre use
      (first steps to have objects without vmt if no virtual !!)
    + added fpu_used field for tabstractprocdef  :
      sets this level to 2 if the functions return with value in FPU
      (is then set to correct value at parsing of implementation)
      THIS MIGHT refuse some code with FPU expression too complex
      that were accepted before and even in some cases
      that don't overflow in fact
      ( like if f : float; is a forward that finally in implementation
       only uses one fpu register !!)
      Nevertheless I think that it will improve security on
      FPU operations !!
    * most other changes only for UseBrowser code
      (added symtable references for record and objects)
      local switch for refs to args and local of each function
      (static symtable still missing)
      UseBrowser still not stable and probably broken by
      the definition hash array !!

  Revision 1.73  1998/09/20 09:38:47  florian
    * hasharray for defs fixed
    * ansistring code generation corrected (init/final, assignement)

  Revision 1.72  1998/09/19 22:56:18  florian
    + hash table for getdefnr added

  Revision 1.71  1998/09/18 08:01:40  pierre
    + improvement on the usebrowser part
      (does not work correctly for now)

  Revision 1.70  1998/09/09 11:50:57  pierre
    * forward def are not put in record or objects
    + added check for forwards also in record and objects
    * dummy parasymtable for unit initialization removed from
    symtable stack

  Revision 1.69  1998/09/07 23:10:25  florian
    * a lot of stuff fixed regarding rtti and publishing of properties,
      basics should now work

  Revision 1.68  1998/09/07 19:33:26  florian
    + some stuff for property rtti added:
       - NameIndex of the TPropInfo record is now written correctly
       - the DEFAULT/NODEFAULT keyword is supported now
       - the default value and the storedsym/def are now written to
         the PPU fiel

  Revision 1.67  1998/09/07 18:46:14  peter
    * update smartlinking, uses getdatalabel
    * renamed ptree.value vars to value_str,value_real,value_set

  Revision 1.66  1998/09/07 17:37:05  florian
    * first fixes for published properties

  Revision 1.65  1998/09/06 22:42:03  florian
    + rtti genreation for properties added

  Revision 1.64  1998/09/05 22:11:04  florian
    + switch -vb
    * while/repeat loops accept now also word/longbool conditions
    * makebooltojump did an invalid ungetregister32, fixed

  Revision 1.63  1998/09/04 17:34:23  pierre
    * bug with datalabel corrected
    + assembler errors better commented
    * one nested record crash removed

  Revision 1.62  1998/09/04 08:42:10  peter
    * updated some error messages

  Revision 1.61  1998/09/03 16:03:21  florian
    + rtti generation
    * init table generation changed

  Revision 1.60  1998/09/01 17:39:52  peter
    + internal constant functions

  Revision 1.59  1998/09/01 12:53:27  peter
    + aktpackenum

  Revision 1.58  1998/09/01 07:54:26  pierre
    * UseBrowser a little updated (might still be buggy !!)
    * bug in psub.pas in function specifier removed
    * stdcall allowed in interface and in implementation
      (FPC will not yet complain if it is missing in either part
      because stdcall is only a dummy !!)

  Revision 1.57  1998/08/31 12:26:33  peter
    * m68k and palmos updates from surebugfixes

  Revision 1.56  1998/08/21 14:08:55  pierre
    + TEST_FUNCRET now default (old code removed)
      works also for m68k (at least compiles)

  Revision 1.55  1998/08/21 08:43:32  pierre
    * pocdecl and poclearstack are now different
      external must but written as last specification

  Revision 1.54  1998/08/20 09:26:48  pierre
    + funcret setting in underproc testing
      compile with _dTEST_FUNCRET

  Revision 1.53  1998/08/19 18:04:56  peter
    * fixed current_module^.in_implementation flag

  Revision 1.51  1998/08/18 14:17:12  pierre
    * bug about assigning the return value of a function to
      a procvar fixed : warning
      assigning a proc to a procvar need @ in FPC mode !!
    * missing file/line info restored

  Revision 1.50  1998/08/17 10:10:13  peter
    - removed OLDPPU

  Revision 1.49  1998/08/12 19:39:31  peter
    * fixed some crashes

  Revision 1.48  1998/08/10 14:50:32  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.47  1998/08/10 10:00:19  peter
    * Moved symbolstream to symtable.pas

  Revision 1.46  1998/08/08 10:19:19  florian
    * small fixes to write the extended type correct

  Revision 1.45  1998/08/02 16:42:00  florian
    * on o : tobject do should also work now, the exceptsymtable shouldn't be
      disposed by dellexlevel

  Revision 1.44  1998/07/30 11:18:21  florian
    + first implementation of try ... except on .. do end;
    * limitiation of 65535 bytes parameters for cdecl removed

  Revision 1.43  1998/07/28 21:52:56  florian
    + implementation of raise and try..finally
    + some misc. exception stuff

  Revision 1.42  1998/07/20 10:23:03  florian
    * better ansi string assignement

  Revision 1.41  1998/07/18 22:54:31  florian
    * some ansi/wide/longstring support fixed:
       o parameter passing
       o returning as result from functions

  Revision 1.40  1998/07/14 14:47:09  peter
    * released NEWINPUT

  Revision 1.39  1998/07/10 00:00:06  peter
    * fixed ttypesym bug finally
    * fileinfo in the symtable and better using for unused vars

  Revision 1.38  1998/07/07 11:20:17  peter
    + NEWINPUT for a better inputfile and scanner object

  Revision 1.37  1998/06/24 14:48:42  peter
    * ifdef newppu -> ifndef oldppu

  Revision 1.36  1998/06/17 14:10:19  peter
    * small os2 fixes
    * fixed interdependent units with newppu (remake3 under linux works now)

  Revision 1.35  1998/06/16 08:56:35  peter
    + targetcpu
    * cleaner pmodules for newppu

  Revision 1.34  1998/06/15 15:38:12  pierre
    * small bug in systems.pas corrected
    + operators in different units better hanlded

  Revision 1.33  1998/06/15 14:10:53  daniel
  * File was ruined, fixed.

  Revision 1.31  1998/06/13 00:10:20  peter
    * working browser and newppu
    * some small fixes against crashes which occured in bp7 (but not in
      fpc?!)

  Revision 1.30  1998/06/09 16:01:53  pierre
    + added procedure directive parsing for procvars
      (accepted are popstack cdecl and pascal)
    + added C vars with the following syntax
      var C calias 'true_c_name';(can be followed by external)
      reason is that you must add the Cprefix

      which is target dependent

  Revision 1.29  1998/06/07 15:30:26  florian
    + first working rtti
    + data init/final. for local variables

  Revision 1.28  1998/06/06 09:27:39  peter
    * new depend file generated

  Revision 1.27  1998/06/05 14:37:38  pierre
    * fixes for inline for operators
    * inline procedure more correctly restricted

  Revision 1.26  1998/06/04 23:52:03  peter
    * m68k compiles
    + .def file creation moved to gendef.pas so it could also be used
      for win32

  Revision 1.25  1998/06/04 09:55:48  pierre
    * demangled name of procsym reworked to become independant of the
      mangling scheme

  Revision 1.24  1998/06/03 22:49:04  peter
    + wordbool,longbool
    * rename bis,von -> high,low
    * moved some systemunit loading/creating to psystem.pas

  Revision 1.23  1998/05/28 14:40:30  peter
    * fixes for newppu, remake3 works now with it

  Revision 1.22  1998/05/27 19:45:09  peter
    * symtable.pas splitted into includefiles
    * symtable adapted for $ifndef OLDPPU

  Revision 1.21  1998/05/23 01:21:31  peter
    + aktasmmode, aktoptprocessor, aktoutputformat
    + smartlink per module $SMARTLINK-/+ (like MMX) and moved to aktswitches
    + $LIBNAME to set the library name where the unit will be put in
    * splitted cgi386 a bit (codeseg to large for bp7)
    * nasm, tasm works again. nasm moved to ag386nsm.pas

  Revision 1.20  1998/05/21 19:33:37  peter
    + better procedure directive handling and only one table

  Revision 1.19  1998/05/20 09:42:37  pierre
    + UseTokenInfo now default
    * unit in interface uses and implementation uses gives error now
    * only one error for unknown symbol (uses lastsymknown boolean)
      the problem came from the label code !
    + first inlined procedures and function work
      (warning there might be allowed cases were the result is still wrong !!)
    * UseBrower updated gives a global list of all position of all used symbols
      with switch -gb

  Revision 1.18  1998/05/11 13:07:57  peter
    + $ifndef OLDPPU for the new ppuformat
    + $define GDB not longer required
    * removed all warnings and stripped some log comments
    * no findfirst/findnext anymore to remove smartlink *.o files

  Revision 1.17  1998/05/06 08:38:48  pierre
    * better position info with UseTokenInfo
      UseTokenInfo greatly simplified
    + added check for changed tree after first time firstpass
      (if we could remove all the cases were it happen
      we could skip all firstpass if firstpasscount > 1)
      Only with ExtDebug

  Revision 1.16  1998/05/05 15:24:20  michael
  * Fix to save units with classes.

  Revision 1.15  1998/05/04 17:54:29  peter
    + smartlinking works (only case jumptable left todo)
    * redesign of systems.pas to support assemblers and linkers
    + Unitname is now also in the PPU-file, increased version to 14

  Revision 1.14  1998/05/01 16:38:46  florian
    * handling of private and protected fixed
    + change_keywords_to_tp implemented to remove
      keywords which aren't supported by tp
    * break and continue are now symbols of the system unit
    + widestring, longstring and ansistring type released

  Revision 1.13  1998/05/01 09:01:25  florian
    + correct semantics of private and protected
    * small fix in variable scope:
       a id can be used in a parameter list of a method, even it is used in
       an anchestor class as field id

  Revision 1.12  1998/05/01 07:43:57  florian
    + basics for rtti implemented
    + switch $m (generate rtti for published sections)

  Revision 1.11  1998/04/30 15:59:42  pierre
    * GDB works again better :
      correct type info in one pass
    + UseTokenInfo for better source position
    * fixed one remaining bug in scanner for line counts
    * several little fixes

  Revision 1.10  1998/04/29 10:34:05  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.9  1998/04/27 23:10:29  peter
    + new scanner
    * $makelib -> if smartlink
    * small filename fixes pmodule.setfilename
    * moved import from files.pas -> import.pas

  Revision 1.8  1998/04/21 10:16:48  peter
    * patches from strasbourg
    * objects is not used anymore in the fpc compiled version

  Revision 1.7  1998/04/13 22:20:36  florian
    + stricter checking for duplicate id, solves also bug0097

  Revision 1.6  1998/04/13 17:20:43  florian
    * tdef.done much faster implemented

  Revision 1.5  1998/04/10 21:36:56  florian
    + some stuff to support method pointers (procedure of object) added
      (declaration, parameter handling)

  Revision 1.4  1998/04/08 16:58:08  pierre
    * several bugfixes
      ADD ADC and AND are also sign extended
      nasm output OK (program still crashes at end
      and creates wrong assembler files !!)
      procsym types sym in tdef removed !!

  Revision 1.3  1998/04/07 13:19:52  pierre
    * bugfixes for reset_gdb_info
      in MEM parsing for go32v2
      better external symbol creation
      support for rhgdb.exe (lowercase file names)

  Revision 1.2  1998/04/06 13:09:04  daniel
  * Emergency solution for bug in reset_gdb_info.
}
