{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl, Daniel Mantione

    Does the parsing of the procedures/functions

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
unit pdecsub;

{$i fpcdefs.inc}

interface

    uses
      tokens,symconst,symtype,symdef,symsym;

    type
      tpdflag=(
        pd_body,       { directive needs a body }
        pd_implemen,   { directive can be used implementation section }
        pd_interface,  { directive can be used interface section }
        pd_object,     { directive can be used object declaration }
        pd_procvar,    { directive can be used procvar declaration }
        pd_notobject,  { directive can not be used object declaration }
        pd_notobjintf, { directive can not be used interface declaration }
        pd_notprocvar  { directive can not be used procvar declaration }
      );
      tpdflags=set of tpdflag;

    function  check_proc_directive(isprocvar:boolean):boolean;

    procedure insert_funcret_local(pd:tprocdef);

    function  proc_add_definition(var pd:tprocdef):boolean;
    function  proc_get_importname(pd:tprocdef):string;
    procedure proc_set_mangledname(pd:tprocdef);

    procedure handle_calling_convention(pd:tabstractprocdef);

    procedure parse_parameter_dec(pd:tabstractprocdef);
    procedure parse_proc_directives(pd:tabstractprocdef;var pdflags:tpdflags);
    procedure parse_var_proc_directives(sym:tsym);
    procedure parse_object_proc_directives(pd:tabstractprocdef);
    function  parse_proc_head(aclass:tobjectdef;potype:tproctypeoption;var pd:tprocdef):boolean;
    function  parse_proc_dec(aclass:tobjectdef):tprocdef;

implementation

    uses
       strings,
       { common }
       cutils,cclasses,
       { global }
       globtype,globals,verbose,
       systems,
       cpuinfo,
       { symtable }
       symbase,symtable,defutil,defcmp,paramgr,cpupara,
       { pass 1 }
       node,htypechk,
       nmat,nadd,ncal,nset,ncnv,ninl,ncon,nld,nflw,
       { parser }
       scanner,
       pbase,pexpr,ptype,pdecl
       ;

    const
      { Please leave this here, this module should NOT use
        these variables.
        Declaring it as string here results in an error when compiling (PFV) }
      current_procinfo = 'error';


    procedure insert_funcret_para(pd:tabstractprocdef);
      var
        storepos : tfileposinfo;
        vs       : tparavarsym;
        paranr   : word;
      begin
        if not(pd.proctypeoption in [potype_constructor,potype_destructor]) and
           not is_void(pd.rettype.def) and
           paramanager.ret_in_param(pd.rettype.def,pd.proccalloption) then
         begin
           storepos:=akttokenpos;
           if pd.deftype=procdef then
            akttokenpos:=tprocdef(pd).fileinfo;

           { For left to right add it at the end to be delphi compatible }
           if pd.proccalloption in pushleftright_pocalls then
             paranr:=paranr_result_leftright
           else
             paranr:=paranr_result;
           { Generate result variable accessing function result }
           vs:=tparavarsym.create('$result',paranr,vs_var,pd.rettype,[vo_is_funcret,vo_is_hidden_para]);
           pd.parast.insert(vs);
           { Store the this symbol as funcretsym for procedures }
           if pd.deftype=procdef then
            tprocdef(pd).funcretsym:=vs;

           akttokenpos:=storepos;
         end;
      end;


    procedure insert_parentfp_para(pd:tabstractprocdef);
      var
        storepos : tfileposinfo;
        vs       : tparavarsym;
      begin
        if pd.parast.symtablelevel>normal_function_level then
          begin
            storepos:=akttokenpos;
            if pd.deftype=procdef then
             akttokenpos:=tprocdef(pd).fileinfo;

            { Generate result variable accessing function result, it
              can't be put in a register since it must be accessable
              from the framepointer }
            vs:=tparavarsym.create('$parentfp',paranr_parentfp,vs_var,voidpointertype,[vo_is_parentfp,vo_is_hidden_para]);
            vs.varregable:=vr_none;
            pd.parast.insert(vs);

            akttokenpos:=storepos;
          end;
      end;


    procedure insert_self_and_vmt_para(pd:tabstractprocdef);
      var
        storepos : tfileposinfo;
        vs       : tparavarsym;
        tt       : ttype;
        vsp      : tvarspez;
      begin
        if (pd.deftype=procvardef) and
           pd.is_methodpointer then
          begin
            { Generate self variable }
            tt:=voidpointertype;
            vs:=tparavarsym.create('$self',paranr_self,vs_value,tt,[vo_is_self,vo_is_hidden_para]);
            pd.parast.insert(vs);
          end
        else
          begin
             if (pd.deftype=procdef) and
                assigned(tprocdef(pd)._class) and
                (pd.parast.symtablelevel=normal_function_level) then
              begin
                storepos:=akttokenpos;
                akttokenpos:=tprocdef(pd).fileinfo;

                { Generate VMT variable for constructor/destructor }
                if pd.proctypeoption in [potype_constructor,potype_destructor] then
                 begin
                   { can't use classrefdef as type because inheriting
                     will then always file because of a type mismatch }
                   tt:=voidpointertype;
                   vs:=tparavarsym.create('$vmt',paranr_vmt,vs_value,tt,[vo_is_vmt,vo_is_hidden_para]);
                   pd.parast.insert(vs);
                 end;

                { Generate self variable, for classes we need
                  to use the generic voidpointer to be compatible with
                  methodpointers }
                vsp:=vs_value;
                if (po_staticmethod in pd.procoptions) or
                   (po_classmethod in pd.procoptions) then
                  begin
                    tt.setdef(tprocdef(pd)._class);
                    tt.setdef(tclassrefdef.create(tt));
                  end
                else
                  begin
                    if is_object(tprocdef(pd)._class) then
                      vsp:=vs_var;
                    tt.setdef(tprocdef(pd)._class);
                  end;
                vs:=tparavarsym.create('$self',paranr_self,vsp,tt,[vo_is_self,vo_is_hidden_para]);
                pd.parast.insert(vs);

                akttokenpos:=storepos;
              end;
          end;
      end;


    procedure insert_funcret_local(pd:tprocdef);
      var
        storepos : tfileposinfo;
        vs       : tlocalvarsym;
        aliasvs  : tabsolutevarsym;
        sl       : tsymlist;
      begin
        { The result from constructors and destructors can't be accessed directly }
        if not(pd.proctypeoption in [potype_constructor,potype_destructor]) and
           not is_void(pd.rettype.def) then
         begin
           storepos:=akttokenpos;
           akttokenpos:=pd.fileinfo;

           { We always need a localsymtable }
           if not assigned(pd.localst) then
            pd.insert_localst;

           { We need to insert a varsym for the result in the localst
             when it is returning in a register }
           if not paramanager.ret_in_param(pd.rettype.def,pd.proccalloption) then
            begin
              vs:=tlocalvarsym.create('$result',vs_value,pd.rettype,[vo_is_funcret]);
              pd.localst.insert(vs);
              pd.funcretsym:=vs;
            end;

           { insert the name of the procedure as alias for the function result,
             we can't use realname because that will not work for compilerprocs
             as the name is lowercase and unreachable from the code }
           if pd.resultname='' then
            pd.resultname:=pd.procsym.name;
           sl:=tsymlist.create;
           sl.addsym(sl_load,pd.funcretsym);
           aliasvs:=tabsolutevarsym.create_ref(pd.resultname,pd.rettype,sl);
           include(aliasvs.varoptions,vo_is_funcret);
           pd.localst.insert(aliasvs);

           { insert result also if support is on }
           if (m_result in aktmodeswitches) then
            begin
              sl:=tsymlist.create;
              sl.addsym(sl_load,pd.funcretsym);
              aliasvs:=tabsolutevarsym.create_ref('RESULT',pd.rettype,sl);
              include(aliasvs.varoptions,vo_is_funcret);
              include(aliasvs.varoptions,vo_is_result);
              pd.localst.insert(aliasvs);
            end;

           akttokenpos:=storepos;
         end;
      end;


    procedure insert_hidden_para(p:tnamedindexitem;arg:pointer);
      var
        hvs : tparavarsym;
        pd  : tabstractprocdef absolute arg;
      begin
        if (tsym(p).typ<>paravarsym) then
         exit;
        with tparavarsym(p) do
         begin
           { We need a local copy for a value parameter when only the
             address is pushed. Open arrays and Array of Const are
             an exception because they are allocated at runtime and the
             address that is pushed is patched }
           if (varspez=vs_value) and
              paramanager.push_addr_param(varspez,vartype.def,pd.proccalloption) and
              not(is_open_array(vartype.def) or
                  is_array_of_const(vartype.def)) then
             include(varoptions,vo_has_local_copy);

           { needs high parameter ? }
           if paramanager.push_high_param(varspez,vartype.def,pd.proccalloption) then
             begin
               hvs:=tparavarsym.create('$high'+name,paranr+1,vs_const,sinttype,[vo_is_high_para,vo_is_hidden_para]);
               owner.insert(hvs);
             end
           else
            begin
              { Give a warning that cdecl routines does not include high()
                support }
              if (pd.proccalloption in [pocall_cdecl,pocall_cppdecl]) and
                 paramanager.push_high_param(varspez,vartype.def,pocall_default) then
               begin
                 if is_open_string(vartype.def) then
                    Message(parser_w_cdecl_no_openstring);
                 if not (po_external in pd.procoptions) then
                   Message(parser_w_cdecl_has_no_high);
               end;
            end;
         end;
      end;

    procedure check_c_para(p:tnamedindexitem;arg:pointer);
      begin
        if (tsym(p).typ<>paravarsym) then
         exit;
        with tparavarsym(p) do
         begin
           case vartype.def.deftype of
             arraydef :
               begin
                 if not is_variant_array(vartype.def) and
                    not is_array_of_const(vartype.def) then
                  begin
                    if (varspez<>vs_var) then
                      Message(parser_h_c_arrays_are_references);
                  end;
                 if is_array_of_const(vartype.def) and
                    assigned(indexnext) and
                    (tsym(indexnext).typ=paravarsym) and
                    not(vo_is_high_para in tparavarsym(indexnext).varoptions) then
                   Message(parser_e_C_array_of_const_must_be_last);
               end;
            end;
         end;
      end;


    procedure check_msg_para(p:tnamedindexitem;arg:pointer);
      begin
        if (tsym(p).typ<>paravarsym) then
         exit;
        with tparavarsym(p) do
          begin
            { Count parameters }
            if (paranr>=10) then
              inc(plongint(arg)^);
            { First parameter must be var }
            if (paranr=10) and
               (varspez<>vs_var) then
              Message(parser_e_ill_msg_param);
          end;
      end;


    procedure check_inline_para(p:tnamedindexitem;arg:pointer);
      var
        pd : tabstractprocdef absolute arg;
      begin
        if (pd.proccalloption<>pocall_inline) or
           (tsym(p).typ<>paravarsym) then
         exit;
        with tparavarsym(p) do
         begin
           case vartype.def.deftype of
             arraydef :
               begin
                 with tarraydef(vartype.def) do
                   if IsVariant or IsConstructor then
                     begin
                       Message1(parser_w_not_supported_for_inline,'array of const');
                       Message(parser_w_inlining_disabled);
                       pd.proccalloption:=pocall_default;
                     end;
               end;
           end;
         end;
      end;


    procedure set_addr_param_regable(p:tnamedindexitem;arg:pointer);
      begin
        if (tsym(p).typ<>paravarsym) then
         exit;
        with tparavarsym(p) do
         begin
           if not vartype.def.needs_inittable and
              paramanager.push_addr_param(varspez,vartype.def,tprocdef(arg).proccalloption) then
             varregable:=vr_intreg;
         end;
      end;


    procedure parse_parameter_dec(pd:tabstractprocdef);
      {
        handle_procvar needs the same changes
      }
      var
        sc      : tsinglelist;
        tt      : ttype;
        arrayelementtype : ttype;
        vs      : tparavarsym;
        srsym   : tsym;
        hs1 : string;
        varspez : Tvarspez;
        defaultvalue : tconstsym;
        defaultrequired : boolean;
        old_object_option : tsymoptions;
        currparast : tparasymtable;
        explicit_paraloc : boolean;
        locationstr : string;
        paranr : integer;
      begin
        explicit_paraloc:=false;
        consume(_LKLAMMER);
        { Delphi/Kylix supports nonsense like }
        { procedure p();                      }
        if try_to_consume(_RKLAMMER) and
          not(m_tp7 in aktmodeswitches) then
          exit;
        { parsing a proc or procvar ? }
        currparast:=tparasymtable(pd.parast);
        { reset }
        sc:=tsinglelist.create;
        defaultrequired:=false;
        paranr:=0;
        { the variables are always public }
        old_object_option:=current_object_option;
        current_object_option:=[sp_public];
        inc(testcurobject);
        repeat
          if try_to_consume(_VAR) then
            varspez:=vs_var
          else
            if try_to_consume(_CONST) then
              varspez:=vs_const
          else
            if (idtoken=_OUT) and (m_out in aktmodeswitches) then
              begin
                 consume(_OUT);
                 varspez:=vs_out
              end
          else
            if (token=_POINTPOINTPOINT) and (m_mac in aktmodeswitches) then
              begin
                consume(_POINTPOINTPOINT);
                include(pd.procoptions,po_varargs);
                break;
              end
          else
              varspez:=vs_value;
          defaultvalue:=nil;
          tt.reset;
          { read identifiers and insert with error type }
          sc.reset;
          repeat
            inc(paranr);
            vs:=tparavarsym.create(orgpattern,paranr*10,varspez,generrortype,[]);
            currparast.insert(vs);
            if assigned(vs.owner) then
             sc.insert(vs)
            else
             vs.free;
            consume(_ID);
          until not try_to_consume(_COMMA);
          locationstr:='';
          { read type declaration, force reading for value and const paras }
          if (token=_COLON) or (varspez=vs_value) then
           begin
             consume(_COLON);
             { check for an open array }
             if token=_ARRAY then
              begin
                consume(_ARRAY);
                consume(_OF);
                { define range and type of range }
                tt.setdef(tarraydef.create(0,-1,s32inttype));
                { array of const ? }
                if (token=_CONST) and (m_objpas in aktmodeswitches) then
                 begin
                   consume(_CONST);
                   srsym:=searchsymonlyin(systemunit,'TVARREC');
                   if not assigned(srsym) then
                     InternalError(200404181);
                   tarraydef(tt.def).setelementtype(ttypesym(srsym).restype);
                   tarraydef(tt.def).IsArrayOfConst:=true;
                 end
                else
                 begin
                   { define field type }
                   single_type(arrayelementtype,hs1,false);
                   tarraydef(tt.def).setelementtype(arrayelementtype);
                 end;
              end
             else
              begin
                { open string ? }
                if (varspez=vs_var) and
                        (
                          (
                            ((token=_STRING) or (idtoken=_SHORTSTRING)) and
                            (cs_openstring in aktmoduleswitches) and
                            not(cs_ansistrings in aktlocalswitches)
                          ) or
                        (idtoken=_OPENSTRING)) then
                 begin
                   consume(token);
                   tt:=openshortstringtype;
                   hs1:='openstring';
                 end
                else
                 begin
                   { everything else }
                   if (m_mac in aktmodeswitches) then
                     try_to_consume(_UNIV); {currently does nothing}
                   single_type(tt,hs1,false);
                 end;

                if (target_info.system in [system_powerpc_morphos,system_m68k_amiga]) then
                  begin
                    if (idtoken=_LOCATION) then
                      begin
                        consume(_LOCATION);
                        locationstr:=pattern;
                        consume(_CSTRING);
                      end
                    else
                      begin
                        if explicit_paraloc then
                          Message(parser_e_paraloc_all_paras);
                        locationstr:='';
                      end;
                  end
                else
                  locationstr:='';

                { default parameter }
                if (m_default_para in aktmodeswitches) then
                 begin
                   if try_to_consume(_EQUAL) then
                    begin
                      vs:=tparavarsym(sc.first);
                      if assigned(vs.listnext) then
                        Message(parser_e_default_value_only_one_para);
                      { prefix 'def' to the parameter name }
                      defaultvalue:=ReadConstant('$def'+vs.name,vs.fileinfo);
                      if assigned(defaultvalue) then
                        begin
                          include(defaultvalue.symoptions,sp_internal);
                          pd.parast.insert(defaultvalue);
                        end;
                      defaultrequired:=true;
                    end
                   else
                    begin
                      if defaultrequired then
                        Message1(parser_e_default_value_expected_for_para,vs.name);
                    end;
                 end;
              end;
           end
          else
           begin
{$ifndef UseNiceNames}
             hs1:='$$$';
{$else UseNiceNames}
             hs1:='var';
{$endif UseNiceNames}
             tt:=cformaltype;
           end;

          { File types are only allowed for var parameters }
          if (tt.def.deftype=filedef) and
             (varspez<>vs_var) then
            CGMessage(cg_e_file_must_call_by_reference);

          vs:=tparavarsym(sc.first);
          while assigned(vs) do
           begin
             { update varsym }
             vs.vartype:=tt;
             vs.defaultconstsym:=defaultvalue;

             if (target_info.system in [system_powerpc_morphos,system_m68k_amiga]) then
               begin
                 if locationstr<>'' then
                   begin
                     if assigned(sc.first.listnext) then
                       Message(parser_e_paraloc_only_one_para);
                     if (paranr>1) and not(explicit_paraloc) then
                       Message(parser_e_paraloc_all_paras);
                     explicit_paraloc:=true;
                     include(vs.varoptions,vo_has_explicit_paraloc);
                     if not(paramanager.parseparaloc(vs,upper(locationstr))) then
                       message(parser_e_illegal_explicit_paraloc);
                   end
                 else
                   if explicit_paraloc then
                     Message(parser_e_paraloc_all_paras);
               end;
             vs:=tparavarsym(vs.listnext);
           end;
        until not try_to_consume(_SEMICOLON);

        if explicit_paraloc then
          begin
            pd.has_paraloc_info:=true;
            include(pd.procoptions,po_explicitparaloc);
          end;
        { remove parasymtable from stack }
        sc.free;
        { reset object options }
        dec(testcurobject);
        current_object_option:=old_object_option;
        consume(_RKLAMMER);
      end;


    function parse_proc_head(aclass:tobjectdef;potype:tproctypeoption;var pd:tprocdef):boolean;
      var
        orgsp,sp : stringid;
        sym : tsym;
        srsym : tsym;
        srsymtable : tsymtable;
        storepos,
        procstartfilepos : tfileposinfo;
        searchagain : boolean;
        i : longint;
        st : tsymtable;
        aprocsym : tprocsym;
      begin
        { Save the position where this procedure really starts }
        procstartfilepos:=akttokenpos;

        result:=false;
        pd:=nil;
        aprocsym:=nil;

        if (potype=potype_operator) then
          begin
            sp:=overloaded_names[optoken];
            orgsp:=sp;
          end
        else
          begin
            sp:=pattern;
            orgsp:=orgpattern;
            consume(_ID);
          end;

        { examine interface map: function/procedure iname.functionname=locfuncname }
        if assigned(aclass) and
           assigned(aclass.implementedinterfaces) and
           (aclass.implementedinterfaces.count>0) and
           try_to_consume(_POINT) then
         begin
           storepos:=akttokenpos;
           akttokenpos:=procstartfilepos;
           { get interface syms}
           searchsym(sp,sym,srsymtable);
           if not assigned(sym) then
            begin
              identifier_not_found(orgsp);
              sym:=generrorsym;
            end;
           akttokenpos:=storepos;
           { qualifier is interface? }
           if (sym.typ=typesym) and
              (ttypesym(sym).restype.def.deftype=objectdef) then
             i:=aclass.implementedinterfaces.searchintf(ttypesym(sym).restype.def)
           else
             i:=-1;
           if (i=-1) then
             Message(parser_e_interface_id_expected);
           consume(_ID);
           consume(_EQUAL);
           if (token=_ID) then
             aclass.implementedinterfaces.addmappings(i,sp,pattern);
           consume(_ID);
           result:=true;
           exit;
         end;

        { method  ? }
        if not assigned(aclass) and
           (potype<>potype_operator) and
           (symtablestack.symtablelevel=main_program_level) and
           try_to_consume(_POINT) then
         begin
           { search for object name }
           storepos:=akttokenpos;
           akttokenpos:=procstartfilepos;
           searchsym(sp,sym,srsymtable);
           if not assigned(sym) then
            begin
              identifier_not_found(orgsp);
              sym:=generrorsym;
            end;
           akttokenpos:=storepos;
           { consume proc name }
           sp:=pattern;
           orgsp:=orgpattern;
           procstartfilepos:=akttokenpos;
           consume(_ID);
           { qualifier is class name ? }
           if (sym.typ=typesym) and
              (ttypesym(sym).restype.def.deftype=objectdef) then
            begin
              aclass:=tobjectdef(ttypesym(sym).restype.def);
              aprocsym:=tprocsym(aclass.symtable.search(sp));
              { we solve this below }
              if assigned(aprocsym) then
               begin
                 if aprocsym.typ<>procsym then
                  begin
                    {  we use a different error message for tp7 so it looks more compatible }
                    if (m_fpc in aktmodeswitches) then
                      Message1(parser_e_overloaded_no_procedure,aprocsym.realname)
                    else
                      Message(parser_e_methode_id_expected);
                    { rename the name to an unique name to avoid an
                      error when inserting the symbol in the symtable }
                    orgsp:=orgsp+'$'+tostr(aktfilepos.line);
                    aprocsym:=nil;
                  end;
               end
              else
               begin
                 Message(parser_e_methode_id_expected);
                 { recover by making it a normal procedure instead of method }
                 aclass:=nil;
               end;
            end
           else
            Message(parser_e_class_id_expected);
         end
        else
         begin
           { check for constructor/destructor which is not allowed here }
           if (not parse_only) and
              (potype in [potype_constructor,potype_destructor]) then
             Message(parser_e_constructors_always_objects);

           repeat
             searchagain:=false;
             akttokenpos:=procstartfilepos;
             srsym:=tsym(symtablestack.search(sp));

             if not(parse_only) and
                not assigned(srsym) and
                (symtablestack.symtabletype=staticsymtable) and
                assigned(symtablestack.next) and
                (symtablestack.next.unitid=0) then
               begin
                 { The procedure we prepare for is in the implementation
                   part of the unit we compile. It is also possible that we
                   are compiling a program, which is also some kind of
                   implementaion part.

                   We need to find out if the procedure is global. If it is
                   global, it is in the global symtable.}
                 srsym:=tsym(symtablestack.next.search(sp));
               end;

             { Check if overloaded is a procsym }
             if assigned(srsym) then
               begin
                 if srsym.typ=procsym then
                   aprocsym:=tprocsym(srsym)
                 else
                   begin
                     { when the other symbol is a unit symbol then hide the unit
                       symbol }
                     if (srsym.typ=unitsym) then
                      begin
                        srsym.owner.rename(srsym.name,'hidden'+srsym.name);
                        searchagain:=true;
                      end
                     else
                      begin
                        {  we use a different error message for tp7 so it looks more compatible }
                        if (m_fpc in aktmodeswitches) then
                         Message1(parser_e_overloaded_no_procedure,srsym.realname)
                        else
                         tstoredsymtable(symtablestack).DuplicateSym(nil,srsym);
                        { rename the name to an unique name to avoid an
                          error when inserting the symbol in the symtable }
                        orgsp:=orgsp+'$'+tostr(aktfilepos.line);
                      end;
                   end;
              end;
           until not searchagain;
         end;

        { test again if assigned, it can be reset to recover }
        if not assigned(aprocsym) then
         begin
           { create a new procsym and set the real filepos }
           akttokenpos:=procstartfilepos;
           { for operator we have only one procsym for each overloaded
             operation }
           if (potype=potype_operator) then
             begin
               Aprocsym:=Tprocsym(symtablestack.search(sp));
               if Aprocsym=nil then
                 Aprocsym:=tprocsym.create('$'+sp);
             end
            else
             aprocsym:=tprocsym.create(orgsp);
            symtablestack.insert(aprocsym);
         end;

        { to get the correct symtablelevel we must ignore objectsymtables }
        st:=symtablestack;
        while not(st.symtabletype in [staticsymtable,globalsymtable,localsymtable]) do
         st:=st.next;
        pd:=tprocdef.create(st.symtablelevel+1);
        pd._class:=aclass;
        pd.procsym:=aprocsym;
        pd.proctypeoption:=potype;
        { methods need to be exported }
        if assigned(aclass) and
           (
            (symtablestack.symtabletype=objectsymtable) or
            (symtablestack.symtablelevel=main_program_level)
           ) then
          include(pd.procoptions,po_global);

        { symbol options that need to be kept per procdef }
        pd.fileinfo:=procstartfilepos;
        pd.symoptions:=current_object_option;

        { parse parameters }
        if token=_LKLAMMER then
          parse_parameter_dec(pd);

        result:=true;
      end;


    function parse_proc_dec(aclass:tobjectdef):tprocdef;
      var
        pd : tprocdef;
        hs : string;
        isclassmethod : boolean;
      begin
        pd:=nil;
        isclassmethod:=false;
        { read class method }
        if try_to_consume(_CLASS) then
         begin
           { class method only allowed for procedures and functions }
           if not(token in [_FUNCTION,_PROCEDURE]) then
             Message(parser_e_procedure_or_function_expected);

           if is_interface(aclass) then
             Message(parser_e_no_static_method_in_interfaces)
           else
             isclassmethod:=true;
         end;
        case token of
          _FUNCTION :
            begin
              consume(_FUNCTION);
              if parse_proc_head(aclass,potype_none,pd) then
                begin
                  { pd=nil when it is a interface mapping }
                  if assigned(pd) then
                    begin
                      if try_to_consume(_COLON) then
                       begin
                         inc(testcurobject);
                         single_type(pd.rettype,hs,false);
                         pd.test_if_fpu_result;
                         dec(testcurobject);
                       end
                      else
                       begin
                          if (
                              parse_only and
                              not(is_interface(pd._class))
                             ) or
                             (m_repeat_forward in aktmodeswitches) then
                          begin
                            consume(_COLON);
                            consume_all_until(_SEMICOLON);
                          end;
                       end;
                      if isclassmethod then
                       include(pd.procoptions,po_classmethod);
                    end;
                end
              else
                begin
                  { recover }
                  consume(_COLON);
                  consume_all_until(_SEMICOLON);
                end;
            end;

          _PROCEDURE :
            begin
              consume(_PROCEDURE);
              if parse_proc_head(aclass,potype_none,pd) then
                begin
                  { pd=nil when it is a interface mapping }
                  if assigned(pd) then
                    begin
                      pd.rettype:=voidtype;
                      if isclassmethod then
                        include(pd.procoptions,po_classmethod);
                    end;
                end;
            end;

          _CONSTRUCTOR :
            begin
              consume(_CONSTRUCTOR);
              parse_proc_head(aclass,potype_constructor,pd);
              if assigned(pd) and
                 assigned(pd._class) then
                begin
                  { Set return type, class constructors return the
                    created instance, object constructors return boolean }
                  if is_class(pd._class) then
                   pd.rettype.setdef(pd._class)
                  else
                   pd.rettype:=booltype;
                end;
            end;

          _DESTRUCTOR :
            begin
              consume(_DESTRUCTOR);
              parse_proc_head(aclass,potype_destructor,pd);
              if assigned(pd) then
                pd.rettype:=voidtype;
            end;

          _OPERATOR :
            begin
              consume(_OPERATOR);
              if (token in [first_overloaded..last_overloaded]) then
               begin
                 optoken:=token;
               end
              else
               begin
                 Message(parser_e_overload_operator_failed);
                 { Use the dummy NOTOKEN that is also declared
                   for the overloaded_operator[] }
                 optoken:=NOTOKEN;
               end;
              consume(token);
              parse_proc_head(aclass,potype_operator,pd);
              if assigned(pd) then
                begin
                  if pd.parast.symtablelevel>normal_function_level then
                    Message(parser_e_no_local_operator);
                  if token<>_ID then
                    begin
                       if not(m_result in aktmodeswitches) then
                         consume(_ID);
                    end
                  else
                    begin
                      pd.resultname:=orgpattern;
                      consume(_ID);
                    end;
                  if not try_to_consume(_COLON) then
                    begin
                      consume(_COLON);
                      pd.rettype:=generrortype;
                      consume_all_until(_SEMICOLON);
                    end
                  else
                   begin
                     single_type(pd.rettype,hs,false);
                     pd.test_if_fpu_result;
                     if (optoken in [_EQUAL,_GT,_LT,_GTE,_LTE]) and
                        ((pd.rettype.def.deftype<>orddef) or
                         (torddef(pd.rettype.def).typ<>bool8bit)) then
                        Message(parser_e_comparative_operator_return_boolean);
                     if (optoken=_ASSIGNMENT) and
                        equal_defs(pd.rettype.def,
                           tparavarsym(pd.parast.symindex.first).vartype.def) then
                       message(parser_e_no_such_assignment)
                     else if not isoperatoracceptable(pd,optoken) then
                       Message(parser_e_overload_impossible);
                   end;
                end
              else
                begin
                  { recover }
                  try_to_consume(_ID);
                  consume(_COLON);
                  consume_all_until(_SEMICOLON);
                end;
            end;
        end;
        { support procedure proc stdcall export; }
        if not(check_proc_directive(false)) then
          consume(_SEMICOLON);
        result:=pd;
      end;


{****************************************************************************
                        Procedure directive handlers
****************************************************************************}

procedure pd_far(pd:tabstractprocdef);
begin
  Message1(parser_w_proc_directive_ignored,'FAR');
end;

procedure pd_near(pd:tabstractprocdef);
begin
  Message1(parser_w_proc_directive_ignored,'NEAR');
end;

procedure pd_export(pd:tabstractprocdef);
begin
  if pd.deftype<>procdef then
    internalerror(200304264);
  if assigned(tprocdef(pd)._class) then
    Message(parser_e_methods_dont_be_export);
  if pd.parast.symtablelevel>normal_function_level then
    Message(parser_e_dont_nest_export);
end;

procedure pd_forward(pd:tabstractprocdef);
begin
  if pd.deftype<>procdef then
    internalerror(200304265);
  tprocdef(pd).forwarddef:=true;
end;


procedure pd_alias(pd:tabstractprocdef);
begin
  if pd.deftype<>procdef then
    internalerror(200304266);
  consume(_COLON);
  tprocdef(pd).aliasnames.insert(get_stringconst);
  include(pd.procoptions,po_has_public_name);
end;


procedure pd_public(pd:tabstractprocdef);
begin
  if pd.deftype<>procdef then
    internalerror(200304266);
  if try_to_consume(_NAME) then
    begin
      tprocdef(pd).aliasnames.insert(get_stringconst);
      include(pd.procoptions,po_has_public_name);
    end;
end;


procedure pd_asmname(pd:tabstractprocdef);
begin
  if pd.deftype<>procdef then
    internalerror(200304267);
  tprocdef(pd).aliasnames.insert(target_info.Cprefix+pattern);
  if token=_CCHAR then
    consume(_CCHAR)
  else
    consume(_CSTRING);
  { we don't need anything else }
  tprocdef(pd).forwarddef:=false;
end;


procedure pd_inline(pd:tabstractprocdef);
begin
  { Check if there are parameters that can't be inlined }
  pd.parast.foreach_static(@check_inline_para,pd);
end;

procedure pd_internconst(pd:tabstractprocdef);
begin
  if pd.deftype<>procdef then
    internalerror(200304268);
  consume(_COLON);
  tprocdef(pd).extnumber:=get_intconst;
end;

procedure pd_internproc(pd:tabstractprocdef);
begin
  if pd.deftype<>procdef then
    internalerror(200304268);
  consume(_COLON);
  tprocdef(pd).extnumber:=get_intconst;
  { the proc is defined }
  tprocdef(pd).forwarddef:=false;
end;

procedure pd_interrupt(pd:tabstractprocdef);
begin
  if pd.parast.symtablelevel>normal_function_level then
    Message(parser_e_dont_nest_interrupt);
end;

procedure pd_abstract(pd:tabstractprocdef);
begin
  if pd.deftype<>procdef then
    internalerror(200304269);
  if (po_virtualmethod in pd.procoptions) then
    include(pd.procoptions,po_abstractmethod)
  else
    Message(parser_e_only_virtual_methods_abstract);
  { the method is defined }
  tprocdef(pd).forwarddef:=false;
end;

procedure pd_virtual(pd:tabstractprocdef);
{$ifdef WITHDMT}
var
  pt : tnode;
{$endif WITHDMT}
begin
  if pd.deftype<>procdef then
    internalerror(2003042610);
  if (pd.proctypeoption=potype_constructor) and
     is_object(tprocdef(pd)._class) then
    Message(parser_e_constructor_cannot_be_not_virtual);
{$ifdef WITHDMT}
  if is_object(tprocdef(pd)._class) and
     (token<>_SEMICOLON) then
    begin
       { any type of parameter is allowed here! }
       pt:=comp_expr(true);
       if is_constintnode(pt) then
         begin
           include(pd.procoptions,po_msgint);
           pd.messageinf.i:=pt^.value;
         end
       else
         Message(parser_e_ill_msg_expr);
       disposetree(pt);
    end;
{$endif WITHDMT}
end;

procedure pd_static(pd:tabstractprocdef);
begin
  if (cs_static_keyword in aktmoduleswitches) then
    begin
      if pd.deftype=procdef then
        include(tprocdef(pd).procsym.symoptions,sp_static);
      include(pd.procoptions,po_staticmethod);
    end;
end;

procedure pd_override(pd:tabstractprocdef);
begin
  if pd.deftype<>procdef then
    internalerror(2003042611);
  if not(is_class_or_interface(tprocdef(pd)._class)) then
    Message(parser_e_no_object_override);
end;

procedure pd_overload(pd:tabstractprocdef);
begin
  if pd.deftype<>procdef then
    internalerror(2003042612);
  include(tprocdef(pd).procsym.symoptions,sp_has_overloaded);
end;

procedure pd_message(pd:tabstractprocdef);
var
  pt : tnode;
  paracnt : longint;
begin
  if pd.deftype<>procdef then
    internalerror(2003042613);
  if not is_class(tprocdef(pd)._class) then
    Message(parser_e_msg_only_for_classes);
  { check parameter type }
  paracnt:=0;
  pd.parast.foreach_static(@check_msg_para,@paracnt);
  if paracnt<>1 then
    Message(parser_e_ill_msg_param);
  pt:=comp_expr(true);
  if pt.nodetype=stringconstn then
    begin
      include(pd.procoptions,po_msgstr);
      tprocdef(pd).messageinf.str:=strnew(tstringconstnode(pt).value_str);
    end
  else
   if is_constintnode(pt) then
    begin
      include(pd.procoptions,po_msgint);
      tprocdef(pd).messageinf.i:=tordconstnode(pt).value;
    end
  else
    Message(parser_e_ill_msg_expr);
  pt.free;
end;


procedure pd_reintroduce(pd:tabstractprocdef);
begin
  if pd.deftype<>procdef then
    internalerror(200401211);
  if not(is_class_or_interface(tprocdef(pd)._class)) then
    Message(parser_e_no_object_reintroduce);
end;


procedure pd_syscall(pd:tabstractprocdef);
{$ifdef powerpc}
var
  vs  : tparavarsym;
  sym : tsym;
  symtable : tsymtable;
{$endif powerpc}
begin
  if pd.deftype<>procdef then
    internalerror(2003042614);
  tprocdef(pd).forwarddef:=false;
{$ifdef powerpc}
   if target_info.system in [system_powerpc_morphos,system_m68k_amiga] then
    begin
      if idtoken=_LEGACY then
        begin
          consume(_LEGACY);
          include(pd.procoptions,po_syscall_legacy);
        end 
      else if idtoken=_SYSV then
        begin
          consume(_SYSV);
          include(pd.procoptions,po_syscall_sysv);
        end
      else 
        if syscall_convention='LEGACY' then 
          include(pd.procoptions,po_syscall_legacy)
        else if syscall_convention='SYSV' then
          include(pd.procoptions,po_syscall_sysv)
        else
          internalerror(2005010404);
      
      if consume_sym(sym,symtable) then
        begin
          if (sym.typ=globalvarsym) and
             (
              (tabstractvarsym(sym).vartype.def.deftype=pointerdef) or
              is_32bitint(tabstractvarsym(sym).vartype.def)
             ) then
            begin
              tprocdef(pd).libsym:=sym;
              if po_syscall_legacy in tprocdef(pd).procoptions then
                begin
                  vs:=tparavarsym.create('$syscalllib',paranr_syscall,vs_value,tabstractvarsym(sym).vartype,[vo_is_syscall_lib,vo_is_hidden_para,vo_has_explicit_paraloc]);
                  paramanager.parseparaloc(vs,'A6');
                  pd.parast.insert(vs);
                end;
            end
          else
            Message(parser_e_32bitint_or_pointer_variable_expected);
        end;
      (paramanager as tppcparamanager).create_funcretloc_info(pd,calleeside);
      (paramanager as tppcparamanager).create_funcretloc_info(pd,callerside);
    end;
{$endif powerpc}
  tprocdef(pd).extnumber:=get_intconst;
end;


procedure pd_external(pd:tabstractprocdef);
{
  If import_dll=nil the procedure is assumed to be in another
  object file. In that object file it should have the name to
  which import_name is pointing to. Otherwise, the procedure is
  assumed to be in the DLL to which import_dll is pointing to. In
  that case either import_nr<>0 or import_name<>nil is true, so
  the procedure is either imported by number or by name. (DM)
}
begin
  if pd.deftype<>procdef then
    internalerror(2003042615);
  with tprocdef(pd) do
    begin
      forwarddef:=false;
      { forbid local external procedures }
      if parast.symtablelevel>normal_function_level then
        Message(parser_e_no_local_proc_external);
      { If the procedure should be imported from a DLL, a constant string follows.
        This isn't really correct, an contant string expression follows
        so we check if an semicolon follows, else a string constant have to
        follow (FK) }
      if not(token=_SEMICOLON) and not(idtoken=_NAME) then
        begin
          import_dll:=stringdup(get_stringconst);
          if (idtoken=_NAME) then
           begin
             consume(_NAME);
             import_name:=stringdup(get_stringconst);
             if import_name^='' then
               message(parser_e_empty_import_name);
           end;
          if (idtoken=_INDEX) then
           begin
             {After the word index follows the index number in the DLL.}
             consume(_INDEX);
             import_nr:=get_intconst;
           end;
          { default is to used the realname of the procedure }
          if (import_nr=0) and not assigned(import_name) then
            import_name:=stringdup(procsym.realname);
        end
      else
        begin
          if (idtoken=_NAME) then
           begin
             consume(_NAME);
             import_name:=stringdup(get_stringconst);
             if import_name^='' then
               message(parser_e_empty_import_name);
           end;
        end;
    end;
end;


type
   pd_handler=procedure(pd:tabstractprocdef);
   proc_dir_rec=record
     idtok     : ttoken;
     pd_flags  : tpdflags;
     handler   : pd_handler;
     pocall    : tproccalloption;
     pooption  : tprocoptions;
     mutexclpocall : tproccalloptions;
     mutexclpotype : tproctypeoptions;
     mutexclpo     : tprocoptions;
   end;
const
  {Should contain the number of procedure directives we support.}
  num_proc_directives=36;
  proc_direcdata:array[1..num_proc_directives] of proc_dir_rec=
   (
    (
      idtok:_ABSTRACT;
      pd_flags : [pd_interface,pd_object,pd_notobjintf];
      handler  : @pd_abstract;
      pocall   : pocall_none;
      pooption : [po_abstractmethod];
      mutexclpocall : [pocall_internproc,pocall_inline];
      mutexclpotype : [];
      mutexclpo     : [po_exports,po_interrupt,po_external]
    ),(
      idtok:_ALIAS;
      pd_flags : [pd_implemen,pd_body,pd_notobjintf];
      handler  : @pd_alias;
      pocall   : pocall_none;
      pooption : [];
      mutexclpocall : [pocall_inline];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_ASMNAME;
      pd_flags : [pd_interface,pd_implemen,pd_notobjintf];
      handler  : @pd_asmname;
      pocall   : pocall_cdecl;
      pooption : [po_external];
      mutexclpocall : [pocall_internproc,pocall_inline];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_ASSEMBLER;
      pd_flags : [pd_interface,pd_implemen,pd_body,pd_notobjintf];
      handler  : nil;
      pocall   : pocall_none;
      pooption : [po_assembler];
      mutexclpocall : [];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_C; {same as cdecl for mode mac}
      pd_flags : [pd_interface,pd_implemen,pd_body,pd_procvar];
      handler  : nil;
      pocall   : pocall_cdecl;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_assembler,po_external]
    ),(
      idtok:_CDECL;
      pd_flags : [pd_interface,pd_implemen,pd_body,pd_procvar];
      handler  : nil;
      pocall   : pocall_cdecl;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_assembler,po_external]
    ),(
      idtok:_DYNAMIC;
      pd_flags : [pd_interface,pd_object,pd_notobjintf];
      handler  : @pd_virtual;
      pocall   : pocall_none;
      pooption : [po_virtualmethod];
      mutexclpocall : [pocall_internproc,pocall_inline];
      mutexclpotype : [];
      mutexclpo     : [po_exports,po_interrupt,po_external,po_overridingmethod]
    ),(
      idtok:_EXPORT;
      pd_flags : [pd_body,pd_interface,pd_implemen,pd_notobjintf];
      handler  : @pd_export;
      pocall   : pocall_none;
      pooption : [po_exports,po_global];
      mutexclpocall : [pocall_internproc,pocall_inline];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_external,po_interrupt]
    ),(
      idtok:_EXTERNAL;
      pd_flags : [pd_implemen,pd_interface,pd_notobject,pd_notobjintf];
      handler  : @pd_external;
      pocall   : pocall_none;
      pooption : [po_external];
      mutexclpocall : [pocall_internproc,pocall_inline,pocall_syscall];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_public,po_exports,po_interrupt,po_assembler]
    ),(
      idtok:_FAR;
      pd_flags : [pd_implemen,pd_body,pd_interface,pd_procvar,pd_notobject,pd_notobjintf];
      handler  : @pd_far;
      pocall   : pocall_none;
      pooption : [];
      mutexclpocall : [pocall_internproc,pocall_inline];
      mutexclpotype : [];
      mutexclpo     : []
    ),(
      idtok:_FAR16;
      pd_flags : [pd_interface,pd_implemen,pd_body,pd_procvar,pd_notobject];
      handler  : nil;
      pocall   : pocall_far16;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_FORWARD;
      pd_flags : [pd_implemen,pd_notobject,pd_notobjintf];
      handler  : @pd_forward;
      pocall   : pocall_none;
      pooption : [];
      mutexclpocall : [pocall_internproc,pocall_inline];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_OLDFPCCALL;
      pd_flags : [pd_interface,pd_implemen,pd_body,pd_procvar];
      handler  : nil;
      pocall   : pocall_oldfpccall;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [];
      mutexclpo     : []
    ),(
      idtok:_INLINE;
      pd_flags : [pd_interface,pd_implemen,pd_body,pd_notobjintf];
      handler  : @pd_inline;
      pocall   : pocall_inline;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_exports,po_external,po_interrupt,po_virtualmethod]
    ),(
      idtok:_INTERNCONST;
      pd_flags : [pd_interface,pd_body,pd_notobject,pd_notobjintf];
      handler  : @pd_internconst;
      pocall   : pocall_none;
      pooption : [po_internconst];
      mutexclpocall : [];
      mutexclpotype : [potype_operator];
      mutexclpo     : []
    ),(
      idtok:_INTERNPROC;
      pd_flags : [pd_interface,pd_notobject,pd_notobjintf];
      handler  : @pd_internproc;
      pocall   : pocall_internproc;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [potype_constructor,potype_destructor,potype_operator];
      mutexclpo     : [po_exports,po_external,po_interrupt,po_assembler,po_iocheck,po_virtualmethod]
    ),(
      idtok:_INTERRUPT;
      pd_flags : [pd_implemen,pd_body,pd_notobject,pd_notobjintf];
      handler  : @pd_interrupt;
      pocall   : pocall_none;
      pooption : [po_interrupt];
      mutexclpocall : [pocall_internproc,pocall_cdecl,pocall_cppdecl,pocall_stdcall,
                       pocall_inline,pocall_pascal,pocall_far16,pocall_oldfpccall];
      mutexclpotype : [potype_constructor,potype_destructor,potype_operator];
      mutexclpo     : [po_external]
    ),(
      idtok:_IOCHECK;
      pd_flags : [pd_implemen,pd_body,pd_notobjintf];
      handler  : nil;
      pocall   : pocall_none;
      pooption : [po_iocheck];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_MESSAGE;
      pd_flags : [pd_interface,pd_object,pd_notobjintf];
      handler  : @pd_message;
      pocall   : pocall_none;
      pooption : []; { can be po_msgstr or po_msgint }
      mutexclpocall : [pocall_inline,pocall_internproc];
      mutexclpotype : [potype_constructor,potype_destructor,potype_operator];
      mutexclpo     : [po_interrupt,po_external]
    ),(
      idtok:_NEAR;
      pd_flags : [pd_implemen,pd_body,pd_procvar,pd_notobjintf];
      handler  : @pd_near;
      pocall   : pocall_none;
      pooption : [];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : []
    ),(
      idtok:_NOSTACKFRAME;
      pd_flags : [pd_implemen,pd_body,pd_procvar,pd_notobjintf];
      handler  : nil;
      pocall   : pocall_none;
      pooption : [po_nostackframe];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : []
    ),(
      idtok:_OVERLOAD;
      pd_flags : [pd_implemen,pd_interface,pd_body];
      handler  : @pd_overload;
      pocall   : pocall_none;
      pooption : [po_overload];
      mutexclpocall : [pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : []
    ),(
      idtok:_OVERRIDE;
      pd_flags : [pd_interface,pd_object,pd_notobjintf];
      handler  : @pd_override;
      pocall   : pocall_none;
      pooption : [po_overridingmethod,po_virtualmethod];
      mutexclpocall : [pocall_inline,pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_exports,po_external,po_interrupt,po_virtualmethod]
    ),(
      idtok:_PASCAL;
      pd_flags : [pd_interface,pd_implemen,pd_body,pd_procvar];
      handler  : nil;
      pocall   : pocall_pascal;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_external]
    ),(
      idtok:_PUBLIC;
      pd_flags : [pd_interface,pd_implemen,pd_body,pd_notobject,pd_notobjintf];
      handler  : @pd_public;
      pocall   : pocall_none;
      pooption : [po_public,po_global];
      mutexclpocall : [pocall_internproc,pocall_inline];
      mutexclpotype : [];
      mutexclpo     : [po_external]
    ),(
      idtok:_REGISTER;
      pd_flags : [pd_interface,pd_implemen,pd_body,pd_procvar];
      handler  : nil;
      pocall   : pocall_register;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_external]
    ),(
      idtok:_REINTRODUCE;
      pd_flags : [pd_interface,pd_object,pd_notobjintf];
      handler  : @pd_reintroduce;
      pocall   : pocall_none;
      pooption : [po_reintroduce];
      mutexclpocall : [pocall_inline,pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_external,po_interrupt,po_exports,po_overridingmethod]
    ),(
      idtok:_SAFECALL;
      pd_flags : [pd_interface,pd_implemen,pd_body,pd_procvar];
      handler  : nil;
      pocall   : pocall_safecall;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_external]
    ),(
      idtok:_SOFTFLOAT;
      pd_flags : [pd_interface,pd_implemen,pd_body,pd_procvar];
      handler  : nil;
      pocall   : pocall_softfloat;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [potype_constructor,potype_destructor];
      { it's available with po_external because the libgcc floating point routines on the arm
        uses this calling convention }
      mutexclpo     : []
    ),(
      idtok:_STATIC;
      pd_flags : [pd_interface,pd_object,pd_notobjintf];
      handler  : @pd_static;
      pocall   : pocall_none;
      pooption : [po_staticmethod];
      mutexclpocall : [pocall_inline,pocall_internproc];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_external,po_interrupt,po_exports]
    ),(
      idtok:_STDCALL;
      pd_flags : [pd_interface,pd_implemen,pd_body,pd_procvar];
      handler  : nil;
      pocall   : pocall_stdcall;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_external]
    ),(
      idtok:_SYSCALL;
      pd_flags : [pd_interface,pd_implemen,pd_notobject,pd_notobjintf];
      handler  : @pd_syscall;
      pocall   : pocall_syscall;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_external,po_assembler,po_interrupt,po_exports]
    ),(
      idtok:_VIRTUAL;
      pd_flags : [pd_interface,pd_object,pd_notobjintf];
      handler  : @pd_virtual;
      pocall   : pocall_none;
      pooption : [po_virtualmethod];
      mutexclpocall : [pocall_inline,pocall_internproc];
      mutexclpotype : [];
      mutexclpo     : [po_external,po_interrupt,po_exports,po_overridingmethod]
    ),(
      idtok:_CPPDECL;
      pd_flags : [pd_interface,pd_implemen,pd_body,pd_procvar];
      handler  : nil;
      pocall   : pocall_cppdecl;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_assembler,po_external,po_virtualmethod]
    ),(
      idtok:_VARARGS;
      pd_flags : [pd_interface,pd_implemen,pd_procvar];
      handler  : nil;
      pocall   : pocall_none;
      pooption : [po_varargs];
      mutexclpocall : [pocall_internproc,pocall_stdcall,pocall_register,
                       pocall_inline,pocall_far16,pocall_oldfpccall];
      mutexclpotype : [];
      mutexclpo     : [po_assembler,po_interrupt]
    ),(
      idtok:_COMPILERPROC;
      pd_flags : [pd_interface,pd_implemen,pd_body,pd_notobjintf];
      handler  : nil;
      pocall   : pocall_compilerproc;
      pooption : [];
      mutexclpocall : [];
      mutexclpotype : [potype_constructor,potype_destructor];
      mutexclpo     : [po_interrupt]
    )
   );


    function check_proc_directive(isprocvar:boolean):boolean;
      var
        i : longint;
      begin
        result:=false;
        for i:=1 to num_proc_directives do
         if proc_direcdata[i].idtok=idtoken then
          begin
            if ((not isprocvar) or
               (pd_procvar in proc_direcdata[i].pd_flags)) and
               { don't eat a public directive in classes }
               not((idtoken=_PUBLIC) and (symtablestack.symtabletype=objectsymtable)) then
              result:=true;
            exit;
          end;
      end;


    function parse_proc_direc(pd:tabstractprocdef;var pdflags:tpdflags):boolean;
      {
        Parse the procedure directive, returns true if a correct directive is found
      }
      var
        p     : longint;
        found : boolean;
        name  : stringid;
      begin
        parse_proc_direc:=false;
        name:=tokeninfo^[idtoken].str;
        found:=false;

      { Hint directive? Then exit immediatly }
        if (m_hintdirective in aktmodeswitches) then
         begin
           case idtoken of
             _LIBRARY,
             _PLATFORM,
             _UNIMPLEMENTED,
             _DEPRECATED :
               exit;
           end;
         end;

        { C directive is MAC only, because it breaks too much existing code
          on other platforms (PFV) }
        if (idtoken=_C) and
           not(m_mac in aktmodeswitches) then
          exit;

      { retrieve data for directive if found }
        for p:=1 to num_proc_directives do
         if proc_direcdata[p].idtok=idtoken then
          begin
            found:=true;
            break;
          end;

      { Check if the procedure directive is known }
        if not found then
         begin
            { parsing a procvar type the name can be any
              next variable !! }
            if ((pdflags * [pd_procvar,pd_object])=[]) and
               not(idtoken=_PROPERTY) then
              Message1(parser_w_unknown_proc_directive_ignored,name);
            exit;
         end;

        { static needs a special treatment }
        if (idtoken=_STATIC) and not (cs_static_keyword in aktmoduleswitches) then
          exit;

        { check if method and directive not for object, like public.
          This needs to be checked also for procvars }
        if (pd_notobject in proc_direcdata[p].pd_flags) and
           (symtablestack.symtabletype=objectsymtable) then
           exit;

        { Conflicts between directives ? }
        if (pd.proctypeoption in proc_direcdata[p].mutexclpotype) or
           (pd.proccalloption in proc_direcdata[p].mutexclpocall) or
           ((pd.procoptions*proc_direcdata[p].mutexclpo)<>[]) then
         begin
           Message1(parser_e_proc_dir_conflict,name);
           exit;
         end;

        { set calling convention }
        if proc_direcdata[p].pocall<>pocall_none then
         begin
           if (po_hascallingconvention in pd.procoptions) then
            begin
              Message2(parser_w_proc_overriding_calling,
                proccalloptionStr[pd.proccalloption],
                proccalloptionStr[proc_direcdata[p].pocall]);
            end;
           { check if the target processor supports this calling convention }
           if not(proc_direcdata[p].pocall in supported_calling_conventions) then
             begin
               Message1(parser_e_illegal_calling_convention,proccalloptionStr[proc_direcdata[p].pocall]);
               { recover }
               proc_direcdata[p].pocall:=pocall_stdcall;
             end;
           pd.proccalloption:=proc_direcdata[p].pocall;
           include(pd.procoptions,po_hascallingconvention);
         end;

        if pd.deftype=procdef then
         begin
           { Check if the directive is only for objects }
           if (pd_object in proc_direcdata[p].pd_flags) and
              not assigned(tprocdef(pd)._class) then
            exit;

           { check if method and directive not for interface }
           if (pd_notobjintf in proc_direcdata[p].pd_flags) and
              is_interface(tprocdef(pd)._class) then
            exit;
         end;

        { consume directive, and turn flag on }
        consume(token);
        parse_proc_direc:=true;

        { Check the pd_flags if the directive should be allowed }
        if (pd_interface in pdflags) and
           not(pd_interface in proc_direcdata[p].pd_flags) then
          begin
            Message1(parser_e_proc_dir_not_allowed_in_interface,name);
            exit;
          end;
        if (pd_implemen in pdflags) and
           not(pd_implemen in proc_direcdata[p].pd_flags) then
          begin
            Message1(parser_e_proc_dir_not_allowed_in_implementation,name);
            exit;
          end;
        if (pd_procvar in pdflags) and
           not(pd_procvar in proc_direcdata[p].pd_flags) then
          begin
            Message1(parser_e_proc_dir_not_allowed_in_procvar,name);
            exit;
          end;

        { Return the new pd_flags }
        if not(pd_body in proc_direcdata[p].pd_flags) then
          exclude(pdflags,pd_body);

        { Add the correct flag }
        pd.procoptions:=pd.procoptions+proc_direcdata[p].pooption;

        { Call the handler }
        if pointer(proc_direcdata[p].handler)<>nil then
          proc_direcdata[p].handler(pd);
      end;



    function proc_get_importname(pd:tprocdef):string;
      begin
        result:='';
        if not(po_external in pd.procoptions) then
          internalerror(200412151);
        { import by number? }
        if pd.import_nr<>0 then
          begin
            { Nothing to do }
          end
        else
        { external name specified }
          if assigned(pd.import_name) then
            begin
              { Win32 imports need to use the normal name since to functions
                can refer to the same DLL function. This is also needed for compatability
                with Delphi and TP7 }
              if not(
                     assigned(pd.import_dll) and
                     (target_info.system in [system_i386_win32,system_i386_wdosx,
                                             system_i386_emx,system_i386_os2])
                    ) then
                begin
                  if not(pd.proccalloption in [pocall_cdecl,pocall_cppdecl]) then
                    result:=pd.import_name^
                  else
                    result:=target_info.Cprefix+pd.import_name^;
                end;
            end
        else
          begin
            { Default names when importing variables }
            case pd.proccalloption of
              pocall_cdecl :
                begin
                  if assigned(pd._class) then
                    result:=target_info.Cprefix+pd._class.objrealname^+'_'+pd.procsym.realname
                  else
                    result:=target_info.Cprefix+pd.procsym.realname;
                end;
              pocall_cppdecl :
                begin
                  result:=target_info.Cprefix+pd.cplusplusmangledname;
                end;
              else
                begin
                  {In MacPas a single "external" has the same effect as "external name 'xxx'" }
                  if (m_mac in aktmodeswitches) then
                    result:=tprocdef(pd).procsym.realname;
                end;
            end;
          end;
      end;


    procedure proc_set_mangledname(pd:tprocdef);
      var
        s : string;
      begin
        { When the mangledname is already set we aren't allowed to change
          it because it can already be used somewhere (PFV) }
        if not(po_has_mangledname in pd.procoptions) then
          begin
            if (po_external in pd.procoptions) then
              begin
                { External Procedures are only allowed to change the mangledname
                  in their first declaration }
                if (pd.forwarddef or (not pd.hasforward)) then
                  begin
                    s:=proc_get_importname(pd);
                    if s<>'' then
                      pd.setmangledname(s);
                  end;
              end
            else
            { Normal procedures }
              begin
                case pd.proccalloption of
                  pocall_compilerproc :
                    begin
                      pd.setmangledname(lower(pd.procsym.name));
                    end;
                end;
              end;
          end;

        { Public/exported alias names }
        if (po_public in pd.procoptions) and
           not(po_has_public_name in pd.procoptions) then
          begin
            case pd.proccalloption of
              pocall_cdecl :
                begin
                  if assigned(pd._class) then
                   pd.aliasnames.insert(target_info.Cprefix+pd._class.objrealname^+'_'+pd.procsym.realname)
                  else
                   pd.aliasnames.insert(target_info.Cprefix+pd.procsym.realname);
                end;
              pocall_cppdecl :
                begin
                  pd.aliasnames.insert(target_info.Cprefix+pd.cplusplusmangledname);
                end;
            end;
            { prevent adding the alias a second time }
            include(pd.procoptions,po_has_public_name);
          end;
      end;


    procedure handle_calling_convention(pd:tabstractprocdef);
      begin
        { set the default calling convention if none provided }
        if not(po_hascallingconvention in pd.procoptions) then
          pd.proccalloption:=aktdefproccall
        else
          begin
            if pd.proccalloption=pocall_none then
              internalerror(200309081);
          end;

        { handle proccall specific settings }
        case pd.proccalloption of
          pocall_cdecl,
          pocall_cppdecl :
            begin
              { check C cdecl para types }
              pd.parast.foreach_static(@check_c_para,nil);
            end;
          pocall_far16 :
            begin
              { Temporary stub, must be rewritten to support OS/2 far16 }
              Message1(parser_w_proc_directive_ignored,'FAR16');
            end;
          pocall_inline :
            begin
              if not(cs_support_inline in aktmoduleswitches) then
               begin
                 Message(parser_e_proc_inline_not_supported);
                 pd.proccalloption:=pocall_default;
               end;
            end;
        end;

        { For varargs directive also cdecl and external must be defined }
        if (po_varargs in pd.procoptions) then
         begin
           { check first for external in the interface, if available there
             then the cdecl must also be there since there is no implementation
             available to contain it }
           if parse_only then
            begin
              { if external is available, then cdecl must also be available,
                procvars don't need external }
              if not((po_external in pd.procoptions) or
                     (pd.deftype=procvardef)) and
                 not(pd.proccalloption in [pocall_cdecl,pocall_cppdecl]) then
                Message(parser_e_varargs_need_cdecl_and_external);
            end
           else
            begin
              { both must be defined now }
              if not((po_external in pd.procoptions) or
                     (pd.deftype=procvardef)) or
                 not(pd.proccalloption in [pocall_cdecl,pocall_cppdecl]) then
                Message(parser_e_varargs_need_cdecl_and_external);
            end;
         end;

        { Make var parameters regable, this must be done after the calling
          convention is set. }
        pd.parast.foreach_static(@set_addr_param_regable,pd);

        { insert hidden high parameters }
        pd.parast.foreach_static(@insert_hidden_para,pd);

        { insert hidden self parameter }
        insert_self_and_vmt_para(pd);

        { insert funcret parameter if required }
        insert_funcret_para(pd);

        { insert parentfp parameter if required }
        insert_parentfp_para(pd);

        { Calculate parameter tlist }
        pd.calcparas;
      end;


    procedure parse_proc_directives(pd:tabstractprocdef;var pdflags:tpdflags);
      {
        Parse the procedure directives. It does not matter if procedure directives
        are written using ;procdir; or ['procdir'] syntax.
      }
      var
        res : boolean;
      begin
        if (m_mac in aktmodeswitches) and (cs_externally_visible in aktlocalswitches) then
          begin
            tprocdef(pd).aliasnames.insert(tprocdef(pd).procsym.realname);
            include(pd.procoptions,po_public);
            include(pd.procoptions,po_has_public_name);
            include(pd.procoptions,po_global);
          end;

        while token in [_ID,_LECKKLAMMER] do
         begin
           if try_to_consume(_LECKKLAMMER) then
            begin
              repeat
                parse_proc_direc(pd,pdflags);
              until not try_to_consume(_COMMA);
              consume(_RECKKLAMMER);
              { we always expect at least '[];' }
              res:=true;
            end
           else
            begin
              res:=parse_proc_direc(pd,pdflags);
            end;
           { A procedure directive normally followed by a semicolon, but in
             a const section or reading a type we should stop when _EQUAL is found,
             because a constant/default value follows }
           if res then
            begin
              if (block_type in [bt_const,bt_type]) and
                 (token=_EQUAL) then
               break;
              { support procedure proc;stdcall export; }
              if not(check_proc_directive((pd.deftype=procvardef))) then
               consume(_SEMICOLON);
            end
           else
            break;
         end;
      end;


    procedure parse_var_proc_directives(sym:tsym);
      var
        pdflags : tpdflags;
        pd      : tabstractprocdef;
      begin
        pdflags:=[pd_procvar];
        pd:=nil;
        case sym.typ of
          fieldvarsym,
          globalvarsym,
          localvarsym,
          paravarsym :
            pd:=tabstractprocdef(tabstractvarsym(sym).vartype.def);
          typedconstsym :
            pd:=tabstractprocdef(ttypedconstsym(sym).typedconsttype.def);
          typesym :
            pd:=tabstractprocdef(ttypesym(sym).restype.def);
          else
            internalerror(2003042617);
        end;
        if pd.deftype<>procvardef then
          internalerror(2003042618);
        { names should never be used anyway }
        parse_proc_directives(pd,pdflags);
      end;


    procedure parse_object_proc_directives(pd:tabstractprocdef);
      var
        pdflags : tpdflags;
      begin
        pdflags:=[pd_object];
        parse_proc_directives(pd,pdflags);
      end;


    function proc_add_definition(var pd:tprocdef):boolean;
      {
        Add definition aprocdef to the overloaded definitions of aprocsym. If a
        forwarddef is found and reused it returns true
      }
      var
        hd    : tprocdef;
        ad,fd : tsym;
        s1,s2 : stringid;
        i     : cardinal;
        forwardfound : boolean;
        po_comp : tprocoptions;
        aprocsym : tprocsym;
      begin
        forwardfound:=false;
        aprocsym:=tprocsym(pd.procsym);

        { check overloaded functions if the same function already exists }
        for i:=1 to aprocsym.procdef_count do
         begin
           hd:=aprocsym.procdef[i];

           { Skip overloaded definitions that are declared in other
             units }
           if hd.procsym<>aprocsym then
             continue;

           { check the parameters, for delphi/tp it is possible to
             leave the parameters away in the implementation (forwarddef=false).
             But for an overload declared function this is not allowed }
           if { check if empty implementation arguments match is allowed }
              (
               not(m_repeat_forward in aktmodeswitches) and
               not(pd.forwarddef) and
               (pd.maxparacount=0) and
               not(po_overload in hd.procoptions)
              ) or
              { check arguments }
              (
               (compare_paras(pd.paras,hd.paras,cp_none,[cpo_comparedefaultvalue])>=te_equal) and
               { for operators equal_paras is not enough !! }
               ((pd.proctypeoption<>potype_operator) or (optoken<>_ASSIGNMENT) or
                equal_defs(hd.rettype.def,pd.rettype.def))
              ) then
             begin
               { Check if we've found the forwarddef, if found then
                 we need to update the forward def with the current
                 implementation settings }
               if hd.forwarddef then
                 begin
                   forwardfound:=true;

                   { Check if the procedure type and return type are correct,
                     also the parameters must match also with the type }
                   if (hd.proctypeoption<>pd.proctypeoption) or
                      (
                       (m_repeat_forward in aktmodeswitches) and
                       (not((pd.maxparacount=0) or
                            (compare_paras(pd.paras,hd.paras,cp_all,[cpo_comparedefaultvalue])>=te_equal)))
                      ) or
                      (
                       ((m_repeat_forward in aktmodeswitches) or
                        not(is_void(pd.rettype.def))) and
                       (not equal_defs(hd.rettype.def,pd.rettype.def))) then
                     begin
                       MessagePos1(pd.fileinfo,parser_e_header_dont_match_forward,
                                   pd.fullprocname(false));
                       aprocsym.write_parameter_lists(pd);
                       break;
                     end;

                   { Check if both are declared forward }
                   if hd.forwarddef and pd.forwarddef then
                    begin
                      MessagePos1(pd.fileinfo,parser_e_function_already_declared_public_forward,
                                  pd.fullprocname(false));
                    end;

                   { internconst or internproc only need to be defined once }
                   if (hd.proccalloption=pocall_internproc) then
                    pd.proccalloption:=hd.proccalloption
                   else
                    if (pd.proccalloption=pocall_internproc) then
                     hd.proccalloption:=pd.proccalloption;

                   { Check calling convention }
                   if (hd.proccalloption<>pd.proccalloption) then
                    begin
                      { In delphi it is possible to specify the calling
                        convention in the interface or implementation if
                        there was no convention specified in the other
                        part }
                      if (m_delphi in aktmodeswitches) then
                       begin
                         if not(po_hascallingconvention in pd.procoptions) then
                          pd.proccalloption:=hd.proccalloption
                         else
                          if not(po_hascallingconvention in hd.procoptions) then
                           hd.proccalloption:=pd.proccalloption
                         else
                          begin
                            MessagePos(pd.fileinfo,parser_e_call_convention_dont_match_forward);
                            aprocsym.write_parameter_lists(pd);
                            { restore interface settings }
                            pd.proccalloption:=hd.proccalloption;
                          end;
                       end
                      else
                       begin
                         MessagePos(pd.fileinfo,parser_e_call_convention_dont_match_forward);
                         aprocsym.write_parameter_lists(pd);
                         { restore interface settings }
                         pd.proccalloption:=hd.proccalloption;
                       end;
                    end;

                   { Check procedure options, Delphi requires that class is
                     repeated in the implementation for class methods }
                   if (m_fpc in aktmodeswitches) then
                     po_comp:=[po_varargs,po_methodpointer,po_interrupt]
                   else
                     po_comp:=[po_classmethod,po_methodpointer];

                   if ((po_comp * hd.procoptions)<>(po_comp * pd.procoptions)) then
                     begin
                       MessagePos1(pd.fileinfo,parser_e_header_dont_match_forward,
                                   pd.fullprocname(false));
                       aprocsym.write_parameter_lists(pd);
                       { This error is non-fatal, we can recover }
                     end;

                   { Forward declaration is external? }
                   if (po_external in hd.procoptions) then
                     MessagePos(pd.fileinfo,parser_e_proc_already_external);

                   { Check parameters }
                   if (m_repeat_forward in aktmodeswitches) or
                      (pd.minparacount>0) then
                    begin
                      { If mangled names are equal then they have the same amount of arguments }
                      { We can check the names of the arguments }
                      { both symtables are in the same order from left to right }
                      ad:=tsym(hd.parast.symindex.first);
                      fd:=tsym(pd.parast.symindex.first);
                      repeat
                        { skip default parameter constsyms }
                        while assigned(ad) and (ad.typ<>paravarsym) do
                         ad:=tsym(ad.indexnext);
                        while assigned(fd) and (fd.typ<>paravarsym) do
                         fd:=tsym(fd.indexnext);
                        { stop when one of the two lists is at the end }
                        if not assigned(ad) or not assigned(fd) then
                         break;
                        { retrieve names, remove reg for register parameters }
                        s1:=ad.name;
                        s2:=fd.name;
                        { compare names }
                        if (s1<>s2) then
                         begin
                           MessagePos3(pd.fileinfo,parser_e_header_different_var_names,
                                       aprocsym.name,s1,s2);
                           break;
                         end;
                        ad:=tsym(ad.indexnext);
                        fd:=tsym(fd.indexnext);
                      until false;
                      if assigned(ad) xor assigned(fd) then
                        internalerror(200204178);
                    end;

                   { Everything is checked, now we can update the forward declaration
                     with the new data from the implementation }
                   hd.forwarddef:=pd.forwarddef;
                   hd.hasforward:=true;
                   hd.procoptions:=hd.procoptions+pd.procoptions;
                   if hd.extnumber=65535 then
                     hd.extnumber:=pd.extnumber;
                   while not pd.aliasnames.empty do
                    hd.aliasnames.insert(pd.aliasnames.getfirst);
                   { update fileinfo so position references the implementation,
                     also update funcretsym if it is already generated }
                   hd.fileinfo:=pd.fileinfo;
                   if assigned(hd.funcretsym) then
                     hd.funcretsym.fileinfo:=pd.fileinfo;
                   { import names }
                   if assigned(pd.import_dll) then
                     begin
                       stringdispose(hd.import_dll);
                       hd.import_dll:=stringdup(pd.import_dll^);
                     end;
                   if assigned(pd.import_name) then
                     begin
                       stringdispose(hd.import_name);
                       hd.import_name:=stringdup(pd.import_name^);
                     end;
                   hd.import_nr:=pd.import_nr;
                   { for compilerproc defines we need to rename and update the
                     symbolname to lowercase }
                   if (pd.proccalloption=pocall_compilerproc) then
                    begin
                      { rename to lowercase so users can't access it }
                      aprocsym.owner.rename(aprocsym.name,lower(aprocsym.name));
                      { also update the realname that is stored in the ppu }
                      stringdispose(aprocsym._realname);
                      aprocsym._realname:=stringdup('$'+aprocsym.name);
                      { the mangeled name is already changed by the pd_compilerproc }
                      { handler. It must be done immediately because if we have a   }
                      { call to a compilerproc before it's implementation is        }
                      { encountered, it must already use the new mangled name (JM)  }
                    end;

                   { the procdef will be released by the symtable, we release
                     at least the parast }
                   pd.releasemem;
                   pd:=hd;
                 end
               else
                begin
                  { abstract methods aren't forward defined, but this }
                  { needs another error message                   }
                  if (po_abstractmethod in hd.procoptions) then
                    MessagePos(pd.fileinfo,parser_e_abstract_no_definition)
                  else
                    begin
                      MessagePos(pd.fileinfo,parser_e_overloaded_have_same_parameters);
                      aprocsym.write_parameter_lists(pd);
                    end;
                 end;

               { we found one proc with the same arguments, there are no others
                 so we can stop }
               break;
             end;

           { check for allowing overload directive }
           if not(m_fpc in aktmodeswitches) then
            begin
              { overload directive turns on overloading }
              if ((po_overload in pd.procoptions) or
                  (po_overload in hd.procoptions)) then
               begin
                 { check if all procs have overloading, but not if the proc is a method or
                   already declared forward, then the check is already done }
                 if not(hd.hasforward or
                        assigned(pd._class) or
                        (pd.forwarddef<>hd.forwarddef) or
                        ((po_overload in pd.procoptions) and
                         (po_overload in hd.procoptions))) then
                  begin
                    MessagePos1(pd.fileinfo,parser_e_no_overload_for_all_procs,aprocsym.realname);
                    break;
                  end;
               end
              else
               begin
                 if not(hd.forwarddef) then
                  begin
                    MessagePos(pd.fileinfo,parser_e_procedure_overloading_is_off);
                    break;
                  end;
               end;
            end; { equal arguments }
         end;

        { if we didn't reuse a forwarddef then we add the procdef to the overloaded
          list }
        if not forwardfound then
          aprocsym.addprocdef(pd);

        proc_add_definition:=forwardfound;
      end;

end.
{
  $Log$
  Revision 1.223  2005-01-04 17:40:33  karoly
    + sysv style syscalls added for MorphOS

  Revision 1.222  2004/12/27 17:32:06  peter
    * don't parse public,private,protected as procdirectives, leave
      procdirective parsing before any other check is done

  Revision 1.221  2004/12/26 20:12:23  peter
    * don't allow class methods in interfaces

  Revision 1.220  2004/12/15 19:30:32  peter
    * syscall with sysv abi for morphos

  Revision 1.219  2004/12/15 16:00:16  peter
    * external is again allowed in implementation

  Revision 1.218  2004/12/07 16:11:52  peter
    * set vo_explicit_paraloc flag

  Revision 1.217  2004/12/05 12:28:11  peter
    * procvar handling for tp procvar mode fixed
    * proc to procvar moved from addrnode to typeconvnode
    * inlininginfo is now allocated only for inline routines that
      can be inlined, introduced a new flag po_has_inlining_info

  Revision 1.216  2004/12/05 00:32:56  olle
    + bugfix for $Z+ for mode macpas

  Revision 1.215  2004/11/29 21:50:08  peter
    * public is allowd in interface

  Revision 1.214  2004/11/29 17:48:34  peter
    * when importing by index don't change mangledname

  Revision 1.213  2004/11/22 12:22:25  jonas
    * fixed importing of cdecl routines for OS'es which have a cprefix

  Revision 1.212  2004/11/21 17:54:59  peter
    * ttempcreatenode.create_reg merged into .create with parameter
      whether a register is allowed
    * funcret_paraloc renamed to funcretloc

  Revision 1.211  2004/11/21 16:33:19  peter
    * fixed message methods
    * fixed typo with win32 dll import from implementation
    * released external check

  Revision 1.210  2004/11/19 08:17:01  michael
  * Split po_public into po_public and po_global (Peter)

  Revision 1.209  2004/11/17 22:41:41  peter
    * make some checks EXTDEBUG only for now so linux cycles again

  Revision 1.208  2004/11/17 22:21:35  peter
  mangledname setting moved to place after the complete proc declaration is read
  import generation moved to place where body is also parsed (still gives problems with win32)

  Revision 1.207  2004/11/16 22:09:57  peter
  * _mangledname for symbols moved only to symbols that really need it
  * overload number removed, add function result type to the mangledname fo
    procdefs

  Revision 1.206  2004/11/16 20:32:40  peter
  * fixes for win32 mangledname

  Revision 1.205  2004/11/15 23:35:31  peter
    * tparaitem removed, use tparavarsym instead
    * parameter order is now calculated from paranr value in tparavarsym

  Revision 1.204  2004/11/14 16:26:29  florian
    * fixed morphos syscall

  Revision 1.203  2004/11/11 19:31:33  peter
    * fixed compile of powerpc,sparc,arm

  Revision 1.202  2004/11/09 22:32:59  peter
    * small m68k updates to bring it up2date
    * give better error for external local variable

  Revision 1.201  2004/11/09 17:26:47  peter
    * fixed wrong typecasts

  Revision 1.200  2004/11/08 22:09:59  peter
    * tvarsym splitted

  Revision 1.199  2004/11/05 21:16:55  peter
    * rename duplicate symbols and insert with unique name in the
      symtable

  Revision 1.198  2004/10/31 18:54:24  peter
    * $fpctarget expands to <cpu>-<os>
    * allow * in middle of the path to support ../*/units/$fpctarget

  Revision 1.197  2004/10/24 20:01:08  peter
    * remove saveregister calling convention

  Revision 1.196  2004/10/24 13:48:50  peter
    * don't give warning for property as unknwon proc directive

  Revision 1.195  2004/10/24 11:44:28  peter
    * small regvar fixes
    * loadref parameter removed from concatcopy,incrrefcount,etc

  Revision 1.194  2004/10/15 09:14:17  mazen
  - remove $IFDEF DELPHI and related code
  - remove $IFDEF FPCPROCVAR and related code

  Revision 1.193  2004/10/11 15:45:35  peter
    * mark non-regable after calling convention is set

  Revision 1.192  2004/10/10 21:08:55  peter
    * parameter regvar fixes

  Revision 1.191  2004/10/08 17:09:43  peter
    * tvarsym.varregable added, split vo_regable from varoptions

  Revision 1.190  2004/08/29 11:28:41  peter
  fixed crash with error in default value
  allow assembler directive in interface

  Revision 1.189  2004/08/25 15:57:19  peter
    * fix for tw3261

  Revision 1.188  2004/08/22 20:11:38  florian
    * morphos now takes any pointer var. as libbase
    * alignment for sparc fixed
    * int -> double conversion on sparc fixed

  Revision 1.187  2004/08/22 11:24:27  peter
    * don't insert result variables for constructor/destructors

  Revision 1.186  2004/08/13 17:53:37  jonas
    * only set the mangled name immediately for external procedures in macpas
      mode if the procedure isn't cdecl (so that the c-prefix is taken into
      account, necessary for Mac OS X)

  Revision 1.185  2004/08/08 12:35:09  florian
    * proc. var declarations in a class doesn't eat a public anymore

  Revision 1.184  2004/07/17 13:51:57  florian
    * function result location for syscalls on MOS hopefully correctly set now

  Revision 1.183  2004/07/14 23:19:21  olle
    + added external facilities for macpas

  Revision 1.182  2004/06/20 08:55:30  florian
    * logs truncated

  Revision 1.181  2004/06/16 20:07:09  florian
    * dwarf branch merged

  Revision 1.180  2004/05/23 20:54:39  peter
    * fixed 3114

  Revision 1.179  2004/05/23 19:06:26  peter
    * expect : after function when it is a forwarddef

  Revision 1.178  2004/05/12 13:21:09  karoly
    * few small changes to add syscall support to M68k/Amiga target

  Revision 1.177  2004/05/11 22:52:48  olle
    * Moved import_implicit_external to symsym

}
