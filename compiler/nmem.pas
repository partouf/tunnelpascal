{
    $Id$
    Copyright (c) 2000-2002 by Florian Klaempfl

    Type checking and register allocation for memory related nodes

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
unit nmem;

{$i fpcdefs.inc}

interface

    uses
       node,
       symdef,symsym,symtable,symtype,
       cpubase;

    type
       tloadvmtaddrnode = class(tunarynode)
          constructor create(l : tnode);virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       end;
       tloadvmtaddrnodeclass = class of tloadvmtaddrnode;

       tloadparentfpnode = class(tunarynode)
          parentpd : tprocdef;
          parentpdderef : tderef;
          constructor create(pd:tprocdef);virtual;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderefimpl;override;
          procedure derefimpl;override;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          function getcopy : tnode;override;
       end;
       tloadparentfpnodeclass = class of tloadparentfpnode;

       taddrnode = class(tunarynode)
          getprocvardef : tprocvardef;
          getprocvardefderef : tderef;
          constructor create(l : tnode);virtual;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure mark_write;override;
          procedure buildderefimpl;override;
          procedure derefimpl;override;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       end;
       taddrnodeclass = class of taddrnode;

       tderefnode = class(tunarynode)
          constructor create(l : tnode);virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          procedure mark_write;override;
       end;
       tderefnodeclass = class of tderefnode;

       tsubscriptnode = class(tunarynode)
          vs : tfieldvarsym;
          vsderef : tderef;
          constructor create(varsym : tsym;l : tnode);virtual;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderefimpl;override;
          procedure derefimpl;override;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
          function docompare(p: tnode): boolean; override;
          function det_resulttype:tnode;override;
          procedure mark_write;override;
       end;
       tsubscriptnodeclass = class of tsubscriptnode;

       tvecnode = class(tbinarynode)
          constructor create(l,r : tnode);virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          procedure mark_write;override;
       end;
       tvecnodeclass = class of tvecnode;

       twithnode = class(tunarynode)
          withsymtable  : twithsymtable;
          tablecount    : longint;
          withrefnode   : tnode;
          constructor create(l:tnode;symtable:twithsymtable;count:longint;r:tnode);
          destructor destroy;override;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
          function docompare(p: tnode): boolean; override;
          function det_resulttype:tnode;override;
       end;
       twithnodeclass = class of twithnode;

    var
       cloadvmtaddrnode : tloadvmtaddrnodeclass;
       cloadparentfpnode : tloadparentfpnodeclass;
       caddrnode : taddrnodeclass;
       cderefnode : tderefnodeclass;
       csubscriptnode : tsubscriptnodeclass;
       cvecnode : tvecnodeclass;
       cwithnode : twithnodeclass;

implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symbase,defutil,defcmp,
      nbas,nutils,
      htypechk,pass_1,ncal,nld,ncon,ncnv,cgbase,procinfo
      ;

{*****************************************************************************
                            TLOADVMTADDRNODE
*****************************************************************************}

    constructor tloadvmtaddrnode.create(l : tnode);
      begin
         inherited create(loadvmtaddrn,l);
      end;


    function tloadvmtaddrnode.det_resulttype:tnode;
      begin
        result:=nil;
        resulttypepass(left);
        if codegenerror then
         exit;

        case left.resulttype.def.deftype of
          classrefdef :
            resulttype:=left.resulttype;
          objectdef :
            resulttype.setdef(tclassrefdef.create(left.resulttype));
          else
            Message(parser_e_pointer_to_class_expected);
        end;
      end;


    function tloadvmtaddrnode.pass_1 : tnode;
      begin
         result:=nil;
         expectloc:=LOC_REGISTER;
         if left.nodetype<>typen then
           begin
             firstpass(left);
             registersint:=left.registersint;
           end;
         if registersint<1 then
           registersint:=1;
      end;


{*****************************************************************************
                        TLOADPARENTFPNODE
*****************************************************************************}

    constructor tloadparentfpnode.create(pd:tprocdef);
      begin
        inherited create(loadparentfpn,nil);
        if not assigned(pd) then
          internalerror(200309288);
        if (pd.parast.symtablelevel>current_procinfo.procdef.parast.symtablelevel) then
          internalerror(200309284);
        parentpd:=pd;
      end;


    constructor tloadparentfpnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        ppufile.getderef(parentpdderef);
      end;


    procedure tloadparentfpnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putderef(parentpdderef);
      end;


    procedure tloadparentfpnode.buildderefimpl;
      begin
        inherited buildderefimpl;
        parentpdderef.build(parentpd);
      end;


    procedure tloadparentfpnode.derefimpl;
      begin
        inherited derefimpl;
        parentpd:=tprocdef(parentpdderef.resolve);
      end;


    function tloadparentfpnode.getcopy : tnode;
      var
         p : tloadparentfpnode;
      begin
         p:=tloadparentfpnode(inherited getcopy);
         p.parentpd:=parentpd;
         getcopy:=p;
      end;


    function tloadparentfpnode.det_resulttype:tnode;
      begin
        result:=nil;
        resulttype:=voidpointertype;
      end;


    function tloadparentfpnode.pass_1 : tnode;
      begin
        result:=nil;
        expectloc:=LOC_REGISTER;
        registersint:=1;
      end;


{*****************************************************************************
                             TADDRNODE
*****************************************************************************}

    constructor taddrnode.create(l : tnode);

      begin
         inherited create(addrn,l);
         getprocvardef:=nil;
      end;


    constructor taddrnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        ppufile.getderef(getprocvardefderef);
      end;


    procedure taddrnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putderef(getprocvardefderef);
      end;

    procedure Taddrnode.mark_write;

    begin
      {@procvar:=nil is legal in Delphi mode.}
      left.mark_write;
    end;

    procedure taddrnode.buildderefimpl;
      begin
        inherited buildderefimpl;
        getprocvardefderef.build(getprocvardef);
      end;


    procedure taddrnode.derefimpl;
      begin
        inherited derefimpl;
        getprocvardef:=tprocvardef(getprocvardefderef.resolve);
      end;


    function taddrnode.getcopy : tnode;

      var
         p : taddrnode;

      begin
         p:=taddrnode(inherited getcopy);
         p.getprocvardef:=getprocvardef;
         getcopy:=p;
      end;


    function taddrnode.det_resulttype:tnode;
      var
         hp  : tnode;
         hp2 : TParaItem;
         hp3 : tabstractprocdef;
      begin
        result:=nil;
        resulttypepass(left);
        if codegenerror then
         exit;

        make_not_regable(left);

        { don't allow constants }
        if is_constnode(left) then
         begin
           aktfilepos:=left.fileinfo;
           CGMessage(type_e_no_addr_of_constant);
           exit;
         end;

        { tp @procvar support (type of @procvar is a void pointer)
          Note: we need to leave the addrn in the tree,
          else we can't see the difference between @procvar and procvar.
          we set the procvarload flag so a secondpass does nothing for
          this node (PFV) }
        if (m_tp_procvar in aktmodeswitches) then
         begin
           case left.nodetype of
             calln :
               begin
                 { a load of a procvar can't have parameters }
                 if assigned(tcallnode(left).left) then
                   CGMessage(parser_e_illegal_expression);
                 { is it a procvar? }
                 hp:=tcallnode(left).right;
                 if assigned(hp) then
                   begin
                     { remove calln node }
                     tcallnode(left).right:=nil;
                     left.free;
                     left:=hp;
                     include(flags,nf_procvarload);
                   end;
               end;
             loadn,
             subscriptn,
             typeconvn,
             vecn,
             derefn :
               begin
                 if left.resulttype.def.deftype=procvardef then
                   include(flags,nf_procvarload);
               end;
           end;
           if nf_procvarload in flags then
            begin
              resulttype:=voidpointertype;
              exit;
            end;
         end;

        { proc 2 procvar ? }
        if left.nodetype=calln then
         { if it were a valid construct, the addr node would already have }
         { been removed in the parser. This happens for (in FPC mode)     }
         { procvar1 := @procvar2(parameters);                             }
         CGMessage(parser_e_illegal_expression)
        else
         if (left.nodetype=loadn) and (tloadnode(left).symtableentry.typ=procsym) then
          begin
            { the address is already available when loading a procedure of object }
            if assigned(tloadnode(left).left) then
             include(flags,nf_procvarload);

            { result is a procedure variable }
            { No, to be TP compatible, you must return a voidpointer to
              the procedure that is stored in the procvar.}
            if not(m_tp_procvar in aktmodeswitches) then
              begin
                 if assigned(getprocvardef) and
                    (tprocsym(tloadnode(left).symtableentry).procdef_count>1) then
                  begin
                    hp3:=tprocsym(tloadnode(left).symtableentry).search_procdef_byprocvardef(getprocvardef);
                    if not assigned(hp3)  then
                     begin
                       IncompatibleTypes(tprocsym(tloadnode(left).symtableentry).first_procdef,getprocvardef);
                       exit;
                     end;
                  end
                 else
                  hp3:=tabstractprocdef(tprocsym(tloadnode(left).symtableentry).first_procdef);


                 { create procvardef }
                 resulttype.setdef(tprocvardef.create(hp3.parast.symtablelevel));
                 tprocvardef(resulttype.def).proctypeoption:=hp3.proctypeoption;
                 tprocvardef(resulttype.def).proccalloption:=hp3.proccalloption;
                 tprocvardef(resulttype.def).procoptions:=hp3.procoptions;
                 tprocvardef(resulttype.def).rettype:=hp3.rettype;

                 { method ? then set the methodpointer flag }
                 if (hp3.owner.symtabletype=objectsymtable) then
                   include(tprocvardef(resulttype.def).procoptions,po_methodpointer);

                 { only need the address of the method? this is needed
                   for @tobject.create }
                 if not assigned(tloadnode(left).left) then
                   include(tprocvardef(resulttype.def).procoptions,po_addressonly);

                 { Add parameters in left to right order }
                 hp2:=TParaItem(hp3.Para.first);
                 while assigned(hp2) do
                   begin
                      tprocvardef(resulttype.def).concatpara(nil,hp2.paratype,hp2.parasym,
                          hp2.defaultvalue,hp2.is_hidden);
                      hp2:=TParaItem(hp2.next);
                   end;
              end
            else
              resulttype:=voidpointertype;
          end
        else
          begin
            { what are we getting the address from an absolute sym? }
            hp:=left;
            while assigned(hp) and (hp.nodetype in [vecn,derefn,subscriptn]) do
             hp:=tunarynode(hp).left;
{$ifdef i386}
            if assigned(hp) and
               (hp.nodetype=loadn) and
               ((tloadnode(hp).symtableentry.typ=absolutevarsym) and
                tabsolutevarsym(tloadnode(hp).symtableentry).absseg) then
             begin
               if not(nf_typedaddr in flags) then
                 resulttype:=voidfarpointertype
               else
                 resulttype.setdef(tpointerdef.createfar(left.resulttype));
             end
            else
{$endif i386}
             begin
               if not(nf_typedaddr in flags) then
                 resulttype:=voidpointertype
               else
                 resulttype.setdef(tpointerdef.create(left.resulttype));
             end;
          end;

         { this is like the function addr }
         inc(parsing_para_level);
         set_varstate(left,vs_used,false);
         dec(parsing_para_level);

      end;


    function taddrnode.pass_1 : tnode;
      begin
         result:=nil;
         firstpass(left);
         if codegenerror then
          exit;

         if nf_procvarload in flags then
          begin
            registersint:=left.registersint;
            registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
            registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
            if registersint<1 then
             registersint:=1;
            expectloc:=left.expectloc;
            exit;
          end;

         { we should allow loc_mem for @string }
         if not(left.expectloc in [LOC_CREFERENCE,LOC_REFERENCE]) then
           begin
             aktfilepos:=left.fileinfo;
             CGMessage(parser_e_illegal_expression);
           end;

         registersint:=left.registersint;
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
         if registersint<1 then
           registersint:=1;
         { is this right for object of methods ?? }
         expectloc:=LOC_REGISTER;
      end;


{*****************************************************************************
                             TDEREFNODE
*****************************************************************************}

    constructor tderefnode.create(l : tnode);

      begin
         inherited create(derefn,l);

      end;

    function tderefnode.det_resulttype:tnode;
      begin
         result:=nil;
         resulttypepass(left);
         set_varstate(left,vs_used,true);
         if codegenerror then
          exit;

         { tp procvar support }
         maybe_call_procvar(left,true);

         if left.resulttype.def.deftype=pointerdef then
          resulttype:=tpointerdef(left.resulttype.def).pointertype
         else
          CGMessage(parser_e_invalid_qualifier);
      end;

    procedure Tderefnode.mark_write;

    begin
      include(flags,nf_write);
    end;

    function tderefnode.pass_1 : tnode;
      begin
         result:=nil;
         firstpass(left);
         if codegenerror then
          exit;

         registersint:=max(left.registersint,1);
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}

         expectloc:=LOC_REFERENCE;
      end;


{*****************************************************************************
                            TSUBSCRIPTNODE
*****************************************************************************}

    constructor tsubscriptnode.create(varsym : tsym;l : tnode);

      begin
         inherited create(subscriptn,l);
         { vs should be changed to tsym! }
         vs:=tfieldvarsym(varsym);
      end;

    constructor tsubscriptnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        ppufile.getderef(vsderef);
      end;


    procedure tsubscriptnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putderef(vsderef);
      end;


    procedure tsubscriptnode.buildderefimpl;
      begin
        inherited buildderefimpl;
        vsderef.build(vs);
      end;


    procedure tsubscriptnode.derefimpl;
      begin
        inherited derefimpl;
        vs:=tfieldvarsym(vsderef.resolve);
      end;


    function tsubscriptnode.getcopy : tnode;

      var
         p : tsubscriptnode;

      begin
         p:=tsubscriptnode(inherited getcopy);
         p.vs:=vs;
         getcopy:=p;
      end;


    function tsubscriptnode.det_resulttype:tnode;
      begin
        result:=nil;
        resulttypepass(left);
        { tp procvar support }
        maybe_call_procvar(left,true);
        resulttype:=vs.vartype;
      end;

    procedure Tsubscriptnode.mark_write;

    begin
      include(flags,nf_write);
    end;

    function tsubscriptnode.pass_1 : tnode;
      begin
         result:=nil;
         firstpass(left);
         if codegenerror then
          exit;

         registersint:=left.registersint;
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
         { classes must be dereferenced implicit }
         if is_class_or_interface(left.resulttype.def) then
           begin
              if registersint=0 then
                registersint:=1;
              expectloc:=LOC_REFERENCE;
           end
         else
           begin
              if (left.expectloc<>LOC_CREFERENCE) and
                 (left.expectloc<>LOC_REFERENCE) then
                CGMessage(parser_e_illegal_expression);
              expectloc:=left.expectloc;
           end;
      end;

    function tsubscriptnode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (vs = tsubscriptnode(p).vs);
      end;


{*****************************************************************************
                               TVECNODE
*****************************************************************************}

    constructor tvecnode.create(l,r : tnode);

      begin
         inherited create(vecn,l,r);
      end;


    function tvecnode.det_resulttype:tnode;
      var
         htype : ttype;
         valid : boolean;
      begin
         result:=nil;
         resulttypepass(left);
         resulttypepass(right);
         { In p[1] p is always valid, it is not possible to
           declared a shortstring or normal array that has
           undefined number of elements. Dynamic array and
           ansi/widestring needs to be valid }
         valid:=is_dynamic_array(left.resulttype.def) or
                is_ansistring(left.resulttype.def) or
                is_widestring(left.resulttype.def);
         set_varstate(left,vs_used,valid);
         set_varstate(right,vs_used,true);
         if codegenerror then
          exit;

         { maybe type conversion for the index value, but
           do not convert enums,booleans,char }
         if (right.resulttype.def.deftype<>enumdef) and
            not(is_char(right.resulttype.def)) and
            not(is_boolean(right.resulttype.def)) then
           begin
             inserttypeconv(right,s32inttype);
           end;

         case left.resulttype.def.deftype of
           arraydef :
             begin
               { check type of the index value }
               if (compare_defs(right.resulttype.def,tarraydef(left.resulttype.def).rangetype.def,right.nodetype)=te_incompatible) then
                 IncompatibleTypes(right.resulttype.def,tarraydef(left.resulttype.def).rangetype.def);
               resulttype:=tarraydef(left.resulttype.def).elementtype;
             end;
           pointerdef :
             begin
               { are we accessing a pointer[], then convert the pointer to
                 an array first, in FPC this is allowed for all pointers in
                 delphi/tp7 it's only allowed for pchars }
               if (m_fpc in aktmodeswitches) or
                  is_pchar(left.resulttype.def) or
                  is_pwidechar(left.resulttype.def) then
                begin
                  { convert pointer to array }
                  htype.setdef(tarraydef.create_from_pointer(tpointerdef(left.resulttype.def).pointertype));
                  inserttypeconv(left,htype);

                  resulttype:=tarraydef(htype.def).elementtype;
                end
               else
                CGMessage(type_e_array_required);
             end;
           stringdef :
             begin
                { indexed access to 0 element is only allowed for shortstrings }
                if (right.nodetype=ordconstn) and
                   (tordconstnode(right).value=0) and
                   not(is_shortstring(left.resulttype.def)) then
                  CGMessage(cg_e_can_access_element_zero);
                case tstringdef(left.resulttype.def).string_typ of
                   st_widestring :
                     resulttype:=cwidechartype;
                 {$ifdef ansistring_bits}
                   st_ansistring16,st_ansistring32,st_ansistring64 :
                 {$else}
                   st_ansistring :
                 {$endif}
                     resulttype:=cchartype;
                   st_longstring :
                     resulttype:=cchartype;
                   st_shortstring :
                     resulttype:=cchartype;
                end;
             end;
           variantdef :
             resulttype:=cvarianttype;
           else
             CGMessage(type_e_array_required);
        end;
      end;

    procedure Tvecnode.mark_write;

    begin
      include(flags,nf_write);
    end;

    function tvecnode.pass_1 : tnode;
{$ifdef consteval}
      var
         tcsym : ttypedconstsym;
{$endif}
      begin
         result:=nil;
         firstpass(left);
         firstpass(right);
         if codegenerror then
           exit;

         if (nf_callunique in flags) and
            (is_ansistring(left.resulttype.def) or
             is_widestring(left.resulttype.def)) then
           begin
             left := ctypeconvnode.create_internal(ccallnode.createintern('fpc_'+tstringdef(left.resulttype.def).stringtypname+'_unique',
               ccallparanode.create(
                 ctypeconvnode.create_internal(left,voidpointertype),nil)),
               left.resulttype);
             firstpass(left);
             { double resulttype passes somwhere else may cause this to be }
             { reset though :/                                             }
             exclude(flags,nf_callunique);
           end;

         { the register calculation is easy if a const index is used }
         if right.nodetype=ordconstn then
           begin
{$ifdef consteval}
              { constant evaluation }
              if (left.nodetype=loadn) and
                 (left.symtableentry.typ=typedconstsym) then
               begin
                 tcsym:=ttypedconstsym(left.symtableentry);
                 if tcsym.defintion^.typ=stringdef then
                  begin

                  end;
               end;
{$endif}
              registersint:=left.registersint;

              { for ansi/wide strings, we need at least one register }
              if is_ansistring(left.resulttype.def) or
                is_widestring(left.resulttype.def) or
              { ... as well as for dynamic arrays }
                is_dynamic_array(left.resulttype.def) then
                registersint:=max(registersint,1);
           end
         else
           begin
              { this rules are suboptimal, but they should give }
              { good results                                }
              registersint:=max(left.registersint,right.registersint);

              { for ansi/wide strings, we need at least one register }
              if is_ansistring(left.resulttype.def) or
                is_widestring(left.resulttype.def) or
              { ... as well as for dynamic arrays }
                is_dynamic_array(left.resulttype.def) then
                registersint:=max(registersint,1);

              { need we an extra register when doing the restore ? }
              if (left.registersint<=right.registersint) and
              { only if the node needs less than 3 registers }
              { two for the right node and one for the       }
              { left address                             }
                (registersint<3) then
                inc(registersint);

              { need we an extra register for the index ? }
              if (right.expectloc<>LOC_REGISTER)
              { only if the right node doesn't need a register }
                and (right.registersint<1) then
                inc(registersint);

              { not correct, but what works better ?
              if left.registersint>0 then
                registersint:=max(registersint,2)
              else
                 min. one register
                registersint:=max(registersint,1);
              }
           end;
         registersfpu:=max(left.registersfpu,right.registersfpu);
{$ifdef SUPPORT_MMX}
         registersmmx:=max(left.registersmmx,right.registersmmx);
{$endif SUPPORT_MMX}
         if left.expectloc=LOC_CREFERENCE then
           expectloc:=LOC_CREFERENCE
         else
           expectloc:=LOC_REFERENCE;
      end;


{*****************************************************************************
                               TWITHNODE
*****************************************************************************}

    constructor twithnode.create(l:tnode;symtable:twithsymtable;count:longint;r:tnode);
      begin
         inherited create(withn,l);
         withrefnode:=r;
         withsymtable:=symtable;
         tablecount:=count;
         set_file_line(l);
      end;


    destructor twithnode.destroy;
      var
        hsymt,
        symt : tsymtable;
        i    : longint;
      begin
        symt:=withsymtable;
        for i:=1 to tablecount do
         begin
           if assigned(symt) then
            begin
              hsymt:=symt.next;
              symt.free;
              symt:=hsymt;
            end;
         end;
        inherited destroy;
      end;


    constructor twithnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        internalerror(200208192);
      end;


    procedure twithnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        internalerror(200208193);
      end;


    function twithnode.getcopy : tnode;

      var
         p : twithnode;

      begin
         p:=twithnode(inherited getcopy);
         p.withsymtable:=withsymtable;
         p.tablecount:=tablecount;
         if assigned(p.withrefnode) then
           p.withrefnode:=withrefnode.getcopy
         else
           p.withrefnode:=nil;
         result:=p;
      end;


    function twithnode.det_resulttype:tnode;
      begin
        result:=nil;
        resulttype:=voidtype;

        resulttypepass(withrefnode);
        //unset_varstate(withrefnode);
        set_varstate(withrefnode,vs_used,true);
        if codegenerror then
         exit;

        if (withrefnode.nodetype=vecn) and
           (nf_memseg in withrefnode.flags) then
          CGMessage(parser_e_no_with_for_variable_in_other_segments);

        if assigned(left) then
          resulttypepass(left);
      end;


    function twithnode.pass_1 : tnode;
      begin
        result:=nil;
        expectloc:=LOC_VOID;

        if assigned(left) then
         begin
           firstpass(left);
           registersint:=left.registersint;
           registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
           registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
         end;
        if assigned(withrefnode) then
          begin
            firstpass(withrefnode);
            if withrefnode.registersint > registersint then
              registersint:=withrefnode.registersint;
            if withrefnode.registersfpu > registersfpu then
              registersint:=withrefnode.registersfpu;
{$ifdef SUPPORT_MMX}
            if withrefnode.registersmmx > registersmmx then
              registersmmx:=withrefnode.registersmmx;
{$endif SUPPORT_MMX}
          end;
      end;


    function twithnode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (withsymtable = twithnode(p).withsymtable) and
          (tablecount = twithnode(p).tablecount) and
          (withrefnode.isequal(twithnode(p).withrefnode));
      end;

begin
  cloadvmtaddrnode := tloadvmtaddrnode;
  caddrnode := taddrnode;
  cderefnode := tderefnode;
  csubscriptnode := tsubscriptnode;
  cvecnode := tvecnode;
  cwithnode := twithnode;
end.
{
  $Log$
  Revision 1.88  2004-11-08 22:09:59  peter
    * tvarsym splitted

  Revision 1.87  2004/11/02 12:55:16  peter
    * nf_internal flag for internal inserted typeconvs. This will
      supress the generation of warning/hints

  Revision 1.86  2004/09/26 17:45:30  peter
    * simple regvar support, not yet finished

  Revision 1.85  2004/06/20 08:55:29  florian
    * logs truncated

  Revision 1.84  2004/06/16 20:07:09  florian
    * dwarf branch merged

  Revision 1.83  2004/04/29 19:56:37  daniel
    * Prepare compiler infrastructure for multiple ansistring types

  Revision 1.82.2.1  2004/04/28 19:55:51  peter
    * new warning for ordinal-pointer when size is different
    * fixed some cg_e_ messages to the correct section type_e_ or parser_e_

  Revision 1.82  2004/03/29 14:42:52  peter
    * variant array support

  Revision 1.81  2004/03/18 16:19:03  peter
    * fixed operator overload allowing for pointer-string
    * replaced some type_e_mismatch with more informational messages

}
