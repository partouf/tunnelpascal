{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Generate i386 assembler for in memory related nodes

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
unit n386mem;

{$i defines.inc}

interface

    uses
      node,nmem,ncgmem;

    type
       ti386addrnode = class(tcgaddrnode)
          procedure pass_2;override;
       end;

       ti386derefnode = class(tcgderefnode)
          procedure pass_2;override;
       end;

       ti386vecnode = class(tvecnode)
          procedure pass_2;override;
       end;

implementation

    uses
{$ifdef delphi}
      sysutils,
{$endif}
      globtype,systems,
      cutils,verbose,globals,
      symconst,symtype,symdef,symsym,symtable,aasm,types,
      cginfo,cgbase,pass_2,
      pass_1,nld,ncon,nadd,
      cpubase,
      cgobj,cga,tgobj,rgobj,ncgutil;

{*****************************************************************************
                             TI386ADDRNODE
*****************************************************************************}

    procedure ti386addrnode.pass_2;

      begin
        inherited pass_2;
        { for use of other segments }
        if left.location.reference.segment<>R_NO then
          location.segment:=left.location.reference.segment;
      end;


{*****************************************************************************
                           TI386DEREFNODE
*****************************************************************************}

    procedure ti386derefnode.pass_2;
      var
        oldglobalswitches : tglobalswitches;
      begin
         oldglobalswitches:=aktglobalswitches;
         exclude(aktglobalswitches,cs_checkpointer);
         inherited pass_2;
         aktglobalswitches:=oldglobalswitches;
         if tpointerdef(left.resulttype.def).is_far then
          location.reference.segment:=R_FS;
         if not tpointerdef(left.resulttype.def).is_far and
            (cs_gdb_heaptrc in aktglobalswitches) and
            (cs_checkpointer in aktglobalswitches) then
          begin
            cg.a_param_reg(exprasmlist, OS_ADDR,location.reference.base,1);
            cg.a_call_name(exprasmlist,'FPC_CHECKPOINTER');
          end;
      end;


{*****************************************************************************
                             TI386VECNODE
*****************************************************************************}

    procedure ti386vecnode.pass_2;

          function get_mul_size:longint;
          begin
            if nf_memindex in flags then
             get_mul_size:=1
            else
             begin
               if (left.resulttype.def.deftype=arraydef) then
                get_mul_size:=tarraydef(left.resulttype.def).elesize
               else
                get_mul_size:=resulttype.def.size;
             end
          end;

          procedure calc_emit_mul;
          var
             l1,l2 : longint;
          begin
            l1:=get_mul_size;
            case l1 of
             1,2,4,8 : location.reference.scalefactor:=l1;
            else
              begin
                 if ispowerof2(l1,l2) then
                   emit_const_reg(A_SHL,S_L,l2,right.location.register)
                 else
                   emit_const_reg(A_IMUL,S_L,l1,right.location.register);
              end;
            end;
          end;

      var
         extraoffset : longint;
         { rl stores the resulttype.def of the left node, this is necessary }
         { to detect if it is an ansistring                          }
         { because in constant nodes which constant index              }
         { the left tree is removed                                  }
         t   : tnode;
         href : treference;
         srsym : tsym;
         pushed : tpushedsaved;
         hightree : tnode;
         isjump  : boolean;
         otl,ofl : tasmlabel;
         newsize : tcgsize;
         pushedregs : tmaybesave;
      begin
         newsize:=def_cgsize(resulttype.def);
         location_reset(location,LOC_REFERENCE,newsize);

         secondpass(left);
         { we load the array reference to location }

         { an ansistring needs to be dereferenced }
         if is_ansistring(left.resulttype.def) or
            is_widestring(left.resulttype.def) then
           begin
              if nf_callunique in flags then
                begin
                   if left.location.loc<>LOC_REFERENCE then
                     begin
                        CGMessage(cg_e_illegal_expression);
                        exit;
                     end;
                   rg.saveusedregisters(exprasmlist,pushed,all_registers);
                   cg.a_paramaddr_ref(exprasmlist,left.location.reference,1);
                   rg.saveregvars(exprasmlist,all_registers);
                   cg.a_call_name(exprasmlist,'FPC_'+Upper(tstringdef(left.resulttype.def).stringtypname)+'_UNIQUE');
                   cg.g_maybe_loadself(exprasmlist);
                   rg.restoreusedregisters(exprasmlist,pushed);
                end;

              case left.location.loc of
                LOC_REGISTER,
                LOC_CREGISTER :
                  location.reference.base:=left.location.register;
                LOC_CREFERENCE,
                LOC_REFERENCE :
                  begin
                    location_release(exprasmlist,left.location);
                    location.reference.base:=rg.getregisterint(exprasmlist);
                    cg.a_load_ref_reg(exprasmlist,OS_ADDR,left.location.reference,location.reference.base);
                  end;
                else
                  internalerror(2002032218);
              end;

              { check for a zero length string,
                we can use the ansistring routine here }
              if (cs_check_range in aktlocalswitches) then
                begin
                   rg.saveusedregisters(exprasmlist,pushed,all_registers);
                   cg.a_param_reg(exprasmlist,OS_ADDR,location.reference.base,1);
                   rg.saveregvars(exprasmlist,all_registers);
                   cg.a_call_name(exprasmlist,'FPC_'+Upper(tstringdef(left.resulttype.def).stringtypname)+'_CHECKZERO');
                   cg.g_maybe_loadself(exprasmlist);
                   rg.restoreusedregisters(exprasmlist,pushed);
                end;

              { in ansistrings/widestrings S[1] is p<w>char(S)[0] !! }
              if is_ansistring(left.resulttype.def) then
                dec(location.reference.offset)
              else
                dec(location.reference.offset,2);

              { we've also to keep left up-to-date, because it is used   }
              { if a constant array index occurs, subject to change (FK) }
              location_copy(left.location,location);
           end
         else if is_dynamic_array(left.resulttype.def) then
         { ... also a dynamic string }
           begin
              case left.location.loc of
                LOC_REGISTER,
                LOC_CREGISTER :
                  location.reference.base:=left.location.register;
                LOC_REFERENCE,
                LOC_CREFERENCE :
                  begin
                     location_release(exprasmlist,left.location);
                     location.reference.base:=rg.getregisterint(exprasmlist);
                     emit_ref_reg(A_MOV,S_L,
                       left.location.reference,location.reference.base);
                  end;
                else
                  internalerror(2002032219);
              end;

{$warning FIXME}
              { check for a zero length string,
                we can use the ansistring routine here }
              if (cs_check_range in aktlocalswitches) then
                begin
                   rg.saveusedregisters(exprasmlist,pushed,all_registers);
                   emit_reg(A_PUSH,S_L,location.reference.base);
                   rg.saveregvars(exprasmlist,all_registers);
                   cg.a_call_name(exprasmlist,'FPC_ANSISTR_CHECKZERO');
                   cg.g_maybe_loadself(exprasmlist);
                   rg.restoreusedregisters(exprasmlist,pushed);
                end;

              { we've also to keep left up-to-date, because it is used   }
              { if a constant array index occurs, subject to change (FK) }
              location_copy(left.location,location);
           end
         else
           location_copy(location,left.location);

         { offset can only differ from 0 if arraydef }
         if (left.resulttype.def.deftype=arraydef) and
           not(is_dynamic_array(left.resulttype.def)) then
           dec(location.reference.offset,
               get_mul_size*tarraydef(left.resulttype.def).lowrange);
         if right.nodetype=ordconstn then
           begin
              { offset can only differ from 0 if arraydef }
              if (left.resulttype.def.deftype=arraydef) then
                begin
                   if not(is_open_array(left.resulttype.def)) and
                      not(is_array_of_const(left.resulttype.def)) and
                      not(is_dynamic_array(left.resulttype.def)) then
                     begin
                        if (tordconstnode(right).value>tarraydef(left.resulttype.def).highrange) or
                           (tordconstnode(right).value<tarraydef(left.resulttype.def).lowrange) then
                           begin
                              if (cs_check_range in aktlocalswitches) then
                                CGMessage(parser_e_range_check_error)
                              else
                                CGMessage(parser_w_range_check_error);
                           end;
                        dec(left.location.reference.offset,
                            get_mul_size*tarraydef(left.resulttype.def).lowrange);
                     end
                   else
                     begin
                        { range checking for open and dynamic arrays !!!! }
{$warning FIXME}
                        {!!!!!!!!!!!!!!!!!}
                     end;
                end
              else if (left.resulttype.def.deftype=stringdef) then
                begin
                   if (tordconstnode(right).value=0) and
                      not(is_shortstring(left.resulttype.def)) then
                     CGMessage(cg_e_can_access_element_zero);

                   if (cs_check_range in aktlocalswitches) then
                    begin
                      case tstringdef(left.resulttype.def).string_typ of
                        { it's the same for ansi- and wide strings }
                        st_widestring,
                        st_ansistring:
                          begin
                             rg.saveusedregisters(exprasmlist,pushed,all_registers);
                             cg.a_param_const(exprasmlist,OS_INT,tordconstnode(right).value,2);
                             href:=location.reference;
                             dec(href.offset,7);
                             cg.a_param_ref(exprasmlist,OS_INT,href,1);
                             rg.saveregvars(exprasmlist,all_registers);
                             cg.a_call_name(exprasmlist,'FPC_'+Upper(tstringdef(left.resulttype.def).stringtypname)+'_RANGECHECK');
                             rg.restoreusedregisters(exprasmlist,pushed);
                             cg.g_maybe_loadself(exprasmlist);
                          end;

                        st_shortstring:
                          begin
                             {!!!!!!!!!!!!!!!!!}
                          end;

                        st_longstring:
                          begin
                             {!!!!!!!!!!!!!!!!!}
                          end;
                      end;
                    end;
                end;
              inc(left.location.reference.offset,
                  get_mul_size*tordconstnode(right).value);
              if nf_memseg in flags then
                left.location.reference.segment:=R_FS;

              location_copy(location,left.location);
           end
         else
         { not nodetype=ordconstn }
           begin
              if (cs_regalloc in aktglobalswitches) and
                 { if we do range checking, we don't }
                 { need that fancy code (it would be }
                 { buggy)                            }
                 not(cs_check_range in aktlocalswitches) and
                 (left.resulttype.def.deftype=arraydef) then
                begin
                   extraoffset:=0;
                   if (right.nodetype=addn) then
                     begin
                        if taddnode(right).right.nodetype=ordconstn then
                          begin
                             extraoffset:=tordconstnode(taddnode(right).right).value;
                             t:=taddnode(right).left;
                             { First pass processed this with the assumption   }
                             { that there was an add node which may require an }
                             { extra register. Fake it or die with IE10 (JM)   }
                             t.registers32 := taddnode(right).registers32;
                             taddnode(right).left:=nil;
                             right.free;
                             right:=t;
                          end
                        else if tordconstnode(taddnode(right).left).nodetype=ordconstn then
                          begin
                             extraoffset:=tordconstnode(taddnode(right).left).value;
                             t:=taddnode(right).right;
                             t.registers32 :=  right.registers32;
                             taddnode(right).right:=nil;
                             right.free;
                             right:=t;
                          end;
                     end
                   else if (right.nodetype=subn) then
                     begin
                        if taddnode(right).right.nodetype=ordconstn then
                          begin
{ this was "extraoffset:=right.right.value;" Looks a bit like
  copy-paste bug :) (JM) }
                             extraoffset:=-tordconstnode(taddnode(right).right).value;
                             t:=taddnode(right).left;
                             t.registers32 :=  right.registers32;
                             taddnode(right).left:=nil;
                             right.free;
                             right:=t;
                          end
{ You also have to negate right.right in this case! I can't add an
  unaryminusn without causing a crash, so I've disabled it (JM)
                        else if right.left.nodetype=ordconstn then
                          begin
                             extraoffset:=right.left.value;
                             t:=right.right;
                             t^.registers32 :=  right.registers32;
                             putnode(right);
                             putnode(right.left);
                             right:=t;
                         end;}
                     end;
                   inc(location.reference.offset,
                       get_mul_size*extraoffset);
                end;
              { calculate from left to right }
              if not(location.loc in [LOC_CREFERENCE,LOC_REFERENCE]) then
                CGMessage(cg_e_illegal_expression);
              isjump:=(right.location.loc=LOC_JUMP);
              if isjump then
               begin
                 otl:=truelabel;
                 getlabel(truelabel);
                 ofl:=falselabel;
                 getlabel(falselabel);
               end;
              maybe_save(exprasmlist,right.registers32,location,pushedregs);
              secondpass(right);
              maybe_restore(exprasmlist,location,pushedregs);
              { here we change the location of right
                and the update was forgotten so it
                led to wrong code in emitrangecheck later PM
                so make range check before }

              if cs_check_range in aktlocalswitches then
               begin
                 if left.resulttype.def.deftype=arraydef then
                   begin
                     if is_open_array(left.resulttype.def) or
                        is_array_of_const(left.resulttype.def) then
                      begin
                        tarraydef(left.resulttype.def).genrangecheck;
                        srsym:=searchsymonlyin(tloadnode(left).symtable,
                          'high'+tvarsym(tloadnode(left).symtableentry).name);
                        hightree:=cloadnode.create(tvarsym(srsym),tloadnode(left).symtable);
                        firstpass(hightree);
                        secondpass(hightree);
                        location_release(exprasmlist,hightree.location);
                        reference_reset_symbol(href,newasmsymbol(tarraydef(left.resulttype.def).getrangecheckstring),4);
                        cg.a_load_loc_ref(exprasmlist,hightree.location,href);
                        hightree.free;
                        hightree:=nil;
                      end;
                     cg.g_rangecheck(exprasmlist,right,left.resulttype.def);
                   end;
               end;

              location_force_reg(exprasmlist,right.location,OS_32,false);

              if isjump then
               begin
                 truelabel:=otl;
                 falselabel:=ofl;
               end;

            { produce possible range check code: }
              if cs_check_range in aktlocalswitches then
               begin
                 if left.resulttype.def.deftype=arraydef then
                   begin
                     { done defore (PM) }
                   end
                 else if (left.resulttype.def.deftype=stringdef) then
                   begin
                      case tstringdef(left.resulttype.def).string_typ of
                         { it's the same for ansi- and wide strings }
                         st_widestring,
                         st_ansistring:
                           begin
                              rg.saveusedregisters(exprasmlist,pushed,all_registers);
                              cg.a_param_reg(exprasmlist,OS_INT,right.location.register,1);
                              href:=location.reference;
                              dec(href.offset,7);
                              cg.a_param_ref(exprasmlist,OS_INT,href,1);
                              rg.saveregvars(exprasmlist,all_registers);
                              cg.a_call_name(exprasmlist,'FPC_'+Upper(tstringdef(left.resulttype.def).stringtypname)+'_RANGECHECK');
                              rg.restoreusedregisters(exprasmlist,pushed);
                              cg.g_maybe_loadself(exprasmlist);
                           end;
                         st_shortstring:
                           begin
                              {!!!!!!!!!!!!!!!!!}
                           end;
                         st_longstring:
                           begin
                              {!!!!!!!!!!!!!!!!!}
                           end;
                      end;
                   end;
               end;

              if location.reference.index=R_NO then
               begin
                 location.reference.index:=right.location.register;
                 calc_emit_mul;
               end
              else
               begin
                 if location.reference.base=R_NO then
                  begin
                    case location.reference.scalefactor of
                     2 : emit_const_reg(A_SHL,S_L,1,location.reference.index);
                     4 : emit_const_reg(A_SHL,S_L,2,location.reference.index);
                     8 : emit_const_reg(A_SHL,S_L,3,location.reference.index);
                    end;
                    calc_emit_mul;
                    location.reference.base:=location.reference.index;
                    location.reference.index:=right.location.register;
                  end
                 else
                  begin
                    emit_ref_reg(A_LEA,S_L,location.reference,location.reference.index);
                    rg.ungetregisterint(exprasmlist,location.reference.base);
                    { the symbol offset is loaded,             }
                    { so release the symbol name and set symbol  }
                    { to nil                                 }
                    location.reference.symbol:=nil;
                    location.reference.offset:=0;
                    calc_emit_mul;
                    location.reference.base:=location.reference.index;
                    location.reference.index:=right.location.register;
                  end;
               end;

              if nf_memseg in flags then
                location.reference.segment:=R_FS;
           end;

        location.size:=newsize;
      end;


begin
   caddrnode:=ti386addrnode;
   cderefnode:=ti386derefnode;
   cvecnode:=ti386vecnode;
end.
{
  $Log$
  Revision 1.30  2002-05-13 19:54:38  peter
    * removed n386ld and n386util units
    * maybe_save/maybe_restore added instead of the old maybe_push

  Revision 1.29  2002/05/12 16:53:17  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.28  2002/04/21 19:02:07  peter
    * removed newn and disposen nodes, the code is now directly
      inlined from pexpr
    * -an option that will write the secondpass nodes to the .s file, this
      requires EXTDEBUG define to actually write the info
    * fixed various internal errors and crashes due recent code changes

  Revision 1.27  2002/04/20 21:37:07  carl
  + generic FPC_CHECKPOINTER
  + first parameter offset in stack now portable
  * rename some constants
  + move some cpu stuff to other units
  - remove unused constents
  * fix stacksize for some targets
  * fix generic size problems which depend now on EXTEND_SIZE constant
  * removing frame pointer in routines is only available for : i386,m68k and vis targets

  Revision 1.26  2002/04/19 15:39:35  peter
    * removed some more routines from cga
    * moved location_force_reg/mem to ncgutil
    * moved arrayconstructnode secondpass to ncgld

  Revision 1.25  2002/04/15 19:12:09  carl
  + target_info.size_of_pointer -> pointer_size
  + some cleanup of unused types/variables
  * move several constants from cpubase to their specific units
    (where they are used)
  + att_Reg2str -> gas_reg2str
  + int_reg2str -> std_reg2str

  Revision 1.24  2002/04/04 19:06:12  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.23  2002/04/02 17:11:36  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.22  2002/04/01 09:44:04  jonas
    * better fix for new/dispose bug with init/final data

  Revision 1.21  2002/03/31 20:26:39  jonas
    + a_loadfpu_* and a_loadmm_* methods in tcg
    * register allocation is now handled by a class and is mostly processor
      independent (+rgobj.pas and i386/rgcpu.pas)
    * temp allocation is now handled by a class (+tgobj.pas, -i386\tgcpu.pas)
    * some small improvements and fixes to the optimizer
    * some register allocation fixes
    * some fpuvaroffset fixes in the unary minus node
    * push/popusedregisters is now called rg.save/restoreusedregisters and
      (for i386) uses temps instead of push/pop's when using -Op3 (that code is
      also better optimizable)
    * fixed and optimized register saving/restoring for new/dispose nodes
    * LOC_FPU locations now also require their "register" field to be set to
      R_ST, not R_ST0 (the latter is used for LOC_CFPUREGISTER locations only)
    - list field removed of the tnode class because it's not used currently
      and can cause hard-to-find bugs

  Revision 1.20  2002/03/04 19:10:14  peter
    * removed compiler warnings

  Revision 1.19  2001/12/30 17:24:47  jonas
    * range checking is now processor independent (part in cgobj,
      part in cg64f32) and should work correctly again (it needed
      some changes after the changes of the low and high of
      tordef's to int64)
    * maketojumpbool() is now processor independent (in ncgutil)
    * getregister32 is now called getregisterint

  Revision 1.18  2001/12/03 21:48:43  peter
    * freemem change to value parameter
    * torddef low/high range changed to int64

  Revision 1.17  2001/09/30 16:17:17  jonas
    * made most constant and mem handling processor independent

  Revision 1.16  2001/08/30 20:13:57  peter
    * rtti/init table updates
    * rttisym for reusable global rtti/init info
    * support published for interfaces

  Revision 1.15  2001/08/26 13:37:00  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.14  2001/07/08 21:00:18  peter
    * various widestring updates, it works now mostly without charset
      mapping supported

  Revision 1.13  2001/04/18 22:02:03  peter
    * registration of targets and assemblers

  Revision 1.12  2001/04/13 01:22:19  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.11  2001/04/02 21:20:38  peter
    * resulttype rewrite

  Revision 1.10  2001/03/11 22:58:52  peter
    * getsym redesign, removed the globals srsym,srsymtable

  Revision 1.9  2001/02/02 22:38:00  peter
    * fixed crash with new(precord), merged

  Revision 1.8  2000/12/25 00:07:33  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.7  2000/12/05 11:44:33  jonas
    + new integer regvar handling, should be much more efficient

  Revision 1.6  2000/11/29 00:30:48  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.5  2000/11/04 14:25:24  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.4  2000/10/31 22:02:57  peter
    * symtable splitted, no real code changes

  Revision 1.3  2000/10/31 14:18:53  jonas
    * merged double deleting of left location when using a temp in
      secondwith (merged from fixes branch). This also fixes web bug1194

  Revision 1.2  2000/10/21 18:16:13  florian
    * a lot of changes:
       - basic dyn. array support
       - basic C++ support
       - some work for interfaces done
       ....

  Revision 1.1  2000/10/15 09:33:32  peter
    * moved n386*.pas to i386/ cpu_target dir

  Revision 1.2  2000/10/14 21:52:54  peter
    * fixed memory leaks

  Revision 1.1  2000/10/14 10:14:49  peter
    * moehrendorf oct 2000 rewrite

}
