{
    $Id$
    Copyright (c) 2000-2002 by Florian Klaempfl and Jonas Maebe

    Code generation for add nodes on the PowerPC

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
unit nppcadd;

{$i fpcdefs.inc}

interface

    uses
       node,nadd,cpubase,cginfo;

    type
       tppcaddnode = class(taddnode)
          procedure pass_2;override;
         private
          procedure pass_left_and_right(var pushedfpu:boolean);
          function  getresflags : tresflags;
{
          procedure left_must_be_reg(opsize:TOpSize;noswap:boolean);
          procedure emit_op_right_left(op:TAsmOp;opsize:TOpSize);
          procedure emit_generic_code(op:TAsmOp;opsize:TOpSize;unsigned,extra_not,mboverflow:boolean);
          procedure set_result_location(cmpop,unsigned:boolean);
}
          procedure second_addboolean; virtual; abstract;
          procedure second_addfloat; virtual; abstract;
          procedure second_addsmallset; virtual; abstract;
{$ifdef SUPPORT_MMX}
          procedure second_addmmx;
{$endif SUPPORT_MMX}
          procedure second_add64bit; virtual; abstract;
       end;

  implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,paramgr,
      aasmbase,aasmtai,aasmcpu,defbase,htypechk,
      cgbase,cpuinfo,pass_2,regvars,
      cpupara,
      ncon,nset,
      cga,ncgutil,tgobj,rgobj,rgcpu,cgobj,cg64f32;

{*****************************************************************************
                                  Helpers
*****************************************************************************}

    procedure tppcaddnode.pass_left_and_right(var pushedfpu:boolean);
      var
        pushedregs : tmaybesave;
      begin
        { calculate the operator which is more difficult }
        firstcomplex(self);

        { in case of constant put it to the left }
        if (left.nodetype=ordconstn) then
         swapleftright;
        secondpass(left);

        { are too few registers free? }
        maybe_save(exprasmlist,right.registers32,left.location,pushedregs);
        if location.loc=LOC_FPUREGISTER then
          pushedfpu:=maybe_pushfpu(exprasmlist,right.registersfpu,left.location)
        else
          pushedfpu:=false;
        secondpass(right);
        maybe_restore(exprasmlist,left.location,pushedregs);
      end;


    function tppcaddnode.getresflags : tresflags;
      begin
        if (left.resulttype.def.deftype <> floatdef) then
          result.cr := R_CR0
        else
          result.cr := R_CR1;
        case nodetype of
          equaln : result.flag:=F_EQ;
          unequaln : result.flag:=F_NE;
        else
          if nf_swaped in flags then
            case nodetype of
              ltn : result.flag:=F_GT;
              lten : result.flag:=F_GE;
              gtn : result.flag:=F_LT;
              gten : result.flag:=F_LE;
            end
          else
            case nodetype of
              ltn : result.flag:=F_LT;
              lten : result.flag:=F_LE;
              gtn : result.flag:=F_GT;
              gten : result.flag:=F_GE;
            end;
        end
      end;

(*
    procedure tppcaddnode.left_must_be_reg(opsize:TOpSize;noswap:boolean);
      begin
        { left location is not a register? }
        if (left.location.loc<>LOC_REGISTER) then
         begin
           { if right is register then we can swap the locations }
           if (not noswap) and
              (right.location.loc=LOC_REGISTER) then
            begin
              location_swap(left.location,right.location);
              toggleflag(nf_swaped);
            end
           else
            begin
              { maybe we can reuse a constant register when the
                operation is a comparison that doesn't change the
                value of the register }
              location_force_reg(exprasmlist,left.location,opsize_2_cgsize[opsize],(nodetype in [ltn,lten,gtn,gten,equaln,unequaln]));
         end;
       end;
*)

(*
    procedure tppcaddnode.emit_op_right_left(op:TAsmOp;opsize:TOpsize);
      begin
        { left must be a register }
        case right.location.loc of
          LOC_REGISTER,
          LOC_CREGISTER :
            exprasmlist.concat(taicpu.op_reg_reg_reg(op,opsize,location.register,left.location.register,right.location.register));
          LOC_CONSTANT :
!!!            exprasmlist.concat(taicpu.op_const_reg(op,opsize,right.location.value,left.location.register));
          else
            internalerror(200203232);
        end;
      end;
*)

(*
    procedure tppcaddnode.set_result_location(cmpop,unsigned:boolean);
      begin
        if cmpop then
         begin
           location_reset(location,LOC_FLAGS,OS_NO);
           location.resflags:=getresflags(unsigned);
         end
        else
         location_copy(location,left.location);
      end;

*)

(*
    procedure tppcaddnode.emit_generic_code(op:TAsmOp;opsize:TOpSize;unsigned,extra_not,mboverflow:boolean);
      var
        power : longint;
        hl4   : tasmlabel;
      begin
        { at this point, left.location.loc should be LOC_REGISTER and right
          should be a constant or in a register }
        if (right.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
         begin
           { right.location is a register }
           { when swapped another result register }
           if (nodetype=subn) and (nf_swaped in flags) then
             op := A_SUBF;
           { adapt for overflow checking if necessary }
           if (cs_check_overflow in aktlocalswitches) then
             case op of
               A_ADD, A_SUB, A_SUBF:
                 // convert to ADDO, SUBO, SUBFO
                 op := succ(succ(op));
             end;
           exprasmlist.concat(taicpu.op_reg_reg_reg(op,location.register,left.location.register,right.location.register);
         end
        else
         begin
           { right.location is a constant }
           if (right.location.loc <> LOC_CONSTANT) then
             internalerror(2002072401);
           if (nodetype=subn) and (nf_swaped in flags) then
            begin
              if extra_not then
                emit_reg(A_NOT,opsize,left.location.register);
              rg.getexplicitregisterint(exprasmlist,R_EDI);
              cg.a_load_loc_reg(exprasmlist,right.location,R_EDI);
              emit_reg_reg(op,opsize,left.location.register,R_EDI);
              emit_reg_reg(A_MOV,opsize,R_EDI,left.location.register);
              rg.ungetregisterint(exprasmlist,R_EDI);
            end
           else
            begin
               { Optimizations when right.location is a constant value }
               if (op=A_CMP) and
                  (nodetype in [equaln,unequaln]) and
                  (right.location.loc=LOC_CONSTANT) and
                  (right.location.value=0) then
                 begin
                   emit_reg_reg(A_TEST,opsize,left.location.register,left.location.register);
                 end
               else
                 if (op=A_ADD) and
                    (right.location.loc=LOC_CONSTANT) and
                    (right.location.value=1) and
                    not(cs_check_overflow in aktlocalswitches) then
                  begin
                    emit_reg(A_INC,opsize,left.location.register);
                  end
               else
                 if (op=A_SUB) and
                    (right.location.loc=LOC_CONSTANT) and
                    (right.location.value=1) and
                    not(cs_check_overflow in aktlocalswitches) then
                  begin
                    emit_reg(A_DEC,opsize,left.location.register);
                  end
               else
                 if (op=A_IMUL) and
                    (right.location.loc=LOC_CONSTANT) and
                    (ispowerof2(right.location.value,power)) and
                    not(cs_check_overflow in aktlocalswitches) then
                  begin
                    emit_const_reg(A_SHL,opsize,power,left.location.register);
                  end
               else
                 begin
                   if extra_not then
                     begin
                        rg.getexplicitregisterint(exprasmlist,R_EDI);
                        cg.a_load_loc_reg(exprasmlist,right.location,R_EDI);
                        emit_reg(A_NOT,S_L,R_EDI);
                        emit_reg_reg(A_AND,S_L,R_EDI,left.location.register);
                        rg.ungetregisterint(exprasmlist,R_EDI);
                     end
                   else
                     begin
                        emit_op_right_left(op,opsize);
                     end;
                 end;
            end;
         end;

        { only in case of overflow operations }
        { produce overflow code }
        { we must put it here directly, because sign of operation }
        { is in unsigned VAR!!                                   }
        if mboverflow then
         begin
           if cs_check_overflow in aktlocalswitches  then
            begin
              getlabel(hl4);
              if unsigned then
               emitjmp(C_NB,hl4)
              else
               emitjmp(C_NO,hl4);
              cg.a_call_name(exprasmlist,'FPC_OVERFLOW');
              cg.a_label(exprasmlist,hl4);
            end;
         end;
      end;

*)

{*****************************************************************************
                                AddBoolean
*****************************************************************************}

(*
    procedure ti386addnode.second_addboolean;
      var
        op      : TAsmOp;
        opsize  : TOpsize;
        cmpop,
        isjump  : boolean;
        otl,ofl : tasmlabel;
        pushedregs : tmaybesave;
      begin
        { calculate the operator which is more difficult }
        firstcomplex(self);

        cmpop:=false;
        if (torddef(left.resulttype.def).typ=bool8bit) or
           (torddef(right.resulttype.def).typ=bool8bit) then
         opsize:=S_B
        else
          if (torddef(left.resulttype.def).typ=bool16bit) or
             (torddef(right.resulttype.def).typ=bool16bit) then
           opsize:=S_W
        else
           opsize:=S_L;

        if (cs_full_boolean_eval in aktlocalswitches) or
           (nodetype in [unequaln,ltn,lten,gtn,gten,equaln,xorn]) then
          begin
            if left.nodetype in [ordconstn,realconstn] then
             swapleftright;

            isjump:=(left.location.loc=LOC_JUMP);
            if isjump then
              begin
                 otl:=truelabel;
                 getlabel(truelabel);
                 ofl:=falselabel;
                 getlabel(falselabel);
              end;
            secondpass(left);
            if left.location.loc in [LOC_FLAGS,LOC_JUMP] then
             location_force_reg(exprasmlist,left.location,opsize_2_cgsize[opsize],false);
            if isjump then
             begin
               truelabel:=otl;
               falselabel:=ofl;
             end;

            maybe_save(exprasmlist,right.registers32,left.location,pushedregs);
            isjump:=(right.location.loc=LOC_JUMP);
            if isjump then
              begin
                 otl:=truelabel;
                 getlabel(truelabel);
                 ofl:=falselabel;
                 getlabel(falselabel);
              end;
            secondpass(right);
            maybe_restore(exprasmlist,left.location,pushedregs);
            if right.location.loc in [LOC_FLAGS,LOC_JUMP] then
             location_force_reg(exprasmlist,right.location,opsize_2_cgsize[opsize],false);
            if isjump then
             begin
               truelabel:=otl;
               falselabel:=ofl;
             end;

            { left must be a register }
            left_must_be_reg(opsize,false);
            { compare the }
            case nodetype of
              ltn,lten,gtn,gten,
              equaln,unequaln :
                begin
                  op:=A_CMP;
                  cmpop:=true;
                end;
              xorn :
                op:=A_XOR;
              orn :
                op:=A_OR;
              andn :
                op:=A_AND;
              else
                internalerror(200203247);
            end;
            emit_op_right_left(op,opsize);
            location_freetemp(exprasmlist,right.location);
            location_release(exprasmlist,right.location);
            if cmpop then
             begin
               location_freetemp(exprasmlist,left.location);
               location_release(exprasmlist,left.location);
             end;
            set_result_location(cmpop,true);
         end
        else
         begin
           case nodetype of
             andn,
             orn :
               begin
                 location_reset(location,LOC_JUMP,OS_NO);
                 case nodetype of
                   andn :
                     begin
                        otl:=truelabel;
                        getlabel(truelabel);
                        secondpass(left);
                        maketojumpbool(exprasmlist,left,lr_load_regvars);
                        cg.a_label(exprasmlist,truelabel);
                        truelabel:=otl;
                     end;
                   orn :
                     begin
                        ofl:=falselabel;
                        getlabel(falselabel);
                        secondpass(left);
                        maketojumpbool(exprasmlist,left,lr_load_regvars);
                        cg.a_label(exprasmlist,falselabel);
                        falselabel:=ofl;
                     end;
                   else
                     CGMessage(type_e_mismatch);
                 end;
                 secondpass(right);
                 maketojumpbool(exprasmlist,right,lr_load_regvars);
               end;
             else
               CGMessage(type_e_mismatch);
           end;
         end;
      end;
*)

{*****************************************************************************
                                AddFloat
*****************************************************************************}

(*
    procedure ti386addnode.second_addfloat;
      var
        op         : TAsmOp;
        resflags   : tresflags;
        pushedfpu,
        cmpop      : boolean;
      begin
        pass_left_and_right(pushedfpu);

        cmpop:=false;
        case nodetype of
          addn :
            op:=A_FADDP;
          muln :
            op:=A_FMULP;
          subn :
            op:=A_FSUBP;
          slashn :
            op:=A_FDIVP;
          ltn,lten,gtn,gten,
          equaln,unequaln :
            begin
              op:=A_FCOMPP;
              cmpop:=true;
            end;
          else
            CGMessage(type_e_mismatch);
        end;

        if (right.location.loc<>LOC_FPUREGISTER) then
         begin
           cg.a_loadfpu_loc_reg(exprasmlist,
               right.location,R_ST);
           if (right.location.loc <> LOC_CFPUREGISTER) and
              pushedfpu then
             location_freetemp(exprasmlist,left.location);
           if (left.location.loc<>LOC_FPUREGISTER) then
            begin
              cg.a_loadfpu_loc_reg(exprasmlist,left.location,R_ST);
              if (left.location.loc <> LOC_CFPUREGISTER) and
                 pushedfpu then
                location_freetemp(exprasmlist,left.location);
            end
           else
            begin
              { left was on the stack => swap }
              toggleflag(nf_swaped);
            end;

           { releases the right reference }
           location_release(exprasmlist,right.location);
         end
        { the nominator in st0 }
        else if (left.location.loc<>LOC_FPUREGISTER) then
         begin
           cg.a_loadfpu_loc_reg(exprasmlist,left.location,R_ST);
           if (left.location.loc <> LOC_CFPUREGISTER) and
              pushedfpu then
             location_freetemp(exprasmlist,left.location);
         end
        else
         begin
           { fpu operands are always in the wrong order on the stack }
           toggleflag(nf_swaped);
         end;

        { releases the left reference }
        if (left.location.loc in [LOC_CREFERENCE,LOC_REFERENCE]) then
          location_release(exprasmlist,left.location);

        { if we swaped the tree nodes, then use the reverse operator }
        if nf_swaped in flags then
          begin
             if (nodetype=slashn) then
               op:=A_FDIVRP
             else if (nodetype=subn) then
               op:=A_FSUBRP;
          end;
        { to avoid the pentium bug
        if (op=FDIVP) and (opt_processors=pentium) then
          cg.a_call_name(exprasmlist,'EMUL_FDIVP')
        else
        }
        { the Intel assemblers want operands }
        if op<>A_FCOMPP then
          begin
             emit_reg_reg(op,S_NO,R_ST,R_ST1);
             dec(trgcpu(rg).fpuvaroffset);
          end
        else
          begin
             emit_none(op,S_NO);
             dec(trgcpu(rg).fpuvaroffset,2);
          end;

        { on comparison load flags }
        if cmpop then
         begin
           if not(R_EAX in rg.unusedregsint) then
             begin
               rg.getexplicitregisterint(exprasmlist,R_EDI);
               emit_reg_reg(A_MOV,S_L,R_EAX,R_EDI);
             end;
           emit_reg(A_FNSTSW,S_NO,R_AX);
           emit_none(A_SAHF,S_NO);
           if not(R_EAX in rg.unusedregsint) then
             begin
               emit_reg_reg(A_MOV,S_L,R_EDI,R_EAX);
               rg.ungetregisterint(exprasmlist,R_EDI);
             end;
           if nf_swaped in flags then
            begin
              case nodetype of
                  equaln : resflags:=F_E;
                unequaln : resflags:=F_NE;
                     ltn : resflags:=F_A;
                    lten : resflags:=F_AE;
                     gtn : resflags:=F_B;
                    gten : resflags:=F_BE;
              end;
            end
           else
            begin
              case nodetype of
                  equaln : resflags:=F_E;
                unequaln : resflags:=F_NE;
                     ltn : resflags:=F_B;
                    lten : resflags:=F_BE;
                     gtn : resflags:=F_A;
                    gten : resflags:=F_AE;
              end;
            end;
           location_reset(location,LOC_FLAGS,OS_NO);
           location.resflags:=resflags;
         end
        else
         begin
           location_reset(location,LOC_FPUREGISTER,def_cgsize(resulttype.def));
           location.register:=R_ST;
         end;
      end;
*)

{*****************************************************************************
                                AddSmallSet
*****************************************************************************}

(*
    procedure ti386addnode.second_addsmallset;
      var
        opsize : TOpSize;
        op     : TAsmOp;
        cmpop,
        pushedfpu,
        extra_not,
        noswap : boolean;
      begin
        pass_left_and_right(pushedfpu);

        { when a setdef is passed, it has to be a smallset }
        if ((left.resulttype.def.deftype=setdef) and
            (tsetdef(left.resulttype.def).settype<>smallset)) or
           ((right.resulttype.def.deftype=setdef) and
            (tsetdef(right.resulttype.def).settype<>smallset)) then
         internalerror(200203301);

        cmpop:=false;
        noswap:=false;
        extra_not:=false;
        opsize:=S_L;
        case nodetype of
          addn :
            begin
              { this is a really ugly hack!!!!!!!!!! }
              { this could be done later using EDI   }
              { as it is done for subn               }
              { instead of two registers!!!!         }
              { adding elements is not commutative }
              if (nf_swaped in flags) and (left.nodetype=setelementn) then
               swapleftright;
              { are we adding set elements ? }
              if right.nodetype=setelementn then
               begin
                 { no range support for smallsets! }
                 if assigned(tsetelementnode(right).right) then
                  internalerror(43244);
                 { bts requires both elements to be registers }
                 location_force_reg(exprasmlist,left.location,opsize_2_cgsize[opsize],false);
                 location_force_reg(exprasmlist,right.location,opsize_2_cgsize[opsize],true);
                 op:=A_BTS;
                 noswap:=true;
               end
              else
               op:=A_OR;
            end;
          symdifn :
            op:=A_XOR;
          muln :
            op:=A_AND;
          subn :
            begin
              op:=A_AND;
              if (not(nf_swaped in flags)) then
                if (right.location.loc=LOC_CONSTANT) then
                  right.location.value := not(right.location.value)
                else
                  op := A_ANDC
              else
                if (left.location.loc=LOC_CONSTANT) then
                  left.location.value := not(left.location.value)
                else
                   begin
                     swapleftright;
                     op := A_ANDC;
                   end;
            end;
          equaln,
          unequaln :
            begin
              op:=A_CMP;
              cmpop:=true;
            end;
          lten,gten:
            begin
              If (not(nf_swaped in flags) and
                  (nodetype = lten)) or
                 ((nf_swaped in flags) and
                  (nodetype = gten)) then
                swapleftright;
              location_force_reg(exprasmlist,left.location,opsize_2_cgsize[opsize],true);
              emit_op_right_left(A_AND,opsize);
              op:=A_CMP;
              cmpop:=true;
              { warning: ugly hack, we need a JE so change the node to equaln }
              nodetype:=equaln;
            end;
          xorn :
            op:=A_XOR;
          orn :
            op:=A_OR;
          andn :
            op:=A_AND;
          else
            begin
              { no < or > support for sets }
              CGMessage(type_e_mismatch);
            end;
        end;
        { left must be a register }
        left_must_be_reg(opsize,noswap);
        emit_generic_code(op,opsize,true,extra_not,false);
        location_freetemp(exprasmlist,right.location);
        location_release(exprasmlist,right.location);
        if cmpop then
         begin
           location_freetemp(exprasmlist,left.location);
           location_release(exprasmlist,left.location);
         end;
        set_result_location(cmpop,true);
      end;
*)

{*****************************************************************************
                                Add64bit
*****************************************************************************}

(*
    procedure ti386addnode.second_add64bit;
      var
        op         : TOpCG;
        op1,op2    : TAsmOp;
        opsize     : TOpSize;
        hregister,
        hregister2 : tregister;
        href       : treference;
        hl4        : tasmlabel;
        pushedfpu,
        mboverflow,
        cmpop,
        unsigned   : boolean;

      procedure firstjmp64bitcmp;

        var
           oldnodetype : tnodetype;

        begin
           load_all_regvars(exprasmlist);
           { the jump the sequence is a little bit hairy }
           case nodetype of
              ltn,gtn:
                begin
                   emitjmp(flags_to_cond(getresflags(unsigned)),truelabel);
                   { cheat a little bit for the negative test }
                   toggleflag(nf_swaped);
                   emitjmp(flags_to_cond(getresflags(unsigned)),falselabel);
                   toggleflag(nf_swaped);
                end;
              lten,gten:
                begin
                   oldnodetype:=nodetype;
                   if nodetype=lten then
                     nodetype:=ltn
                   else
                     nodetype:=gtn;
                   emitjmp(flags_to_cond(getresflags(unsigned)),truelabel);
                   { cheat for the negative test }
                   if nodetype=ltn then
                     nodetype:=gtn
                   else
                     nodetype:=ltn;
                   emitjmp(flags_to_cond(getresflags(unsigned)),falselabel);
                   nodetype:=oldnodetype;
                end;
              equaln:
                emitjmp(C_NE,falselabel);
              unequaln:
                emitjmp(C_NE,truelabel);
           end;
        end;


      procedure secondjmp64bitcmp;

        begin
           { the jump the sequence is a little bit hairy }
           case nodetype of
              ltn,gtn,lten,gten:
                begin
                   { the comparisaion of the low dword have to be }
                   {  always unsigned!                            }
                   emitjmp(flags_to_cond(getresflags(true)),truelabel);
                   cg.a_jmp_always(exprasmlist,falselabel);
                end;
              equaln:
                begin
                   emitjmp(C_NE,falselabel);
                   cg.a_jmp_always(exprasmlist,truelabel);
                end;
              unequaln:
                begin
                   emitjmp(C_NE,truelabel);
                   cg.a_jmp_always(exprasmlist,falselabel);
                end;
           end;
        end;

      begin
        firstcomplex(self);

        pass_left_and_right(pushedfpu);

        op1:=A_NONE;
        op2:=A_NONE;
        mboverflow:=false;
        cmpop:=false;
        opsize:=S_L;
        unsigned:=((left.resulttype.def.deftype=orddef) and
                   (torddef(left.resulttype.def).typ=u64bit)) or
                  ((right.resulttype.def.deftype=orddef) and
                   (torddef(right.resulttype.def).typ=u64bit));
        case nodetype of
          addn :
            begin
              op:=OP_ADD;
              mboverflow:=true;
            end;
          subn :
            begin
              op:=OP_SUB;
              op1:=A_SUB;
              op2:=A_SBB;
              mboverflow:=true;
            end;
          ltn,lten,
          gtn,gten,
          equaln,unequaln:
            begin
              op:=OP_NONE;
              cmpop:=true;
            end;
          xorn:
            op:=OP_XOR;
          orn:
            op:=OP_OR;
          andn:
            op:=OP_AND;
          muln:
            begin
              { should be handled in pass_1 (JM) }
              internalerror(200109051);
            end;
          else
            CGMessage(type_e_mismatch);
        end;

        { left and right no register?  }
        { then one must be demanded    }
        if (left.location.loc<>LOC_REGISTER) then
         begin
           if (right.location.loc<>LOC_REGISTER) then
            begin
              { we can reuse a CREGISTER for comparison }
              if not((left.location.loc=LOC_CREGISTER) and cmpop) then
               begin
                 if (left.location.loc<>LOC_CREGISTER) then
                  begin
                    location_freetemp(exprasmlist,left.location);
                    location_release(exprasmlist,left.location);
                  end;
                 hregister:=rg.getregisterint(exprasmlist);
                 hregister2:=rg.getregisterint(exprasmlist);
                 cg64.a_load64_loc_reg(exprasmlist,left.location,joinreg64(hregister,hregister2));
                 location_reset(left.location,LOC_REGISTER,OS_64);
                 left.location.registerlow:=hregister;
                 left.location.registerhigh:=hregister2;
               end;
            end
           else
            begin
              location_swap(left.location,right.location);
              toggleflag(nf_swaped);
            end;
         end;

        { at this point, left.location.loc should be LOC_REGISTER }
        if right.location.loc=LOC_REGISTER then
         begin
           { when swapped another result register }
           if (nodetype=subn) and (nf_swaped in flags) then
            begin
              cg64.a_op64_reg_reg(exprasmlist,op,
                left.location.register64,
                right.location.register64);
              location_swap(left.location,right.location);
              toggleflag(nf_swaped);
            end
           else if cmpop then
            begin
              emit_reg_reg(A_CMP,S_L,right.location.registerhigh,left.location.registerhigh);
              firstjmp64bitcmp;
              emit_reg_reg(A_CMP,S_L,right.location.registerlow,left.location.registerlow);
              secondjmp64bitcmp;
            end
           else
            begin
              cg64.a_op64_reg_reg(exprasmlist,op,
                right.location.register64,
                left.location.register64);
            end;
           location_release(exprasmlist,right.location);
         end
        else
         begin
           { right.location<>LOC_REGISTER }
           if (nodetype=subn) and (nf_swaped in flags) then
            begin
              rg.getexplicitregisterint(exprasmlist,R_EDI);
              cg64.a_load64low_loc_reg(exprasmlist,right.location,R_EDI);
              emit_reg_reg(op1,opsize,left.location.registerlow,R_EDI);
              emit_reg_reg(A_MOV,opsize,R_EDI,left.location.registerlow);
              cg64.a_load64high_loc_reg(exprasmlist,right.location,R_EDI);
              { the carry flag is still ok }
              emit_reg_reg(op2,opsize,left.location.registerhigh,R_EDI);
              emit_reg_reg(A_MOV,opsize,R_EDI,left.location.registerhigh);
              rg.ungetregisterint(exprasmlist,R_EDI);
              if right.location.loc<>LOC_CREGISTER then
               begin
                 location_freetemp(exprasmlist,right.location);
                 location_release(exprasmlist,right.location);
               end;
            end
           else if cmpop then
            begin
              case right.location.loc of
                LOC_CREGISTER :
                  begin
                    emit_reg_reg(A_CMP,S_L,right.location.registerhigh,left.location.registerhigh);
                    firstjmp64bitcmp;
                    emit_reg_reg(A_CMP,S_L,right.location.registerlow,left.location.registerlow);
                    secondjmp64bitcmp;
                  end;
                LOC_CREFERENCE,
                LOC_REFERENCE :
                  begin
                    href:=right.location.reference;
                    inc(href.offset,4);
                    emit_ref_reg(A_CMP,S_L,href,left.location.registerhigh);
                    firstjmp64bitcmp;
                    emit_ref_reg(A_CMP,S_L,right.location.reference,left.location.registerlow);
                    secondjmp64bitcmp;
                    cg.a_jmp_always(exprasmlist,falselabel);
                    location_freetemp(exprasmlist,right.location);
                    location_release(exprasmlist,right.location);
                  end;
                LOC_CONSTANT :
                  begin
                    exprasmlist.concat(taicpu.op_const_reg(A_CMP,S_L,right.location.valuehigh,left.location.registerhigh));
                    firstjmp64bitcmp;
                    exprasmlist.concat(taicpu.op_const_reg(A_CMP,S_L,right.location.valuelow,left.location.registerlow));
                    secondjmp64bitcmp;
                  end;
                else
                  internalerror(200203282);
              end;
            end

           else
            begin
              cg64.a_op64_loc_reg(exprasmlist,op,right.location,
                left.location.register64);
              if (right.location.loc<>LOC_CREGISTER) then
               begin
                 location_freetemp(exprasmlist,right.location);
                 location_release(exprasmlist,right.location);
               end;
            end;
         end;

        if (left.location.loc<>LOC_CREGISTER) and cmpop then
         begin
           location_freetemp(exprasmlist,left.location);
           location_release(exprasmlist,left.location);
         end;

        { only in case of overflow operations }
        { produce overflow code }
        { we must put it here directly, because sign of operation }
        { is in unsigned VAR!!                              }
        if mboverflow then
         begin
           if cs_check_overflow in aktlocalswitches  then
            begin
              getlabel(hl4);
              if unsigned then
               emitjmp(C_NB,hl4)
              else
               emitjmp(C_NO,hl4);
              cg.a_call_name(exprasmlist,'FPC_OVERFLOW');
              cg.a_label(exprasmlist,hl4);
            end;
         end;

        { we have LOC_JUMP as result }
        if cmpop then
         location_reset(location,LOC_JUMP,OS_NO)
        else
         location_copy(location,left.location);
      end;
*)


{*****************************************************************************
                                AddMMX
*****************************************************************************}

{$ifdef SUPPORT_MMX}
    procedure ti386addnode.second_addmmx;
      var
        op         : TAsmOp;
        pushedfpu,
        cmpop      : boolean;
        mmxbase    : tmmxtype;
        hregister  : tregister;
      begin
        pass_left_and_right(pushedfpu);

        cmpop:=false;
        mmxbase:=mmx_type(left.resulttype.def);
        case nodetype of
          addn :
            begin
              if (cs_mmx_saturation in aktlocalswitches) then
                begin
                   case mmxbase of
                      mmxs8bit:
                        op:=A_PADDSB;
                      mmxu8bit:
                        op:=A_PADDUSB;
                      mmxs16bit,mmxfixed16:
                        op:=A_PADDSB;
                      mmxu16bit:
                        op:=A_PADDUSW;
                   end;
                end
              else
                begin
                   case mmxbase of
                      mmxs8bit,mmxu8bit:
                        op:=A_PADDB;
                      mmxs16bit,mmxu16bit,mmxfixed16:
                        op:=A_PADDW;
                      mmxs32bit,mmxu32bit:
                        op:=A_PADDD;
                   end;
                end;
            end;
          muln :
            begin
               case mmxbase of
                  mmxs16bit,mmxu16bit:
                    op:=A_PMULLW;
                  mmxfixed16:
                    op:=A_PMULHW;
               end;
            end;
          subn :
            begin
              if (cs_mmx_saturation in aktlocalswitches) then
                begin
                   case mmxbase of
                      mmxs8bit:
                        op:=A_PSUBSB;
                      mmxu8bit:
                        op:=A_PSUBUSB;
                      mmxs16bit,mmxfixed16:
                        op:=A_PSUBSB;
                      mmxu16bit:
                        op:=A_PSUBUSW;
                   end;
                end
              else
                begin
                   case mmxbase of
                      mmxs8bit,mmxu8bit:
                        op:=A_PSUBB;
                      mmxs16bit,mmxu16bit,mmxfixed16:
                        op:=A_PSUBW;
                      mmxs32bit,mmxu32bit:
                        op:=A_PSUBD;
                   end;
                end;
            end;
          xorn:
            op:=A_PXOR;
          orn:
            op:=A_POR;
          andn:
            op:=A_PAND;
          else
            CGMessage(type_e_mismatch);
        end;

        { left and right no register?  }
        { then one must be demanded    }
        if (left.location.loc<>LOC_MMXREGISTER) then
         begin
           if (right.location.loc=LOC_MMXREGISTER) then
            begin
              location_swap(left.location,right.location);
              toggleflag(nf_swaped);
            end
           else
            begin
              { register variable ? }
              if (left.location.loc=LOC_CMMXREGISTER) then
               begin
                 hregister:=rg.getregistermm(exprasmlist);
                 emit_reg_reg(A_MOVQ,S_NO,left.location.register,hregister);
               end
              else
               begin
                 if not(left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
                  internalerror(200203245);

                 location_release(exprasmlist,left.location);

                 hregister:=rg.getregistermm(exprasmlist);
                 emit_ref_reg(A_MOVQ,S_NO,left.location.reference,hregister);
               end;

              location_reset(left.location,LOC_MMXREGISTER,OS_NO);
              left.location.register:=hregister;
            end;
         end;

        { at this point, left.location.loc should be LOC_MMXREGISTER }
        if right.location.loc<>LOC_MMXREGISTER then
         begin
           if (nodetype=subn) and (nf_swaped in flags) then
            begin
              if right.location.loc=LOC_CMMXREGISTER then
               begin
                 emit_reg_reg(A_MOVQ,S_NO,right.location.register,R_MM7);
                 emit_reg_reg(op,S_NO,left.location.register,R_MM7);
                 emit_reg_reg(A_MOVQ,S_NO,R_MM7,left.location.register);
               end
              else
               begin
                 if not(left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
                  internalerror(200203247);
                 emit_ref_reg(A_MOVQ,S_NO,right.location.reference,R_MM7);
                 emit_reg_reg(op,S_NO,left.location.register,R_MM7);
                 emit_reg_reg(A_MOVQ,S_NO,R_MM7,left.location.register);
                 location_release(exprasmlist,right.location);
               end;
            end
           else
            begin
              if (right.location.loc=LOC_CMMXREGISTER) then
               begin
                 emit_reg_reg(op,S_NO,right.location.register,left.location.register);
               end
              else
               begin
                 if not(right.location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
                  internalerror(200203246);
                 emit_ref_reg(op,S_NO,right.location.reference,left.location.register);
                 location_release(exprasmlist,right.location);
               end;
            end;
          end
        else
          begin
            { right.location=LOC_MMXREGISTER }
            if (nodetype=subn) and (nf_swaped in flags) then
             begin
               emit_reg_reg(op,S_NO,left.location.register,right.location.register);
               location_swap(left.location,right.location);
               toggleflag(nf_swaped);
             end
            else
             begin
               emit_reg_reg(op,S_NO,right.location.register,left.location.register);
             end;
          end;

        location_freetemp(exprasmlist,right.location);
        location_release(exprasmlist,right.location);
        if cmpop then
         begin
           location_freetemp(exprasmlist,left.location);
           location_release(exprasmlist,left.location);
         end;
        set_result_location(cmpop,true);
      end;
{$endif SUPPORT_MMX}


{*****************************************************************************
                                pass_2
*****************************************************************************}

    procedure tppcaddnode.pass_2;
    { is also being used for xor, and "mul", "sub, or and comparative }
    { operators                                                }
      var
         pushedfpu,useconst,
         { mboverflow, } cmpop : boolean;
         op : tasmop;
         cgop: topcg;
{         cgsize : tcgsize; }
         tmpreg: tregister;

         { true, if unsigned types are compared }
         unsigned : boolean;
         { true, if for sets subtractions the extra not should generated }
{         extra_not : boolean; }

         regstopush: tregisterset;

      begin
         { to make it more readable, string and set (not smallset!) have their
           own procedures }
         case left.resulttype.def.deftype of
           orddef :
             begin
               { handling boolean expressions }
               if is_boolean(left.resulttype.def) and
                  is_boolean(right.resulttype.def) then
                 begin
                   second_addboolean;
                   exit;
                 end
               { 64bit operations }
               else if is_64bitint(left.resulttype.def) then
                 begin
                   second_add64bit;
                   exit;
                 end;
             end;
           stringdef :
             begin
               internalerror(2002072402);
               exit;
             end;
           setdef :
             begin
               { normalsets are already handled in pass1 }
               if (tsetdef(left.resulttype.def).settype<>smallset) then
                internalerror(200109041);
               second_addsmallset;
               exit;
             end;
           arraydef :
             begin
{$ifdef SUPPORT_MMX}
               if is_mmx_able_array(left.resulttype.def) then
                begin
                  second_addmmx;
                  exit;
                end;
{$endif SUPPORT_MMX}
             end;
           floatdef :
             begin
               second_addfloat;
               exit;
             end;
         end;

         { defaults }
         {is_in_dest:=false;}
{         extra_not:=false; }
{         mboverflow:=false; }
         cmpop:=nodetype in [ltn,lten,gtn,gten,equaln,unequaln];
         unsigned:=not(is_signed(left.resulttype.def)) or
                   not(is_signed(right.resulttype.def));
{         cgsize := def_cgsize(left.resulttype.def);}

         pass_left_and_right(pushedfpu);

(*
         if (left.resulttype.def.deftype=pointerdef) or
            (right.resulttype.def.deftype=pointerdef) or

            (is_class_or_interface(right.resulttype.def) and is_class_or_interface(left.resulttype.def)) or

            (left.resulttype.def.deftype=classrefdef) or

            (left.resulttype.def.deftype=procvardef) or

            ((left.resulttype.def.deftype=enumdef) and
             (left.resulttype.def.size=4)) or

            ((left.resulttype.def.deftype=orddef) and
             (torddef(left.resulttype.def).typ in [s32bit,u32bit])) or
            ((right.resulttype.def.deftype=orddef) and
             (torddef(right.resulttype.def).typ in [s32bit,u32bit])) then
*)
          begin
            { Convert flags to register first }
            { can any of these things be in the flags actually?? (JM) }

            if (left.location.loc = LOC_FLAGS) or
               (right.location.loc = LOC_FLAGS) then
              internalerror(2002072602);
{
            if (left.location.loc=LOC_FLAGS) then
             location_force_reg(exprasmlist,left.location,opsize_2_cgsize[opsize],false);
            if (right.location.loc=LOC_FLAGS) then
             location_force_reg(exprasmlist,right.location,opsize_2_cgsize[opsize],false);
}

            { set result location }
            if not cmpop then
              location_reset(location,LOC_REGISTER,def_cgsize(resulttype.def))
             else
              location_reset(location,LOC_FLAGS,OS_NO);

            case left.location.loc of
              LOC_REGISTER:
                if not cmpop then
                  location.register := left.location.register;
              LOC_REFERENCE,LOC_CREFERENCE:
                begin
                  location_force_reg(exprasmlist,left.location,def_cgsize(left.resulttype.def),false);
                  if not cmpop then
                    location.register := left.location.register;
                end;
            end;
            case right.location.loc of
              LOC_REGISTER:
                if not cmpop then
                  location.register := right.location.register;
              LOC_REFERENCE,LOC_CREFERENCE:
                begin
                  location_force_reg(exprasmlist,right.location,def_cgsize(right.resulttype.def),false);
                  if not cmpop then
                    location.register := right.location.register;
                end;
            end;

            // when overflow checking is on, all operands have to be in
            // a register
            if (cs_check_overflow in aktlocalswitches) and
               not cmpop and
               not (nodetype in [orn,andn,xorn]) then
              begin
                { left and right can't be both constants }
                if (left.location.loc = LOC_CONSTANT) then
                  begin
                    location_force_reg(exprasmlist,left.location,
                      def_cgsize(left.resulttype.def),false);
                     location.register := left.location.register;
                   end
                else if (right.location.loc = LOC_CONSTANT) then
                  begin
                    location_force_reg(exprasmlist,right.location,
                      def_cgsize(right.resulttype.def),false);
                    location.register := right.location.register;
                  end;
              end;

            if (location.register = R_NO) and
               not(cmpop) then
              location.register := rg.getregisterint(exprasmlist);

            if not(cs_check_overflow in aktlocalswitches) or
               (cmpop) or
               (nodetype in [orn,andn,xorn]) then
              begin
                case nodetype of
                  addn, muln, xorn, orn, andn:
                    begin
                      case nodetype of
                        addn:
                          cgop := OP_ADD;
                        muln:
                          if unsigned then
                            cgop := OP_MUL
                          else 
                            cgop := OP_IMUL;
                        xorn:
                          cgop := OP_XOR;
                        orn:
                          cgop := OP_OR;
                        andn:
                          cgop := OP_AND;
                      end; 
                      if (left.location.loc = LOC_CONSTANT) then
                        swapleftright;
                      if (right.location.loc <> LOC_CONSTANT) then
                        cg.a_op_reg_reg_reg(exprasmlist,cgop,OS_INT,
                          left.location.register,right.location.register,
                          location.register)
                      else
                        cg.a_op_const_reg_reg(exprasmlist,cgop,OS_INT,
                          aword(right.location.value),left.location.register,
                        location.register);
                    end;
                  subn:
                    begin
                      if (nf_swaped in flags) then
                        swapleftright;
                      if left.location.loc <> LOC_CONSTANT then
                        if right.location.loc <> LOC_CONSTANT then
                          cg.a_op_reg_reg_reg(exprasmlist,OP_SUB,OS_INT,
                            right.location.register,left.location.register,
                            location.register)
                        else
                          cg.a_op_const_reg_reg(exprasmlist,OP_SUB,OS_INT,
                            aword(right.location.value),left.location.register,
                            location.register)
                      else
                        if (longint(left.location.value) >= low(smallint)) and
                           (longint(left.location.value) <= high(smallint)) then
                          begin
                            exprasmlist.concat(taicpu.op_reg_reg_const(A_SUBFIC,
                              location.register,right.location.register,
                              left.location.value));
                          end
                        else
                          begin
                            tmpreg := cg.get_scratch_reg_int(exprasmlist);
                            cg.a_load_const_reg(exprasmlist,OS_INT,
                              left.location.value,tmpreg);
                            cg.a_op_reg_reg_reg(exprasmlist,OP_SUB,OS_INT,
                              right.location.register,tmpreg,location.register);
                            cg.free_scratch_reg(exprasmlist,tmpreg);
                          end;
                    end;
                  ltn,lten,gtn,gten, equaln,unequaln :
                    begin
                      // get the constant on the right if there is one
                      if (left.location.loc = LOC_CONSTANT) then
                        swapleftright;
                      // can we use an immediate, or do we have to load the
                      // constant in a register first?
                      if (right.location.loc = LOC_CONSTANT) then
                        if (unsigned and
                            (aword(right.location.value) > high(word))) or
                           (not(unsigned) and
                            ((longint(right.location.value) < low(smallint)) or
                              (longint(right.location.value) > high(smallint)))) then
                           useconst := true
                        else
                          begin
                            useconst := false;
                            tmpreg := cg.get_scratch_reg_int(exprasmlist);
                            cg.a_load_const_reg(exprasmlist,OS_INT,
                              aword(right.location.value),tmpreg);
                           end
                      else
                        useconst := false;
                      location.loc := LOC_FLAGS;
                      location.resflags := getresflags;
                      if not unsigned then
                        if useconst then
                          op := A_CMPWI
                        else
                          op := A_CMPW
                      else
                        if useconst then
                          op := A_CMPLWI
                        else
                          op := A_CMPLW;

                      if (right.location.loc = LOC_CONSTANT) then
                        if useconst then
                          exprasmlist.concat(taicpu.op_reg_const(op,
                            left.location.register,right.location.value))
                        else
                          begin
                            exprasmlist.concat(taicpu.op_reg_reg(op,
                              left.location.register,tmpreg));
                            cg.free_scratch_reg(exprasmlist,tmpreg);
                          end
                      else
                        exprasmlist.concat(taicpu.op_reg_reg(op,
                          left.location.register,right.location.register));
                    end;
                end
              end
            else
              // overflow checking is on and we have an addn, subn or muln
              begin
                case nodetype of
                  addn:
                    op := A_ADDO;
                  subn:
                    op := A_SUBO;
                  muln:
                     op := A_MULLWO;
                  else
                    internalerror(2002072601);
                end;
                exprasmlist.concat(taicpu.op_reg_reg_reg(op,location.register,
                  left.location.register,right.location.register));
              end;

          end;

          if (right.location.loc = LOC_REGISTER) and
             (cmpop or
              (location.register <> right.location.register)) then
            rg.ungetregister(exprasmlist,right.location.register);
          if (left.location.loc = LOC_REGISTER) and
             (cmpop or
              (location.register <> left.location.register)) then
            rg.ungetregister(exprasmlist,left.location.register);

            cg.g_overflowcheck(exprasmlist,self);
(*
         { 8/16 bit enum,char,wchar types }
         else
          if ((left.resulttype.def.deftype=orddef) and
              (torddef(left.resulttype.def).typ in [uchar,uwidechar])) or
             ((left.resulttype.def.deftype=enumdef) and
              ((left.resulttype.def.size=1) or
               (left.resulttype.def.size=2))) then
           begin
             case nodetype of
               ltn,lten,gtn,gten,
               equaln,unequaln :
                 cmpop:=true;
               else
                 CGMessage(type_e_mismatch);
             end;
             left_must_be_reg(opsize,false);
             emit_op_right_left(A_CMP,opsize);
             location_freetemp(exprasmlist,right.location);
             location_release(exprasmlist,right.location);
             if left.location.loc<>LOC_CREGISTER then
              begin
                location_freetemp(exprasmlist,left.location);
                location_release(exprasmlist,left.location);
              end;
             set_result_location(true,true);
           end
         else
           CGMessage(type_e_mismatch);
*)
      end;

begin
   caddnode:=tppcaddnode;
end.
{
  $Log$
  Revision 1.1  2002-07-26 12:31:57  jonas
    + intial implementation of add nodes, only integer/enumeration/pointer/...
      handling is finished


}
