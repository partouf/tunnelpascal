{
    Copyright (c) 1998-2010 by Florian Klaempfl and Jonas Maebe
    Member of the Free Pascal development team

    This unit implements the jvm high level code generator

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
unit hlcgcpu;

{$i fpcdefs.inc}

interface

uses
  globtype,
  aasmbase,aasmdata,
  symbase,symconst,symtype,symdef,symsym,
  cpubase, hlcgobj, cgbase, cgutils, parabase;

  type

    { thlcgjvm }

    thlcgjvm = class(thlcgobj)
     private
      fevalstackheight,
      fmaxevalstackheight: longint;
     public
      constructor create;

      procedure incstack(list : TAsmList;slots: longint);
      procedure decstack(list : TAsmList;slots: longint);

      function def2regtyp(def: tdef): tregistertype; override;

      procedure a_call_name(list : TAsmList;pd : tprocdef;const s : string; weak: boolean);override;
      procedure a_call_name_inherited(list : TAsmList;pd : tprocdef;const s : string);override;

      procedure a_load_const_reg(list : TAsmList;tosize : tdef;a : aint;register : tregister);override;
      procedure a_load_const_ref(list : TAsmList;tosize : tdef;a : aint;const ref : treference);override;
      procedure a_load_reg_ref(list : TAsmList;fromsize, tosize : tdef;register : tregister;const ref : treference);override;
      procedure a_load_reg_reg(list : TAsmList;fromsize, tosize : tdef;reg1,reg2 : tregister);override;
      procedure a_load_ref_reg(list : TAsmList;fromsize, tosize : tdef;const ref : treference;register : tregister);override;
      procedure a_load_ref_ref(list : TAsmList;fromsize, tosize : tdef;const sref : treference;const dref : treference);override;
      procedure a_loadaddr_ref_reg(list : TAsmList;fromsize, tosize : tdef;const ref : treference;r : tregister);override;

      procedure a_op_const_reg(list: TAsmList; Op: TOpCG; size: tdef; a: Aint; reg: TRegister); override;
      procedure a_op_const_reg_reg(list: TAsmList; op: TOpCg; size: tdef; a: aint; src, dst: tregister); override;
      procedure a_op_const_ref(list: TAsmList; Op: TOpCG; size: tdef; a: Aint; const ref: TReference); override;

      procedure a_op_ref_reg(list: TAsmList; Op: TOpCG; size: tdef; const ref: TReference; reg: TRegister); override;
      procedure a_op_reg_reg_reg(list: TAsmList; op: TOpCg; size: tdef; src1, src2, dst: tregister); override;
      procedure a_op_reg_reg(list: TAsmList; Op: TOpCG; size: tdef; reg1, reg2: TRegister); override;

      procedure a_cmp_const_ref_label(list: TAsmList; size: tdef; cmp_op: topcmp; a: aint; const ref: treference; l: tasmlabel); override;
      procedure a_cmp_const_reg_label(list: TAsmList; size: tdef; cmp_op: topcmp; a: aint; reg: tregister; l: tasmlabel); override;
      procedure a_cmp_ref_reg_label(list: TAsmList; size: tdef; cmp_op: topcmp; const ref: treference; reg: tregister; l: tasmlabel); override;
      procedure a_cmp_reg_ref_label(list: TAsmList; size: tdef; cmp_op: topcmp; reg: tregister; const ref: treference; l: tasmlabel); override;
      procedure a_cmp_reg_reg_label(list: TAsmList; size: tdef; cmp_op: topcmp; reg1, reg2: tregister; l: tasmlabel); override;

      procedure a_jmp_always(list : TAsmList;l: tasmlabel); override;

      procedure g_concatcopy(list : TAsmList;size: tdef; const source,dest : treference);override;
      procedure g_copyshortstring(list : TAsmList;const source,dest : treference;strdef:tstringdef);override;

      procedure a_loadfpu_ref_ref(list: TAsmList; fromsize, tosize: tdef; const ref1, ref2: treference); override;
      procedure a_loadfpu_ref_reg(list: TAsmList; fromsize, tosize: tdef; const ref: treference; reg: tregister); override;
      procedure a_loadfpu_reg_ref(list: TAsmList; fromsize, tosize: tdef; reg: tregister; const ref: treference); override;
      procedure a_loadfpu_reg_reg(list: TAsmList; fromsize, tosize: tdef; reg1, reg2: tregister); override;

      procedure g_proc_entry(list : TAsmList;localsize : longint;nostackframe:boolean); override;
      procedure g_proc_exit(list : TAsmList;parasize:longint;nostackframe:boolean); override;

      procedure gen_load_return_value(list:TAsmList);override;
      procedure record_generated_code_for_procdef(pd: tprocdef; code, data: TAsmList); override;

      procedure g_incrrefcount(list : TAsmList;t: tdef; const ref: treference);override;
      procedure g_decrrefcount(list : TAsmList;t: tdef; const ref: treference);override;
      procedure g_array_rtti_helper(list: TAsmList; t: tdef; const ref: treference; const highloc: tlocation; const name: string); override;
      procedure g_initialize(list : TAsmList;t : tdef;const ref : treference);override;
      procedure g_finalize(list : TAsmList;t : tdef;const ref : treference);override;

      procedure location_get_data_ref(list:TAsmList;def: tdef; const l:tlocation;var ref:treference;loadref:boolean; alignment: longint);override;
      procedure g_copyvaluepara_openarray(list: TAsmList; const ref: treference; const lenloc: tlocation; arrdef: tarraydef; destreg: tregister); override;
      procedure g_releasevaluepara_openarray(list: TAsmList; arrdef: tarraydef; const l: tlocation); override;

      procedure gen_initialize_code(list: TAsmList); override;

      procedure gen_entry_code(list: TAsmList); override;
      procedure gen_exit_code(list: TAsmList); override;

      { JVM-specific routines }

      procedure a_load_stack_reg(list : TAsmList;size: tdef;reg: tregister);
      { extra_slots are the slots that are used by the reference, and that
        will be removed by the store operation }
      procedure a_load_stack_ref(list : TAsmList;size: tdef;const ref: treference;extra_slots: longint);
      procedure a_load_reg_stack(list : TAsmList;size: tdef;reg: tregister);
      { extra_slots are the slots that are used by the reference, and that
        will be removed by the load operation }
      procedure a_load_ref_stack(list : TAsmList;size: tdef;const ref: treference;extra_slots: longint);
      procedure a_load_const_stack(list : TAsmList;size: tdef;a :aint; typ: TRegisterType);

      procedure a_load_stack_loc(list : TAsmList;size: tdef;const loc: tlocation);
      procedure a_load_loc_stack(list : TAsmList;size: tdef;const loc: tlocation);

      procedure a_loadfpu_const_stack(list : TAsmList;size: tdef;a :double);

      procedure a_op_stack(list : TAsmList;op: topcg; size: tdef; trunc32: boolean);
      procedure a_op_const_stack(list : TAsmList;op: topcg; size: tdef;a : aint);
      procedure a_op_reg_stack(list : TAsmList;op: topcg; size: tdef;reg: tregister);
      procedure a_op_ref_stack(list : TAsmList;op: topcg; size: tdef;const ref: treference);
      procedure a_op_loc_stack(list : TAsmList;op: topcg; size: tdef;const loc: tlocation);

      procedure g_reference_loc(list: TAsmList; def: tdef; const fromloc: tlocation; out toloc: tlocation); override;

      { assumes that initdim dimensions have already been pushed on the
        evaluation stack, and creates a new array of type arrdef with these
        dimensions }
      procedure g_newarray(list : TAsmList; arrdef: tdef; initdim: longint);
      { gets the length of the array whose reference is stored in arrloc,
        and puts it on the evaluation stack }
      procedure g_getarraylen(list : TAsmList; const arrloc: tlocation);

      { this routine expects that all values are already massaged into the
        required form (sign bits xor'ed for gt/lt comparisons for OS_32/OS_64,
        see http://stackoverflow.com/questions/4068973/c-performing-signed-comparison-in-unsigned-variables-without-casting ) }
      procedure a_cmp_stack_label(list : TAsmlist; size: tdef; cmp_op: topcmp; lab: tasmlabel);
      { these 2 routines perform the massaging expected by the previous one }
      procedure maybe_adjust_cmp_stackval(list : TAsmlist; size: tdef; cmp_op: topcmp);
      function maybe_adjust_cmp_constval(size: tdef; cmp_op: topcmp; a: aint): aint;
      { truncate/sign extend after performing operations on values < 32 bit
        that may have overflowed outside the range }
      procedure maybe_adjust_op_result(list: TAsmList; op: TOpCg; size: tdef);

      { performs sign/zero extension as required }
      procedure resize_stack_int_val(list: TAsmList;fromsize,tosize: tcgsize; forarraystore: boolean);

      property maxevalstackheight: longint read fmaxevalstackheight;

      procedure gen_initialize_fields_code(list:TAsmList);
     protected
      procedure allocate_implicit_structs_for_st_with_base_ref(list: TAsmList; st: tsymtable; const ref: treference; allocvartyp: tsymtyp);
      procedure allocate_implicit_struct_with_base_ref(list: TAsmList; vs: tabstractvarsym; ref: treference);
      procedure gen_load_uninitialized_function_result(list: TAsmList; pd: tprocdef; resdef: tdef; const resloc: tcgpara); override;

      procedure inittempvariables(list:TAsmList);override;


      { in case of an array, the array base address and index have to be
        put on the evaluation stack before the stored value; similarly, for
        fields the self pointer has to be loaded first. Also checks whether
        the reference is valid. If dup is true, the necessary values are stored
        twice. Returns how many stack slots have been consumed, disregarding
        the "dup". }
      function prepare_stack_for_ref(list: TAsmList; const ref: treference; dup: boolean): longint;
      { return the load/store opcode to load/store from/to ref; if the result
        has to be and'ed after a load to get the final value, that constant
        is returned in finishandval (otherwise that value is set to -1) }
      function loadstoreopcref(def: tdef; isload: boolean; const ref: treference; out finishandval: aint): tasmop;
      { return the load/store opcode to load/store from/to reg; if the result
        has to be and'ed after a load to get the final value, that constant
        is returned in finishandval (otherwise that value is set to -1) }
      function loadstoreopc(def: tdef; isload, isarray: boolean; out finishandval: aint): tasmop;
      procedure resizestackfpuval(list: TAsmList; fromsize, tosize: tcgsize);
      { in case of an OS_32 OP_DIV, we have to use an OS_S64 OP_IDIV because the
        JVM does not support unsigned divisions }
      procedure maybepreparedivu32(list: TAsmList; var op: topcg; size: tdef; out isdivu32: boolean);
      { common implementation of a_call_* }
      procedure a_call_name_intern(list : TAsmList;pd : tprocdef;const s : string; inheritedcall: boolean);

      { concatcopy helpers }
      procedure concatcopy_normal_array(list: TAsmList; size: tdef; const source, dest: treference);
      procedure concatcopy_record(list: TAsmList; size: tdef; const source, dest: treference);
      procedure concatcopy_shortstring(list: TAsmList; size: tdef; const source, dest: treference);

      { generate a call to a routine in the system unit }
      procedure g_call_system_proc(list: TAsmList; const procname: string);
    end;

  procedure create_hlcodegen;


  const
    opcmp2if: array[topcmp] of tasmop = (A_None,
      a_ifeq,a_ifgt,a_iflt,a_ifge,a_ifle,
      a_ifne,a_ifle,a_iflt,a_ifge,a_ifgt);

implementation

  uses
    verbose,cutils,globals,fmodule,
    defutil,
    aasmtai,aasmcpu,
    symtable,jvmdef,
    procinfo,cgcpu,tgobj;

  const
    TOpCG2IAsmOp : array[topcg] of TAsmOp=(                       { not = xor -1 }
      A_None,A_None,a_iadd,a_iand,A_none,a_idiv,a_imul,a_imul,a_ineg,A_None,a_ior,a_ishr,a_ishl,a_iushr,a_isub,a_ixor,A_None,A_None
    );
    TOpCG2LAsmOp : array[topcg] of TAsmOp=(                       { not = xor -1 }
      A_None,A_None,a_ladd,a_land,A_none,a_ldiv,a_lmul,a_lmul,a_lneg,A_None,a_lor,a_lshr,a_lshl,a_lushr,a_lsub,a_lxor,A_None,A_None
    );

  constructor thlcgjvm.create;
    begin
      fevalstackheight:=0;
      fmaxevalstackheight:=0;
    end;

  procedure thlcgjvm.incstack(list: TasmList;slots: longint);
    begin
      if slots=0 then
        exit;
      inc(fevalstackheight,slots);
      if (fevalstackheight>fmaxevalstackheight) then
        fmaxevalstackheight:=fevalstackheight;
      if cs_asm_regalloc in current_settings.globalswitches then
        list.concat(tai_comment.Create(strpnew('allocated '+tostr(slots)+', stack height = '+tostr(fevalstackheight))));
    end;

  procedure thlcgjvm.decstack(list: TAsmList;slots: longint);
    begin
      if slots=0 then
        exit;
      dec(fevalstackheight,slots);
      if (fevalstackheight<0) and
         not(cs_no_regalloc in current_settings.globalswitches) then
        internalerror(2010120501);
      if cs_asm_regalloc in current_settings.globalswitches then
        list.concat(tai_comment.Create(strpnew('    freed '+tostr(slots)+', stack height = '+tostr(fevalstackheight))));
    end;

  function thlcgjvm.def2regtyp(def: tdef): tregistertype;
    begin
      case def.typ of
        { records are implemented via classes }
        recorddef:
          result:=R_ADDRESSREGISTER;
        setdef:
          if is_smallset(def) then
            result:=R_INTREGISTER
          else
            result:=R_ADDRESSREGISTER;
        { shortstrings are implemented via classes }
        else if is_shortstring(def) then
          result:=R_ADDRESSREGISTER
        else
          result:=inherited;
      end;
    end;

  procedure thlcgjvm.a_call_name(list: TAsmList; pd: tprocdef; const s: string; weak: boolean);
    begin
      a_call_name_intern(list,pd,s,false);
    end;

  procedure thlcgjvm.a_call_name_inherited(list: TAsmList; pd: tprocdef; const s: string);
    begin
      a_call_name_intern(list,pd,s,true);
    end;

  procedure thlcgjvm.a_load_const_stack(list : TAsmList;size : tdef;a : aint; typ: TRegisterType);
    const
      int2opc: array[-1..5] of tasmop = (a_iconst_m1,a_iconst_0,a_iconst_1,
        a_iconst_2,a_iconst_3,a_iconst_4,a_iconst_5);
    begin
      case typ of
        R_INTREGISTER:
          begin
            case def_cgsize(size) of
              OS_8,OS_16,OS_32,
              OS_S8,OS_S16,OS_S32:
                begin
                  { convert cardinals to longints }
                  a:=longint(a);
                  if (a>=-1) and
                     (a<=5) then
                    list.concat(taicpu.op_none(int2opc[a]))
                  else if (a>=low(shortint)) and
                          (a<=high(shortint)) then
                    list.concat(taicpu.op_const(a_bipush,a))
                  else if (a>=low(smallint)) and
                          (a<=high(smallint)) then
                    list.concat(taicpu.op_const(a_sipush,a))
                  else
                    list.concat(taicpu.op_const(a_ldc,a));
                end;
              OS_64,OS_S64:
                begin
                  case a of
                    0:
                      list.concat(taicpu.op_none(a_lconst_0));
                    1:
                      list.concat(taicpu.op_none(a_lconst_1));
                    else
                      list.concat(taicpu.op_const(a_ldc2_w,a));
                  end;
                  incstack(list,1);
                end;
              else
                internalerror(2010110702);
            end;
          end;
        R_ADDRESSREGISTER:
          begin
            if a<>0 then
              internalerror(2010110701);
            list.concat(taicpu.op_none(a_aconst_null));
          end;
        else
          internalerror(2010110703);
      end;
      incstack(list,1);
    end;

  procedure thlcgjvm.a_load_stack_loc(list: TAsmList; size: tdef; const loc: tlocation);
    begin
      case loc.loc of
        LOC_REGISTER,LOC_CREGISTER,
        LOC_FPUREGISTER,LOC_CFPUREGISTER:
          a_load_stack_reg(list,size,loc.register);
        LOC_REFERENCE:
          a_load_stack_ref(list,size,loc.reference,prepare_stack_for_ref(list,loc.reference,false));
        else
          internalerror(2011020501);
      end;
    end;

  procedure thlcgjvm.a_load_loc_stack(list: TAsmList;size: tdef;const loc: tlocation);
    begin
      case loc.loc of
        LOC_REGISTER,LOC_CREGISTER,
        LOC_FPUREGISTER,LOC_CFPUREGISTER:
          a_load_reg_stack(list,size,loc.register);
        LOC_REFERENCE,LOC_CREFERENCE:
          a_load_ref_stack(list,size,loc.reference,prepare_stack_for_ref(list,loc.reference,false));
        LOC_CONSTANT:
          a_load_const_stack(list,size,loc.value,def2regtyp(size));
        else
          internalerror(2011010401);
      end;
    end;

  procedure thlcgjvm.a_loadfpu_const_stack(list: TAsmList; size: tdef; a: double);
    begin
      case tfloatdef(size).floattype of
        s32real:
          begin
            if a=0.0 then
              list.concat(taicpu.op_none(a_fconst_0))
            else if a=1.0 then
              list.concat(taicpu.op_none(a_fconst_1))
            else if a=2.0 then
              list.concat(taicpu.op_none(a_fconst_2))
            else
              list.concat(taicpu.op_single(a_ldc,a));
            incstack(list,1);
          end;
        s64real:
          begin
            if a=0.0 then
              list.concat(taicpu.op_none(a_dconst_0))
            else if a=1.0 then
              list.concat(taicpu.op_none(a_dconst_1))
            else
              list.concat(taicpu.op_double(a_ldc2_w,a));
            incstack(list,2);
          end
        else
          internalerror(2011010501);
      end;
    end;

  procedure thlcgjvm.a_op_stack(list: TAsmList; op: topcg; size: tdef; trunc32: boolean);
    var
      cgsize: tcgsize;
    begin
      if not trunc32 then
        cgsize:=def_cgsize(size)
      else
        begin
          resize_stack_int_val(list,OS_32,OS_S64,false);
          cgsize:=OS_S64;
        end;
      case cgsize of
        OS_8,OS_S8,
        OS_16,OS_S16,
        OS_32,OS_S32:
          begin
            { not = xor 1 for boolean, xor -1 for the rest}
            if op=OP_NOT then
              begin
                if not is_pasbool(size) then
                  a_load_const_stack(list,s32inttype,high(cardinal),R_INTREGISTER)
                else
                  a_load_const_stack(list,size,1,R_INTREGISTER);
                op:=OP_XOR;
              end;
            if TOpCG2IAsmOp[op]=A_None then
              internalerror(2010120532);
            list.concat(taicpu.op_none(TOpCG2IAsmOp[op]));
            maybe_adjust_op_result(list,op,size);
            if op<>OP_NEG then
              decstack(list,1);
          end;
        OS_64,OS_S64:
          begin
            { unsigned 64 bit division must be done via a helper }
            if op=OP_DIV then
              internalerror(2010120530);
            { not = xor -1 }
            if op=OP_NOT then
              begin
                a_load_const_stack(list,s64inttype,-1,R_INTREGISTER);
                op:=OP_XOR;
              end;
            if TOpCG2LAsmOp[op]=A_None then
              internalerror(2010120533);
            list.concat(taicpu.op_none(TOpCG2LAsmOp[op]));
            case op of
              OP_NOT,
              OP_NEG:
                ;
              { the second argument here is an int rather than a long }
              OP_SHL,OP_SHR,OP_SAR:
                decstack(list,1);
              else
                decstack(list,2);
            end;
          end;
        else
          internalerror(2010120531);
      end;
      if trunc32 then
        begin
          list.concat(taicpu.op_none(a_l2i));
          decstack(list,1);
        end;
    end;

  procedure thlcgjvm.a_op_const_stack(list: TAsmList;op: topcg;size: tdef;a: aint);
    var
      trunc32: boolean;
    begin
      { use "integer to (wide)char" narrowing opcode for "and 65535" }
      if (op=OP_AND) and
         (def_cgsize(size) in [OS_16,OS_S16,OS_32,OS_S32]) and
         (a=65535) then
        list.concat(taicpu.op_none(a_i2c))
      else
        begin
          maybepreparedivu32(list,op,size,trunc32);
          case op of
            OP_NEG,OP_NOT:
              internalerror(2011010801);
            OP_SHL,OP_SHR,OP_SAR:
              { the second argument here is an int rather than a long }
              a_load_const_stack(list,s32inttype,a,R_INTREGISTER);
            else
              a_load_const_stack(list,size,a,R_INTREGISTER);
          end;
          a_op_stack(list,op,size,trunc32);
        end;
    end;

  procedure thlcgjvm.a_op_reg_stack(list: TAsmList; op: topcg; size: tdef; reg: tregister);
    var
      trunc32: boolean;
    begin
      maybepreparedivu32(list,op,size,trunc32);
      case op of
        OP_NEG,OP_NOT:
          ;
        OP_SHL,OP_SHR,OP_SAR:
          if not is_64bitint(size) then
            a_load_reg_stack(list,size,reg)
          else
            begin
              { the second argument here is an int rather than a long }
              if getsubreg(reg)=R_SUBQ then
                internalerror(2011010802);
              a_load_reg_stack(list,s32inttype,reg)
            end
        else
          a_load_reg_stack(list,size,reg);
      end;
      a_op_stack(list,op,size,trunc32);
    end;

  procedure thlcgjvm.a_op_ref_stack(list: TAsmList; op: topcg; size: tdef; const ref: treference);
    var
      trunc32: boolean;
    begin
      { ref must not be the stack top, because that may indicate an error
        (it means that we will perform an operation of the stack top onto
         itself, so that means the two values have been loaded manually prior
         to calling this routine, instead of letting this routine load one of
         them; if something like that is needed, call a_op_stack() directly) }
      if ref.base=NR_EVAL_STACK_BASE then
        internalerror(2010121102);
      maybepreparedivu32(list,op,size,trunc32);
      case op of
        OP_NEG,OP_NOT:
          ;
        OP_SHL,OP_SHR,OP_SAR:
          begin
            if not is_64bitint(size) then
              a_load_ref_stack(list,size,ref,prepare_stack_for_ref(list,ref,false))
            else
              a_load_ref_stack(list,s32inttype,ref,prepare_stack_for_ref(list,ref,false));
          end;
        else
          a_load_ref_stack(list,size,ref,prepare_stack_for_ref(list,ref,false));
      end;
      a_op_stack(list,op,size,trunc32);
    end;

  procedure thlcgjvm.a_op_loc_stack(list: TAsmList; op: topcg; size: tdef; const loc: tlocation);
    begin
      case loc.loc of
        LOC_REGISTER,LOC_CREGISTER:
          a_op_reg_stack(list,op,size,loc.register);
        LOC_REFERENCE,LOC_CREFERENCE:
          a_op_ref_stack(list,op,size,loc.reference);
        LOC_CONSTANT:
          a_op_const_stack(list,op,size,loc.value);
        else
          internalerror(2011011415)
      end;
    end;

  procedure thlcgjvm.g_reference_loc(list: TAsmList; def: tdef; const fromloc: tlocation; out toloc: tlocation);

    procedure handle_reg_move(regsize: tdef; const fromreg: tregister; out toreg: tregister; regtyp: tregistertype);
      begin
        case regtyp of
          R_INTREGISTER:
            toreg:=getintregister(list,regsize);
          R_ADDRESSREGISTER:
            toreg:=getaddressregister(list,regsize);
          R_FPUREGISTER:
            toreg:=getfpuregister(list,regsize);
        end;
        a_load_reg_reg(list,regsize,regsize,fromreg,toreg);
      end;

    begin
      toloc:=fromloc;
      case fromloc.loc of
        { volatile location, can't get a permanent reference }
        LOC_REGISTER,
        LOC_FPUREGISTER:
          internalerror(2011031406);
        LOC_CONSTANT:
          { finished }
          ;
        LOC_CREGISTER:
          handle_reg_move(def,fromloc.reference.index,toloc.reference.index,R_INTREGISTER);
        LOC_CFPUREGISTER:
          handle_reg_move(def,fromloc.reference.index,toloc.reference.index,R_FPUREGISTER);
        { although LOC_CREFERENCE cannot be an lvalue, we may want to take a
          reference to such a location for multiple reading }
        LOC_CREFERENCE,
        LOC_REFERENCE:
          begin
            if (fromloc.reference.base<>NR_NO) and
               (fromloc.reference.base<>current_procinfo.framepointer) and
               (fromloc.reference.base<>NR_STACK_POINTER_REG) then
              handle_reg_move(java_jlobject,fromloc.reference.base,toloc.reference.base,R_ADDRESSREGISTER);
            case fromloc.reference.arrayreftype of
              art_indexreg:
                begin
                  { all array indices in Java are 32 bit ints }
                  handle_reg_move(s32inttype,fromloc.reference.index,toloc.reference.index,R_INTREGISTER);
                end;
              art_indexref:
                begin
                  if (fromloc.reference.indexbase<>NR_NO) and
                     (fromloc.reference.indexbase<>NR_STACK_POINTER_REG) then
                    handle_reg_move(s32inttype,fromloc.reference.indexbase,toloc.reference.indexbase,R_ADDRESSREGISTER);
                end;
            end;
          end;
        else
          internalerror(2011031407);
      end;
    end;

  procedure thlcgjvm.g_newarray(list: TAsmList; arrdef: tdef; initdim: longint);
    var
      recref: treference;
      elemdef: tdef;
      i: longint;
      mangledname: string;
      opc: tasmop;
      primitivetype: boolean;
    begin
      elemdef:=arrdef;
      if initdim>1 then
        begin
          { multianewarray typedesc ndim }
          list.concat(taicpu.op_sym_const(a_multianewarray,
            current_asmdata.RefAsmSymbol(jvmarrtype(elemdef,primitivetype)),initdim));
          { has to be a multi-dimensional array type }
          if primitivetype then
            internalerror(2011012207);
        end
      else
        begin
          { for primitive types:
              newarray typedesc
            for reference types:
              anewarray typedesc
          }
          { get the type of the elements of the array we are creating }
          elemdef:=tarraydef(arrdef).elementdef;
          mangledname:=jvmarrtype(elemdef,primitivetype);
          if primitivetype then
            opc:=a_newarray
          else
            opc:=a_anewarray;
          list.concat(taicpu.op_sym(opc,current_asmdata.RefAsmSymbol(mangledname)));
        end;
      { all dimensions are removed from the stack, an array reference is
        added }
      decstack(list,initdim-1);
      { in case of an array of records or shortstrings, initialise }
      elemdef:=tarraydef(arrdef).elementdef;
      for i:=1 to pred(initdim) do
        elemdef:=tarraydef(elemdef).elementdef;
      if (elemdef.typ=recorddef) or
         is_shortstring(elemdef) then
        begin
          { duplicate array reference }
          list.concat(taicpu.op_none(a_dup));
          incstack(list,1);
          a_load_const_stack(list,s32inttype,initdim-1,R_INTREGISTER);
          if elemdef.typ=recorddef then
            begin
              tg.gethltemp(list,elemdef,elemdef.size,tt_persistent,recref);
              a_load_ref_stack(list,elemdef,recref,prepare_stack_for_ref(list,recref,false));
              g_call_system_proc(list,'fpc_initialize_array_record');
              tg.ungettemp(list,recref);
            end
          else
            begin
              a_load_const_stack(list,u8inttype,tstringdef(elemdef).len,R_INTREGISTER);
              g_call_system_proc(list,'fpc_initialize_array_shortstring');
            end;
          decstack(list,3);
        end;
    end;

  procedure thlcgjvm.g_getarraylen(list: TAsmList; const arrloc: tlocation);
    var
      nillab,endlab: tasmlabel;
    begin
      { inline because we have to use the arraylength opcode, which
        cannot be represented directly in Pascal. Even though the JVM
        supports allocated arrays with length=0, we still also have to
        check for nil pointers because even if FPC always generates
        allocated empty arrays under all circumstances, external Java
        code could pass in nil pointers.

        Note that this means that assigned(arr) can be different from
        length(arr)<>0 for dynamic arrays when targeting the JVM.
      }
      current_asmdata.getjumplabel(nillab);
      current_asmdata.getjumplabel(endlab);

      { if assigned(arr) ... }
      a_load_loc_stack(list,java_jlobject,arrloc);
      list.concat(taicpu.op_none(a_dup));
      incstack(list,1);
      list.concat(taicpu.op_none(a_aconst_null));
      incstack(list,1);
      list.concat(taicpu.op_sym(a_if_acmpeq,nillab));
      decstack(list,2);

      { ... then result:=arraylength(arr) ... }
      list.concat(taicpu.op_none(a_arraylength));
      a_jmp_always(list,endlab);

      { ... else result:=0 }
      a_label(list,nillab);
      list.concat(taicpu.op_none(a_pop));
      decstack(list,1);
      list.concat(taicpu.op_none(a_iconst_0));
      incstack(list,1);

      a_label(list,endlab);
    end;

    procedure thlcgjvm.a_cmp_stack_label(list: TAsmlist; size: tdef; cmp_op: topcmp; lab: tasmlabel);
      const
        opcmp2icmp: array[topcmp] of tasmop = (A_None,
          a_if_icmpeq,a_if_icmpgt,a_if_icmplt,a_if_icmpge,a_if_icmple,
          a_if_icmpne,a_if_icmple,a_if_icmplt,a_if_icmpge,a_if_icmpgt);
      var
        cgsize: tcgsize;
      begin
        case def2regtyp(size) of
          R_INTREGISTER:
            begin
              cgsize:=def_cgsize(size);
              case cgsize of
                OS_S8,OS_8,
                OS_16,OS_S16,
                OS_S32,OS_32:
                  begin
                    list.concat(taicpu.op_sym(opcmp2icmp[cmp_op],lab));
                    decstack(list,2);
                  end;
                OS_64,OS_S64:
                  begin
                    list.concat(taicpu.op_none(a_lcmp));
                    decstack(list,3);
                    list.concat(taicpu.op_sym(opcmp2if[cmp_op],lab));
                    decstack(list,1);
                  end;
                else
                  internalerror(2010120538);
              end;
            end;
          R_ADDRESSREGISTER:
            begin
              case cmp_op of
                OC_EQ:
                  list.concat(taicpu.op_sym(a_if_acmpeq,lab));
                OC_NE:
                  list.concat(taicpu.op_sym(a_if_acmpne,lab));
                else
                  internalerror(2010120537);
              end;
              decstack(list,2);
            end;
          else
            internalerror(2010120538);
        end;
      end;

    procedure thlcgjvm.maybe_adjust_cmp_stackval(list: TAsmlist; size: tdef; cmp_op: topcmp);
      begin
        if (cmp_op in [OC_EQ,OC_NE]) or
           (def2regtyp(size)<>R_INTREGISTER) then
          exit;
        { http://stackoverflow.com/questions/4068973/c-performing-signed-comparison-in-unsigned-variables-without-casting }
        case def_cgsize(size) of
          OS_32:
            a_op_const_stack(list,OP_XOR,size,cardinal($80000000));
          OS_64:
            a_op_const_stack(list,OP_XOR,size,aint($8000000000000000));
        end;
      end;

    function thlcgjvm.maybe_adjust_cmp_constval(size: tdef; cmp_op: topcmp; a: aint): aint;
      begin
        result:=a;
        if (cmp_op in [OC_EQ,OC_NE]) or
           (def2regtyp(size)<>R_INTREGISTER) then
          exit;
        case def_cgsize(size) of
          OS_32:
            result:=a xor cardinal($80000000);
          OS_64:
            result:=a xor aint($8000000000000000);
        end;
      end;

    procedure thlcgjvm.maybe_adjust_op_result(list: TAsmList; op: TOpCg; size: tdef);
      const
        overflowops = [OP_MUL,OP_SHL,OP_ADD,OP_SUB,OP_NOT,OP_NEG];
      begin
        if (op in overflowops) and
           (def_cgsize(size) in [OS_8,OS_S8,OS_16,OS_S16]) then
          resize_stack_int_val(list,OS_S32,def_cgsize(size),false);
      end;

  procedure thlcgjvm.gen_load_uninitialized_function_result(list: TAsmList; pd: tprocdef; resdef: tdef; const resloc: tcgpara);
    begin
      { constructors don't return anything in Java }
      if pd.proctypeoption=potype_constructor then
        exit;
      { must return a value of the correct type on the evaluation stack }
      case def2regtyp(resdef) of
        R_INTREGISTER,
        R_ADDRESSREGISTER:
          a_load_const_cgpara(list,resdef,0,resloc);
        R_FPUREGISTER:
          case tfloatdef(resdef).floattype of
            s32real:
             list.concat(taicpu.op_none(a_fconst_0));
            s64real:
             list.concat(taicpu.op_none(a_dconst_0));
            else
              internalerror(2011010302);
          end
        else
          internalerror(2011010301);
      end;
    end;

  procedure thlcgjvm.inittempvariables(list: TAsmList);
    begin
      { these are automatically initialised when allocated if necessary }
    end;

  function thlcgjvm.prepare_stack_for_ref(list: TAsmList; const ref: treference; dup: boolean): longint;
    var
      href: treference;
    begin
      result:=0;
      { fake location that indicates the value is already on the stack? }
      if (ref.base=NR_EVAL_STACK_BASE) then
        exit;
      if ref.arrayreftype=art_none then
        begin
          { non-array accesses cannot have an index reg }
          if ref.index<>NR_NO then
            internalerror(2010120509);
          if (ref.base<>NR_NO) then
            begin
              if (ref.base<>NR_STACK_POINTER_REG) then
                begin
                  { regular field -> load self on the stack }
                  a_load_reg_stack(list,voidpointertype,ref.base);
                  if dup then
                    begin
                      list.concat(taicpu.op_none(a_dup));
                      incstack(list,1);
                    end;
                  { field name/type encoded in symbol, no index/offset }
                  if not assigned(ref.symbol) or
                     (ref.offset<>0) then
                    internalerror(2010120524);
                  result:=1;
                end
              else
                begin
                  { local variable -> offset encoded in opcode and nothing to
                    do here, except for checking that it's a valid reference }
                  if assigned(ref.symbol) then
                    internalerror(2010120523);
                end;
            end
          else
            begin
              { static field -> nothing to do here, except for validity check }
              if not assigned(ref.symbol) or
                 (ref.offset<>0) then
                internalerror(2010120525);
            end;
        end
      else
        begin
          { arrays have implicit dereference -> pointer to array must have been
            loaded into base reg }
          if (ref.base=NR_NO) or
             (ref.base=NR_STACK_POINTER_REG) then
            internalerror(2010120511);
          if assigned(ref.symbol) then
            internalerror(2010120512);

          { stack: ... -> ..., arrayref, index }
          { load array base address }
          a_load_reg_stack(list,voidpointertype,ref.base);
          { index can either be in a register, or located in a simple memory
            location (since we have to load it anyway) }
          case ref.arrayreftype of
            art_indexreg:
              begin
                if ref.index=NR_NO then
                  internalerror(2010120513);
                { all array indices in Java are 32 bit ints }
                a_load_reg_stack(list,s32inttype,ref.index);
              end;
            art_indexref:
              begin
                reference_reset_base(href,ref.indexbase,ref.indexoffset,4);
                href.symbol:=ref.indexsymbol;
                a_load_ref_stack(list,s32inttype,href,prepare_stack_for_ref(list,href,false));
              end;
            art_indexconst:
              begin
                a_load_const_stack(list,s32inttype,ref.indexoffset,R_INTREGISTER);
              end;
            else
              internalerror(2011012001);
          end;
          { adjustment of the index }
          if ref.offset<>0 then
            a_op_const_stack(list,OP_ADD,s32inttype,ref.offset);
          if dup then
            begin
              list.concat(taicpu.op_none(a_dup2));
              incstack(list,2);
            end;
          result:=2;
        end;
    end;

  procedure thlcgjvm.a_load_const_reg(list: TAsmList; tosize: tdef; a: aint; register: tregister);
    begin
      a_load_const_stack(list,tosize,a,def2regtyp(tosize));
      a_load_stack_reg(list,tosize,register);
    end;

  procedure thlcgjvm.a_load_const_ref(list: TAsmList; tosize: tdef; a: aint; const ref: treference);
    var
      extra_slots: longint;
    begin
      extra_slots:=prepare_stack_for_ref(list,ref,false);
      a_load_const_stack(list,tosize,a,def2regtyp(tosize));
      a_load_stack_ref(list,tosize,ref,extra_slots);
    end;

  procedure thlcgjvm.a_load_reg_ref(list: TAsmList; fromsize, tosize: tdef; register: tregister; const ref: treference);
    var
      extra_slots: longint;
    begin
      extra_slots:=prepare_stack_for_ref(list,ref,false);
      a_load_reg_stack(list,fromsize,register);
      a_load_stack_ref(list,tosize,ref,extra_slots);
    end;

  procedure thlcgjvm.a_load_reg_reg(list: TAsmList; fromsize, tosize: tdef; reg1, reg2: tregister);
    begin
      a_load_reg_stack(list,fromsize,reg1);
      if def2regtyp(fromsize)=R_INTREGISTER then
        resize_stack_int_val(list,def_cgsize(fromsize),def_cgsize(tosize),false);
      a_load_stack_reg(list,tosize,reg2);
    end;

  procedure thlcgjvm.a_load_ref_reg(list: TAsmList; fromsize, tosize: tdef; const ref: treference; register: tregister);
    var
      extra_slots: longint;
    begin
      extra_slots:=prepare_stack_for_ref(list,ref,false);
      a_load_ref_stack(list,fromsize,ref,extra_slots);

      if def2regtyp(fromsize)=R_INTREGISTER then
        resize_stack_int_val(list,def_cgsize(fromsize),def_cgsize(tosize),false);
      a_load_stack_reg(list,tosize,register);
    end;

  procedure thlcgjvm.a_load_ref_ref(list: TAsmList; fromsize, tosize: tdef; const sref: treference; const dref: treference);
    var
      extra_sslots,
      extra_dslots: longint;
    begin
      { make sure the destination reference is on top, since in the end the
        order has to be "destref, value" -> first create "destref, sourceref" }
      extra_dslots:=prepare_stack_for_ref(list,dref,false);
      extra_sslots:=prepare_stack_for_ref(list,sref,false);
      a_load_ref_stack(list,fromsize,sref,extra_sslots);
      if def2regtyp(fromsize)=R_INTREGISTER then
        resize_stack_int_val(list,def_cgsize(fromsize),def_cgsize(tosize),dref.arrayreftype<>art_none);
      a_load_stack_ref(list,tosize,dref,extra_dslots);
    end;

  procedure thlcgjvm.a_loadaddr_ref_reg(list: TAsmList; fromsize, tosize: tdef; const ref: treference; r: tregister);
    begin
      { only allowed for types that are not implicit pointers in Pascal (in
        that case, ref contains a pointer to the actual data and we simply
        return that pointer) }
      if not jvmimplicitpointertype(fromsize) then
        internalerror(2010120534);
      a_load_ref_reg(list,java_jlobject,java_jlobject,ref,r);
    end;

  procedure thlcgjvm.a_op_const_reg(list: TAsmList; Op: TOpCG; size: tdef; a: Aint; reg: TRegister);
    begin
      a_op_const_reg_reg(list,op,size,a,reg,reg);
    end;

  procedure thlcgjvm.a_op_const_reg_reg(list: TAsmList; op: TOpCg; size: tdef; a: aint; src, dst: tregister);
    begin
      a_load_reg_stack(list,size,src);
      a_op_const_stack(list,op,size,a);
      a_load_stack_reg(list,size,dst);
    end;

  procedure thlcgjvm.a_op_const_ref(list: TAsmList; Op: TOpCG; size: tdef; a: Aint; const ref: TReference);
    var
      extra_slots: longint;
    begin
      extra_slots:=prepare_stack_for_ref(list,ref,true);
      { TODO, here or in peepholeopt: use iinc when possible }
      a_load_ref_stack(list,size,ref,extra_slots);
      a_op_const_stack(list,op,size,a);
      a_load_stack_ref(list,size,ref,extra_slots);
    end;

  procedure thlcgjvm.a_op_ref_reg(list: TAsmList; Op: TOpCG; size: tdef; const ref: TReference; reg: TRegister);
    begin
      a_load_reg_stack(list,size,reg);
      a_op_ref_stack(list,op,size,ref);
      a_load_stack_reg(list,size,reg);
    end;

  procedure thlcgjvm.a_op_reg_reg_reg(list: TAsmList; op: TOpCg; size: tdef; src1, src2, dst: tregister);
    begin
      a_load_reg_stack(list,size,src2);
      a_op_reg_stack(list,op,size,src1);
      a_load_stack_reg(list,size,dst);
    end;

  procedure thlcgjvm.a_op_reg_reg(list: TAsmList; Op: TOpCG; size: tdef; reg1, reg2: TRegister);
    begin
      a_op_reg_reg_reg(list,op,size,reg1,reg2,reg2);
    end;

  procedure thlcgjvm.a_cmp_const_ref_label(list: TAsmList; size: tdef; cmp_op: topcmp; a: aint; const ref: treference; l: tasmlabel);
    begin
      if ref.base<>NR_EVAL_STACK_BASE then
        a_load_ref_stack(list,size,ref,prepare_stack_for_ref(list,ref,false));
      maybe_adjust_cmp_stackval(list,size,cmp_op);
      a_load_const_stack(list,size,maybe_adjust_cmp_constval(size,cmp_op,a),def2regtyp(size));
      a_cmp_stack_label(list,size,cmp_op,l);
    end;

  procedure thlcgjvm.a_cmp_const_reg_label(list: TAsmList; size: tdef; cmp_op: topcmp; a: aint; reg: tregister; l: tasmlabel);
    begin
      a_load_reg_stack(list,size,reg);
      maybe_adjust_cmp_stackval(list,size,cmp_op);
      a_load_const_stack(list,size,maybe_adjust_cmp_constval(size,cmp_op,a),def2regtyp(size));
      a_cmp_stack_label(list,size,cmp_op,l);
    end;

  procedure thlcgjvm.a_cmp_ref_reg_label(list: TAsmList; size: tdef; cmp_op: topcmp; const ref: treference; reg: tregister; l: tasmlabel);
    begin
      a_load_reg_stack(list,size,reg);
      maybe_adjust_cmp_stackval(list,size,cmp_op);
      if ref.base<>NR_EVAL_STACK_BASE then
        a_load_ref_stack(list,size,ref,prepare_stack_for_ref(list,ref,false))
      else
        list.concat(taicpu.op_none(a_swap));
      maybe_adjust_cmp_stackval(list,size,cmp_op);
      a_cmp_stack_label(list,size,cmp_op,l);
    end;

  procedure thlcgjvm.a_cmp_reg_ref_label(list: TAsmList; size: tdef; cmp_op: topcmp; reg: tregister; const ref: treference; l: tasmlabel);
    begin
      if ref.base<>NR_EVAL_STACK_BASE then
        a_load_ref_stack(list,size,ref,prepare_stack_for_ref(list,ref,false));
      maybe_adjust_cmp_stackval(list,size,cmp_op);
      a_load_reg_stack(list,size,reg);
      maybe_adjust_cmp_stackval(list,size,cmp_op);
      a_cmp_stack_label(list,size,cmp_op,l);
    end;

  procedure thlcgjvm.a_cmp_reg_reg_label(list: TAsmList; size: tdef; cmp_op: topcmp; reg1, reg2: tregister; l: tasmlabel);
    begin
      a_load_reg_stack(list,size,reg2);
      maybe_adjust_cmp_stackval(list,size,cmp_op);
      a_load_reg_stack(list,size,reg1);
      maybe_adjust_cmp_stackval(list,size,cmp_op);
      a_cmp_stack_label(list,size,cmp_op,l);
    end;

  procedure thlcgjvm.a_jmp_always(list: TAsmList; l: tasmlabel);
    begin
      list.concat(taicpu.op_sym(a_goto,current_asmdata.RefAsmSymbol(l.name)));
    end;

  procedure thlcgjvm.concatcopy_normal_array(list: TAsmList; size: tdef; const source, dest: treference);
    var
      procname: string;
      eledef: tdef;
      ndim: longint;
      adddefaultlenparas: boolean;
    begin
      { load copy helper parameters on the stack }
      a_load_ref_stack(list,java_jlobject,source,prepare_stack_for_ref(list,source,false));
      a_load_ref_stack(list,java_jlobject,dest,prepare_stack_for_ref(list,dest,false));
      { call copy helper }
      eledef:=tarraydef(size).elementdef;
      ndim:=1;
      adddefaultlenparas:=true;
      case eledef.typ of
        orddef:
          begin
            case torddef(eledef).ordtype of
              pasbool8,s8bit,u8bit,bool8bit,uchar,
              s16bit,u16bit,bool16bit,pasbool16,
              uwidechar,
              s32bit,u32bit,bool32bit,pasbool32,
              s64bit,u64bit,bool64bit,pasbool64,scurrency:
                procname:='FPC_COPY_SHALLOW_ARRAY'
              else
                internalerror(2011020504);
            end;
          end;
        arraydef:
          begin
            { call fpc_setlength_dynarr_multidim with deepcopy=true, and extra
              parameters }
            while (eledef.typ=arraydef) and
                  not is_dynamic_array(eledef) do
              begin
                eledef:=tarraydef(eledef).elementdef;
                inc(ndim)
              end;
            if (ndim=1) then
              procname:='FPC_COPY_SHALLOW_ARRAY'
            else
              begin
                { deepcopy=true }
                a_load_const_stack(list,pasbool8type,1,R_INTREGISTER);
                { ndim }
                a_load_const_stack(list,s32inttype,ndim,R_INTREGISTER);
                { eletype }
                a_load_const_stack(list,cwidechartype,ord(jvmarrtype_setlength(eledef)),R_INTREGISTER);
                adddefaultlenparas:=false;
                procname:='FPC_SETLENGTH_DYNARR_MULTIDIM';
              end;
          end;
        recorddef:
          procname:='FPC_COPY_JRECORD_ARRAY';
        floatdef:
          procname:='FPC_COPY_SHALLOW_ARRAY';
        stringdef:
          if is_shortstring(eledef) then
            procname:='FPC_COPY_JSHORTSTRING_ARRAY'
          else
            procname:='FPC_COPY_SHALLOW_ARRAY';
        setdef,
        variantdef:
          begin
{$ifndef nounsupported}
            procname:='FPC_COPY_SHALLOW_ARRAY';
{$else}
            { todo: make a deep copy via clone... }
            internalerror(2011020505);
{$endif}
          end;
        else
          procname:='FPC_COPY_SHALLOW_ARRAY';
      end;
     if adddefaultlenparas then
       begin
         { -1, -1 means "copy entire array" }
         a_load_const_stack(list,s32inttype,-1,R_INTREGISTER);
         a_load_const_stack(list,s32inttype,-1,R_INTREGISTER);
       end;
     g_call_system_proc(list,procname);
     if ndim=1 then
       begin
         decstack(list,2);
         if adddefaultlenparas then
           decstack(list,2);
       end
     else
       begin
         decstack(list,4);
         { pop return value, must be the same as dest }
         list.concat(taicpu.op_none(a_pop));
         decstack(list,1);
       end;
    end;

    procedure thlcgjvm.concatcopy_record(list: TAsmList; size: tdef; const source, dest: treference);
      var
        srsym: tsym;
        pd: tprocdef;
      begin
        { self }
        a_load_ref_stack(list,size,source,prepare_stack_for_ref(list,source,false));
        { result }
        a_load_ref_stack(list,size,dest,prepare_stack_for_ref(list,dest,false));
        { call fpcDeepCopy helper }
        srsym:=search_struct_member(tabstractrecorddef(size),'FPCDEEPCOPY');
        if not assigned(srsym) or
           (srsym.typ<>procsym) then
          Message1(cg_f_unknown_compilerproc,'FpcRecordBaseType.fpcDeepCopy');
        pd:=tprocdef(tprocsym(srsym).procdeflist[0]);
        a_call_name(list,pd,pd.mangledname,false);
        { both parameters are removed, no function result }
        decstack(list,2);
      end;


    procedure thlcgjvm.concatcopy_shortstring(list: TAsmList; size: tdef; const source, dest: treference);
      var
        srsym: tsym;
        pd: tprocdef;
      begin
        { self }
        a_load_ref_stack(list,size,source,prepare_stack_for_ref(list,source,false));
        { result }
        a_load_ref_stack(list,size,dest,prepare_stack_for_ref(list,dest,false));
        { call fpcDeepCopy helper }
        srsym:=search_struct_member(java_shortstring,'FPCDEEPCOPY');
        if not assigned(srsym) or
           (srsym.typ<>procsym) then
          Message1(cg_f_unknown_compilerproc,'ShortstringClass.FpcDeepCopy');
        pd:=tprocdef(tprocsym(srsym).procdeflist[0]);
        a_call_name(list,pd,pd.mangledname,false);
        { both parameters are removed, no function result }
        decstack(list,2);
      end;


  procedure thlcgjvm.g_concatcopy(list: TAsmList; size: tdef; const source, dest: treference);
    var
      handled: boolean;
    begin
      handled:=false;
      case size.typ of
        arraydef:
          begin
            if not is_dynamic_array(size) then
              begin
                concatcopy_normal_array(list,size,source,dest);
                handled:=true;
              end;
          end;
        recorddef:
          begin
            concatcopy_record(list,size,source,dest);
            handled:=true;
          end;
        stringdef:
          begin
            if is_shortstring(size) then
              begin
                concatcopy_shortstring(list,size,source,dest);
                handled:=true;
              end;
          end;
      end;
      if not handled then
        inherited;
    end;

  procedure thlcgjvm.g_copyshortstring(list: TAsmList; const source, dest: treference; strdef: tstringdef);
    begin
      concatcopy_shortstring(list,strdef,source,dest);
    end;

  procedure thlcgjvm.a_loadfpu_ref_ref(list: TAsmList; fromsize, tosize: tdef; const ref1, ref2: treference);
    var
      dstack_slots: longint;
    begin
      dstack_slots:=prepare_stack_for_ref(list,ref2,false);
      a_load_ref_stack(list,fromsize,ref1,prepare_stack_for_ref(list,ref1,false));
      resizestackfpuval(list,def_cgsize(fromsize),def_cgsize(tosize));
      a_load_stack_ref(list,tosize,ref2,dstack_slots);
    end;

  procedure thlcgjvm.a_loadfpu_ref_reg(list: TAsmList; fromsize, tosize: tdef; const ref: treference; reg: tregister);
    begin
      a_load_ref_stack(list,fromsize,ref,prepare_stack_for_ref(list,ref,false));
      resizestackfpuval(list,def_cgsize(fromsize),def_cgsize(tosize));
      a_load_stack_reg(list,tosize,reg);
    end;

  procedure thlcgjvm.a_loadfpu_reg_ref(list: TAsmList; fromsize, tosize: tdef; reg: tregister; const ref: treference);
    var
      dstack_slots: longint;
    begin
      dstack_slots:=prepare_stack_for_ref(list,ref,false);
      a_load_reg_stack(list,fromsize,reg);
      resizestackfpuval(list,def_cgsize(fromsize),def_cgsize(tosize));
      a_load_stack_ref(list,tosize,ref,dstack_slots);
    end;

  procedure thlcgjvm.a_loadfpu_reg_reg(list: TAsmList; fromsize, tosize: tdef; reg1, reg2: tregister);
    begin
      a_load_reg_stack(list,fromsize,reg1);
      resizestackfpuval(list,def_cgsize(fromsize),def_cgsize(tosize));
      a_load_stack_reg(list,tosize,reg2);
    end;

  procedure thlcgjvm.g_proc_entry(list: TAsmList; localsize: longint; nostackframe: boolean);
    begin
      { the localsize is based on tg.lasttemp -> already in terms of stack
        slots rather than bytes }
      list.concat(tai_directive.Create(asd_jlimit,'locals '+tostr(localsize)));
      { we insert the unit initialisation code afterwards in the proginit code,
        and it uses one stack slot }
      if (current_procinfo.procdef.proctypeoption=potype_proginit) then
        fmaxevalstackheight:=max(1,fmaxevalstackheight);
      list.concat(tai_directive.Create(asd_jlimit,'stack '+tostr(fmaxevalstackheight)));
    end;

  procedure thlcgjvm.g_proc_exit(list: TAsmList; parasize: longint; nostackframe: boolean);
    var
      retdef: tdef;
      opc: tasmop;
    begin
      if current_procinfo.procdef.proctypeoption in [potype_constructor,potype_class_constructor] then
        retdef:=voidtype
      else
        retdef:=current_procinfo.procdef.returndef;
      case retdef.typ of
        orddef:
          case torddef(retdef).ordtype of
            uvoid:
              opc:=a_return;
            s64bit,
            u64bit,
            scurrency:
              opc:=a_lreturn;
            else
              opc:=a_ireturn;
          end;
        floatdef:
          case tfloatdef(retdef).floattype of
            s32real:
              opc:=a_freturn;
            s64real:
              opc:=a_dreturn;
            else
              internalerror(2011010213);
          end;
        else
          opc:=a_areturn;
      end;
      list.concat(taicpu.op_none(opc));
    end;

  procedure thlcgjvm.gen_load_return_value(list: TAsmList);
    begin
      { constructors don't return anything in the jvm }
      if current_procinfo.procdef.proctypeoption in [potype_constructor,potype_class_constructor] then
        exit;
      inherited gen_load_return_value(list);
    end;

  procedure thlcgjvm.record_generated_code_for_procdef(pd: tprocdef; code, data: TAsmList);
    begin
      { add something to the al_procedures list as well, because if all al_*
        lists are empty, the assembler writer isn't called }
      if not code.empty and
         current_asmdata.asmlists[al_procedures].empty then
        current_asmdata.asmlists[al_procedures].concat(tai_align.Create(4));
      pd.exprasmlist:=TAsmList.create;
      pd.exprasmlist.concatlist(code);
      if assigned(data) and
         not data.empty then
        internalerror(2010122801);
    end;

  procedure thlcgjvm.g_incrrefcount(list: TAsmList; t: tdef; const ref: treference);
    begin
      // do nothing
    end;

  procedure thlcgjvm.g_decrrefcount(list: TAsmList; t: tdef; const ref: treference);
    begin
      // do nothing
    end;

  procedure thlcgjvm.g_array_rtti_helper(list: TAsmList; t: tdef; const ref: treference; const highloc: tlocation; const name: string);
    var
      normaldim: longint;
      recref: treference;
    begin
      { only in case of initialisation, we have to set all elements to "empty" }
      if name<>'FPC_INITIALIZE_ARRAY' then
        exit;
      { put array on the stack }
      a_load_ref_stack(list,java_jlobject,ref,prepare_stack_for_ref(list,ref,false));
      { in case it's an open array whose elements are regular arrays, put the
        dimension of the regular arrays on the stack (otherwise pass 0) }
      normaldim:=0;
      while (t.typ=arraydef) and
            not is_dynamic_array(t) do
        begin
          inc(normaldim);
          t:=tarraydef(t).elementdef;
        end;
      a_load_const_stack(list,s32inttype,normaldim,R_INTREGISTER);
      { highloc is invalid, the length is part of the array in Java }
      if is_wide_or_unicode_string(t) then
        g_call_system_proc(list,'fpc_initialize_array_unicodestring')
      else if is_ansistring(t) then
        g_call_system_proc(list,'fpc_initialize_array_ansistring')
      else if is_dynamic_array(t) then
        g_call_system_proc(list,'fpc_initialize_array_dynarr')
      else if is_record(t) then
        begin
          tg.gethltemp(list,t,t.size,tt_persistent,recref);
          a_load_ref_stack(list,t,recref,prepare_stack_for_ref(list,recref,false));
          g_call_system_proc(list,'fpc_initialize_array_record');
          tg.ungettemp(list,recref);
        end
      else
        internalerror(2011031901);
    end;

  procedure thlcgjvm.g_initialize(list: TAsmList; t: tdef; const ref: treference);
    var
      dummyloc: tlocation;
      recref: treference;
    begin
      if (t.typ=arraydef) and
         not is_dynamic_array(t) then
        begin
          dummyloc.loc:=LOC_INVALID;
          g_array_rtti_helper(list,tarraydef(t).elementdef,ref,dummyloc,'FPC_INITIALIZE_ARRAY')
        end
      else if is_record(t) then
        begin
          { create a new, empty record and replace the contents of the old one
            with those of the new one (in the future we can generate a dedicate
            initialization helper) }
          tg.gethltemp(list,t,t.size,tt_persistent,recref);
          g_concatcopy(list,t,recref,ref);
          tg.ungettemp(list,recref);
        end
      else
        a_load_const_ref(list,t,0,ref);
    end;

  procedure thlcgjvm.g_finalize(list: TAsmList; t: tdef; const ref: treference);
    begin
      // do nothing
    end;

  procedure thlcgjvm.location_get_data_ref(list: TAsmList; def: tdef; const l: tlocation; var ref: treference; loadref: boolean; alignment: longint);
    var
      tmploc: tlocation;
    begin
      { This routine is a combination of a generalised a_loadaddr_ref_reg()
        that also works for addresses in registers (in case loadref is false)
        and of a_load_ref_reg (in case loadref is true). It is used for
        a) getting the address of managed types
        b) getting to the actual data of value types that are passed by
           reference by the compiler (and then get a local copy at the caller
           side). Normally, depending on whether this reference is passed in a
           register or reference, we either need a reference with that register
           as base or load the address in that reference and use that as a new
           base.

        Since the JVM cannot take the address of anything, all
        "pass-by-reference" value parameters (which are always aggregate types)
        are already simply the implicit pointer to the data (since arrays,
        records, etc are already internally implicit pointers). This means
        that if "loadref" is true, we must simply return this implicit pointer.
        If it is false, we are supposed the take the address of this implicit
        pointer, which is not possible.

        However, managed types are also implicit pointers in Pascal, so in that
        case "taking the address" again consists of simply returning the
        implicit pointer/current value.
      }
      if not loadref then
        begin
          if not is_managed_type(def) then
            internalerror(2011020601);
        end
      else
        begin
          if not jvmimplicitpointertype(def) then
            internalerror(2011020602);
        end;
      case l.loc of
        LOC_REGISTER,
        LOC_CREGISTER :
          begin
            { the implicit pointer is in a register and has to be in a
              reference -> create a reference and put it there }
            tmploc:=l;
            location_force_mem(list,tmploc,java_jlobject);
            ref:=tmploc.reference;
          end;
        LOC_REFERENCE,
        LOC_CREFERENCE :
          begin
            ref:=l.reference;
          end;
        else
          internalerror(2011020603);
      end;
    end;

  procedure thlcgjvm.g_copyvaluepara_openarray(list: TAsmList; const ref: treference; const lenloc: tlocation; arrdef: tarraydef; destreg: tregister);
    var
      localref: treference;
      arrloc: tlocation;
      stackslots: longint;
    begin
      { temporary reference for passing to concatcopy }
      tg.gethltemp(list,java_jlobject,java_jlobject.size,tt_persistent,localref);
      stackslots:=prepare_stack_for_ref(list,localref,false);
      { create the local copy of the array (lenloc is invalid, get length
        directly from the array) }
      location_reset_ref(arrloc,LOC_REFERENCE,OS_ADDR,sizeof(pint));
      arrloc.reference:=ref;
      g_getarraylen(list,arrloc);
      g_newarray(list,arrdef,1);
      a_load_stack_ref(list,java_jlobject,localref,stackslots);
      { copy the source array to the destination }
      g_concatcopy(list,arrdef,ref,localref);
      { and put the array pointer in the register as expected by the caller }
      a_load_ref_reg(list,java_jlobject,java_jlobject,localref,destreg);
    end;

  procedure thlcgjvm.g_releasevaluepara_openarray(list: TAsmList; arrdef: tarraydef; const l: tlocation);
    begin
      // do nothing, long live garbage collection!
    end;

  procedure thlcgjvm.gen_initialize_code(list: TAsmList);
    var
      ref: treference;
    begin
      { create globals with wrapped types such as arrays/records  }
      case current_procinfo.procdef.proctypeoption of
        potype_unitinit:
          begin
            reference_reset_base(ref,NR_NO,0,1);
            if assigned(current_module.globalsymtable) then
              allocate_implicit_structs_for_st_with_base_ref(list,current_module.globalsymtable,ref,staticvarsym);
            allocate_implicit_structs_for_st_with_base_ref(list,current_module.localsymtable,ref,staticvarsym);
          end;
        potype_class_constructor:
          begin
            { also initialise local variables, if any }
            inherited;
            { initialise class fields }
            reference_reset_base(ref,NR_NO,0,1);
            allocate_implicit_structs_for_st_with_base_ref(list,tabstractrecorddef(current_procinfo.procdef.owner.defowner).symtable,ref,staticvarsym);
          end
        else
          inherited
      end;
    end;

  procedure thlcgjvm.gen_entry_code(list: TAsmList);
    begin
      list.concat(Tai_force_line.Create);
    end;

  procedure thlcgjvm.gen_exit_code(list: TAsmList);
    begin
      { nothing }
    end;

  procedure thlcgjvm.a_load_stack_reg(list: TAsmList; size: tdef; reg: tregister);
    var
      opc: tasmop;
      finishandval: aint;
    begin
      opc:=loadstoreopc(size,false,false,finishandval);
      list.concat(taicpu.op_reg(opc,reg));
      { avoid problems with getting the size of an open array etc }
      if jvmimplicitpointertype(size) then
        size:=java_jlobject;
      decstack(list,1+ord(size.size>4));
    end;

  procedure thlcgjvm.a_load_stack_ref(list: TAsmList; size: tdef; const ref: treference; extra_slots: longint);
    var
      opc: tasmop;
      finishandval: aint;
    begin
      { fake location that indicates the value has to remain on the stack }
      if ref.base=NR_EVAL_STACK_BASE then
        exit;
      opc:=loadstoreopcref(size,false,ref,finishandval);
      if ref.arrayreftype=art_none then
        list.concat(taicpu.op_ref(opc,ref))
      else
        list.concat(taicpu.op_none(opc));
      { avoid problems with getting the size of an open array etc }
      if jvmimplicitpointertype(size) then
        size:=java_jlobject;
      decstack(list,1+ord(size.size>4)+extra_slots);
    end;

  procedure thlcgjvm.a_load_reg_stack(list: TAsmList; size: tdef; reg: tregister);
    var
      opc: tasmop;
      finishandval: aint;
    begin
      opc:=loadstoreopc(size,true,false,finishandval);
      list.concat(taicpu.op_reg(opc,reg));
      { avoid problems with getting the size of an open array etc }
      if jvmimplicitpointertype(size) then
        size:=java_jlobject;
      incstack(list,1+ord(size.size>4));
      if finishandval<>-1 then
        a_op_const_stack(list,OP_AND,size,finishandval);
    end;

  procedure thlcgjvm.a_load_ref_stack(list: TAsmList; size: tdef; const ref: treference; extra_slots: longint);
    var
      opc: tasmop;
      finishandval: aint;
    begin
      { fake location that indicates the value is already on the stack? }
      if (ref.base=NR_EVAL_STACK_BASE) then
        exit;
      opc:=loadstoreopcref(size,true,ref,finishandval);
      if ref.arrayreftype=art_none then
        list.concat(taicpu.op_ref(opc,ref))
      else
        list.concat(taicpu.op_none(opc));
      { avoid problems with getting the size of an open array etc }
      if jvmimplicitpointertype(size) then
        size:=java_jlobject;
      incstack(list,1+ord(size.size>4)-extra_slots);
      if finishandval<>-1 then
        a_op_const_stack(list,OP_AND,size,finishandval);
    end;

  function thlcgjvm.loadstoreopcref(def: tdef; isload: boolean; const ref: treference; out finishandval: aint): tasmop;
    const
                     { isload  static }
      getputopc: array[boolean,boolean] of tasmop =
        ((a_putfield,a_putstatic),
         (a_getfield,a_getstatic));
    begin
      if assigned(ref.symbol) then
        begin
          { -> either a global (static) field, or a regular field. If a regular
            field, then ref.base contains the self pointer, otherwise
            ref.base=NR_NO. In both cases, the symbol contains all other
            information (combined field name and type descriptor) }
          result:=getputopc[isload,ref.base=NR_NO];
          finishandval:=-1;
          { erase sign extension for byte/smallint loads }
          if (def2regtyp(def)=R_INTREGISTER) and
             not is_signed(def) then
            case def.size of
              1: finishandval:=255;
              2: finishandval:=65535;
            end;
        end
      else
        result:=loadstoreopc(def,isload,ref.arrayreftype<>art_none,finishandval);
    end;

  function thlcgjvm.loadstoreopc(def: tdef; isload, isarray: boolean; out finishandval: aint): tasmop;
    var
      size: longint;
    begin
      finishandval:=-1;
      case def2regtyp(def) of
        R_INTREGISTER:
          begin
            size:=def.size;
            if not isarray then
              begin
                case size of
                  1,2,3,4:
                    if isload then
                      result:=a_iload
                    else
                      result:=a_istore;
                  8:
                    if isload then
                      result:=a_lload
                    else
                      result:=a_lstore;
                  else
                    internalerror(2011032814);
                end;
              end
            { array }
            else if isload then
              begin
                case size of
                  1:
                    begin
                      result:=a_baload;
                      if not is_signed(def) then
                        finishandval:=255;
                    end;
                  2:
                    begin
                      if is_widechar(def) then
                        result:=a_caload
                      else
                        begin
                          result:=a_saload;
                          { if we'd treat arrays of word as "array of widechar" we
                            could use a_caload, but that would make for even more
                            awkward interfacing with external Java code }
                          if not is_signed(def) then
                            finishandval:=65535;
                        end;
                    end;
                  4: result:=a_iaload;
                  8: result:=a_laload;
                  else
                    internalerror(2010120503);
                end
              end
            else
              begin
                case size of
                  1: result:=a_bastore;
                  2: if not is_widechar(def) then
                       result:=a_sastore
                     else
                       result:=a_castore;
                  4: result:=a_iastore;
                  8: result:=a_lastore;
                  else
                    internalerror(2010120508);
                end
              end
          end;
        R_ADDRESSREGISTER:
          if not isarray then
            if isload then
              result:=a_aload
            else
              result:=a_astore
          else if isload then
            result:=a_aaload
          else
            result:=a_aastore;
        R_FPUREGISTER:
          begin
            case tfloatdef(def).floattype of
              s32real:
                if not isarray then
                  if isload then
                    result:=a_fload
                  else
                    result:=a_fstore
                else if isload then
                  result:=a_faload
                else
                  result:=a_fastore;
              s64real:
                if not isarray then
                  if isload then
                    result:=a_dload
                  else
                    result:=a_dstore
                else if isload then
                  result:=a_daload
                else
                  result:=a_dastore;
              else
                internalerror(2010120504);
            end
          end
        else
          internalerror(2010120502);
      end;
    end;

  procedure thlcgjvm.resize_stack_int_val(list: TAsmList; fromsize, tosize: tcgsize; forarraystore: boolean);
    begin
      if fromsize in [OS_S64,OS_64] then
        begin
          if not(tosize in [OS_S64,OS_64]) then
            begin
              { truncate }
              list.concat(taicpu.op_none(a_l2i));
              decstack(list,1);
            end;
        end
      else if tosize in [OS_S64,OS_64] then
        begin
          { extend }
          list.concat(taicpu.op_none(a_i2l));
          incstack(list,1);
          { if it was an unsigned 32 bit value, remove sign extension }
          if fromsize=OS_32 then
            a_op_const_stack(list,OP_AND,s64inttype,cardinal($ffffffff));
        end;
      { if the value is immediately stored to an array afterwards, the store
        instruction will properly truncate the value; otherwise we may need
        additional truncation, except for 64/32 bit conversions, which are
        already handled above }
      if not forarraystore and
         (not(fromsize in [OS_S64,OS_64,OS_32,OS_S32]) or
          not(tosize in [OS_S64,OS_64,OS_32,OS_S32])) and
         (tcgsize2size[fromsize]>tcgsize2size[tosize]) or
         ((tcgsize2size[fromsize]=tcgsize2size[tosize]) and
          (fromsize<>tosize)) or
         { needs to mask out the sign in the top 16 bits }
         ((fromsize=OS_S8) and
          (tosize=OS_16)) then
        case tosize of
          OS_8:
            a_op_const_stack(list,OP_AND,s32inttype,255);
          OS_S8:
            list.concat(taicpu.op_none(a_i2b));
          OS_16:
            list.concat(taicpu.op_none(a_i2c));
          OS_S16:
            list.concat(taicpu.op_none(a_i2s));
        end;
    end;

  procedure thlcgjvm.allocate_implicit_struct_with_base_ref(list: TAsmList; vs: tabstractvarsym; ref: treference);
    var
      tmpref: treference;
    begin
      ref.symbol:=current_asmdata.RefAsmSymbol(vs.mangledname);
      tg.gethltemp(list,vs.vardef,vs.vardef.size,tt_persistent,tmpref);
      { only copy the reference, not the actual data }
      a_load_ref_ref(list,java_jlobject,java_jlobject,tmpref,ref);
      { remains live since there's still a reference to the created
        entity }
      tg.ungettemp(list,tmpref);
    end;

  procedure thlcgjvm.allocate_implicit_structs_for_st_with_base_ref(list: TAsmList; st: tsymtable; const ref: treference; allocvartyp: tsymtyp);
    var
      vs: tabstractvarsym;
      i: longint;
    begin
      for i:=0 to st.symlist.count-1 do
        begin
          if (tsym(st.symlist[i]).typ<>allocvartyp) then
            continue;
          vs:=tabstractvarsym(st.symlist[i]);
          if sp_internal in vs.symoptions then
            continue;
          if not jvmimplicitpointertype(vs.vardef) then
            continue;
          allocate_implicit_struct_with_base_ref(list,vs,ref);
        end;
    end;

  procedure thlcgjvm.gen_initialize_fields_code(list: TAsmList);
    var
      selfpara: tparavarsym;
      selfreg: tregister;
      ref: treference;
      obj: tabstractrecorddef;
      i: longint;
      needinit: boolean;
    begin
      obj:=tabstractrecorddef(current_procinfo.procdef.owner.defowner);
      { check whether there are any fields that need initialisation }
      needinit:=false;
      for i:=0 to obj.symtable.symlist.count-1 do
        if (tsym(obj.symtable.symlist[i]).typ=fieldvarsym) and
           jvmimplicitpointertype(tfieldvarsym(obj.symtable.symlist[i]).vardef) then
          begin
            needinit:=true;
            break;
          end;
      if not needinit then
        exit;
      selfpara:=tparavarsym(current_procinfo.procdef.parast.find('self'));
      if not assigned(selfpara) then
        internalerror(2011033001);
      selfreg:=getaddressregister(list,selfpara.vardef);
      a_load_loc_reg(list,obj,obj,selfpara.localloc,selfreg);
      reference_reset_base(ref,selfreg,0,1);
      allocate_implicit_structs_for_st_with_base_ref(list,obj.symtable,ref,fieldvarsym);
    end;

  procedure thlcgjvm.resizestackfpuval(list: TAsmList; fromsize, tosize: tcgsize);
    begin
      if (fromsize=OS_F32) and
         (tosize=OS_F64) then
        begin
          list.concat(taicpu.op_none(a_f2d));
          incstack(list,1);
        end
      else if (fromsize=OS_F64) and
              (tosize=OS_F32) then
        begin
          list.concat(taicpu.op_none(a_d2f));
          decstack(list,1);
        end;
    end;

  procedure thlcgjvm.maybepreparedivu32(list: TAsmList; var op: topcg; size: tdef; out isdivu32: boolean);
    begin
      if (op=OP_DIV) and
         (def_cgsize(size)=OS_32) then
        begin
          { needs zero-extension to 64 bit, because the JVM only supports
            signed divisions }
          resize_stack_int_val(list,OS_32,OS_S64,false);
          op:=OP_IDIV;
          isdivu32:=true;
        end
      else
        isdivu32:=false;
    end;

  procedure thlcgjvm.a_call_name_intern(list: TAsmList; pd: tprocdef; const s: string; inheritedcall: boolean);
    var
      opc: tasmop;
    begin
      {
        invoke types:
          * invokeinterface: call method from an interface (must also specify
              number of parameters in terms of stack slot count!)
          * invokespecial: invoke a constructor, method in a superclass,
              or private instance method
          * invokestatic: invoke a class method (private or not)
          * invokevirtual: invoke a regular method
      }
      case pd.owner.symtabletype of
        globalsymtable,
        staticsymtable,
        localsymtable:
          { regular and nested procedures are turned into static methods }
          opc:=a_invokestatic;
        objectsymtable:
          begin
            case tobjectdef(pd.owner.defowner).objecttype of
              odt_javaclass:
                begin
                  if (po_classmethod in pd.procoptions) then
                    opc:=a_invokestatic
                  else if (pd.visibility=vis_private) or
                     (pd.proctypeoption=potype_constructor) or
                     inheritedcall then
                    opc:=a_invokespecial
                  else
                    opc:=a_invokevirtual;
                end;
              odt_interfacejava:
                { static interface methods are not allowed }
                opc:=a_invokeinterface;
              else
                internalerror(2010122601);
            end;
          end;
        recordsymtable:
          begin
            if (po_staticmethod in pd.procoptions) then
              opc:=a_invokestatic
            else if (pd.visibility=vis_private) or
               (pd.proctypeoption=potype_constructor) or
               inheritedcall then
              opc:=a_invokespecial
            else
              opc:=a_invokevirtual;
          end
        else
          internalerror(2010122602);
      end;
      if (opc<>a_invokeinterface) then
        list.concat(taicpu.op_sym(opc,current_asmdata.RefAsmSymbol(s)))
      else
        begin
          pd.init_paraloc_info(calleeside);
          list.concat(taicpu.op_sym_const(opc,current_asmdata.RefAsmSymbol(s),pd.calleeargareasize));
        end;
    end;

  procedure thlcgjvm.g_call_system_proc(list: TAsmList; const procname: string);
    var
      srsym: tsym;
      pd: tprocdef;
    begin
      srsym:=tsym(systemunit.find(procname));
      if not assigned(srsym) or
         (srsym.typ<>procsym) then
        Message1(cg_f_unknown_compilerproc,procname);
      pd:=tprocdef(tprocsym(srsym).procdeflist[0]);
      a_call_name(list,pd,pd.mangledname,false);
    end;

  procedure create_hlcodegen;
    begin
      hlcg:=thlcgjvm.create;
      create_codegen;
    end;

end.
