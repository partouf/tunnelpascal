{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate i386 assembler for type converting nodes

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
unit n386cnv;

{$i fpcdefs.inc}

interface

    uses
      node,ncgcnv,defbase;

    type
       ti386typeconvnode = class(tcgtypeconvnode)
         protected
         { procedure second_int_to_int;override; }
         { procedure second_string_to_string;override; }
         { procedure second_cstring_to_pchar;override; }
         { procedure second_string_to_chararray;override; }
         { procedure second_array_to_pointer;override; }
         { procedure second_pointer_to_array;override; }
         { procedure second_chararray_to_string;override; }
         { procedure second_char_to_string;override; }
          function first_int_to_real: tnode; override;
          procedure second_int_to_real;override;
         { procedure second_real_to_real;override; }
         { procedure second_cord_to_pointer;override; }
         { procedure second_proc_to_procvar;override; }
         { procedure second_bool_to_int;override; }
          procedure second_int_to_bool;override;
         { procedure second_load_smallset;override;  }
         { procedure second_ansistring_to_pchar;override; }
         { procedure second_pchar_to_string;override; }
         { procedure second_class_to_intf;override;  }
         { procedure second_char_to_char;override; }
{$ifdef TESTOBJEXT2}
          procedure checkobject;override;
{$endif TESTOBJEXT2}
          procedure second_call_helper(c : tconverttype);override;
       end;


implementation

   uses
      verbose,systems,
      symconst,symdef,aasmbase,aasmtai,aasmcpu,
      cginfo,cgbase,pass_2,
      ncon,ncal,ncnv,
      cpubase,
      cgobj,cga,tgobj,rgobj,rgcpu,ncgutil;


{*****************************************************************************
                             SecondTypeConv
*****************************************************************************}

    function ti386typeconvnode.first_int_to_real : tnode;

      begin
        first_int_to_real:=nil;
         if registersfpu<1 then
          registersfpu:=1;
        location.loc:=LOC_FPUREGISTER;
      end;


    procedure ti386typeconvnode.second_int_to_real;

      var
         href : treference;
         hregister : tregister;
         l1,l2 : tasmlabel;
         freereg : boolean;

      begin
         location_reset(location,LOC_FPUREGISTER,def_cgsize(resulttype.def));
         hregister:=R_NO;
         freereg:=false;

         { for u32bit a solution is to push $0 and to load a comp }
         { does this first, it destroys maybe EDI }
         if torddef(left.resulttype.def).typ=u32bit then
          exprasmlist.concat(taicpu.op_const(A_PUSH,S_L,0));

         case left.location.loc of
           LOC_REGISTER,
           LOC_CREGISTER :
             begin
               case left.location.size of
                 OS_64,OS_S64 :
                   begin
                     exprasmlist.concat(taicpu.op_reg(A_PUSH,S_L,left.location.registerhigh));
                     hregister:=left.location.registerlow;
                   end;
                 OS_32,OS_S32 :
                   hregister:=left.location.register;
                 else
                   begin
                     hregister:=cg.get_scratch_reg_int(exprasmlist);
                     freereg:=true;
                     cg.a_load_reg_reg(exprasmlist,left.location.size,OS_32,left.location.register,hregister);
                   end;
               end;
             end;
           LOC_REFERENCE,
           LOC_CREFERENCE :
             begin
               hregister:=cg.get_scratch_reg_int(exprasmlist);
               freereg:=true;
               if left.location.size in [OS_64,OS_S64] then
                begin
                  href:=left.location.reference;
                  inc(href.offset,4);
                  cg.a_load_ref_reg(exprasmlist,OS_32,href,hregister);
                  exprasmlist.concat(taicpu.op_reg(A_PUSH,S_L,hregister));
                  cg.a_load_ref_reg(exprasmlist,OS_32,left.location.reference,hregister);
                end
               else
                cg.a_load_ref_reg(exprasmlist,left.location.size,left.location.reference,hregister);
             end;
           else
             internalerror(2002032218);
         end;
         location_release(exprasmlist,left.location);
         location_freetemp(exprasmlist,left.location);

         { for 64 bit integers, the high dword is already pushed }
         exprasmlist.concat(taicpu.op_reg(A_PUSH,S_L,hregister));
         if freereg then
           cg.free_scratch_reg(exprasmlist,hregister);
         reference_reset_base(href,R_ESP,0);
         case torddef(left.resulttype.def).typ of
           u32bit:
             begin
                emit_ref(A_FILD,S_IQ,href);
                emit_const_reg(A_ADD,S_L,8,R_ESP);
             end;
           s64bit:
             begin
                emit_ref(A_FILD,S_IQ,href);
                emit_const_reg(A_ADD,S_L,8,R_ESP);
             end;
           u64bit:
             begin
                { unsigned 64 bit ints are harder to handle: }
                { we load bits 0..62 and then check bit 63:  }
                { if it is 1 then we add $80000000 000000000 }
                { as double                                  }
                inc(href.offset,4);
                rg.getexplicitregisterint(exprasmlist,R_EDI);
                emit_ref_reg(A_MOV,S_L,href,R_EDI);
                reference_reset_base(href,R_ESP,4);
                emit_const_ref(A_AND,S_L,$7fffffff,href);
                emit_const_reg(A_TEST,S_L,longint($80000000),R_EDI);
                rg.ungetregisterint(exprasmlist,R_EDI);
                reference_reset_base(href,R_ESP,0);
                emit_ref(A_FILD,S_IQ,href);
                objectlibrary.getdatalabel(l1);
                objectlibrary.getlabel(l2);
                emitjmp(C_Z,l2);
                Consts.concat(Tai_label.Create(l1));
                { I got this constant from a test progtram (FK) }
                Consts.concat(Tai_const.Create_32bit(0));
                Consts.concat(Tai_const.Create_32bit(1138753536));
                reference_reset_symbol(href,l1,0);
                emit_ref(A_FADD,S_FL,href);
                cg.a_label(exprasmlist,l2);
                emit_const_reg(A_ADD,S_L,8,R_ESP);
             end
           else
             begin
                emit_ref(A_FILD,S_IL,href);
                rg.getexplicitregisterint(exprasmlist,R_EDI);
                emit_reg(A_POP,S_L,R_EDI);
                rg.ungetregisterint(exprasmlist,R_EDI);
             end;
         end;
         inc(trgcpu(rg).fpuvaroffset);
         location.register:=R_ST;
      end;


    procedure ti386typeconvnode.second_int_to_bool;
      var
        hregister : tregister;
        pref      : treference;
        resflags  : tresflags;
        hlabel,oldtruelabel,oldfalselabel : tasmlabel;
      begin
         oldtruelabel:=truelabel;
         oldfalselabel:=falselabel;
         objectlibrary.getlabel(truelabel);
         objectlibrary.getlabel(falselabel);
         secondpass(left);
         if codegenerror then
          exit;
         { byte(boolean) or word(wordbool) or longint(longbool) must }
         { be accepted for var parameters                            }
         if (nf_explizit in flags) and
            (left.resulttype.def.size=resulttype.def.size) and
            (left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE,LOC_CREGISTER]) then
           begin
              location_copy(location,left.location);
              truelabel:=oldtruelabel;
              falselabel:=oldfalselabel;
              exit;
           end;

         { Load left node into flag F_NE/F_E }
         resflags:=F_NE;
         location_release(exprasmlist,left.location);
         case left.location.loc of
            LOC_CREFERENCE,
            LOC_REFERENCE :
              begin
                if left.location.size in [OS_64,OS_S64] then
                 begin
                   hregister:=rg.getregisterint(exprasmlist);
                   emit_ref_reg(A_MOV,S_L,left.location.reference,hregister);
                   pref:=left.location.reference;
                   inc(pref.offset,4);
                   emit_ref_reg(A_OR,S_L,pref,hregister);
                 end
                else
                 begin
                   location_force_reg(exprasmlist,left.location,left.location.size,true);
                   cg.a_op_reg_reg(exprasmlist,OP_OR,left.location.size,left.location.register,left.location.register);
                 end;
              end;
            LOC_FLAGS :
              begin
                resflags:=left.location.resflags;
              end;
            LOC_REGISTER,LOC_CREGISTER :
              begin
                if left.location.size in [OS_64,OS_S64] then
                 begin
                   hregister:=cg.get_scratch_reg_int(exprasmlist);
                   cg.a_load_reg_reg(exprasmlist,OS_32,OS_32,left.location.registerlow,hregister);
                   cg.a_op_reg_reg(exprasmlist,OP_OR,OS_32,left.location.registerhigh,hregister);
                   cg.free_scratch_reg(exprasmlist,hregister);
                 end
                else
                 cg.a_op_reg_reg(exprasmlist,OP_OR,left.location.size,left.location.register,left.location.register);
              end;
            LOC_JUMP :
              begin
                hregister:=rg.getregisterint(exprasmlist);
                objectlibrary.getlabel(hlabel);
                cg.a_label(exprasmlist,truelabel);
                cg.a_load_const_reg(exprasmlist,OS_INT,1,hregister);
                cg.a_jmp_always(exprasmlist,hlabel);
                cg.a_label(exprasmlist,falselabel);
                cg.a_load_const_reg(exprasmlist,OS_INT,0,hregister);
                cg.a_label(exprasmlist,hlabel);
                cg.a_op_reg_reg(exprasmlist,OP_OR,OS_INT,hregister,hregister);
              end;
            else
              internalerror(10062);
         end;
         { load flags to register }
         location_reset(location,LOC_REGISTER,def_cgsize(resulttype.def));
         location.register:=def_getreg(resulttype.def);
         cg.g_flags2reg(exprasmlist,location.size,resflags,location.register);
         truelabel:=oldtruelabel;
         falselabel:=oldfalselabel;
       end;

{$ifdef TESTOBJEXT2}
    procedure ti386typeconvnode.checkobject;
      var
         r : preference;
         nillabel : plabel;
       begin
         new(r);
         reset_reference(r^);
         if p^.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
          r^.base:=p^.location.register
         else
           begin
              rg.getexplicitregisterint(exprasmlist,R_EDI);
              emit_mov_loc_reg(p^.location,R_EDI);
              r^.base:=R_EDI;
           end;
         { NIL must be accepted !! }
         emit_reg_reg(A_OR,S_L,r^.base,r^.base);
         rg.ungetregisterint(exprasmlist,R_EDI);
         objectlibrary.getlabel(nillabel);
         emitjmp(C_E,nillabel);
         { this is one point where we need vmt_offset (PM) }
         r^.offset:= tobjectdef(tpointerdef(p^.resulttype.def).definition).vmt_offset;
         rg.getexplicitregisterint(exprasmlist,R_EDI);
         emit_ref_reg(A_MOV,S_L,r,R_EDI);
         emit_sym(A_PUSH,S_L,
           objectlibrary.newasmsymbol(tobjectdef(tpointerdef(p^.resulttype.def).definition).vmt_mangledname));
         emit_reg(A_PUSH,S_L,R_EDI);
         rg.ungetregister32(exprasmlist,R_EDI);
         emitcall('FPC_CHECK_OBJECT_EXT');
         emitlab(nillabel);
       end;
{$endif TESTOBJEXT2}


    procedure ti386typeconvnode.second_call_helper(c : tconverttype);
      const
         secondconvert : array[tconverttype] of pointer = (
           @second_nothing, {equal}
           @second_nothing, {not_possible}
           @second_nothing, {second_string_to_string, handled in resulttype pass }
           @second_char_to_string,
           @second_nothing, {char_to_charray}
           @second_nothing, { pchar_to_string, handled in resulttype pass }
           @second_nothing, {cchar_to_pchar}
           @second_cstring_to_pchar,
           @second_ansistring_to_pchar,
           @second_string_to_chararray,
           @second_nothing, { chararray_to_string, handled in resulttype pass }
           @second_array_to_pointer,
           @second_pointer_to_array,
           @second_int_to_int,
           @second_int_to_bool,
           @second_bool_to_bool,
           @second_bool_to_int,
           @second_real_to_real,
           @second_int_to_real,
           @second_proc_to_procvar,
           @second_nothing, { arrayconstructor_to_set }
           @second_nothing, { second_load_smallset, handled in first pass }
           @second_cord_to_pointer,
           @second_nothing, { interface 2 string }
           @second_nothing, { interface 2 guid   }
           @second_class_to_intf,
           @second_char_to_char,
           @second_nothing,  { normal_2_smallset }
           @second_nothing   { dynarray_2_openarray }
         );
      type
         tprocedureofobject = procedure of object;

      var
         r : packed record
                proc : pointer;
                obj : pointer;
             end;

      begin
         { this is a little bit dirty but it works }
         { and should be quite portable too        }
         r.proc:=secondconvert[c];
         r.obj:=self;
         tprocedureofobject(r){$ifdef FPC}();{$endif FPC}
      end;

begin
   ctypeconvnode:=ti386typeconvnode;
end.
{
  $Log$
  Revision 1.49  2002-09-17 18:54:03  jonas
    * a_load_reg_reg() now has two size parameters: source and dest. This
      allows some optimizations on architectures that don't encode the
      register size in the register name.

  Revision 1.48  2002/08/14 19:19:14  carl
    * first_int_to_real moved to i386 (other one is generic)

  Revision 1.47  2002/08/11 14:32:30  peter
    * renamed current_library to objectlibrary

  Revision 1.46  2002/08/11 13:24:16  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.45  2002/07/27 19:53:51  jonas
    + generic implementation of tcg.g_flags2ref()
    * tcg.flags2xxx() now also needs a size parameter

  Revision 1.44  2002/07/20 11:58:01  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.43  2002/07/01 18:46:31  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.42  2002/05/20 13:30:40  carl
  * bugfix of hdisponen (base must be set, not index)
  * more portability fixes

  Revision 1.41  2002/05/18 13:34:24  peter
    * readded missing revisions

  Revision 1.40  2002/05/16 19:46:51  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.38  2002/05/12 16:53:17  peter
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

  Revision 1.37  2002/04/21 19:02:07  peter
    * removed newn and disposen nodes, the code is now directly
      inlined from pexpr
    * -an option that will write the secondpass nodes to the .s file, this
      requires EXTDEBUG define to actually write the info
    * fixed various internal errors and crashes due recent code changes

  Revision 1.36  2002/04/21 15:35:23  carl
  * changeregsize -> rg.makeregsize

  Revision 1.35  2002/04/19 15:39:35  peter
    * removed some more routines from cga
    * moved location_force_reg/mem to ncgutil
    * moved arrayconstructnode secondpass to ncgld

  Revision 1.34  2002/04/15 19:44:21  peter
    * fixed stackcheck that would be called recursively when a stack
      error was found
    * generic changeregsize(reg,size) for i386 register resizing
    * removed some more routines from cga unit
    * fixed returnvalue handling
    * fixed default stacksize of linux and go32v2, 8kb was a bit small :-)

  Revision 1.33  2002/04/04 19:06:10  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.32  2002/04/02 17:11:36  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.31  2002/03/31 20:26:38  jonas
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

  Revision 1.30  2002/03/04 19:10:13  peter
    * removed compiler warnings

}
