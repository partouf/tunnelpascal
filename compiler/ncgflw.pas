{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate assembler for nodes that influence the flow which are
    the same for all (most?) processors

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
unit ncgflw;

{$i fpcdefs.inc}

interface

    uses
      node,nflw;

    type
       tcgwhilerepeatnode = class(twhilerepeatnode)
          procedure pass_2;override;
       end;

       tcgifnode = class(tifnode)
          procedure pass_2;override;
       end;

       tcgfornode = class(tfornode)
          procedure pass_2;override;
       end;

       tcgexitnode = class(texitnode)
          procedure pass_2;override;
       end;

       tcgbreaknode = class(tbreaknode)
          procedure pass_2;override;
       end;

       tcgcontinuenode = class(tcontinuenode)
          procedure pass_2;override;
       end;

       tcggotonode = class(tgotonode)
          procedure pass_2;override;
       end;

       tcglabelnode = class(tlabelnode)
          procedure pass_2;override;
       end;

       tcgfailnode = class(tfailnode)
          procedure pass_2;override;
       end;

       tcgraisenode = class(traisenode)
          procedure pass_2;override;
       end;

       tcgtryexceptnode = class(ttryexceptnode)
          procedure pass_2;override;
       end;

       tcgtryfinallynode = class(ttryfinallynode)
          procedure pass_2;override;
       end;

       tcgonnode = class(tonnode)
          procedure pass_2;override;
       end;



implementation

    uses
      verbose,globals,systems,globtype,
      symconst,symsym,aasmbase,aasmtai,aasmcpu,defbase,
      cginfo,cgbase,pass_2,
      cpubase,cpuinfo,
      nld,ncon,
      ncgutil,
      tgobj,rgobj,paramgr,
      regvars,cgobj,cgcpu,cg64f32;

{*****************************************************************************
                         Second_While_RepeatN
*****************************************************************************}

    procedure tcgwhilerepeatnode.pass_2;
      var
         lcont,lbreak,lloop,
         oldclabel,oldblabel : tasmlabel;
         otlabel,oflabel : tasmlabel;

      begin
         objectlibrary.getlabel(lloop);
         objectlibrary.getlabel(lcont);
         objectlibrary.getlabel(lbreak);
         { arrange continue and breaklabels: }
         oldclabel:=aktcontinuelabel;
         oldblabel:=aktbreaklabel;

         load_all_regvars(exprasmlist);
         { handling code at the end as it is much more efficient, and makes
           while equal to repeat loop, only the end true/false is swapped (PFV) }
         if nf_testatbegin in flags then
           cg.a_jmp_always(exprasmlist,lcont);

         if not(cs_littlesize in aktglobalswitches) then
            { align loop target }
            exprasmList.concat(Tai_align.Create(aktalignment.loopalign));

         cg.a_label(exprasmlist,lloop);

         aktcontinuelabel:=lcont;
         aktbreaklabel:=lbreak;
         rg.cleartempgen;
         if assigned(right) then
           secondpass(right);

         load_all_regvars(exprasmlist);

         cg.a_label(exprasmlist,lcont);
         otlabel:=truelabel;
         oflabel:=falselabel;
         if nf_checknegate in flags then
          begin
            truelabel:=lbreak;
            falselabel:=lloop;
          end
         else
          begin
            truelabel:=lloop;
            falselabel:=lbreak;
          end;
         rg.cleartempgen;
         secondpass(left);

         maketojumpbool(exprasmlist,left,lr_load_regvars);
         cg.a_label(exprasmlist,lbreak);
         truelabel:=otlabel;
         falselabel:=oflabel;

         aktcontinuelabel:=oldclabel;
         aktbreaklabel:=oldblabel;
         { a break/continue in a while/repeat block can't be seen outside }
         flowcontrol:=flowcontrol-[fc_break,fc_continue];
      end;


{*****************************************************************************
                               tcgIFNODE
*****************************************************************************}

    procedure tcgifnode.pass_2;

      var
         hl,otlabel,oflabel : tasmlabel;
         org_regvar_loaded,
         then_regvar_loaded,
         else_regvar_loaded : regvar_booleanarray;
         org_list,
         then_list,
         else_list : taasmoutput;

      begin
         otlabel:=truelabel;
         oflabel:=falselabel;
         objectlibrary.getlabel(truelabel);
         objectlibrary.getlabel(falselabel);
         rg.cleartempgen;
         secondpass(left);


         { save regvars loaded in the beginning so that we can restore them }
         { when processing the else-block                                   }
         if cs_regalloc in aktglobalswitches then
           begin
             org_list := exprasmlist;
             exprasmlist := taasmoutput.create;
           end;
         maketojumpbool(exprasmlist,left,lr_dont_load_regvars);

         if cs_regalloc in aktglobalswitches then
           org_regvar_loaded := rg.regvar_loaded;

         if assigned(right) then
           begin
              cg.a_label(exprasmlist,truelabel);
              rg.cleartempgen;
              secondpass(right);
           end;

         { save current asmlist (previous instructions + then-block) and }
         { loaded regvar state and create new clean ones                 }
         if cs_regalloc in aktglobalswitches then
           begin
             then_regvar_loaded := rg.regvar_loaded;
             rg.regvar_loaded := org_regvar_loaded;
             then_list := exprasmlist;
             exprasmlist := taasmoutput.create;
           end;

         if assigned(t1) then
           begin
              if assigned(right) then
                begin
                   objectlibrary.getlabel(hl);
                   { do go back to if line !! }
                   if not(cs_regalloc in aktglobalswitches) then
                     aktfilepos:=exprasmList.getlasttaifilepos^
                   else
                     aktfilepos:=then_list.getlasttaifilepos^;
                   cg.a_jmp_always(exprasmlist,hl);
                end;
              cg.a_label(exprasmlist,falselabel);
              rg.cleartempgen;
              secondpass(t1);
              { save current asmlist (previous instructions + else-block) }
              { and loaded regvar state and create a new clean list       }
              if cs_regalloc in aktglobalswitches then
                begin
                  else_regvar_loaded := rg.regvar_loaded;
                  else_list := exprasmlist;
                  exprasmlist := taasmoutput.create;
                end;
              if assigned(right) then
                cg.a_label(exprasmlist,hl);
           end
         else
           begin
              if cs_regalloc in aktglobalswitches then
                begin
                  else_regvar_loaded := rg.regvar_loaded;
                  else_list := exprasmlist;
                  exprasmlist := taasmoutput.create;
                end;
              cg.a_label(exprasmlist,falselabel);
           end;
         if not(assigned(right)) then
           begin
              cg.a_label(exprasmlist,truelabel);
           end;

         if cs_regalloc in aktglobalswitches then
           begin
             { add loads of regvars at the end of the then- and else-blocks  }
             { so that at the end of both blocks the same regvars are loaded }

             { no else block? }
             if not assigned(t1) then
               sync_regvars(org_list,then_list,org_regvar_loaded,
                 then_regvar_loaded)
             { no then block? }
             else if not assigned(right) then
               sync_regvars(org_list,else_list,org_regvar_loaded,
                 else_regvar_loaded)
             { both else and then blocks }
             else
               sync_regvars(then_list,else_list,then_regvar_loaded,
                 else_regvar_loaded);
             { add all lists together }
             org_list.concatlist(then_list);
             then_list.free;
             org_list.concatlist(else_list);
             else_list.free;
             org_list.concatlist(exprasmlist);
             exprasmlist.free;
             exprasmlist := org_list;
           end;
         truelabel:=otlabel;
         falselabel:=oflabel;
      end;


{*****************************************************************************
                              SecondFor
*****************************************************************************}

    procedure tcgfornode.pass_2;
      var
         l3,oldclabel,oldblabel : tasmlabel;
         temptovalue : boolean;
         hs : byte;
         temp1 : treference;
         hop : topcg;
         hcond : topcmp;
         opsize : tcgsize;
         count_var_is_signed : boolean;

      begin
         oldclabel:=aktcontinuelabel;
         oldblabel:=aktbreaklabel;
         objectlibrary.getlabel(aktcontinuelabel);
         objectlibrary.getlabel(aktbreaklabel);
         objectlibrary.getlabel(l3);

         { only calculate reference }
         rg.cleartempgen;
         secondpass(t2);
         hs := t2.resulttype.def.size;
         opsize := def_cgsize(t2.resulttype.def);

         { first set the to value
           because the count var can be in the expression !! }
         rg.cleartempgen;
         secondpass(right);
         { calculate pointer value and check if changeable and if so }
         { load into temporary variable                       }
         if right.nodetype<>ordconstn then
           begin
              tg.GetTemp(exprasmlist,hs,tt_normal,temp1);
              temptovalue:=true;
              if (right.location.loc=LOC_REGISTER) or
                 (right.location.loc=LOC_CREGISTER) then
                begin
                   cg.a_load_reg_ref(exprasmlist,opsize,
                     right.location.register,temp1);
                   rg.ungetregister(exprasmlist,right.location.register);
                 end
              else
                cg.g_concatcopy(exprasmlist,right.location.reference,temp1,
                  hs,true,false);
           end
         else
           temptovalue:=false;

         { produce start assignment }
         rg.cleartempgen;
         secondpass(left);
         count_var_is_signed:=is_signed(t2.resulttype.def);

         if nf_backward in flags then
           if count_var_is_signed then
             hcond:=OC_LT
           else
             hcond:=OC_B
         else
           if count_var_is_signed then
             hcond:=OC_GT
           else
             hcond:=OC_A;

         load_all_regvars(exprasmlist);

         if temptovalue then
           begin
             cg.a_cmp_ref_loc_label(exprasmlist,opsize,hcond,
               temp1,t2.location,aktbreaklabel);
           end
         else
           begin
             if nf_testatbegin in flags then
               begin
                 cg.a_cmp_const_loc_label(exprasmlist,opsize,hcond,
                   aword(tordconstnode(right).value),
                   t2.location,aktbreaklabel);
               end;
           end;

         if nf_backward in flags then
           hop:=OP_ADD
         else
           hop:=OP_SUB;
         cg.a_op_const_loc(exprasmlist,hop,1,t2.location);

         if not(cs_littlesize in aktglobalswitches) then
            { align loop target }
            exprasmList.concat(Tai_align.Create(aktalignment.loopalign));
         cg.a_label(exprasmlist,l3);

         { according to count direction DEC or INC... }
         if nf_backward in flags then
           hop:=OP_SUB
         else
           hop:=OP_ADD;
         cg.a_op_const_loc(exprasmlist,hop,1,t2.location);

         { help register must not be in instruction block }
         rg.cleartempgen;
         if assigned(t1) then
           begin
             secondpass(t1);
             load_all_regvars(exprasmlist);
           end;

         cg.a_label(exprasmlist,aktcontinuelabel);

         { makes no problems there }
         rg.cleartempgen;

         if nf_backward in flags then
           if count_var_is_signed then
             hcond:=OC_GT
           else
             hcond:=OC_A
          else
            if count_var_is_signed then
              hcond:=OC_LT
            else
              hcond:=OC_B;
         load_all_regvars(exprasmlist);

         { produce comparison and the corresponding }
         { jump                                     }
         if temptovalue then
           begin
             cg.a_cmp_ref_loc_label(exprasmlist,opsize,hcond,temp1,
               t2.location,l3);
           end
         else
           begin
             cg.a_cmp_const_loc_label(exprasmlist,opsize,hcond,
               aword(tordconstnode(right).value),t2.location,l3);
           end;

         if temptovalue then
           tg.ungetiftemp(exprasmlist,temp1);

         { this is the break label: }
         cg.a_label(exprasmlist,aktbreaklabel);

         aktcontinuelabel:=oldclabel;
         aktbreaklabel:=oldblabel;
         { a break/continue in a for block can't be seen outside }
         flowcontrol:=flowcontrol-[fc_break,fc_continue];
      end;


{*****************************************************************************
                              SecondExitN
*****************************************************************************}

    procedure tcgexitnode.pass_2;

      var
         {op : tasmop;
         s : topsize;}
         otlabel,oflabel : tasmlabel;
         cgsize : tcgsize;
         hreg : tregister;
         allocated_acc,
         allocated_acchigh: boolean;
      label
         do_jmp;
      begin
{         load_all_regvars(exprasmlist); }
         include(flowcontrol,fc_exit);
         if assigned(left) then
         if left.nodetype=assignn then
           begin
              { just do a normal assignment followed by exit }
              secondpass(left);
              cg.a_jmp_always(exprasmlist,aktexitlabel);
           end
         else
           begin
              allocated_acc := false;
              allocated_acchigh := false;
              otlabel:=truelabel;
              oflabel:=falselabel;
              objectlibrary.getlabel(truelabel);
              objectlibrary.getlabel(falselabel);
              secondpass(left);
              { the result of left is not needed anymore after this
                node }
              location_freetemp(exprasmlist,left.location);
              location_release(exprasmlist,left.location);
              case left.location.loc of
                LOC_FPUREGISTER :
                  goto do_jmp;
                LOC_FLAGS :
                  begin
                    cg.a_reg_alloc(exprasmlist,accumulator);
                    allocated_acc := true;
                    cg.g_flags2reg(exprasmlist,OS_INT,left.location.resflags,accumulator);
                    goto do_jmp;
                  end;
                LOC_JUMP :
                  begin
                    cg.a_reg_alloc(exprasmlist,accumulator);
                    { get an 8-bit register }
                    hreg:=rg.makeregsize(accumulator,OS_8);
                    allocated_acc := true;
                    cg.a_label(exprasmlist,truelabel);
                    cg.a_load_const_reg(exprasmlist,OS_8,1,hreg);
                    cg.a_jmp_always(exprasmlist,aktexit2label);
                    cg.a_label(exprasmlist,falselabel);
                    cg.a_load_const_reg(exprasmlist,OS_8,0,hreg);
                    goto do_jmp;
                  end;
              end;
              case aktprocdef.rettype.def.deftype of
                pointerdef,
                procvardef :
                  begin
                    cg.a_reg_alloc(exprasmlist,accumulator);
                    allocated_acc := true;
                    cg.a_load_loc_reg(exprasmlist,left.location,accumulator);
                  end;
                floatdef :
                  begin
{$ifndef i386}
                    cg.a_reg_alloc(exprasmlist,FPU_RESULT_REG);
{$endif not i386}
                    cg.a_loadfpu_loc_reg(exprasmlist,left.location,FPU_RESULT_REG);
                  end;
                else
                  begin
                    cgsize:=def_cgsize(aktprocdef.rettype.def);
                    cg.a_reg_alloc(exprasmlist,accumulator);
                    allocated_acc := true;
                    case cgsize of
                      OS_64,OS_S64 :
                        begin
                          cg.a_reg_alloc(exprasmlist,accumulatorhigh);
                          allocated_acchigh := true;
                          cg64.a_load64_loc_reg(exprasmlist,left.location,
                              joinreg64(accumulator,accumulatorhigh));
                        end
                      else
                        begin
                          hreg:=rg.makeregsize(accumulator,cgsize);
                          cg.a_load_loc_reg(exprasmlist,left.location,hreg);
                        end;
                    end;
                 end;
              end;
do_jmp:
              truelabel:=otlabel;
              falselabel:=oflabel;
              cg.a_jmp_always(exprasmlist,aktexit2label);
              if allocated_acc then
                cg.a_reg_dealloc(exprasmlist,accumulator);
              if allocated_acchigh then
                cg.a_reg_dealloc(exprasmlist,accumulatorhigh);
{$ifndef i386}
              if (aktprocdef.rettype.def.deftype = floatdef) then
                cg.a_reg_dealloc(exprasmlist,FPU_RESULT_REG);
{$endif not i386}
           end
         else
           cg.a_jmp_always(exprasmlist,aktexitlabel);
       end;


{*****************************************************************************
                              SecondBreakN
*****************************************************************************}

    procedure tcgbreaknode.pass_2;
      begin
         include(flowcontrol,fc_break);
         if aktbreaklabel<>nil then
           begin
             load_all_regvars(exprasmlist);
             cg.a_jmp_always(exprasmlist,aktbreaklabel)
           end
         else
           CGMessage(cg_e_break_not_allowed);
      end;


{*****************************************************************************
                              SecondContinueN
*****************************************************************************}

    procedure tcgcontinuenode.pass_2;
      begin
         include(flowcontrol,fc_continue);
         if aktcontinuelabel<>nil then
           begin
             load_all_regvars(exprasmlist);
             cg.a_jmp_always(exprasmlist,aktcontinuelabel)
           end
         else
           CGMessage(cg_e_continue_not_allowed);
      end;


{*****************************************************************************
                             SecondGoto
*****************************************************************************}

    procedure tcggotonode.pass_2;

       begin
         load_all_regvars(exprasmlist);
         cg.a_jmp_always(exprasmlist,labsym.lab)
       end;


{*****************************************************************************
                             SecondLabel
*****************************************************************************}

    procedure tcglabelnode.pass_2;
      begin
         load_all_regvars(exprasmlist);
         cg.a_label(exprasmlist,labelnr);
         rg.cleartempgen;
         secondpass(left);
      end;


{*****************************************************************************
                             SecondFail
*****************************************************************************}

    procedure tcgfailnode.pass_2;
      begin
        cg.a_jmp_always(exprasmlist,faillabel);
      end;


{*****************************************************************************
                             SecondRaise
*****************************************************************************}

    procedure tcgraisenode.pass_2;

      var
         a : tasmlabel;
         href : treference;
         href2: treference;
      begin
         if assigned(left) then
           begin
              { multiple parameters? }
              if assigned(right) then
                begin
                  { push frame }
                  if assigned(frametree) then
                    begin
                      secondpass(frametree);
                      if codegenerror then
                       exit;
                      cg.a_param_loc(exprasmlist,frametree.location,paramanager.getintparaloc(2));
                    end
                  else
                    cg.a_param_const(exprasmlist,OS_INT,0,paramanager.getintparaloc(2));
                  { push address }
                  secondpass(right);
                  if codegenerror then
                   exit;
                  cg.a_param_loc(exprasmlist,right.location,paramanager.getintparaloc(1));
                end
              else
                begin
                   objectlibrary.getaddrlabel(a);
                   cg.a_label(exprasmlist,a);
                   reference_reset_symbol(href2,a,0);
                   cg.a_paramaddr_ref(exprasmlist,href2,paramanager.getintparaloc(2));
                   cg.a_param_reg(exprasmlist,OS_ADDR,FRAME_POINTER_REG,paramanager.getintparaloc(3));
                end;
              { push object }
              secondpass(left);
              if codegenerror then
                exit;
              cg.a_param_loc(exprasmlist,left.location,paramanager.getintparaloc(1));
              cg.a_call_name(exprasmlist,'FPC_RAISEEXCEPTION');
           end
         else
           begin
              cg.a_call_name(exprasmlist,'FPC_POPADDRSTACK');
              cg.a_call_name(exprasmlist,'FPC_RERAISE');
           end;
       end;


{*****************************************************************************
                             SecondTryExcept
*****************************************************************************}

    var
       endexceptlabel : tasmlabel;


    procedure try_new_exception(list : taasmoutput;var jmpbuf,envbuf, href : treference;
      a : aword; exceptlabel : tasmlabel);
     begin
       tg.GetTemp(list,JMP_BUF_SIZE,tt_persistant,jmpbuf);
       tg.GetTemp(list,12,tt_persistant,envbuf);
       tg.GetTemp(list,sizeof(aword),tt_persistant,href);
       new_exception(list, jmpbuf,envbuf, href, a, exceptlabel);
     end;


    procedure try_free_exception(list : taasmoutput;var jmpbuf, envbuf, href : treference;
     a : aword ; endexceptlabel : tasmlabel; onlyfree : boolean);
     begin
         free_exception(list, jmpbuf, envbuf, href, a, endexceptlabel, onlyfree);
         tg.Ungettemp(list,jmpbuf);
         tg.ungettemp(list,envbuf);
     end;



    { does the necessary things to clean up the object stack }
    { in the except block                                    }
    procedure cleanupobjectstack;

      begin
         cg.a_call_name(exprasmlist,'FPC_POPOBJECTSTACK');
         cg.a_param_reg(exprasmlist,OS_ADDR,accumulator,paramanager.getintparaloc(1));
         cg.a_call_name(exprasmlist,'FPC_DESTROYEXCEPTION');
         cg.g_maybe_loadself(exprasmlist);
      end;


    procedure tcgtryexceptnode.pass_2;

      var
         exceptlabel,doexceptlabel,oldendexceptlabel,
         lastonlabel,
         exitexceptlabel,
         continueexceptlabel,
         breakexceptlabel,
         exittrylabel,
         continuetrylabel,
         breaktrylabel,
         doobjectdestroy,
         doobjectdestroyandreraise,
         oldaktexitlabel,
         oldaktexit2label,
         oldaktcontinuelabel,
         oldaktbreaklabel : tasmlabel;
         oldflowcontrol,tryflowcontrol,
         exceptflowcontrol : tflowcontrol;
         tempbuf,tempaddr : treference;
         href : treference;

      label
         errorexit;
      begin
         oldflowcontrol:=flowcontrol;
         flowcontrol:=[];
         { this can be called recursivly }
         oldendexceptlabel:=endexceptlabel;

         { save the old labels for control flow statements }
         oldaktexitlabel:=aktexitlabel;
         oldaktexit2label:=aktexit2label;
         if assigned(aktbreaklabel) then
           begin
              oldaktcontinuelabel:=aktcontinuelabel;
              oldaktbreaklabel:=aktbreaklabel;
           end;

         { get new labels for the control flow statements }
         objectlibrary.getlabel(exittrylabel);
         objectlibrary.getlabel(exitexceptlabel);
         if assigned(aktbreaklabel) then
           begin
              objectlibrary.getlabel(breaktrylabel);
              objectlibrary.getlabel(continuetrylabel);
              objectlibrary.getlabel(breakexceptlabel);
              objectlibrary.getlabel(continueexceptlabel);
           end;

         objectlibrary.getlabel(exceptlabel);
         objectlibrary.getlabel(doexceptlabel);
         objectlibrary.getlabel(endexceptlabel);
         objectlibrary.getlabel(lastonlabel);

         try_new_exception(exprasmlist,tempbuf,tempaddr,href,1,exceptlabel);

         { try block }
         { set control flow labels for the try block }
         aktexitlabel:=exittrylabel;
         aktexit2label:=exittrylabel;
         if assigned(oldaktbreaklabel) then
          begin
            aktcontinuelabel:=continuetrylabel;
            aktbreaklabel:=breaktrylabel;
          end;

         flowcontrol:=[];
         secondpass(left);
         tryflowcontrol:=flowcontrol;
         if codegenerror then
           goto errorexit;

         cg.a_label(exprasmlist,exceptlabel);


         try_free_exception(exprasmlist,tempbuf,tempaddr,href,0,endexceptlabel,false);


         cg.a_label(exprasmlist,doexceptlabel);

         { set control flow labels for the except block }
         { and the on statements                        }
         aktexitlabel:=exitexceptlabel;
         aktexit2label:=exitexceptlabel;
         if assigned(oldaktbreaklabel) then
          begin
            aktcontinuelabel:=continueexceptlabel;
            aktbreaklabel:=breakexceptlabel;
          end;

         flowcontrol:=[];
         { on statements }
         if assigned(right) then
           secondpass(right);

         cg.a_label(exprasmlist,lastonlabel);
         { default handling except handling }
         if assigned(t1) then
           begin
              { FPC_CATCHES must be called with
                'default handler' flag (=-1)
              }
              cg.a_param_const(exprasmlist,OS_ADDR,aword(-1),paramanager.getintparaloc(1));
              cg.a_call_name(exprasmlist,'FPC_CATCHES');
              cg.g_maybe_loadself(exprasmlist);

              { the destruction of the exception object must be also }
              { guarded by an exception frame                        }
              objectlibrary.getlabel(doobjectdestroy);
              objectlibrary.getlabel(doobjectdestroyandreraise);

              try_new_exception(exprasmlist,tempbuf,tempaddr,href,1,exceptlabel);

              { here we don't have to reset flowcontrol           }
              { the default and on flowcontrols are handled equal }
              secondpass(t1);
              exceptflowcontrol:=flowcontrol;

              cg.a_label(exprasmlist,doobjectdestroyandreraise);

              try_free_exception(exprasmlist,tempbuf,tempaddr,href,0,doobjectdestroy,false);

              cg.a_call_name(exprasmlist,'FPC_POPSECONDOBJECTSTACK');

              cg.a_param_reg(exprasmlist, OS_ADDR, accumulator, paramanager.getintparaloc(1));
              cg.a_call_name(exprasmlist,'FPC_DESTROYEXCEPTION');
              { we don't need to restore esi here because reraise never }
              { returns                                                 }
              cg.a_call_name(exprasmlist,'FPC_RERAISE');

              cg.a_label(exprasmlist,doobjectdestroy);
              cleanupobjectstack;
              cg.a_jmp_always(exprasmlist,endexceptlabel);
           end
         else
           begin
              cg.a_call_name(exprasmlist,'FPC_RERAISE');
              exceptflowcontrol:=flowcontrol;
           end;

         if fc_exit in exceptflowcontrol then
           begin
              { do some magic for exit in the try block }
              cg.a_label(exprasmlist,exitexceptlabel);
              { we must also destroy the address frame which guards }
              { exception object                                    }
              cg.a_call_name(exprasmlist,'FPC_POPADDRSTACK');
              cg.g_exception_reason_load(exprasmlist,href);
              cleanupobjectstack;
              cg.a_jmp_always(exprasmlist,oldaktexitlabel);
           end;

         if fc_break in exceptflowcontrol then
           begin
              cg.a_label(exprasmlist,breakexceptlabel);
              { we must also destroy the address frame which guards }
              { exception object                                    }
              cg.a_call_name(exprasmlist,'FPC_POPADDRSTACK');
              cg.g_exception_reason_load(exprasmlist,href);
              cleanupobjectstack;
              cg.a_jmp_always(exprasmlist,oldaktbreaklabel);
           end;

         if fc_continue in exceptflowcontrol then
           begin
              cg.a_label(exprasmlist,continueexceptlabel);
              { we must also destroy the address frame which guards }
              { exception object                                    }
              cg.a_call_name(exprasmlist,'FPC_POPADDRSTACK');
              cg.g_exception_reason_load(exprasmlist,href);
              cleanupobjectstack;
              cg.a_jmp_always(exprasmlist,oldaktcontinuelabel);
           end;

         if fc_exit in tryflowcontrol then
           begin
              { do some magic for exit in the try block }
              cg.a_label(exprasmlist,exittrylabel);
              cg.a_call_name(exprasmlist,'FPC_POPADDRSTACK');
              cg.g_exception_reason_load(exprasmlist,href);
              cg.a_jmp_always(exprasmlist,oldaktexitlabel);
           end;

         if fc_break in tryflowcontrol then
           begin
              cg.a_label(exprasmlist,breaktrylabel);
              cg.a_call_name(exprasmlist,'FPC_POPADDRSTACK');
              cg.g_exception_reason_load(exprasmlist,href);
              cg.a_jmp_always(exprasmlist,oldaktbreaklabel);
           end;

         if fc_continue in tryflowcontrol then
           begin
              cg.a_label(exprasmlist,continuetrylabel);
              cg.a_call_name(exprasmlist,'FPC_POPADDRSTACK');
              cg.g_exception_reason_load(exprasmlist,href);
              cg.a_jmp_always(exprasmlist,oldaktcontinuelabel);
           end;

         cg.a_label(exprasmlist,endexceptlabel);

       errorexit:
         { restore all saved labels }
         endexceptlabel:=oldendexceptlabel;

         { restore the control flow labels }
         aktexitlabel:=oldaktexitlabel;
         aktexit2label:=oldaktexit2label;
         if assigned(oldaktbreaklabel) then
          begin
            aktcontinuelabel:=oldaktcontinuelabel;
            aktbreaklabel:=oldaktbreaklabel;
          end;

         { return all used control flow statements }
         flowcontrol:=oldflowcontrol+exceptflowcontrol+
           tryflowcontrol;
      end;

    procedure tcgonnode.pass_2;
      var
         nextonlabel,
         exitonlabel,
         continueonlabel,
         breakonlabel,
         oldaktexitlabel,
         oldaktexit2label,
         oldaktcontinuelabel,
         doobjectdestroyandreraise,
         doobjectdestroy,
         oldaktbreaklabel : tasmlabel;
         ref : treference;
         oldflowcontrol : tflowcontrol;
         tempbuf,tempaddr : treference;
         href : treference;
         href2: treference;

      begin
         oldflowcontrol:=flowcontrol;
         flowcontrol:=[];
         objectlibrary.getlabel(nextonlabel);

         { send the vmt parameter }
         reference_reset_symbol(href2,objectlibrary.newasmsymbol(excepttype.vmt_mangledname),0);
         cg.a_paramaddr_ref(exprasmlist,href2,paramanager.getintparaloc(1));
         cg.a_call_name(exprasmlist,'FPC_CATCHES');

         { is it this catch? No. go to next onlabel }
         cg.a_cmp_const_reg_label(exprasmlist,OS_ADDR,OC_EQ,0,accumulator,nextonlabel);
         ref.symbol:=nil;
         tg.GetTemp(exprasmlist,pointer_size,tt_normal,ref);

         { what a hack ! }
         if assigned(exceptsymtable) then
           tvarsym(exceptsymtable.symindex.first).address:=ref.offset;
         cg.a_load_reg_ref(exprasmlist, OS_ADDR, accumulator, ref);


         { in the case that another exception is risen }
         { we've to destroy the old one                }
         objectlibrary.getlabel(doobjectdestroyandreraise);

         { call setjmp, and jump to finally label on non-zero result }
         try_new_exception(exprasmlist,tempbuf,tempaddr,href,1,doobjectdestroyandreraise);

         if assigned(right) then
           begin
              oldaktexitlabel:=aktexitlabel;
              oldaktexit2label:=aktexit2label;
              objectlibrary.getlabel(exitonlabel);
              aktexitlabel:=exitonlabel;
              aktexit2label:=exitonlabel;
              if assigned(aktbreaklabel) then
               begin
                 oldaktcontinuelabel:=aktcontinuelabel;
                 oldaktbreaklabel:=aktbreaklabel;
                 objectlibrary.getlabel(breakonlabel);
                 objectlibrary.getlabel(continueonlabel);
                 aktcontinuelabel:=continueonlabel;
                 aktbreaklabel:=breakonlabel;
               end;

              { esi is destroyed by FPC_CATCHES }
              cg.g_maybe_loadself(exprasmlist);
              secondpass(right);
           end;
         objectlibrary.getlabel(doobjectdestroy);
         cg.a_label(exprasmlist,doobjectdestroyandreraise);

         try_free_exception(exprasmlist,tempbuf,tempaddr,href,0,doobjectdestroy,false);

         cg.a_call_name(exprasmlist,'FPC_POPSECONDOBJECTSTACK');
         cg.a_param_reg(exprasmlist, OS_ADDR, accumulator, paramanager.getintparaloc(1));
         cg.a_call_name(exprasmlist,'FPC_DESTROYEXCEPTION');
         { we don't need to restore esi here because reraise never }
         { returns                                                 }
         cg.a_call_name(exprasmlist,'FPC_RERAISE');

         cg.a_label(exprasmlist,doobjectdestroy);
         cleanupobjectstack;
         { clear some stuff }
         tg.ungetiftemp(exprasmlist,ref);
         cg.a_jmp_always(exprasmlist,endexceptlabel);

         if assigned(right) then
           begin
              { special handling for control flow instructions }
              if fc_exit in flowcontrol then
                begin
                   { the address and object pop does secondtryexcept }
                   cg.a_label(exprasmlist,exitonlabel);
                   cg.a_jmp_always(exprasmlist,oldaktexitlabel);
                end;

              if fc_break in flowcontrol then
                begin
                   { the address and object pop does secondtryexcept }
                   cg.a_label(exprasmlist,breakonlabel);
                   cg.a_jmp_always(exprasmlist,oldaktbreaklabel);
                end;

              if fc_continue in flowcontrol then
                begin
                   { the address and object pop does secondtryexcept }
                   cg.a_label(exprasmlist,continueonlabel);
                   cg.a_jmp_always(exprasmlist,oldaktcontinuelabel);
                end;

              aktexitlabel:=oldaktexitlabel;
              aktexit2label:=oldaktexit2label;
              if assigned(oldaktbreaklabel) then
               begin
                 aktcontinuelabel:=oldaktcontinuelabel;
                 aktbreaklabel:=oldaktbreaklabel;
               end;
           end;

         cg.a_label(exprasmlist,nextonlabel);
         flowcontrol:=oldflowcontrol+flowcontrol;
         { next on node }
         if assigned(left) then
           begin
              rg.cleartempgen;
              secondpass(left);
           end;
      end;

{*****************************************************************************
                             SecondTryFinally
*****************************************************************************}

    procedure tcgtryfinallynode.pass_2;
      var
         reraiselabel,
         finallylabel,
         endfinallylabel,
         exitfinallylabel,
         continuefinallylabel,
         breakfinallylabel,
         oldaktexitlabel,
         oldaktexit2label,
         oldaktcontinuelabel,
         oldaktbreaklabel : tasmlabel;
         oldflowcontrol,tryflowcontrol : tflowcontrol;
         decconst : longint;
         tempbuf,tempaddr : treference;
         href : treference;

      begin
         { check if child nodes do a break/continue/exit }
         oldflowcontrol:=flowcontrol;
         flowcontrol:=[];
         objectlibrary.getlabel(finallylabel);
         objectlibrary.getlabel(endfinallylabel);
         objectlibrary.getlabel(reraiselabel);

         { the finally block must catch break, continue and exit }
         { statements                                            }
         oldaktexitlabel:=aktexitlabel;
         oldaktexit2label:=aktexit2label;
         objectlibrary.getlabel(exitfinallylabel);
         aktexitlabel:=exitfinallylabel;
         aktexit2label:=exitfinallylabel;
         if assigned(aktbreaklabel) then
          begin
            oldaktcontinuelabel:=aktcontinuelabel;
            oldaktbreaklabel:=aktbreaklabel;
            objectlibrary.getlabel(breakfinallylabel);
            objectlibrary.getlabel(continuefinallylabel);
            aktcontinuelabel:=continuefinallylabel;
            aktbreaklabel:=breakfinallylabel;
          end;

         { call setjmp, and jump to finally label on non-zero result }
         try_new_exception(exprasmlist,tempbuf,tempaddr,href,1,finallylabel);

         { try code }
         if assigned(left) then
           begin
              secondpass(left);
              tryflowcontrol:=flowcontrol;
              if codegenerror then
                exit;
           end;

         cg.a_label(exprasmlist,finallylabel);
         { just free the frame information }
         try_free_exception(exprasmlist,tempbuf,tempaddr,href,1,finallylabel,true);

         { finally code }
         flowcontrol:=[];
         secondpass(right);
         if flowcontrol<>[] then
           CGMessage(cg_e_control_flow_outside_finally);
         if codegenerror then
           exit;

         { the value should now be in the exception handler }
         cg.g_exception_reason_load(exprasmlist,href);
         cg.a_cmp_reg_reg_label(exprasmlist,OS_S32,OC_NE,accumulator,accumulator,finallylabel);
         cg.a_op_const_reg(exprasmlist,OP_SUB,1,accumulator);
         cg.a_cmp_const_reg_label(exprasmlist,OS_S32,OC_EQ,0,accumulator,reraiselabel);
         if fc_exit in tryflowcontrol then
           begin
              cg.a_op_const_reg(exprasmlist,OP_SUB,1,accumulator);
              cg.a_cmp_const_reg_label(exprasmlist,OS_S32,OC_EQ,0,accumulator,oldaktexitlabel);
              decconst:=1;
           end
         else
           decconst:=2;
         if fc_break in tryflowcontrol then
           begin
              cg.a_op_const_reg(exprasmlist,OP_SUB,decconst,accumulator);
              cg.a_cmp_const_reg_label(exprasmlist,OS_S32,OC_EQ,0,accumulator,oldaktbreaklabel);
              decconst:=1;
           end
         else
           inc(decconst);
         if fc_continue in tryflowcontrol then
           begin
              cg.a_op_const_reg(exprasmlist,OP_SUB,decconst,accumulator);
              cg.a_cmp_const_reg_label(exprasmlist,OS_S32,OC_EQ,0,accumulator,oldaktcontinuelabel);
           end;
         cg.a_label(exprasmlist,reraiselabel);
         cg.a_call_name(exprasmlist,'FPC_RERAISE');
         { do some magic for exit,break,continue in the try block }
         if fc_exit in tryflowcontrol then
           begin
              cg.a_label(exprasmlist,exitfinallylabel);
              cg.g_exception_reason_load(exprasmlist,href);
              cg.g_exception_reason_save_const(exprasmlist,href,2);
              cg.a_jmp_always(exprasmlist,finallylabel);
           end;
         if fc_break in tryflowcontrol then
          begin
             cg.a_label(exprasmlist,breakfinallylabel);
             cg.g_exception_reason_load(exprasmlist,href);
             cg.g_exception_reason_save_const(exprasmlist,href,3);
             cg.a_jmp_always(exprasmlist,finallylabel);
           end;
         if fc_continue in tryflowcontrol then
           begin
              cg.a_label(exprasmlist,continuefinallylabel);
              cg.g_exception_reason_load(exprasmlist,href);
              cg.g_exception_reason_save_const(exprasmlist,href,4);
              cg.a_jmp_always(exprasmlist,finallylabel);
           end;

         cg.a_label(exprasmlist,endfinallylabel);

         aktexitlabel:=oldaktexitlabel;
         aktexit2label:=oldaktexit2label;
         if assigned(aktbreaklabel) then
          begin
            aktcontinuelabel:=oldaktcontinuelabel;
            aktbreaklabel:=oldaktbreaklabel;
          end;
         flowcontrol:=oldflowcontrol+tryflowcontrol;
      end;




begin
   cwhilerepeatnode:=tcgwhilerepeatnode;
   cifnode:=tcgifnode;
   cfornode:=tcgfornode;
   cexitnode:=tcgexitnode;
   cbreaknode:=tcgbreaknode;
   ccontinuenode:=tcgcontinuenode;
   cgotonode:=tcggotonode;
   clabelnode:=tcglabelnode;
   cfailnode:=tcgfailnode;
   craisenode:=tcgraisenode;
   ctryexceptnode:=tcgtryexceptnode;
   ctryfinallynode:=tcgtryfinallynode;
   connode:=tcgonnode;
end.
{
  $Log$
  Revision 1.38  2002-08-23 16:14:48  peter
    * tempgen cleanup
    * tt_noreuse temp type added that will be used in genentrycode

  Revision 1.37  2002/08/19 19:36:43  peter
    * More fixes for cross unit inlining, all tnodes are now implemented
    * Moved pocall_internconst to po_internconst because it is not a
      calling type at all and it conflicted when inlining of these small
      functions was requested

  Revision 1.36  2002/08/15 15:15:55  carl
    * jmpbuf size allocation for exceptions is now cpu specific (as it should)
    * more generic nodes for maths
    * several fixes for better m68k support

  Revision 1.35  2002/08/13 18:01:52  carl
    * rename swatoperands to swapoperands
    + m68k first compilable version (still needs a lot of testing):
        assembler generator, system information , inline
        assembler reader.

  Revision 1.34  2002/08/11 14:32:26  peter
    * renamed current_library to objectlibrary

  Revision 1.33  2002/08/11 13:24:11  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.32  2002/08/09 19:10:59  carl
    * fixed generic exception management

  Revision 1.31  2002/08/04 19:06:41  carl
    + added generic exception support (still does not work!)
    + more documentation

  Revision 1.30  2002/07/27 19:53:51  jonas
    + generic implementation of tcg.g_flags2ref()
    * tcg.flags2xxx() now also needs a size parameter

  Revision 1.29  2002/07/25 17:56:29  carl
    + FPURESULTREG -> FPU_RESULT_REG

  Revision 1.28  2002/07/21 06:58:49  daniel
  * Changed booleans into flags

  Revision 1.27  2002/07/20 12:54:53  daniel
  * Optimized the code generated for for nodes. The shootout/nestloop benchmark
    now runs 5% faster on my computer.

  Revision 1.26  2002/07/20 11:57:54  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.25  2002/07/20 11:15:51  daniel
  * The for node does a check if the first comparision can be skipped. I moved
    the check from the second pass to the resulttype pass. The advantage is
    that the state tracker can now decide to skip the first comparision too.

  Revision 1.24  2002/07/20 08:14:24  daniel
  * Loops should not be aligned when optimizing for size

  Revision 1.23  2002/07/19 11:41:35  daniel
  * State tracker work
  * The whilen and repeatn are now completely unified into whilerepeatn. This
    allows the state tracker to change while nodes automatically into
    repeat nodes.
  * Resulttypepass improvements to the notn. 'not not a' is optimized away and
    'not(a>b)' is optimized into 'a<=b'.
  * Resulttypepass improvements to the whilerepeatn. 'while not a' is optimized
    by removing the notn and later switchting the true and falselabels. The
    same is done with 'repeat until not a'.

  Revision 1.22  2002/07/04 20:43:01  florian
    * first x86-64 patches

  Revision 1.21  2002/07/01 18:46:22  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.20  2002/07/01 16:23:53  peter
    * cg64 patch
    * basics for currency
    * asnode updates for class and interface (not finished)

  Revision 1.19  2002/05/20 13:30:40  carl
  * bugfix of hdisponen (base must be set, not index)
  * more portability fixes

  Revision 1.18  2002/05/18 13:34:09  peter
    * readded missing revisions

  Revision 1.17  2002/05/16 19:46:37  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.15  2002/05/13 19:54:37  peter
    * removed n386ld and n386util units
    * maybe_save/maybe_restore added instead of the old maybe_push

  Revision 1.14  2002/05/12 16:53:07  peter
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

  Revision 1.13  2002/04/21 15:24:38  carl
  + a_jmp_cond -> a_jmp_always (a_jmp_cond is NOT portable)
  + changeregsize -> rg.makeregsize

  Revision 1.12  2002/04/15 19:44:19  peter
    * fixed stackcheck that would be called recursively when a stack
      error was found
    * generic changeregsize(reg,size) for i386 register resizing
    * removed some more routines from cga unit
    * fixed returnvalue handling
    * fixed default stacksize of linux and go32v2, 8kb was a bit small :-)

  Revision 1.11  2002/04/04 19:05:57  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.10  2002/04/02 17:11:28  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.9  2002/03/31 20:26:34  jonas
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

  Revision 1.8  2002/03/04 19:10:11  peter
    * removed compiler warnings

}
