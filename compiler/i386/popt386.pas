{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl and Jonas Maebe

    This unit contains the peephole optimizer.

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
unit popt386;

{$i fpcdefs.inc}

interface

uses Aasmbase,aasmtai,aasmcpu,verbose;

procedure PrePeepHoleOpts(asml: taasmoutput; BlockStart, BlockEnd: tai);
procedure PeepHoleOptPass1(asml: taasmoutput; BlockStart, BlockEnd: tai);
procedure PeepHoleOptPass2(asml: taasmoutput; BlockStart, BlockEnd: tai);
procedure PostPeepHoleOpts(asml: taasmoutput; BlockStart, BlockEnd: tai);

implementation

uses
  globtype,systems,
  globals,cgbase,procinfo,
  symsym,symdef,
{$ifdef finaldestdebug}
  cobjects,
{$endif finaldestdebug}
  cpuinfo,cpubase,cgobj,daopt386,rgobj;

function RegUsedAfterInstruction(reg: Tregister; p: tai; var UsedRegs: TRegSet): Boolean;
var
  supreg: tsuperregister;
begin
  supreg := getsupreg(reg);
  UpdateUsedRegs(UsedRegs, tai(p.Next));
  RegUsedAfterInstruction :=
    (supreg in UsedRegs) and
    (not(getNextInstruction(p,p)) or
     not(regLoadedWithNewValue(supreg,false,p)));
end;


function doFpuLoadStoreOpt(asmL: TAAsmoutput; var p: tai): boolean;
{ returns true if a "continue" should be done after this optimization }
var hp1, hp2: tai;
begin
  doFpuLoadStoreOpt := false;
  if (taicpu(p).oper[0]^.typ = top_ref) and
     getNextInstruction(p, hp1) and
     (hp1.typ = ait_instruction) and
     (((taicpu(hp1).opcode = A_FLD) and
       (taicpu(p).opcode = A_FSTP)) or
      ((taicpu(p).opcode = A_FISTP) and
       (taicpu(hp1).opcode = A_FILD))) and
     (taicpu(hp1).oper[0]^.typ = top_ref) and
     (taicpu(hp1).opsize = taicpu(p).opsize) and
     refsEqual(taicpu(p).oper[0]^.ref^, taicpu(hp1).oper[0]^.ref^) then
    begin
      if getNextInstruction(hp1, hp2) and
         (hp2.typ = ait_instruction) and
         ((taicpu(hp2).opcode = A_LEAVE) or
          (taicpu(hp2).opcode = A_RET)) and
         (taicpu(p).oper[0]^.ref^.base = current_procinfo.FramePointer) and
         (taicpu(p).oper[0]^.ref^.offset >= tvarsym(current_procinfo.procdef.funcretsym).localloc.reference.offset) and
         (taicpu(p).oper[0]^.ref^.index = NR_NO) then
        begin
          asml.remove(p);
          asml.remove(hp1);
          p.free;
          hp1.free;
          p := hp2;
          removeLastDeallocForFuncRes(asmL, p);
          doFPULoadStoreOpt := true;
        end
      else
        { fst can't store an extended value! }
        if (taicpu(p).opsize <> S_FX) and
           (taicpu(p).opsize <> S_IQ) then
          begin
            if (taicpu(p).opcode = A_FSTP) then
              taicpu(p).opcode := A_FST
            else taicpu(p).opcode := A_FIST;
            asml.remove(hp1);
            hp1.free;
          end
    end;
end;


procedure PrePeepHoleOpts(asml: taasmoutput; BlockStart, BlockEnd: tai);
var
  p,hp1: tai;
  l: Aword;
  tmpRef: treference;
begin
  p := BlockStart;
  while (p <> BlockEnd) Do
    begin
      case p.Typ Of
        Ait_Instruction:
          begin
            case taicpu(p).opcode Of
              A_IMUL:
                {changes certain "imul const, %reg"'s to lea sequences}
                begin
                  if (taicpu(p).oper[0]^.typ = Top_Const) and
                     (taicpu(p).oper[1]^.typ = Top_Reg) and
                     (taicpu(p).opsize = S_L) then
                    if (taicpu(p).oper[0]^.val = 1) then
                      if (taicpu(p).ops = 2) then
                       {remove "imul $1, reg"}
                        begin
                          hp1 := tai(p.Next);
                          asml.remove(p);
                          p.free;
                          p := hp1;
                          continue;
                        end
                      else
                       {change "imul $1, reg1, reg2" to "mov reg1, reg2"}
                        begin
                          hp1 := taicpu.Op_Reg_Reg(A_MOV, S_L, taicpu(p).oper[1]^.reg,taicpu(p).oper[2]^.reg);
                          InsertLLItem(asml, p.previous, p.next, hp1);
                          p.free;
                          p := hp1;
                        end
                    else if
                     ((taicpu(p).ops <= 2) or
                      (taicpu(p).oper[2]^.typ = Top_Reg)) and
                     (aktoptprocessor < ClassPentium2) and
                     (taicpu(p).oper[0]^.val <= 12) and
                     not(CS_LittleSize in aktglobalswitches) and
                     (not(GetNextInstruction(p, hp1)) or
                       {GetNextInstruction(p, hp1) and}
                       not((tai(hp1).typ = ait_instruction) and
                           ((taicpu(hp1).opcode=A_Jcc) and
                            (taicpu(hp1).condition in [C_O,C_NO])))) then
                      begin
                        reference_reset(tmpref);
                        case taicpu(p).oper[0]^.val Of
                          3: begin
                             {imul 3, reg1, reg2 to
                                lea (reg1,reg1,2), reg2
                              imul 3, reg1 to
                                lea (reg1,reg1,2), reg1}
                               TmpRef.base := taicpu(p).oper[1]^.reg;
                               TmpRef.index := taicpu(p).oper[1]^.reg;
                               TmpRef.ScaleFactor := 2;
                               if (taicpu(p).ops = 2) then
                                 hp1 := taicpu.op_ref_reg(A_LEA, S_L, TmpRef, taicpu(p).oper[1]^.reg)
                               else
                                 hp1 := taicpu.op_ref_reg(A_LEA, S_L, TmpRef, taicpu(p).oper[2]^.reg);
                               InsertLLItem(asml,p.previous, p.next, hp1);
                               p.free;
                               p := hp1;
                            end;
                         5: begin
                            {imul 5, reg1, reg2 to
                               lea (reg1,reg1,4), reg2
                             imul 5, reg1 to
                               lea (reg1,reg1,4), reg1}
                              TmpRef.base := taicpu(p).oper[1]^.reg;
                              TmpRef.index := taicpu(p).oper[1]^.reg;
                              TmpRef.ScaleFactor := 4;
                              if (taicpu(p).ops = 2) then
                                hp1 := taicpu.op_ref_reg(A_LEA, S_L, TmpRef, taicpu(p).oper[1]^.reg)
                              else
                                hp1 := taicpu.op_ref_reg(A_LEA, S_L, TmpRef, taicpu(p).oper[2]^.reg);
                              InsertLLItem(asml,p.previous, p.next, hp1);
                              p.free;
                              p := hp1;
                            end;
                         6: begin
                            {imul 6, reg1, reg2 to
                               lea (,reg1,2), reg2
                               lea (reg2,reg1,4), reg2
                             imul 6, reg1 to
                               lea (reg1,reg1,2), reg1
                               add reg1, reg1}
                              if (aktoptprocessor <= Class386) then
                                begin
                                  TmpRef.index := taicpu(p).oper[1]^.reg;
                                  if (taicpu(p).ops = 3) then
                                    begin
                                      TmpRef.base := taicpu(p).oper[2]^.reg;
                                      TmpRef.ScaleFactor := 4;
                                      hp1 :=  taicpu.op_ref_reg(A_LEA, S_L, TmpRef, taicpu(p).oper[1]^.reg);
                                    end
                                  else
                                    begin
                                      hp1 :=  taicpu.op_reg_reg(A_ADD, S_L,
                                        taicpu(p).oper[1]^.reg,taicpu(p).oper[1]^.reg);
                                    end;
                                  InsertLLItem(asml,p, p.next, hp1);
                                  reference_reset(tmpref);
                                  TmpRef.index := taicpu(p).oper[1]^.reg;
                                  TmpRef.ScaleFactor := 2;
                                  if (taicpu(p).ops = 3) then
                                    begin
                                      TmpRef.base := NR_NO;
                                      hp1 :=  taicpu.op_ref_reg(A_LEA, S_L, TmpRef,
                                        taicpu(p).oper[2]^.reg);
                                    end
                                  else
                                    begin
                                      TmpRef.base := taicpu(p).oper[1]^.reg;
                                      hp1 := taicpu.op_ref_reg(A_LEA, S_L, TmpRef, taicpu(p).oper[1]^.reg);
                                    end;
                                  InsertLLItem(asml,p.previous, p.next, hp1);
                                  p.free;
                                  p := tai(hp1.next);
                                end
                            end;
                          9: begin
                             {imul 9, reg1, reg2 to
                                lea (reg1,reg1,8), reg2
                              imul 9, reg1 to
                                lea (reg1,reg1,8), reg1}
                               TmpRef.base := taicpu(p).oper[1]^.reg;
                               TmpRef.index := taicpu(p).oper[1]^.reg;
                               TmpRef.ScaleFactor := 8;
                               if (taicpu(p).ops = 2) then
                                 hp1 := taicpu.op_ref_reg(A_LEA, S_L, TmpRef, taicpu(p).oper[1]^.reg)
                               else
                                 hp1 := taicpu.op_ref_reg(A_LEA, S_L, TmpRef, taicpu(p).oper[2]^.reg);
                               InsertLLItem(asml,p.previous, p.next, hp1);
                               p.free;
                               p := hp1;
                             end;
                         10: begin
                            {imul 10, reg1, reg2 to
                               lea (reg1,reg1,4), reg2
                               add reg2, reg2
                             imul 10, reg1 to
                               lea (reg1,reg1,4), reg1
                               add reg1, reg1}
                               if (aktoptprocessor <= Class386) then
                                 begin
                                   if (taicpu(p).ops = 3) then
                                     hp1 :=  taicpu.op_reg_reg(A_ADD, S_L,
                                       taicpu(p).oper[2]^.reg,taicpu(p).oper[2]^.reg)
                                   else
                                     hp1 := taicpu.op_reg_reg(A_ADD, S_L,
                                       taicpu(p).oper[1]^.reg,taicpu(p).oper[1]^.reg);
                                   InsertLLItem(asml,p, p.next, hp1);
                                   TmpRef.base := taicpu(p).oper[1]^.reg;
                                   TmpRef.index := taicpu(p).oper[1]^.reg;
                                   TmpRef.ScaleFactor := 4;
                                   if (taicpu(p).ops = 3) then
                                      hp1 :=  taicpu.op_ref_reg(A_LEA, S_L, TmpRef, taicpu(p).oper[2]^.reg)
                                    else
                                      hp1 :=  taicpu.op_ref_reg(A_LEA, S_L, TmpRef, taicpu(p).oper[1]^.reg);
                                   InsertLLItem(asml,p.previous, p.next, hp1);
                                   p.free;
                                   p := tai(hp1.next);
                                 end
                             end;
                         12: begin
                            {imul 12, reg1, reg2 to
                               lea (,reg1,4), reg2
                               lea (,reg1,8) reg2
                             imul 12, reg1 to
                               lea (reg1,reg1,2), reg1
                               lea (,reg1,4), reg1}
                               if (aktoptprocessor <= Class386)
                                 then
                                   begin
                                     TmpRef.index := taicpu(p).oper[1]^.reg;
                                     if (taicpu(p).ops = 3) then
                                       begin
                                         TmpRef.base := taicpu(p).oper[2]^.reg;
                                         TmpRef.ScaleFactor := 8;
                                         hp1 :=  taicpu.op_ref_reg(A_LEA, S_L, TmpRef, taicpu(p).oper[2]^.reg);
                                       end
                                     else
                                       begin
                                         TmpRef.base := NR_NO;
                                         TmpRef.ScaleFactor := 4;
                                         hp1 :=  taicpu.op_ref_reg(A_LEA, S_L, TmpRef, taicpu(p).oper[1]^.reg);
                                       end;
                                     InsertLLItem(asml,p, p.next, hp1);
                                     reference_reset(tmpref);
                                     TmpRef.index := taicpu(p).oper[1]^.reg;
                                     if (taicpu(p).ops = 3) then
                                       begin
                                         TmpRef.base := NR_NO;
                                         TmpRef.ScaleFactor := 4;
                                         hp1 :=  taicpu.op_ref_reg(A_LEA, S_L, TmpRef, taicpu(p).oper[2]^.reg);
                                       end
                                     else
                                       begin
                                         TmpRef.base := taicpu(p).oper[1]^.reg;
                                         TmpRef.ScaleFactor := 2;
                                         hp1 :=  taicpu.op_ref_reg(A_LEA, S_L, TmpRef, taicpu(p).oper[1]^.reg);
                                       end;
                                     InsertLLItem(asml,p.previous, p.next, hp1);
                                     p.free;
                                     p := tai(hp1.next);
                                   end
                             end
                        end;
                      end;
                end;
              A_SAR, A_SHR:
                  {changes the code sequence
                   shr/sar const1, x
                   shl     const2, x
                   to either "sar/and", "shl/and" or just "and" depending on const1 and const2}
                begin
                  if GetNextInstruction(p, hp1) and
                     (tai(hp1).typ = ait_instruction) and
                     (taicpu(hp1).opcode = A_SHL) and
                     (taicpu(p).oper[0]^.typ = top_const) and
                     (taicpu(hp1).oper[0]^.typ = top_const) and
                     (taicpu(hp1).opsize = taicpu(p).opsize) and
                     (taicpu(hp1).oper[1]^.typ = taicpu(p).oper[1]^.typ) and
                     OpsEqual(taicpu(hp1).oper[1]^, taicpu(p).oper[1]^) then
                    if (taicpu(p).oper[0]^.val > taicpu(hp1).oper[0]^.val) and
                       not(CS_LittleSize in aktglobalswitches) then
                  { shr/sar const1, %reg
                    shl     const2, %reg
                    with const1 > const2 }
                      begin
                        taicpu(p).LoadConst(0,taicpu(p).oper[0]^.val-taicpu(hp1).oper[0]^.val);
                        taicpu(hp1).opcode := A_AND;
                        l := (1 shl (taicpu(hp1).oper[0]^.val)) - 1;
                        case taicpu(p).opsize Of
                          S_L: taicpu(hp1).LoadConst(0,l Xor aword($ffffffff));
                          S_B: taicpu(hp1).LoadConst(0,l Xor $ff);
                          S_W: taicpu(hp1).LoadConst(0,l Xor $ffff);
                        end;
                      end
                    else if (taicpu(p).oper[0]^.val<taicpu(hp1).oper[0]^.val) and
                            not(CS_LittleSize in aktglobalswitches) then
                  { shr/sar const1, %reg
                    shl     const2, %reg
                    with const1 < const2 }
                      begin
                        taicpu(hp1).LoadConst(0,taicpu(hp1).oper[0]^.val-taicpu(p).oper[0]^.val);
                        taicpu(p).opcode := A_AND;
                        l := (1 shl (taicpu(p).oper[0]^.val))-1;
                        case taicpu(p).opsize Of
                          S_L: taicpu(p).LoadConst(0,l Xor aword($ffffffff));
                          S_B: taicpu(p).LoadConst(0,l Xor $ff);
                          S_W: taicpu(p).LoadConst(0,l Xor $ffff);
                        end;
                      end
                    else
                  { shr/sar const1, %reg
                    shl     const2, %reg
                    with const1 = const2 }
                      if (taicpu(p).oper[0]^.val = taicpu(hp1).oper[0]^.val) then
                        begin
                          taicpu(p).opcode := A_AND;
                          l := (1 shl (taicpu(p).oper[0]^.val))-1;
                          case taicpu(p).opsize Of
                            S_B: taicpu(p).LoadConst(0,l Xor $ff);
                            S_W: taicpu(p).LoadConst(0,l Xor $ffff);
                            S_L: taicpu(p).LoadConst(0,l Xor aword($ffffffff));
                          end;
                          asml.remove(hp1);
                          hp1.free;
                        end;
                end;
              A_XOR:
                if (taicpu(p).oper[0]^.typ = top_reg) and
                   (taicpu(p).oper[1]^.typ = top_reg) and
                   (taicpu(p).oper[0]^.reg = taicpu(p).oper[1]^.reg) then
                 { temporarily change this to 'mov reg,0' to make it easier }
                 { for the CSE. Will be changed back in pass 2              }
                  begin
                    taicpu(p).opcode := A_MOV;
                    taicpu(p).loadconst(0,0);
                  end;
            end;
          end;
      end;
      p := tai(p.next)
    end;
end;



procedure PeepHoleOptPass1(Asml: taasmoutput; BlockStart, BlockEnd: tai);
{First pass of peepholeoptimizations}

var
  l,l1 : longint;
  p,hp1,hp2 : tai;
  hp3,hp4: tai;

  TmpRef: TReference;

  UsedRegs, TmpUsedRegs: TRegSet;

  TmpBool1, TmpBool2: Boolean;

  function SkipLabels(hp: tai; var hp2: tai): boolean;
  {skips all labels and returns the next "real" instruction}
  begin
    while assigned(hp.next) and
          (tai(hp.next).typ in SkipInstr + [ait_label,ait_align]) Do
      hp := tai(hp.next);
    if assigned(hp.next) then
      begin
        SkipLabels := True;
        hp2 := tai(hp.next)
      end
    else
      begin
        hp2 := hp;
        SkipLabels := False
      end;
  end;

  function GetFinalDestination(asml: taasmoutput; hp: taicpu; level: longint): boolean;
  {traces sucessive jumps to their final destination and sets it, e.g.
   je l1                je l3
   <code>               <code>
   l1:       becomes    l1:
   je l2                je l3
   <code>               <code>
   l2:                  l2:
   jmp l3               jmp l3

   the level parameter denotes how deeep we have already followed the jump,
   to avoid endless loops with constructs such as "l5: ; jmp l5"           }

  var p1, p2: tai;
      l: tasmlabel;

    function FindAnyLabel(hp: tai; var l: tasmlabel): Boolean;
    begin
      FindAnyLabel := false;
      while assigned(hp.next) and
            (tai(hp.next).typ in (SkipInstr+[ait_align])) Do
        hp := tai(hp.next);
      if assigned(hp.next) and
         (tai(hp.next).typ = ait_label) then
        begin
          FindAnyLabel := true;
          l := tai_label(hp.next).l;
        end
    end;

  begin
    if level > 20 then
      exit;
    GetfinalDestination := false;
    p1 := dfa.getlabelwithsym(tasmlabel(hp.oper[0]^.sym));
    if assigned(p1) then
      begin
        SkipLabels(p1,p1);
        if (tai(p1).typ = ait_instruction) and
           (taicpu(p1).is_jmp) then
          if { the next instruction after the label where the jump hp arrives}
             { is unconditional or of the same type as hp, so continue       }
             (taicpu(p1).condition in [C_None,hp.condition]) or
             { the next instruction after the label where the jump hp arrives}
             { is the opposite of hp (so this one is never taken), but after }
             { that one there is a branch that will be taken, so perform a   }
             { little hack: set p1 equal to this instruction (that's what the}
             { last SkipLabels is for, only works with short bool evaluation)}
             ((taicpu(p1).condition = inverse_cond[hp.condition]) and
              SkipLabels(p1,p2) and
              (p2.typ = ait_instruction) and
              (taicpu(p2).is_jmp) and
              (taicpu(p2).condition in [C_None,hp.condition]) and
              SkipLabels(p1,p1)) then
            begin
              { quick check for loops of the form "l5: ; jmp l5 }
              if (tasmlabel(taicpu(p1).oper[0]^.sym).labelnr =
                   tasmlabel(hp.oper[0]^.sym).labelnr) then
                exit;
              if not GetFinalDestination(asml, taicpu(p1),succ(level)) then
                exit;
              tasmlabel(hp.oper[0]^.sym).decrefs;
              hp.oper[0]^.sym:=taicpu(p1).oper[0]^.sym;
              tasmlabel(hp.oper[0]^.sym).increfs;
            end
          else
            if (taicpu(p1).condition = inverse_cond[hp.condition]) then
              if not FindAnyLabel(p1,l) then
                begin
  {$ifdef finaldestdebug}
                  insertllitem(asml,p1,p1.next,tai_comment.Create(
                    strpnew('previous label inserted'))));
  {$endif finaldestdebug}
                  objectlibrary.getlabel(l);
                  insertllitem(asml,p1,p1.next,tai_label.Create(l));
                  tasmlabel(taicpu(hp).oper[0]^.sym).decrefs;
                  hp.oper[0]^.sym := l;
                  l.increfs;
  {               this won't work, since the new label isn't in the labeltable }
  {               so it will fail the rangecheck. Labeltable should become a   }
  {               hashtable to support this:                                   }
  {               GetFinalDestination(asml, hp);                               }
                end
              else
                begin
  {$ifdef finaldestdebug}
                  insertllitem(asml,p1,p1.next,tai_comment.Create(
                    strpnew('next label reused'))));
  {$endif finaldestdebug}
                  l.increfs;
                  hp.oper[0]^.sym := l;
                  if not GetFinalDestination(asml, hp,succ(level)) then
                    exit;
                end;
      end;
    GetFinalDestination := true;
  end;

  function DoSubAddOpt(var p: tai): Boolean;
  begin
    DoSubAddOpt := False;
    if GetLastInstruction(p, hp1) and
       (hp1.typ = ait_instruction) and
       (taicpu(hp1).opsize = taicpu(p).opsize) then
      case taicpu(hp1).opcode Of
        A_DEC:
          if (taicpu(hp1).oper[0]^.typ = top_reg) and
             (taicpu(hp1).oper[0]^.reg = taicpu(p).oper[1]^.reg) then
            begin
              taicpu(p).LoadConst(0,taicpu(p).oper[0]^.val+1);
              asml.remove(hp1);
              hp1.free;
            end;
         A_SUB:
           if (taicpu(hp1).oper[0]^.typ = top_const) and
              (taicpu(hp1).oper[1]^.typ = top_reg) and
              (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg) then
             begin
               taicpu(p).LoadConst(0,taicpu(p).oper[0]^.val+taicpu(hp1).oper[0]^.val);
               asml.remove(hp1);
               hp1.free;
             end;
         A_ADD:
           if (taicpu(hp1).oper[0]^.typ = top_const) and
              (taicpu(hp1).oper[1]^.typ = top_reg) and
              (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg) then
             begin
               taicpu(p).LoadConst(0,AWord(int64(taicpu(p).oper[0]^.val)-int64(taicpu(hp1).oper[0]^.val)));
               asml.remove(hp1);
               hp1.free;
               if (taicpu(p).oper[0]^.val = 0) then
                 begin
                   hp1 := tai(p.next);
                   asml.remove(p);
                   p.free;
                   if not GetLastInstruction(hp1, p) then
                     p := hp1;
                   DoSubAddOpt := True;
                 end
             end;
       end;
  end;

begin
  p := BlockStart;
  UsedRegs := [];
  while (p <> BlockEnd) Do
    begin
      UpDateUsedRegs(UsedRegs, tai(p.next));
      case p.Typ Of
        ait_instruction:
          begin
            { Handle Jmp Optimizations }
            if taicpu(p).is_jmp then
              begin
      {the following if-block removes all code between a jmp and the next label,
        because it can never be executed}
                if (taicpu(p).opcode = A_JMP) then
                  begin
                    while GetNextInstruction(p, hp1) and
                          (hp1.typ <> ait_label) do
                      if not(hp1.typ in ([ait_label,ait_align]+skipinstr)) then
                        begin
                          asml.remove(hp1);
                          hp1.free;
                        end
                      else break;
                    end;
                { remove jumps to a label coming right after them }
                if GetNextInstruction(p, hp1) then
                  begin
                    if FindLabel(tasmlabel(taicpu(p).oper[0]^.sym), hp1) and
  {$warning FIXME removing the first instruction fails}
                        (p<>blockstart) then
                      begin
                        hp2:=tai(hp1.next);
                        asml.remove(p);
                        p.free;
                        p:=hp2;
                        continue;
                      end
                    else
                      begin
                        if hp1.typ = ait_label then
                          SkipLabels(hp1,hp1);
                        if (tai(hp1).typ=ait_instruction) and
                            (taicpu(hp1).opcode=A_JMP) and
                            GetNextInstruction(hp1, hp2) and
                            FindLabel(tasmlabel(taicpu(p).oper[0]^.sym), hp2) then
                          begin
                            if taicpu(p).opcode=A_Jcc then
                              begin
                                taicpu(p).condition:=inverse_cond[taicpu(p).condition];
                                tai_label(hp2).l.decrefs;
                                taicpu(p).oper[0]^.sym:=taicpu(hp1).oper[0]^.sym;
                                taicpu(p).oper[0]^.sym.increfs;
                                asml.remove(hp1);
                                hp1.free;
                                GetFinalDestination(asml, taicpu(p),0);
                              end
                            else
                              begin
                                GetFinalDestination(asml, taicpu(p),0);
                                p:=tai(p.next);
                                continue;
                              end;
                          end
                        else
                          GetFinalDestination(asml, taicpu(p),0);
                      end;
                  end;
              end
            else
            { All other optimizes }
              begin
                for l := 0 to taicpu(p).ops-1 Do
                  if (taicpu(p).oper[l]^.typ = top_ref) then
                    With taicpu(p).oper[l]^.ref^ Do
                      begin
                        if (base = NR_NO) and
                           (index <> NR_NO) and
                           (scalefactor in [0,1]) then
                          begin
                            base := index;
                            index := NR_NO
                          end
                      end;
                case taicpu(p).opcode Of
                  A_AND:
                    begin
                      if (taicpu(p).oper[0]^.typ = top_const) and
                         (taicpu(p).oper[1]^.typ = top_reg) and
                         GetNextInstruction(p, hp1) and
                         (tai(hp1).typ = ait_instruction) and
                         (taicpu(hp1).opcode = A_AND) and
                         (taicpu(hp1).oper[0]^.typ = top_const) and
                         (taicpu(hp1).oper[1]^.typ = top_reg) and
                         (taicpu(p).oper[1]^.reg = taicpu(hp1).oper[1]^.reg) then
    {change "and const1, reg; and const2, reg" to "and (const1 and const2), reg"}
                        begin
                          taicpu(p).LoadConst(0,taicpu(p).oper[0]^.val and taicpu(hp1).oper[0]^.val);
                          asml.remove(hp1);
                          hp1.free;
                        end
                      else
    {change "and x, reg; jxx" to "test x, reg", if reg is deallocated before the
    jump, but only if it's a conditional jump (PFV) }
                        if (taicpu(p).oper[1]^.typ = top_reg) and
                           GetNextInstruction(p, hp1) and
                           (hp1.typ = ait_instruction) and
                           (taicpu(hp1).is_jmp) and
                           (taicpu(hp1).opcode<>A_JMP) and
                           not(getsupreg(taicpu(p).oper[1]^.reg) in UsedRegs) then
                          taicpu(p).opcode := A_TEST;
                    end;
                  A_CMP:
                    begin
                      if (taicpu(p).oper[0]^.typ = top_const) and
                         (taicpu(p).oper[1]^.typ in [top_reg,top_ref]) and
                         (taicpu(p).oper[0]^.val = 0) and
                         GetNextInstruction(p, hp1) and
                         (hp1.typ = ait_instruction) and
                         (taicpu(hp1).is_jmp) and
                         (taicpu(hp1).opcode=A_Jcc) and
                         (taicpu(hp1).condition in [C_LE,C_BE]) and
                         GetNextInstruction(hp1,hp2) and
                         (hp2.typ = ait_instruction) and
                         (taicpu(hp2).opcode = A_DEC) and
                         OpsEqual(taicpu(hp2).oper[0]^,taicpu(p).oper[1]^) and
                         GetNextInstruction(hp2, hp3) and
                         (hp3.typ = ait_instruction) and
                         (taicpu(hp3).is_jmp) and
                         (taicpu(hp3).opcode = A_JMP) and
                         GetNextInstruction(hp3, hp4) and
                         FindLabel(tasmlabel(taicpu(hp1).oper[0]^.sym),hp4) then
                        begin
                          taicpu(hp2).Opcode := A_SUB;
                          taicpu(hp2).Loadoper(1,taicpu(hp2).oper[0]^);
                          taicpu(hp2).LoadConst(0,1);
                          taicpu(hp2).ops:=2;
                          taicpu(hp3).Opcode := A_Jcc;
                          case taicpu(hp1).condition of
                            C_LE: taicpu(hp3).condition := C_GE;
                            C_BE: taicpu(hp3).condition := C_AE;
                          end;
                          asml.remove(p);
                          asml.remove(hp1);
                          p.free;
                          hp1.free;
                          p := hp2;
                          continue;
                        end
                    end;
                  A_FLD:
                    begin
                      if (taicpu(p).oper[0]^.typ = top_reg) and
                         GetNextInstruction(p, hp1) and
                         (hp1.typ = Ait_Instruction) and
                          (taicpu(hp1).oper[0]^.typ = top_reg) and
                         (taicpu(hp1).oper[1]^.typ = top_reg) and
                         (taicpu(hp1).oper[0]^.reg = NR_ST) and
                         (taicpu(hp1).oper[1]^.reg = NR_ST1) then
                         { change                        to
                             fld      reg               fxxx reg,st
                             fxxxp    st, st1 (hp1)
                           Remark: non commutative operations must be reversed!
                         }
                        begin
                            case taicpu(hp1).opcode Of
                              A_FMULP,A_FADDP,
                              A_FSUBP,A_FDIVP,A_FSUBRP,A_FDIVRP:
                                begin
                                  case taicpu(hp1).opcode Of
                                    A_FADDP: taicpu(hp1).opcode := A_FADD;
                                    A_FMULP: taicpu(hp1).opcode := A_FMUL;
                                    A_FSUBP: taicpu(hp1).opcode := A_FSUBR;
                                    A_FSUBRP: taicpu(hp1).opcode := A_FSUB;
                                    A_FDIVP: taicpu(hp1).opcode := A_FDIVR;
                                    A_FDIVRP: taicpu(hp1).opcode := A_FDIV;
                                  end;
                                  taicpu(hp1).oper[0]^.reg := taicpu(p).oper[0]^.reg;
                                  taicpu(hp1).oper[1]^.reg := NR_ST;
                                  asml.remove(p);
                                  p.free;
                                  p := hp1;
                                  continue;
                                end;
                            end;
                        end
                      else
                        if (taicpu(p).oper[0]^.typ = top_ref) and
                           GetNextInstruction(p, hp2) and
                           (hp2.typ = Ait_Instruction) and
                           (taicpu(hp2).ops = 2) and
                           (taicpu(hp2).oper[0]^.typ = top_reg) and
                           (taicpu(hp2).oper[1]^.typ = top_reg) and
                           (taicpu(p).opsize in [S_FS, S_FL]) and
                           (taicpu(hp2).oper[0]^.reg = NR_ST) and
                           (taicpu(hp2).oper[1]^.reg = NR_ST1) then
                          if GetLastInstruction(p, hp1) and
                             (hp1.typ = Ait_Instruction) and
                             ((taicpu(hp1).opcode = A_FLD) or
                              (taicpu(hp1).opcode = A_FST)) and
                             (taicpu(hp1).opsize = taicpu(p).opsize) and
                             (taicpu(hp1).oper[0]^.typ = top_ref) and
                             RefsEqual(taicpu(p).oper[0]^.ref^, taicpu(hp1).oper[0]^.ref^) then
                            if ((taicpu(hp2).opcode = A_FMULP) or
                                (taicpu(hp2).opcode = A_FADDP)) then
                            { change                      to
                                fld/fst   mem1  (hp1)       fld/fst   mem1
                                fld       mem1  (p)         fadd/
                                faddp/                       fmul     st, st
                                fmulp  st, st1 (hp2) }
                              begin
                                asml.remove(p);
                                p.free;
                                p := hp1;
                                if (taicpu(hp2).opcode = A_FADDP) then
                                  taicpu(hp2).opcode := A_FADD
                                else
                                  taicpu(hp2).opcode := A_FMUL;
                                taicpu(hp2).oper[1]^.reg := NR_ST;
                              end
                            else
                            { change              to
                                fld/fst mem1 (hp1)   fld/fst mem1
                                fld     mem1 (p)     fld      st}
                              begin
                                taicpu(p).changeopsize(S_FL);
                                taicpu(p).loadreg(0,NR_ST);
                              end
                          else
                            begin
                              case taicpu(hp2).opcode Of
                                A_FMULP,A_FADDP,A_FSUBP,A_FDIVP,A_FSUBRP,A_FDIVRP:
                          { change                        to
                              fld/fst  mem1    (hp1)      fld/fst    mem1
                              fld      mem2    (p)        fxxx       mem2
                              fxxxp    st, st1 (hp2)                      }

                                  begin
                                    case taicpu(hp2).opcode Of
                                      A_FADDP: taicpu(p).opcode := A_FADD;
                                      A_FMULP: taicpu(p).opcode := A_FMUL;
                                      A_FSUBP: taicpu(p).opcode := A_FSUBR;
                                      A_FSUBRP: taicpu(p).opcode := A_FSUB;
                                      A_FDIVP: taicpu(p).opcode := A_FDIVR;
                                      A_FDIVRP: taicpu(p).opcode := A_FDIV;
                                    end;
                                    asml.remove(hp2);
                                    hp2.free;
                                  end
                              end
                            end
                    end;
                  A_FSTP,A_FISTP:
                    if doFpuLoadStoreOpt(asmL,p) then
                      continue;
                  A_LEA:
                    begin
                      {removes seg register prefixes from LEA operations, as they
                      don't do anything}
                      taicpu(p).oper[0]^.ref^.Segment := NR_NO;
                      {changes "lea (%reg1), %reg2" into "mov %reg1, %reg2"}
                      if (taicpu(p).oper[0]^.ref^.base <> NR_NO) and
                         (getsupreg(taicpu(p).oper[0]^.ref^.base) in [RS_EAX..RS_ESP]) and
                         (taicpu(p).oper[0]^.ref^.index = NR_NO) and
                         (not(Assigned(taicpu(p).oper[0]^.ref^.Symbol))) then
                        if (taicpu(p).oper[0]^.ref^.base <> taicpu(p).oper[1]^.reg) and
                           (taicpu(p).oper[0]^.ref^.offset = 0) then
                          begin
                            hp1 := taicpu.op_reg_reg(A_MOV, S_L,taicpu(p).oper[0]^.ref^.base,
                              taicpu(p).oper[1]^.reg);
                            InsertLLItem(asml,p.previous,p.next, hp1);
                            p.free;
                            p := hp1;
                            continue;
                          end
                        else if (taicpu(p).oper[0]^.ref^.offset = 0) then
                          begin
                            hp1 := tai(p.Next);
                            asml.remove(p);
                            p.free;
                            p := hp1;
                            continue;
                          end
                        else
                          with taicpu(p).oper[0]^.ref^ do
                            if (base = taicpu(p).oper[1]^.reg) then
                              begin
                                l := offset;
                                if (l=1) then
                                  begin
                                    taicpu(p).opcode := A_INC;
                                    taicpu(p).loadreg(0,taicpu(p).oper[1]^.reg);
                                    taicpu(p).ops := 1
                                  end
                                else if (l=-1) then
                                  begin
                                    taicpu(p).opcode := A_DEC;
                                    taicpu(p).loadreg(0,taicpu(p).oper[1]^.reg);
                                    taicpu(p).ops := 1;
                                  end
                                else
                                  begin
                                    taicpu(p).opcode := A_ADD;
                                    taicpu(p).loadconst(0,aword(l));
                                  end;
                              end;
                    end;
                  A_MOV:
                    begin
                      TmpUsedRegs := UsedRegs;
                      if (taicpu(p).oper[1]^.typ = top_reg) and
                         (getsupreg(taicpu(p).oper[1]^.reg) in [RS_EAX, RS_EBX, RS_ECX, RS_EDX, RS_ESI, RS_EDI]) and
                         GetNextInstruction(p, hp1) and
                         (tai(hp1).typ = ait_instruction) and
                         (taicpu(hp1).opcode = A_MOV) and
                         (taicpu(hp1).oper[0]^.typ = top_reg) and
                         (taicpu(hp1).oper[0]^.reg = taicpu(p).oper[1]^.reg) then
                        begin
                    {we have "mov x, %treg; mov %treg, y}
                          if not(RegUsedAfterInstruction(taicpu(p).oper[1]^.reg, hp1, TmpUsedRegs)) then
                    {we've got "mov x, %treg; mov %treg, y; with %treg is not used after }
                            case taicpu(p).oper[0]^.typ Of
                              top_reg:
                                begin
                                  { change "mov %reg, %treg; mov %treg, y"
                                    to "mov %reg, y" }
                                  taicpu(p).LoadOper(1,taicpu(hp1).oper[1]^);
                                  asml.remove(hp1);
                                  hp1.free;
                                  continue;
                                end;
                              top_ref:
                                if (taicpu(hp1).oper[1]^.typ = top_reg) then
                                begin
                                  { change "mov mem, %treg; mov %treg, %reg"
                                    to "mov mem, %reg" }
                                  taicpu(p).Loadoper(1,taicpu(hp1).oper[1]^);
                                  asml.remove(hp1);
                                  hp1.free;
                                  continue;
                                end;
                            end
                        end
                      else
                    {Change "mov %reg1, %reg2; xxx %reg2, ???" to
                    "mov %reg1, %reg2; xxx %reg1, ???" to avoid a write/read
                    penalty}
                        if (taicpu(p).oper[0]^.typ = top_reg) and
                           (taicpu(p).oper[1]^.typ = top_reg) and
                           GetNextInstruction(p,hp1) and
                           (tai(hp1).typ = ait_instruction) and
                           (taicpu(hp1).ops >= 1) and
                           (taicpu(hp1).oper[0]^.typ = top_reg) and
                           (taicpu(hp1).oper[0]^.reg = taicpu(p).oper[1]^.reg) then
                    {we have "mov %reg1, %reg2; XXX %reg2, ???"}
                          begin
                            if ((taicpu(hp1).opcode = A_OR) or
                                (taicpu(hp1).opcode = A_TEST)) and
                               (taicpu(hp1).oper[1]^.typ = top_reg) and
                               (taicpu(hp1).oper[0]^.reg = taicpu(hp1).oper[1]^.reg) then
                  {we have "mov %reg1, %reg2; test/or %reg2, %reg2"}
                              begin
                                TmpUsedRegs := UsedRegs;
                                { reg1 will be used after the first instruction, }
                                { so update the allocation info                  }
                                allocRegBetween(asmL,taicpu(p).oper[0]^.reg,p,hp1);
                                if GetNextInstruction(hp1, hp2) and
                                   (hp2.typ = ait_instruction) and
                                   taicpu(hp2).is_jmp and
                                   not(RegUsedAfterInstruction(taicpu(hp1).oper[0]^.reg, hp1, TmpUsedRegs)) then
                { change "mov %reg1, %reg2; test/or %reg2, %reg2; jxx" to
                  "test %reg1, %reg1; jxx" }
                                    begin
                                      taicpu(hp1).Loadoper(0,taicpu(p).oper[0]^);
                                      taicpu(hp1).Loadoper(1,taicpu(p).oper[0]^);
                                      asml.remove(p);
                                      p.free;
                                      p := hp1;
                                      continue
                                    end
                                  else
                {change "mov %reg1, %reg2; test/or %reg2, %reg2" to
                  "mov %reg1, %reg2; test/or %reg1, %reg1"}
                                    begin
                                      taicpu(hp1).Loadoper(0,taicpu(p).oper[0]^);
                                      taicpu(hp1).Loadoper(1,taicpu(p).oper[0]^);
                                    end;
                              end
{                              else
                                if (taicpu(p.next)^.opcode
                                  in [A_PUSH, A_OR, A_XOR, A_AND, A_TEST])}
                        {change "mov %reg1, %reg2; push/or/xor/... %reg2, ???" to
                          "mov %reg1, %reg2; push/or/xor/... %reg1, ???"}
                          end
                        else
                    {leave out the mov from "mov reg, x(%frame_pointer); leave/ret" (with
                    x >= RetOffset) as it doesn't do anything (it writes either to a
                    parameter or to the temporary storage room for the function
                    result)}
                          if GetNextInstruction(p, hp1) and
                             (tai(hp1).typ = ait_instruction) then
                            if ((taicpu(hp1).opcode = A_LEAVE) or
                                (taicpu(hp1).opcode = A_RET)) and
                               (taicpu(p).oper[1]^.typ = top_ref) and
                               (taicpu(p).oper[1]^.ref^.base = current_procinfo.FramePointer) and
                               (taicpu(p).oper[1]^.ref^.offset >= tvarsym(current_procinfo.procdef.funcretsym).localloc.reference.offset) and
                               (taicpu(p).oper[1]^.ref^.index = NR_NO) and
                               (taicpu(p).oper[0]^.typ = top_reg) then
                              begin
                                asml.remove(p);
                                p.free;
                                p := hp1;
                                RemoveLastDeallocForFuncRes(asmL,p);
                              end
                            else
                              if (taicpu(p).oper[0]^.typ = top_reg) and
                                  (taicpu(p).oper[1]^.typ = top_ref) and
                                  (taicpu(p).opsize = taicpu(hp1).opsize) and
                                  (taicpu(hp1).opcode = A_CMP) and
                                  (taicpu(hp1).oper[1]^.typ = top_ref) and
                                  RefsEqual(taicpu(p).oper[1]^.ref^, taicpu(hp1).oper[1]^.ref^) then
          {change "mov reg1, mem1; cmp x, mem1" to "mov reg, mem1; cmp x, reg1"}
                                begin
                                  taicpu(hp1).loadreg(1,taicpu(p).oper[0]^.reg);
                                  allocRegBetween(asmL,taicpu(p).oper[0]^.reg,p,hp1);
                                end;
                    { Next instruction is also a MOV ? }
                      if GetNextInstruction(p, hp1) and
                         (tai(hp1).typ = ait_instruction) and
                         (taicpu(hp1).opcode = A_MOV) and
                         (taicpu(hp1).opsize = taicpu(p).opsize) then
                        begin
                          if (taicpu(hp1).oper[0]^.typ = taicpu(p).oper[1]^.typ) and
                             (taicpu(hp1).oper[1]^.typ = taicpu(p).oper[0]^.typ) then
                              {mov reg1, mem1     or     mov mem1, reg1
                              mov mem2, reg2            mov reg2, mem2}
                            begin
                              if OpsEqual(taicpu(hp1).oper[1]^,taicpu(p).oper[0]^) then
                            {mov reg1, mem1     or     mov mem1, reg1
                            mov mem2, reg1            mov reg2, mem1}
                                begin
                                  if OpsEqual(taicpu(hp1).oper[0]^,taicpu(p).oper[1]^) then
                        { Removes the second statement from
                            mov reg1, mem1/reg2
                            mov mem1/reg2, reg1 }
                                    begin
                                      if (taicpu(p).oper[0]^.typ = top_reg) then
                                        AllocRegBetween(asmL,taicpu(p).oper[0]^.reg,p,hp1);
                                      asml.remove(hp1);
                                      hp1.free;
                                    end
                                  else
                                    begin
                                      TmpUsedRegs := UsedRegs;
                                      UpdateUsedRegs(TmpUsedRegs, tai(hp1.next));
                                      if (taicpu(p).oper[1]^.typ = top_ref) and
                                        { mov reg1, mem1
                                          mov mem2, reg1 }
                                         GetNextInstruction(hp1, hp2) and
                                         (hp2.typ = ait_instruction) and
                                         (taicpu(hp2).opcode = A_CMP) and
                                         (taicpu(hp2).opsize = taicpu(p).opsize) and
                                         (taicpu(hp2).oper[0]^.typ = TOp_Ref) and
                                         (taicpu(hp2).oper[1]^.typ = TOp_Reg) and
                                         RefsEqual(taicpu(hp2).oper[0]^.ref^, taicpu(p).oper[1]^.ref^) and
                                         (taicpu(hp2).oper[1]^.reg= taicpu(p).oper[0]^.reg) and
                                         not(RegUsedAfterInstruction(taicpu(p).oper[0]^.reg, hp2, TmpUsedRegs)) then
                          { change                   to
                              mov reg1, mem1           mov reg1, mem1
                              mov mem2, reg1           cmp reg1, mem2
                              cmp mem1, reg1                          }
                                        begin
                                          asml.remove(hp2);
                                          hp2.free;
                                          taicpu(hp1).opcode := A_CMP;
                                          taicpu(hp1).loadref(1,taicpu(hp1).oper[0]^.ref^);
                                          taicpu(hp1).loadreg(0,taicpu(p).oper[0]^.reg);
                                        end;
                                    end;
                                end
                              else
                                begin
                                  tmpUsedRegs := UsedRegs;
                                  if GetNextInstruction(hp1, hp2) and
                                     (taicpu(p).oper[0]^.typ = top_ref) and
                                     (taicpu(p).oper[1]^.typ = top_reg) and
                                     (taicpu(hp1).oper[0]^.typ = top_reg) and
                                     (taicpu(hp1).oper[0]^.reg = taicpu(p).oper[1]^.reg) and
                                     (taicpu(hp1).oper[1]^.typ = top_ref) and
                                     (tai(hp2).typ = ait_instruction) and
                                     (taicpu(hp2).opcode = A_MOV) and
                                     (taicpu(hp2).opsize = taicpu(p).opsize) and
                                     (taicpu(hp2).oper[1]^.typ = top_reg) and
                                     (taicpu(hp2).oper[0]^.typ = top_ref) and
                                     RefsEqual(taicpu(hp2).oper[0]^.ref^, taicpu(hp1).oper[1]^.ref^)  then
                                    if not regInRef(getsupreg(taicpu(hp2).oper[1]^.reg),taicpu(hp2).oper[0]^.ref^) and
                                       not(RegUsedAfterInstruction(taicpu(p).oper[1]^.reg,hp1,tmpUsedRegs)) then
                            {   mov mem1, %reg1
                                mov %reg1, mem2
                                mov mem2, reg2
                              to:
                                mov mem1, reg2
                                mov reg2, mem2}
                                      begin
                                        AllocRegBetween(asmL,taicpu(hp2).oper[1]^.reg,p,hp2);
                                        taicpu(p).Loadoper(1,taicpu(hp2).oper[1]^);
                                        taicpu(hp1).loadoper(0,taicpu(hp2).oper[1]^);
                                        asml.remove(hp2);
                                        hp2.free;
                                      end
                                    else
                                      if (taicpu(p).oper[1]^.reg <> taicpu(hp2).oper[1]^.reg) and
                                         not(RegInRef(getsupreg(taicpu(p).oper[1]^.reg),taicpu(p).oper[0]^.ref^)) and
                                         not(RegInRef(getsupreg(taicpu(hp2).oper[1]^.reg),taicpu(hp2).oper[0]^.ref^)) then
                          {   mov mem1, reg1         mov mem1, reg1
                              mov reg1, mem2         mov reg1, mem2
                              mov mem2, reg2         mov mem2, reg1
                            to:                    to:
                              mov mem1, reg1         mov mem1, reg1
                              mov mem1, reg2         mov reg1, mem2
                              mov reg1, mem2

                        or (if mem1 depends on reg1
                            and/or if mem2 depends on reg2)
                            to:
                              mov mem1, reg1
                              mov reg1, mem2
                              mov reg1, reg2
                        }
                                        begin
                                          taicpu(hp1).LoadRef(0,taicpu(p).oper[0]^.ref^);
                                          taicpu(hp1).LoadReg(1,taicpu(hp2).oper[1]^.reg);
                                          taicpu(hp2).LoadRef(1,taicpu(hp2).oper[0]^.ref^);
                                          taicpu(hp2).LoadReg(0,taicpu(p).oper[1]^.reg);
                                          allocRegBetween(asmL,taicpu(p).oper[1]^.reg,p,hp2);
                                          if (taicpu(p).oper[0]^.ref^.base <> NR_NO) and
                                             (getsupreg(taicpu(p).oper[0]^.ref^.base) in [RS_EAX,RS_EBX,RS_ECX,RS_EDX,RS_ESI,RS_EDI]) then
                                            allocRegBetween(asmL,taicpu(p).oper[0]^.ref^.base,p,hp2);
                                          if (taicpu(p).oper[0]^.ref^.index <> NR_NO) and
                                             (getsupreg(taicpu(p).oper[0]^.ref^.index) in [RS_EAX,RS_EBX,RS_ECX,RS_EDX,RS_ESI,RS_EDI]) then
                                            allocRegBetween(asmL,taicpu(p).oper[0]^.ref^.index,p,hp2);
                                        end
                                      else
                                        if (taicpu(hp1).Oper[0]^.reg <> taicpu(hp2).Oper[1]^.reg) then
                                          begin
                                            taicpu(hp2).LoadReg(0,taicpu(hp1).Oper[0]^.reg);
                                            allocRegBetween(asmL,taicpu(p).oper[1]^.reg,p,hp2);
                                          end
                                        else
                                          begin
                                            asml.remove(hp2);
                                            hp2.free;
                                          end
                                end
                            end
                          else
(*                          {movl [mem1],reg1
                            movl [mem1],reg2
                            to:
                              movl [mem1],reg1
                              movl reg1,reg2 }
                            if (taicpu(p).oper[0]^.typ = top_ref) and
                              (taicpu(p).oper[1]^.typ = top_reg) and
                              (taicpu(hp1).oper[0]^.typ = top_ref) and
                              (taicpu(hp1).oper[1]^.typ = top_reg) and
                              (taicpu(p).opsize = taicpu(hp1).opsize) and
                              RefsEqual(TReference(taicpu(p).oper[0]^^),taicpu(hp1).oper[0]^^.ref^) and
                              (taicpu(p).oper[1]^.reg<>taicpu(hp1).oper[0]^^.ref^.base) and
                              (taicpu(p).oper[1]^.reg<>taicpu(hp1).oper[0]^^.ref^.index) then
                              taicpu(hp1).LoadReg(0,taicpu(p).oper[1]^.reg)
                            else*)
                            {   movl const1,[mem1]
                                movl [mem1],reg1
                            to:
                                movl const1,reg1
                                movl reg1,[mem1] }
                              if (taicpu(p).oper[0]^.typ = top_const) and
                                 (taicpu(p).oper[1]^.typ = top_ref) and
                                 (taicpu(hp1).oper[0]^.typ = top_ref) and
                                 (taicpu(hp1).oper[1]^.typ = top_reg) and
                                 (taicpu(p).opsize = taicpu(hp1).opsize) and
                                 RefsEqual(taicpu(hp1).oper[0]^.ref^,taicpu(p).oper[1]^.ref^) then
                                begin
                                  allocregbetween(asml,taicpu(hp1).oper[1]^.reg,p,hp1);
                                  { allocregbetween doesn't insert this because at }
                                  { this time, no regalloc info is available in    }
                                  { the optinfo field, so do it manually (JM)      }
                                  hp2 := tai_regalloc.Alloc(taicpu(hp1).oper[1]^.reg);
                                  insertllitem(asml,p.previous,p,hp2);
                                  taicpu(hp1).LoadReg(0,taicpu(hp1).oper[1]^.reg);
                                  taicpu(hp1).LoadRef(1,taicpu(p).oper[1]^.ref^);
                                  taicpu(p).LoadReg(1,taicpu(hp1).oper[0]^.reg);
                                end
                        end;
                    end;
                  A_MOVZX:
                    begin
                    {removes superfluous And's after movzx's}
                      if (taicpu(p).oper[1]^.typ = top_reg) and
                         GetNextInstruction(p, hp1) and
                         (tai(hp1).typ = ait_instruction) and
                         (taicpu(hp1).opcode = A_AND) and
                         (taicpu(hp1).oper[0]^.typ = top_const) and
                         (taicpu(hp1).oper[1]^.typ = top_reg) and
                         (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg) then
                        case taicpu(p).opsize Of
                          S_BL, S_BW:
                            if (taicpu(hp1).oper[0]^.val = $ff) then
                              begin
                                asml.remove(hp1);
                                hp1.free;
                              end;
                          S_WL:
                            if (taicpu(hp1).oper[0]^.val = $ffff) then
                              begin
                                asml.remove(hp1);
                                hp1.free;
                              end;
                        end;
                    {changes some movzx constructs to faster synonims (all examples
                    are given with eax/ax, but are also valid for other registers)}
                      if (taicpu(p).oper[1]^.typ = top_reg) then
                        if (taicpu(p).oper[0]^.typ = top_reg) then
                          case taicpu(p).opsize of
                            S_BW:
                              begin
                                if (getsupreg(taicpu(p).oper[0]^.reg)=getsupreg(taicpu(p).oper[1]^.reg)) and
                                   not(CS_LittleSize in aktglobalswitches) then
                                  {Change "movzbw %al, %ax" to "andw $0x0ffh, %ax"}
                                  begin
                                    taicpu(p).opcode := A_AND;
                                    taicpu(p).changeopsize(S_W);
                                    taicpu(p).LoadConst(0,$ff);
                                  end
                                else if GetNextInstruction(p, hp1) and
                                     (tai(hp1).typ = ait_instruction) and
                                     (taicpu(hp1).opcode = A_AND) and
                                     (taicpu(hp1).oper[0]^.typ = top_const) and
                                     (taicpu(hp1).oper[1]^.typ = top_reg) and
                                     (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg) then
                                 {Change "movzbw %reg1, %reg2; andw $const, %reg2"
                                  to "movw %reg1, reg2; andw $(const1 and $ff), %reg2"}
                                  begin
                                    taicpu(p).opcode := A_MOV;
                                    taicpu(p).changeopsize(S_W);
                                    setsubreg(taicpu(p).oper[0]^.reg,R_SUBW);
                                    taicpu(hp1).LoadConst(0,taicpu(hp1).oper[0]^.val and $ff);
                                  end;
                              end;
                            S_BL:
                              begin
                                if (getsupreg(taicpu(p).oper[0]^.reg)=getsupreg(taicpu(p).oper[1]^.reg)) and
                                   not(CS_LittleSize in aktglobalswitches) then
                                  {Change "movzbl %al, %eax" to "andl $0x0ffh, %eax"}
                                  begin
                                    taicpu(p).opcode := A_AND;
                                    taicpu(p).changeopsize(S_L);
                                    taicpu(p).loadconst(0,$ff)
                                  end
                                else if GetNextInstruction(p, hp1) and
                                    (tai(hp1).typ = ait_instruction) and
                                    (taicpu(hp1).opcode = A_AND) and
                                    (taicpu(hp1).oper[0]^.typ = top_const) and
                                    (taicpu(hp1).oper[1]^.typ = top_reg) and
                                    (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg) then
                                {Change "movzbl %reg1, %reg2; andl $const, %reg2"
                                  to "movl %reg1, reg2; andl $(const1 and $ff), %reg2"}
                                  begin
                                    taicpu(p).opcode := A_MOV;
                                    taicpu(p).changeopsize(S_L);
                                    setsubreg(taicpu(p).oper[0]^.reg,R_SUBWHOLE);
                                    taicpu(hp1).LoadConst(0,taicpu(hp1).oper[0]^.val and $ff);
                                  end
                              end;
                            S_WL:
                              begin
                                if (getsupreg(taicpu(p).oper[0]^.reg)=getsupreg(taicpu(p).oper[1]^.reg)) and
                                   not(CS_LittleSize in aktglobalswitches) then
                                {Change "movzwl %ax, %eax" to "andl $0x0ffffh, %eax"}
                                  begin
                                    taicpu(p).opcode := A_AND;
                                    taicpu(p).changeopsize(S_L);
                                    taicpu(p).LoadConst(0,$ffff);
                                  end
                                else if GetNextInstruction(p, hp1) and
                                    (tai(hp1).typ = ait_instruction) and
                                    (taicpu(hp1).opcode = A_AND) and
                                    (taicpu(hp1).oper[0]^.typ = top_const) and
                                    (taicpu(hp1).oper[1]^.typ = top_reg) and
                                    (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg) then
                                  {Change "movzwl %reg1, %reg2; andl $const, %reg2"
                                  to "movl %reg1, reg2; andl $(const1 and $ffff), %reg2"}
                                  begin
                                    taicpu(p).opcode := A_MOV;
                                    taicpu(p).changeopsize(S_L);
                                    setsubreg(taicpu(p).oper[0]^.reg,R_SUBWHOLE);
                                    taicpu(hp1).LoadConst(0,taicpu(hp1).oper[0]^.val and $ffff);
                                  end;
                              end;
                            end
                          else if (taicpu(p).oper[0]^.typ = top_ref) then
                            begin
                              if GetNextInstruction(p, hp1) and
                                 (tai(hp1).typ = ait_instruction) and
                                 (taicpu(hp1).opcode = A_AND) and
                                 (taicpu(hp1).oper[0]^.typ = Top_Const) and
                                 (taicpu(hp1).oper[1]^.typ = Top_Reg) and
                                 (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg) then
                                begin
                                  taicpu(p).opcode := A_MOV;
                                  case taicpu(p).opsize Of
                                    S_BL:
                                      begin
                                        taicpu(p).changeopsize(S_L);
                                        taicpu(hp1).LoadConst(0,taicpu(hp1).oper[0]^.val and $ff);
                                      end;
                                    S_WL:
                                      begin
                                        taicpu(p).changeopsize(S_L);
                                        taicpu(hp1).LoadConst(0,taicpu(hp1).oper[0]^.val and $ffff);
                                      end;
                                    S_BW:
                                      begin
                                        taicpu(p).changeopsize(S_W);
                                        taicpu(hp1).LoadConst(0,taicpu(hp1).oper[0]^.val and $ff);
                                      end;
                                  end;
                                end;
                            end;
                    end;
(* should not be generated anymore by the current code generator
                  A_POP:
                    begin
                      if target_info.system=system_i386_go32v2 then
                      begin
                        { Transform a series of pop/pop/pop/push/push/push to }
                        { 'movl x(%esp),%reg' for go32v2 (not for the rest,   }
                        { because I'm not sure whether they can cope with     }
                        { 'movl x(%esp),%reg' with x > 0, I believe we had    }
                        { such a problem when using esp as frame pointer (JM) }
                        if (taicpu(p).oper[0]^.typ = top_reg) then
                          begin
                            hp1 := p;
                            hp2 := p;
                            l := 0;
                            while getNextInstruction(hp1,hp1) and
                                  (hp1.typ = ait_instruction) and
                                  (taicpu(hp1).opcode = A_POP) and
                                  (taicpu(hp1).oper[0]^.typ = top_reg) do
                              begin
                                hp2 := hp1;
                                inc(l,4);
                              end;
                            getLastInstruction(p,hp3);
                            l1 := 0;
                            while (hp2 <> hp3) and
                                  assigned(hp1) and
                                  (hp1.typ = ait_instruction) and
                                  (taicpu(hp1).opcode = A_PUSH) and
                                  (taicpu(hp1).oper[0]^.typ = top_reg) and
                                  (taicpu(hp1).oper[0]^.reg.enum = taicpu(hp2).oper[0]^.reg.enum) do
                              begin
                                { change it to a two op operation }
                                taicpu(hp2).oper[1]^.typ:=top_none;
                                taicpu(hp2).ops:=2;
                                taicpu(hp2).opcode := A_MOV;
                                taicpu(hp2).Loadoper(1,taicpu(hp1).oper[0]^);
                                reference_reset(tmpref);
                                tmpRef.base.enum:=R_INTREGISTER;
                                tmpRef.base.number:=NR_STACK_POINTER_REG;
                                convert_register_to_enum(tmpref.base);
                                tmpRef.offset := l;
                                taicpu(hp2).loadRef(0,tmpRef);
                                hp4 := hp1;
                                getNextInstruction(hp1,hp1);
                                asml.remove(hp4);
                                hp4.free;
                                getLastInstruction(hp2,hp2);
                                dec(l,4);
                                inc(l1);
                              end;
                            if l <> -4 then
                              begin
                                inc(l,4);
                                for l1 := l1 downto 1 do
                                  begin
                                    getNextInstruction(hp2,hp2);
                                    dec(taicpu(hp2).oper[0]^.ref^.offset,l);
                                  end
                              end
                          end
                        end
                      else
                        begin
                          if (taicpu(p).oper[0]^.typ = top_reg) and
                            GetNextInstruction(p, hp1) and
                            (tai(hp1).typ=ait_instruction) and
                            (taicpu(hp1).opcode=A_PUSH) and
                            (taicpu(hp1).oper[0]^.typ = top_reg) and
                            (taicpu(hp1).oper[0]^.reg.enum=taicpu(p).oper[0]^.reg.enum) then
                            begin
                              { change it to a two op operation }
                              taicpu(p).oper[1]^.typ:=top_none;
                              taicpu(p).ops:=2;
                              taicpu(p).opcode := A_MOV;
                              taicpu(p).Loadoper(1,taicpu(p).oper[0]^);
                              reference_reset(tmpref);
                              TmpRef.base.enum := R_ESP;
                              taicpu(p).LoadRef(0,TmpRef);
                              asml.remove(hp1);
                              hp1.free;
                            end;
                        end;
                    end;
*)
                  A_PUSH:
                    begin
                      if (taicpu(p).opsize = S_W) and
                         (taicpu(p).oper[0]^.typ = Top_Const) and
                         GetNextInstruction(p, hp1) and
                         (tai(hp1).typ = ait_instruction) and
                         (taicpu(hp1).opcode = A_PUSH) and
                         (taicpu(hp1).oper[0]^.typ = Top_Const) and
                         (taicpu(hp1).opsize = S_W) then
                        begin
                          taicpu(p).changeopsize(S_L);
                          taicpu(p).LoadConst(0,taicpu(p).oper[0]^.val shl 16 + word(taicpu(hp1).oper[0]^.val));
                          asml.remove(hp1);
                          hp1.free;
                        end;
                    end;
                  A_SHL, A_SAL:
                    begin
                      if (taicpu(p).oper[0]^.typ = Top_Const) and
                         (taicpu(p).oper[1]^.typ = Top_Reg) and
                         (taicpu(p).opsize = S_L) and
                         (taicpu(p).oper[0]^.val <= 3) then
                    {Changes "shl const, %reg32; add const/reg, %reg32" to one lea statement}
                        begin
                          TmpBool1 := True; {should we check the next instruction?}
                          TmpBool2 := False; {have we found an add/sub which could be
                                              integrated in the lea?}
                          reference_reset(tmpref);
                          TmpRef.index := taicpu(p).oper[1]^.reg;
                          TmpRef.scalefactor := 1 shl taicpu(p).oper[0]^.val;
                          while TmpBool1 and
                                GetNextInstruction(p, hp1) and
                                (tai(hp1).typ = ait_instruction) and
                                ((((taicpu(hp1).opcode = A_ADD) or
                                   (taicpu(hp1).opcode = A_SUB)) and
                                  (taicpu(hp1).oper[1]^.typ = Top_Reg) and
                                  (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg)) or
                                 (((taicpu(hp1).opcode = A_INC) or
                                   (taicpu(hp1).opcode = A_DEC)) and
                                  (taicpu(hp1).oper[0]^.typ = Top_Reg) and
                                  (taicpu(hp1).oper[0]^.reg = taicpu(p).oper[1]^.reg))) Do
                            begin
                              TmpBool1 := False;
                              if (taicpu(hp1).oper[0]^.typ = Top_Const) then
                                begin
                                  TmpBool1 := True;
                                  TmpBool2 := True;
                                  case taicpu(hp1).opcode of
                                    A_ADD:
                                      inc(TmpRef.offset, longint(taicpu(hp1).oper[0]^.val));
                                    A_SUB:
                                      dec(TmpRef.offset, longint(taicpu(hp1).oper[0]^.val));
                                  end;
                                  asml.remove(hp1);
                                  hp1.free;
                                end
                              else
                                if (taicpu(hp1).oper[0]^.typ = Top_Reg) and
                                   (((taicpu(hp1).opcode = A_ADD) and
                                     (TmpRef.base = NR_NO)) or
                                    (taicpu(hp1).opcode = A_INC) or
                                    (taicpu(hp1).opcode = A_DEC)) then
                                  begin
                                    TmpBool1 := True;
                                    TmpBool2 := True;
                                    case taicpu(hp1).opcode of
                                      A_ADD:
                                        TmpRef.base := taicpu(hp1).oper[0]^.reg;
                                      A_INC:
                                        inc(TmpRef.offset);
                                      A_DEC:
                                        dec(TmpRef.offset);
                                    end;
                                    asml.remove(hp1);
                                    hp1.free;
                                  end;
                            end;
                          if TmpBool2 or
                             ((aktoptprocessor < ClassPentium2) and
                             (taicpu(p).oper[0]^.val <= 3) and
                             not(CS_LittleSize in aktglobalswitches)) then
                            begin
                              if not(TmpBool2) and
                                  (taicpu(p).oper[0]^.val = 1) then
                                begin
                                  hp1 := taicpu.Op_reg_reg(A_ADD,taicpu(p).opsize,
                                            taicpu(p).oper[1]^.reg, taicpu(p).oper[1]^.reg)
                                end
                              else
                                hp1 := taicpu.op_ref_reg(A_LEA, S_L, TmpRef,
                                            taicpu(p).oper[1]^.reg);
                              InsertLLItem(asml,p.previous, p.next, hp1);
                              p.free;
                              p := hp1;
                            end;
                        end
                      else
                        if (aktoptprocessor < ClassPentium2) and
                           (taicpu(p).oper[0]^.typ = top_const) and
                           (taicpu(p).oper[1]^.typ = top_reg) then
                          if (taicpu(p).oper[0]^.val = 1) then
    {changes "shl $1, %reg" to "add %reg, %reg", which is the same on a 386,
    but faster on a 486, and Tairable in both U and V pipes on the Pentium
    (unlike shl, which is only Tairable in the U pipe)}
                            begin
                              hp1 := taicpu.Op_reg_reg(A_ADD,taicpu(p).opsize,
                                        taicpu(p).oper[1]^.reg, taicpu(p).oper[1]^.reg);
                              InsertLLItem(asml,p.previous, p.next, hp1);
                              p.free;
                              p := hp1;
                            end
                          else if (taicpu(p).opsize = S_L) and
                                  (taicpu(p).oper[0]^.val<= 3) then
                    {changes "shl $2, %reg" to "lea (,%reg,4), %reg"
                            "shl $3, %reg" to "lea (,%reg,8), %reg}
                              begin
                                reference_reset(tmpref);
                                TmpRef.index := taicpu(p).oper[1]^.reg;
                                TmpRef.scalefactor := 1 shl taicpu(p).oper[0]^.val;
                                hp1 := taicpu.Op_ref_reg(A_LEA,S_L,TmpRef, taicpu(p).oper[1]^.reg);
                                InsertLLItem(asml,p.previous, p.next, hp1);
                                p.free;
                                p := hp1;
                              end
                    end;
                  A_SETcc :
                    { changes
                        setcc (funcres)             setcc reg
                        movb (funcres), reg      to leave/ret
                        leave/ret                               }
                    begin
                      if (taicpu(p).oper[0]^.typ = top_ref) and
                         GetNextInstruction(p, hp1) and
                         GetNextInstruction(hp1, hp2) and
                         (hp2.typ = ait_instruction) and
                         ((taicpu(hp2).opcode = A_LEAVE) or
                          (taicpu(hp2).opcode = A_RET)) and
                         (taicpu(p).oper[0]^.ref^.base = current_procinfo.FramePointer) and
                         (taicpu(p).oper[0]^.ref^.index = NR_NO) and
                         (taicpu(p).oper[0]^.ref^.offset >= tvarsym(current_procinfo.procdef.funcretsym).localloc.reference.offset) and
                         (hp1.typ = ait_instruction) and
                         (taicpu(hp1).opcode = A_MOV) and
                         (taicpu(hp1).opsize = S_B) and
                         (taicpu(hp1).oper[0]^.typ = top_ref) and
                         RefsEqual(taicpu(hp1).oper[0]^.ref^, taicpu(p).oper[0]^.ref^) then
                        begin
                          taicpu(p).LoadReg(0,taicpu(hp1).oper[1]^.reg);
                          asml.remove(hp1);
                          hp1.free;
                        end
                    end;
                  A_SUB:
                    { * change "subl $2, %esp; pushw x" to "pushl x"}
                    { * change "sub/add const1, reg" or "dec reg" followed by
                        "sub const2, reg" to one "sub ..., reg" }
                    begin
                      if (taicpu(p).oper[0]^.typ = top_const) and
                         (taicpu(p).oper[1]^.typ = top_reg) then
                        if (taicpu(p).oper[0]^.val = 2) and
                           (taicpu(p).oper[1]^.reg = NR_ESP) and
                           { Don't do the sub/push optimization if the sub }
                           { comes from setting up the stack frame (JM)    }
                           (not getLastInstruction(p,hp1) or
                           (hp1.typ <> ait_instruction) or
                           (taicpu(hp1).opcode <> A_MOV) or
                           (taicpu(hp1).oper[0]^.typ <> top_reg) or
                           (taicpu(hp1).oper[0]^.reg <> NR_ESP) or
                           (taicpu(hp1).oper[1]^.typ <> top_reg) or
                           (taicpu(hp1).oper[1]^.reg <> NR_EBP)) then
                          begin
                            hp1 := tai(p.next);
                            while Assigned(hp1) and
                                  (tai(hp1).typ in [ait_instruction]+SkipInstr) and
                                  not regReadByInstruction(RS_ESP,hp1) and
                                  not regModifiedByInstruction(RS_ESP,hp1) do
                              hp1 := tai(hp1.next);
                            if Assigned(hp1) and
                               (tai(hp1).typ = ait_instruction) and
                               (taicpu(hp1).opcode = A_PUSH) and
                               (taicpu(hp1).opsize = S_W) then
                              begin
                                taicpu(hp1).changeopsize(S_L);
                                if taicpu(hp1).oper[0]^.typ=top_reg then
                                  setsubreg(taicpu(hp1).oper[0]^.reg,R_SUBWHOLE);
                                hp1 := tai(p.next);
                                asml.remove(p);
                                p.free;
                                p := hp1;
                                continue
                              end;
                            if DoSubAddOpt(p) then
                              continue;
                          end
                        else if DoSubAddOpt(p) then
                          continue
                    end;
                end;
            end; { if is_jmp }
          end;
      end;
      updateUsedRegs(UsedRegs,p);
      p:=tai(p.next);
    end;
end;


function isFoldableArithOp(hp1: taicpu; reg: tregister): boolean;
begin
  isFoldableArithOp := False;
  case hp1.opcode of
    A_ADD,A_SUB,A_OR,A_XOR,A_AND,A_SHL,A_SHR,A_SAR:
      isFoldableArithOp :=
        ((taicpu(hp1).oper[0]^.typ = top_const) or
         ((taicpu(hp1).oper[0]^.typ = top_reg) and
          (taicpu(hp1).oper[0]^.reg <> reg))) and
        (taicpu(hp1).oper[1]^.typ = top_reg) and
        (taicpu(hp1).oper[1]^.reg = reg);
    A_INC,A_DEC:
      isFoldableArithOp :=
        (taicpu(hp1).oper[0]^.typ = top_reg) and
        (taicpu(hp1).oper[0]^.reg = reg);
  end;
end;


procedure PeepHoleOptPass2(asml: taasmoutput; BlockStart, BlockEnd: tai);

  function CanBeCMOV(p : tai) : boolean;
    begin
       CanBeCMOV:=assigned(p) and (p.typ=ait_instruction) and
         (taicpu(p).opcode=A_MOV) and
         (taicpu(p).opsize in [S_L,S_W]) and
         (taicpu(p).oper[0]^.typ in [top_reg,top_ref]) and
         (taicpu(p).oper[1]^.typ in [top_reg]);
    end;

var
  p,hp1,hp2: tai;
{$ifdef  USECMOV}
  l : longint;
  condition : tasmcond;
  hp3: tai;
{$endif USECMOV}
  UsedRegs, TmpUsedRegs: TRegSet;

begin
  p := BlockStart;
  UsedRegs := [];
  while (p <> BlockEnd) Do
    begin
      UpdateUsedRegs(UsedRegs, tai(p.next));
      case p.Typ Of
        Ait_Instruction:
          begin
            case taicpu(p).opcode Of
{$ifdef USECMOV}
              A_Jcc:
                if (aktspecificoptprocessor>=ClassPentium2) then
                  begin
                     { check for
                            jCC   xxx
                            <several movs>
                         xxx:
                     }
                     l:=0;
                     GetNextInstruction(p, hp1);
                     while assigned(hp1) and
                       CanBeCMOV(hp1) do
                       begin
                          inc(l);
                          GetNextInstruction(hp1,hp1);
                       end;
                     if assigned(hp1) then
                       begin
                          if FindLabel(tasmlabel(taicpu(p).oper[0]^.sym),hp1) then
                            begin
                               if (l<=4) and (l>0) then
                                 begin
                                    condition:=inverse_cond[taicpu(p).condition];
                                    GetNextInstruction(p,hp1);
                                    asml.remove(p);
                                    p.free;
                                    p:=hp1;
                                    repeat
                                      taicpu(hp1).opcode:=A_CMOVcc;
                                      taicpu(hp1).condition:=condition;
                                      GetNextInstruction(hp1,hp1);
                                    until not(assigned(hp1)) or
                                      not(CanBeCMOV(hp1));
                                    asml.remove(hp1);
                                    hp1.free;
                                    continue;
                                 end;
                            end
                          else
                            begin
                               { check further for
                                      jCC   xxx
                                      <several movs>
                                      jmp   yyy
                              xxx:
                                      <several movs>
                              yyy:
                               }
                              { hp2 points to jmp xxx }
                              hp2:=hp1;
                              { skip hp1 to xxx }
                              GetNextInstruction(hp1, hp1);
                              if assigned(hp2) and
                                assigned(hp1) and
                                (l<=3) and
                                (hp2.typ=ait_instruction) and
                                (taicpu(hp2).is_jmp) and
                                (taicpu(hp2).condition=C_None) and
                                FindLabel(tasmlabel(taicpu(p).oper[0]^.sym),hp1) then
                                 begin
                                    l:=0;
                                    while assigned(hp1) and
                                      CanBeCMOV(hp1) do
                                      begin
                                         inc(l);
                                         GetNextInstruction(hp1, hp1);
                                      end;
                                 end;
                              {
                              if assigned(hp1) and
                                FindLabel(tasmlabel(taicpu(hp2).oper[0]^.sym),hp1) then
                                begin
                                   condition:=inverse_cond[taicpu(p).condition];
                                   GetNextInstruction(p,hp1);
                                   asml.remove(p);
                                   p.free;
                                   p:=hp1;
                                   repeat
                                     taicpu(hp1).opcode:=A_CMOVcc;
                                     taicpu(hp1).condition:=condition;
                                     GetNextInstruction(hp1,hp1);
                                   until not(assigned(hp1)) or
                                     not(CanBeCMOV(hp1));
                                   hp2:=hp1.next;
                                   condition:=inverse_cond[condition];

                                   asml.remove(hp1.next)
                                   hp1.next.free;
                                   asml.remove(hp1);
                                   hp1.free;
                                   continue;
                                end;
                              }
                            end;
                       end;
                  end;
{$endif USECMOV}
              A_FSTP,A_FISTP:
                if doFpuLoadStoreOpt(asmL,p) then
                  continue;
              A_IMUL:
                begin
                  if (taicpu(p).ops >= 2) and
                     ((taicpu(p).oper[0]^.typ = top_const) or
                      (taicpu(p).oper[0]^.typ = top_symbol)) and
                     (taicpu(p).oper[1]^.typ = top_reg) and
                     ((taicpu(p).ops = 2) or
                      ((taicpu(p).oper[2]^.typ = top_reg) and
                       (taicpu(p).oper[2]^.reg = taicpu(p).oper[1]^.reg))) and
                     getLastInstruction(p,hp1) and
                     (hp1.typ = ait_instruction) and
                     (taicpu(hp1).opcode = A_MOV) and
                     (taicpu(hp1).oper[0]^.typ = top_reg) and
                     (taicpu(hp1).oper[1]^.typ = top_reg) and
                     (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg) then
              { change "mov reg1,reg2; imul y,reg2" to "imul y,reg1,reg2" }
                    begin
                      taicpu(p).ops := 3;
                      taicpu(p).loadreg(1,taicpu(hp1).oper[0]^.reg);
                      taicpu(p).loadreg(2,taicpu(hp1).oper[1]^.reg);
                      asml.remove(hp1);
                      hp1.free;
                    end;
                end;
              A_MOV:
                begin
                  if (taicpu(p).oper[0]^.typ = top_reg) and
                     (taicpu(p).oper[1]^.typ = top_reg) and
                     GetNextInstruction(p, hp1) and
                     (hp1.typ = ait_Instruction) and
                     ((taicpu(hp1).opcode = A_MOV) or
                      (taicpu(hp1).opcode = A_MOVZX) or
                      (taicpu(hp1).opcode = A_MOVSX)) and
                     (taicpu(hp1).oper[0]^.typ = top_ref) and
                     (taicpu(hp1).oper[1]^.typ = top_reg) and
                     ((taicpu(hp1).oper[0]^.ref^.base = taicpu(p).oper[1]^.reg) or
                      (taicpu(hp1).oper[0]^.ref^.index = taicpu(p).oper[1]^.reg)) and
                     (getsupreg(taicpu(hp1).oper[1]^.reg) = getsupreg(taicpu(p).oper[1]^.reg)) then
              {mov reg1, reg2
               mov/zx/sx (reg2, ..), reg2      to   mov/zx/sx (reg1, ..), reg2}
                    begin
                      if (taicpu(hp1).oper[0]^.ref^.base = taicpu(p).oper[1]^.reg) then
                        taicpu(hp1).oper[0]^.ref^.base := taicpu(p).oper[0]^.reg;
                      if (taicpu(hp1).oper[0]^.ref^.index = taicpu(p).oper[1]^.reg) then
                        taicpu(hp1).oper[0]^.ref^.index := taicpu(p).oper[0]^.reg;
                      asml.remove(p);
                      p.free;
                      p := hp1;
                      continue;
                    end
                  else if (taicpu(p).oper[0]^.typ = top_ref) and
                     GetNextInstruction(p,hp1) and
                     (hp1.typ = ait_instruction) and
                     IsFoldableArithOp(taicpu(hp1),taicpu(p).oper[1]^.reg) and
                     GetNextInstruction(hp1,hp2) and
                     (hp2.typ = ait_instruction) and
                     (taicpu(hp2).opcode = A_MOV) and
                     (taicpu(hp2).oper[0]^.typ = top_reg) and
                     (taicpu(hp2).oper[0]^.reg = taicpu(p).oper[1]^.reg) and
                     (taicpu(hp2).oper[1]^.typ = top_ref) then
                    begin
                      TmpUsedRegs := UsedRegs;
                      UpdateUsedRegs(TmpUsedRegs,tai(hp1.next));
                      if (RefsEqual(taicpu(hp2).oper[1]^.ref^, taicpu(p).oper[0]^.ref^) and
                         not(RegUsedAfterInstruction(taicpu(p).oper[1]^.reg,
                              hp2, TmpUsedRegs))) then
  { change   mov            (ref), reg            }
  {          add/sub/or/... reg2/$const, reg      }
  {          mov            reg, (ref)            }
  {          # release reg                        }
  { to       add/sub/or/... reg2/$const, (ref)    }
                        begin
                          case taicpu(hp1).opcode of
                            A_INC,A_DEC:
                              taicpu(hp1).LoadRef(0,taicpu(p).oper[0]^.ref^)
                            else
                              taicpu(hp1).LoadRef(1,taicpu(p).oper[0]^.ref^);
                          end;
                          asml.remove(p);
                          asml.remove(hp2);
                          p.free;
                          hp2.free;
                          p := hp1
                        end;
                    end
                end;
            end;
          end;
      end;
      p := tai(p.next)
    end;
end;


procedure PostPeepHoleOpts(asml: taasmoutput; BlockStart, BlockEnd: tai);
var
  p,hp1,hp2: tai;
begin
  p := BlockStart;
  while (p <> BlockEnd) Do
    begin
      case p.Typ Of
        Ait_Instruction:
          begin
            case taicpu(p).opcode Of
              A_CALL:
                if (AktOptProcessor < ClassPentium2) and
                   GetNextInstruction(p, hp1) and
                   (hp1.typ = ait_instruction) and
                   (taicpu(hp1).opcode = A_JMP) and
                   (taicpu(hp1).oper[0]^.typ = top_symbol) then
                  begin
                    hp2 := taicpu.Op_sym(A_PUSH,S_L,taicpu(hp1).oper[0]^.sym);
                    InsertLLItem(asml, p.previous, p, hp2);
                    taicpu(p).opcode := A_JMP;
                    taicpu(p).is_jmp := true;
                    asml.remove(hp1);
                    hp1.free;
                  end;
              A_CMP:
                begin
                  if (taicpu(p).oper[0]^.typ = top_const) and
                     (taicpu(p).oper[0]^.val = 0) and
                     (taicpu(p).oper[1]^.typ = top_reg) then
                   {change "cmp $0, %reg" to "test %reg, %reg"}
                    begin
                      taicpu(p).opcode := A_TEST;
                      taicpu(p).loadreg(0,taicpu(p).oper[1]^.reg);
                      continue;
                    end;
                end;
(*
Optimization is not safe; xor clears the carry flag.
See test/tgadint64 in the test suite.
              A_MOV:
                if (taicpu(p).oper[0]^.typ = Top_Const) and
                   (taicpu(p).oper[0]^.val = 0) and
                   (taicpu(p).oper[1]^.typ = Top_Reg) then
                  { change "mov $0, %reg" into "xor %reg, %reg" }
                  begin
                    taicpu(p).opcode := A_XOR;
                    taicpu(p).LoadReg(0,taicpu(p).oper[1]^.reg);
                  end;
*)
              A_MOVZX:
                { if register vars are on, it's possible there is code like }
                {   "cmpl $3,%eax; movzbl 8(%ebp),%ebx; je .Lxxx"           }
                { so we can't safely replace the movzx then with xor/mov,   }
                { since that would change the flags (JM)                    }
                if not(cs_regvars in aktglobalswitches) then
                 begin
                  if (taicpu(p).oper[1]^.typ = top_reg) then
                    if (taicpu(p).oper[0]^.typ = top_reg)
                      then
                        case taicpu(p).opsize of
                          S_BL:
                            begin
                              if IsGP32Reg(getsupreg(taicpu(p).oper[1]^.reg)) and
                                 not(CS_LittleSize in aktglobalswitches) and
                                 (aktoptprocessor = ClassPentium) then
                                  {Change "movzbl %reg1, %reg2" to
                                   "xorl %reg2, %reg2; movb %reg1, %reg2" for Pentium and
                                   PentiumMMX}
                                begin
                                  hp1 := taicpu.op_reg_reg(A_XOR, S_L,
                                              taicpu(p).oper[1]^.reg, taicpu(p).oper[1]^.reg);
                                  InsertLLItem(asml,p.previous, p, hp1);
                                  taicpu(p).opcode := A_MOV;
                                  taicpu(p).changeopsize(S_B);
                                  setsubreg(taicpu(p).oper[1]^.reg,R_SUBL);
                                end;
                            end;
                        end
                      else if (taicpu(p).oper[0]^.typ = top_ref) and
                          (taicpu(p).oper[0]^.ref^.base <> taicpu(p).oper[1]^.reg) and
                          (taicpu(p).oper[0]^.ref^.index <> taicpu(p).oper[1]^.reg) and
                          not(CS_LittleSize in aktglobalswitches) and
                          IsGP32Reg(getsupreg(taicpu(p).oper[1]^.reg)) and
                          (aktoptprocessor = ClassPentium) and
                          (taicpu(p).opsize = S_BL) then
                        {changes "movzbl mem, %reg" to "xorl %reg, %reg; movb mem, %reg8" for
                          Pentium and PentiumMMX}
                        begin
                          hp1 := taicpu.Op_reg_reg(A_XOR, S_L, taicpu(p).oper[1]^.reg,
                                      taicpu(p).oper[1]^.reg);
                          taicpu(p).opcode := A_MOV;
                          taicpu(p).changeopsize(S_B);
                          setsubreg(taicpu(p).oper[1]^.reg,R_SUBL);
                          InsertLLItem(asml,p.previous, p, hp1);
                        end;
                 end;
              A_TEST, A_OR:
                {removes the line marked with (x) from the sequence
                 and/or/xor/add/sub/... $x, %y
                 test/or %y, %y   (x)
                 j(n)z _Label
                    as the first instruction already adjusts the ZF}
                 begin
                   if OpsEqual(taicpu(p).oper[0]^,taicpu(p).oper[1]^) then
                    if GetLastInstruction(p, hp1) and
                      (tai(hp1).typ = ait_instruction) then
                     case taicpu(hp1).opcode Of
                       A_ADD, A_SUB, A_OR, A_XOR, A_AND{, A_SHL, A_SHR}:
                         begin
                           if OpsEqual(taicpu(hp1).oper[1]^,taicpu(p).oper[0]^) then
                             begin
                               hp1 := tai(p.next);
                               asml.remove(p);
                               p.free;
                               p := tai(hp1);
                               continue
                             end;
                         end;
                       A_DEC, A_INC, A_NEG:
                         begin
                           if OpsEqual(taicpu(hp1).oper[0]^,taicpu(p).oper[0]^) then
                             begin
                               case taicpu(hp1).opcode Of
                                 A_DEC, A_INC:
 {replace inc/dec with add/sub 1, because inc/dec doesn't set the carry flag}
                                   begin
                                     case taicpu(hp1).opcode Of
                                       A_DEC: taicpu(hp1).opcode := A_SUB;
                                       A_INC: taicpu(hp1).opcode := A_ADD;
                                     end;
                                     taicpu(hp1).Loadoper(1,taicpu(hp1).oper[0]^);
                                     taicpu(hp1).LoadConst(0,1);
                                     taicpu(hp1).ops:=2;
                                   end
                                 end;
                               hp1 := tai(p.next);
                               asml.remove(p);
                               p.free;
                               p := tai(hp1);
                               continue
                             end;
                         end
                     end
                 end;
            end;
          end;
      end;
      p := tai(p.next)
    end;
end;



end.

{
  $Log$
  Revision 1.53  2003-12-15 21:25:49  peter
    * reg allocations for imaginary register are now inserted just
      before reg allocation
    * tregister changed to enum to allow compile time check
    * fixed several tregister-tsuperregister errors

  Revision 1.52  2003/12/14 22:42:14  peter
    * fixed csdebug

  Revision 1.51  2003/12/13 15:48:47  jonas
    * isgp32reg was being called with both tsuperregister and tregister
      parameters, so changed type to tsuperregister (fixes bug reported by
      Bas Steendijk)
    * improved regsizesok() checking so it gives no false positives anymore

  Revision 1.50  2003/11/22 00:40:19  jonas
    * fixed optimiser so it compiles again
    * fixed several bugs which were in there already for a long time, but
      which only popped up now :) -O2/-O3 will now optimise less than in
      the past (and correctly so), but -O2u/-O3u will optimise a bit more
    * some more small improvements for -O3 are still possible

  Revision 1.49  2003/11/07 15:58:32  florian
    * Florian's culmutative nr. 1; contains:
      - invalid calling conventions for a certain cpu are rejected
      - arm softfloat calling conventions
      - -Sp for cpu dependend code generation
      - several arm fixes
      - remaining code for value open array paras on heap

  Revision 1.48  2003/08/09 18:56:54  daniel
    * cs_regalloc renamed to cs_regvars to avoid confusion with register
      allocator
    * Some preventive changes to i386 spillinh code

  Revision 1.47  2003/06/08 18:48:03  jonas
    * first small steps towards an oop optimizer

  Revision 1.46  2003/06/03 21:09:05  peter
    * internal changeregsize for optimizer
    * fix with a hack to not remove the first instruction of a block
      which will leave blockstart pointing to invalid memory

  Revision 1.45  2003/06/02 21:42:05  jonas
    * function results can now also be regvars
    - removed tprocinfo.return_offset, never use it again since it's invalid
      if the result is a regvar

  Revision 1.44  2003/05/30 23:57:08  peter
    * more sparc cleanup
    * accumulator removed, splitted in function_return_reg (called) and
      function_result_reg (caller)

  Revision 1.43  2003/04/27 11:21:35  peter
    * aktprocdef renamed to current_procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.42  2003/03/28 19:16:57  peter
    * generic constructor working for i386
    * remove fixed self register
    * esi added as address register for i386

  Revision 1.41  2003/02/26 13:24:59  daniel
    * Disabled mov reg,0 -> xor reg,reg optimization

  Revision 1.40  2003/02/25 07:41:54  daniel
    * Properly fixed reversed operands bug

  Revision 1.39  2003/02/24 21:27:01  daniel
    * Reversed operand order in an optimization in postpeepholeopt

  Revision 1.38  2003/02/19 22:39:56  daniel
    * Fixed a few issues

  Revision 1.37  2003/02/19 22:00:16  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.36  2003/01/08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.35  2002/11/15 16:30:54  peter
    * made tasmsymbol.refs private (merged)

  Revision 1.34  2002/08/18 20:06:30  peter
    * inlining is now also allowed in interface
    * renamed write/load to ppuwrite/ppuload
    * tnode storing in ppu
    * nld,ncon,nbas are already updated for storing in ppu

  Revision 1.33  2002/08/17 09:23:46  florian
    * first part of procinfo rewrite

  Revision 1.32  2002/08/11 14:32:30  peter
    * renamed current_library to objectlibrary

  Revision 1.31  2002/08/11 13:24:17  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. in the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.30  2002/07/26 21:15:43  florian
    * rewrote the system handling

  Revision 1.29  2002/07/01 18:46:34  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.28  2002/06/09 12:55:23  jonas
    * fixed detection of register usage

  Revision 1.27  2002/05/18 13:34:25  peter
    * readded missing revisions

  Revision 1.26  2002/05/16 19:46:52  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.24  2002/05/12 16:53:18  peter
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

  Revision 1.23  2002/04/21 15:40:49  carl
  * changeregsize -> changeregsize

  Revision 1.22  2002/04/20 21:37:07  carl
  + generic FPC_CHECKPOINTER
  + first parameter offset in stack now portable
  * rename some constants
  + move some cpu stuff to other units
  - remove unused constents
  * fix stacksize for some targets
  * fix generic size problems which depend now on EXTEND_SIZE constant
  * removing frame pointer in routines is only available for : i386,m68k and vis targets

  Revision 1.21  2002/04/15 19:44:21  peter
    * fixed stackcheck that would be called recursively when a stack
      error was found
    * generic changeregsize(reg,size) for i386 register resizing
    * removed some more routines from cga unit
    * fixed returnvalue handling
    * fixed default stacksize of linux and go32v2, 8kb was a bit small :-)

  Revision 1.20  2002/04/02 20:30:16  jonas
    + support for folding inc/dec in shl/add/sub sequences toa single lea
      instruction

  Revision 1.19  2002/04/02 13:01:58  jonas
    * fixed nasty bug in "and" peepholeoptimization that caused wrong
      optimizations after Peter's big location patch

  Revision 1.18  2002/03/31 20:26:40  jonas
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

}
