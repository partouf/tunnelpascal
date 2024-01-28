{
    Copyright (c) 1998-2004 by Jonas Maebe

    This unit calls the optimization procedures to optimize the assembler
    code for sparc

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

unit aoptcpu;

{$i fpcdefs.inc}

interface

uses cpubase, aasmtai, aopt, aoptx86;

type
  TCpuAsmOptimizer = class(TX86AsmOptimizer)
    function PrePeepHoleOptsCpu(var p: tai): boolean; override;
    function PeepHoleOptPass1Cpu(var p: tai): boolean; override;
    function PeepHoleOptPass2Cpu(var p: tai): boolean; override;
    function PostPeepHoleOptsCpu(var p : tai) : boolean; override;
  end;

implementation

uses
  globals,
  cgbase,
  globtype,
  aasmcpu;

    function TCpuAsmOptimizer.PrePeepHoleOptsCpu(var p : tai) : boolean;
      begin
        result := false;
        case p.typ of
          ait_instruction:
            begin
              case taicpu(p).opcode of
                A_IMUL:
                  result:=PrePeepholeOptIMUL(p);
                A_SAR,A_SHR:
                  result:=PrePeepholeOptSxx(p);
                A_AND:
                  Result:=PrePeepholeOptAND(p);
                else
                  ;
              end;
            end;
          else
            ;
        end;

        { If this flag is set, something was optimised ahead of p, so move
          ahead by 1 instruction but treat as if Result was set to True }
        if aoc_ForceNewIteration in OptsToCheck then
          begin
            Exclude(OptsToCheck, aoc_ForceNewIteration);

            if not Result then
              begin
                if (p.typ in SkipInstr) then
                  UpdateUsedRegs(p);

                p := tai(p.Next);
                Result := True;
              end;
          end;
      end;


    function TCpuAsmOptimizer.PeepHoleOptPass1Cpu(var p: tai): boolean;
      var
        CurrentReg: TRegister;
      begin
        result:=False;
        case p.typ of
          ait_instruction:
            begin
              case taicpu(p).opcode of
                A_ADD:
                  Result:=OptPass1ADD(p);
                A_AND:
                  Result:=OptPass1AND(p);
                A_IMUL:
                  Result:=OptPass1Imul(p);
                A_MOV:
                  Result:=OptPass1MOV(p);
                A_MOVSX,
                A_MOVSXD,
                A_MOVZX:
                  Result:=OptPass1Movx(p);
                A_MOVDQA,
                A_MOVAPD,
                A_MOVAPS,
                A_MOVUPD,
                A_MOVUPS,
                A_VMOVAPS,
                A_VMOVAPD,
                A_VMOVUPS,
                A_VMOVUPD:
                  result:=OptPass1_V_MOVAP(p);
                A_VMINSS,
                A_VMINSD,
                A_VMAXSS,
                A_VMAXSD,
                A_VSQRTSD,
                A_VSQRTSS,
                A_VDIVSD,
                A_VDIVSS,
                A_VSUBSD,
                A_VSUBSS,
                A_VMULSD,
                A_VMULSS,
                A_VADDSD,
                A_VADDSS,
                A_VANDPD,
                A_VANDPS,
                A_VORPD,
                A_VORPS:
                  result:=OptPass1VOP(p);
                A_MULSD,
                A_MULSS,
                A_ADDSD,
                A_ADDSS:
                  result:=OptPass1OP(p);
                A_VMOVSD,
                A_VMOVSS,
                A_MOVSD,
                A_MOVSS:
                  result:=OptPass1MOVXX(p);
                A_LEA:
                  result:=OptPass1LEA(p);
                A_SUB:
                  result:=OptPass1Sub(p);
                A_SHL,A_SAL:
                  result:=OptPass1SHLSAL(p);
                A_SHR:
                  result:=OptPass1SHR(p);
                A_FSTP,A_FISTP:
                  result:=OptPass1FSTP(p);
                A_FLD:
                  result:=OptPass1FLD(p);
                A_CMP:
                  result:=OptPass1Cmp(p);
                A_VPXORD,
                A_VPXORQ,
                A_VXORPS,
                A_VXORPD,
                A_VPXOR:
                  Result:=OptPass1VPXor(p);
                A_VMOVDQA,
                A_VMOVDQU:
                  Result:=OptPass1VMOVDQ(p);
                A_XORPS,
                A_XORPD,
                A_PXOR:
                  Result:=OptPass1PXor(p);
                A_TEST:
                  Result:=OptPass1Test(p);
                A_Jcc:
                  Result:=OptPass1Jcc(p);
                A_SHRX,
                A_SHLX:
                  Result:=OptPass1SHXX(p);
                A_VCVTSS2SD,
                A_CVTSS2SD:
                  Result:=OptPass1_V_Cvtss2sd(p);
                else
                  ;
              end;

              { If a 32-bit register is written, search ahead for a MOV instruction,
                if one is found that copies that register to another, leave a hint
                for the compiler that the upper 32 bits are zero.  This permits even
                deeper optimisations wtih DeepMovOPT }
              if not Result and
                { This is anything but quick! }
                (cs_opt_level3 in current_settings.optimizerswitches) and
                (taicpu(p).opsize in [S_NO, S_L, S_BL, S_WL]) then
                begin
                  if MatchInstruction(p, A_MUL, A_DIV, [{ No need to specify S_L again }]) then
                    begin
                      ZeroUpperHint(p, NR_EAX);
                      ZeroUpperHint(p, NR_EDX);
                    end
                  else if MatchInstruction(p, A_IMUL, A_IDIV, []) then
                    begin
                      { IMUL and IDIV only write to registers }
                      if (taicpu(p).ops = 1) then
                        begin
                          ZeroUpperHint(p, NR_EAX);
                          ZeroUpperHint(p, NR_EDX);
                        end
                      else
                        ZeroUpperHint(p, taicpu(p).oper[taicpu(p).ops - 1]^.reg);
                    end
                  else if
                    (
                      { There might be a better way to do this, but for now, just check
                        for particular opcodes }
                      MatchInstruction(p, [A_ADD, A_ADC, A_SUB, A_SBB, A_INC, A_DEC, A_MULX,
                        A_LEA, A_MOV, A_MOVZX, A_MOVSX { NOTE: not MOVSXD },
                        A_ANDN, A_AND, A_OR, A_XOR, A_NEG, A_NOT,
                        A_BSF, A_BSR, A_BZHI, A_LZCNT, A_POPCNT,
                        A_SHL, A_SHR, A_SAR, A_ROL, A_ROR, A_SHLX, A_SHRX, A_SARX, A_RORX,
                        A_CVTSD2SI, A_VCVTSD2SI, A_CVTSS2SI, A_VCVTSS2SI], []
                      )
                    ) and
                    (taicpu(p).oper[taicpu(p).ops - 1]^.typ = top_reg) then
                    begin
                      CurrentReg := taicpu(p).oper[taicpu(p).ops - 1]^.reg;
                      if getsubreg(CurrentReg) = R_SUBD then
                        ZeroUpperHint(p, CurrentReg);
                    end;
                end;
            end;
          else
            ;
        end;
        { If this flag is set, force another run of pass 1 even if p wasn't
          changed }
        if aoc_ForceNewIteration in OptsToCheck then
          begin
            Exclude(OptsToCheck, aoc_ForceNewIteration);

            if not Result then
              begin
                if (p.typ in SkipInstr) then
                  UpdateUsedRegs(p);

                p := tai(p.Next);
                Result := True;
              end;
          end;
      end;


    function TCpuAsmOptimizer.PeepHoleOptPass2Cpu(var p : tai) : boolean;
      begin
        Result := False;
        case p.typ of
          ait_instruction:
            begin
              case taicpu(p).opcode of
                A_MOV:
                  Result:=OptPass2MOV(p);
                A_MOVZX:
                  Result:=OptPass2Movx(p);
                A_IMUL:
                  Result:=OptPass2Imul(p);
                A_JMP:
                  Result:=OptPass2Jmp(p);
                A_Jcc:
                  Result:=OptPass2Jcc(p);
                A_Lea:
                  Result:=OptPass2Lea(p);
                A_SUB:
                  Result:=OptPass2SUB(p);
                A_ADD:
                  Result:=OptPass2ADD(p);
                A_SETcc:
                  result:=OptPass2SETcc(p);
                A_CMP:
                  Result:=OptPass2CMP(p);
                A_TEST:
                  Result:=OptPass2TEST(p);
                else
                  ;
              end;
            end;
          else
            ;
        end;
        { If this flag is set, force another run of pass 2 even if p wasn't
          changed (-O3 only), but otherwise move p ahead by 1 instruction
          and treat as if Result was set to True }
        if aoc_ForceNewIteration in OptsToCheck then
          begin
            Exclude(OptsToCheck, aoc_ForceNewIteration);

            if not Result then
              begin
                if (p.typ in SkipInstr) then
                  UpdateUsedRegs(p);

                p := tai(p.Next);
                Result := True;
              end;
          end;
      end;


    function TCpuAsmOptimizer.PostPeepHoleOptsCpu(var p: tai): boolean;
      begin
        result := false;
        case p.typ of
          ait_instruction:
            begin
              case taicpu(p).opcode of
                A_MOV:
                  Result:=PostPeepholeOptMov(p);
                A_AND:
                  Result:=PostPeepholeOptAnd(p);
                A_MOVSX,
                A_MOVSXD:
                  Result:=PostPeepholeOptMOVSX(p);
                A_MOVZX:
                  Result:=PostPeepholeOptMovzx(p);
                A_CMP:
                  Result:=PostPeepholeOptCmp(p);
                A_OR,
                A_TEST:
                  Result:=PostPeepholeOptTestOr(p);
                A_XOR:
                  Result:=PostPeepholeOptXor(p);
                A_CALL:
                  Result:=PostPeepholeOptCall(p);
                A_LEA:
                  Result:=PostPeepholeOptLea(p);
                A_PUSH:
                  Result:=PostPeepholeOptPush(p);
                A_SHR:
                  Result:=PostPeepholeOptShr(p);
                A_ADD,
                A_SUB:
                  Result:=PostPeepholeOptADDSUB(p);
                A_VPXOR:
                  Result:=PostPeepholeOptVPXOR(p);
                else
                  ;
              end;

              { Optimise any reference-type operands (if Result is True, the
                instruction will be checked on the next iteration) }
              if not Result then
                OptimizeRefs(taicpu(p));

            end;
          else
            ;
        end;
        { If this flag is set, something was optimised ahead of p, so move
          ahead by 1 instruction but treat as if Result was set to True }
        if aoc_ForceNewIteration in OptsToCheck then
          begin
            Exclude(OptsToCheck, aoc_ForceNewIteration);

            if not Result then
              begin
                if (p.typ in SkipInstr) then
                  UpdateUsedRegs(p);

                p := tai(p.Next);
                Result := True;
              end;
          end;
      end;


begin
  casmoptimizer := TCpuAsmOptimizer;
end.

