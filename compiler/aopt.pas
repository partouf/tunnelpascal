{
    $Id$
    Copyright (c) 1998-2004 by Jonas Maebe, member of the Free Pascal
    Development Team

    This unit contains the interface routines between the code generator
    and the optimizer.

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
Unit aopt;

{$i fpcdefs.inc}

  Interface

    Uses
      aasmbase,aasmtai,aasmcpu,
      aoptobj;

    Type
      TAsmOptimizer = class(TAoptObj)

        { _AsmL is the PAasmOutpout list that has to be optimized }
        Constructor create(_AsmL: taasmoutput);

        { call the necessary optimizer procedures }
        Procedure Optimize;
        Destructor destroy;override;

      private
        Function FindLoHiLabels: tai;
        Procedure BuildLabelTableAndFixRegAlloc;
      End;

    var
      casmoptimizer : class of tasmoptimizer;

    procedure Optimize(AsmL:taasmoutput);

  Implementation

    uses
      globtype, globals,
      aoptda,aoptcpu,aoptcpud;

    Constructor TAsmOptimizer.create(_AsmL: taasmoutput);
      Begin
        inherited create(_asml,nil,nil,nil);
      {setup labeltable, always necessary}
        New(LabelInfo);
        LabelInfo^.LowLabel := High(AWord);
        LabelInfo^.HighLabel := 0;
        LabelInfo^.LabelDif := 0;
        LabelInfo^.LabelTable:=nil;
      End;

    Function TAsmOptimizer.FindLoHiLabels: tai;
      { Walks through the paasmlist to find the lowest and highest label number.  }
      { Returns the last Pai object of the current block                          }
      Var LabelFound: Boolean;
          p: tai;
      Begin
        LabelFound := False;
        P := BlockStart;
        With LabelInfo^ Do
          Begin
            While Assigned(P) And
                  ((P.typ <> Ait_Marker) Or
                   (tai_Marker(P).Kind <> AsmBlockStart)) Do
              Begin
                If (p.typ = ait_label) Then
                  If (tai_Label(p).l.is_used) Then
                    Begin
                      LabelFound := True;
                      If (tai_Label(p).l.labelnr < LowLabel) Then
                        LowLabel := tai_Label(p).l.labelnr;
                      If (tai_Label(p).l.labelnr > HighLabel) Then
                        HighLabel := tai_Label(p).l.labelnr
                    End;
                GetNextInstruction(p, p)
              End;
            FindLoHiLabels := p;
            If LabelFound
              Then LabelDif := HighLabel-LowLabel+1
              Else LabelDif := 0
          End
      End;

    Procedure TAsmOptimizer.BuildLabelTableAndFixRegAlloc;
    { Builds a table with the locations of the labels in the taasmoutput.       }
    { Also fixes some RegDeallocs like "# %eax released; push (%eax)"           }
    Var p, hp1, hp2: tai;
        UsedRegs: TRegSet;
    Begin
      UsedRegs := [];
      With LabelInfo^ Do
        If (LabelDif <> 0) Then
          Begin
            GetMem(LabelTable, LabelDif*SizeOf(TLabelTableItem));
            FillChar(LabelTable^, LabelDif*SizeOf(TLabelTableItem), 0);
            p := BlockStart;
            While (P <> BlockEnd) Do
              Begin
                Case p.typ Of
                  ait_Label:
                    If tai_label(p).l.is_used Then
                      LabelTable^[tai_label(p).l.labelnr-LowLabel].PaiObj := p;
                  ait_regAlloc:
                    begin
                    {!!!!!!!!!
                      if tai_regalloc(p).ratype=ra_alloc then
                        Begin
                          If Not(tai_regalloc(p).Reg in UsedRegs) Then
                            UsedRegs := UsedRegs + [tai_regalloc(p).Reg]
                          Else
                            Begin
                              hp1 := p;
                              hp2 := nil;
                              While GetLastInstruction(hp1, hp1) And
                                    Not(RegInInstruction(tai_regalloc(p).Reg, hp1)) Do
                                hp2:=hp1;
                              If hp2<>nil Then
                                Begin
                                  hp1:=tai_regalloc.DeAlloc(tai_regalloc(p).Reg,hp2);
                                  InsertLLItem(tai(hp2.previous), hp2, hp1);
                                End;
                            End;
                        End
                      else
                        Begin
                          UsedRegs := UsedRegs - [tai_regalloc(p).Reg];
                          hp1 := p;
                          hp2 := nil;
                          While Not(FindRegAlloc(tai_regalloc(p).Reg, tai(hp1.Next))) And
                                GetNextInstruction(hp1, hp1) And
                                RegInInstruction(tai_regalloc(p).Reg, hp1) Do
                            hp2 := hp1;
                          If hp2 <> nil Then
                            Begin
                              hp1 := tai(p.previous);
                              AsmL.Remove(p);
                              InsertLLItem(hp2, tai(hp2.Next), p);
                              p := hp1;
                            End
                        End
                    };
                    End
                End
              End;
            P := tai(p.Next);
            While Assigned(p) And
                  (p.typ in (SkipInstr - [ait_regalloc])) Do
              P := tai(P.Next)
          End
    End;



    Procedure TAsmOptimizer.Optimize;
      Var
        HP: tai;
        pass: longint;
      Begin
        pass:=0;
        BlockStart := tai(AsmL.First);
        While Assigned(BlockStart) Do
          Begin
             if pass = 0 then
               PrePeepHoleOpts;
            { Peephole optimizations }
             PeepHoleOptPass1;
            { Only perform them twice in the first pass }
             if pass = 0 then
               PeepHoleOptPass1;
            If (cs_slowoptimize in aktglobalswitches) Then
              Begin
                // DFA:=TAOptDFACpu.Create(AsmL,BlockStart,BlockEnd,LabelInfo);
                { data flow analyzer }
                DFA.DoDFA;
                { common subexpression elimination }
      {          CSE;}
              End;
            { more peephole optimizations }
      {      PeepHoleOptPass2;}
            {dispose labeltabel}
            If Assigned(LabelInfo^.LabelTable) Then
              Begin
                Dispose(LabelInfo^.LabelTable);
                LabelInfo := Nil
              End;
            { continue where we left off, BlockEnd is either the start of an }
            { assembler block or nil}
            BlockStart := BlockEnd;
            While Assigned(BlockStart) And
                  (BlockStart.typ = ait_Marker) And
                  (tai_Marker(BlockStart).Kind = AsmBlockStart) Do
              Begin
               { we stopped at an assembler block, so skip it }
                While GetNextInstruction(BlockStart, BlockStart) And
                      ((BlockStart.Typ <> Ait_Marker) Or
                       (tai_Marker(Blockstart).Kind <> AsmBlockEnd)) Do;
               { blockstart now contains a tai_marker(asmblockend) }
                If Not(GetNextInstruction(BlockStart, HP) And
                       ((HP.typ <> ait_Marker) Or
                        (tai_Marker(HP).Kind <> AsmBlockStart)
                       )
                      ) Then
                 {skip the next assembler block }
                 BlockStart := HP;
               { otherwise there is no assembler block anymore after the current }
               { one, so optimize the next block of "normal" instructions        }
              End
          End;
      End;

    Destructor TAsmOptimizer.Destroy;
      Begin
        Dispose(LabelInfo)
      End;


    procedure Optimize(AsmL:taasmoutput);
      var
        p : TAsmOptimizer;
      begin
        p:=casmoptimizer.Create(AsmL);
        p.Optimize;
        p.free
      end;


begin
  casmoptimizer:=TAsmOptimizer;
end.

{Virtual methods, most have to be overridden by processor dependent methods}

{
 $Log$
 Revision 1.8  2004-10-31 21:45:02  peter
   * generic tlocation
   * move tlocation to cgutils

 Revision 1.7  2004/10/30 15:21:37  florian
   * fixed generic optimizer
   * enabled generic optimizer for sparc

 Revision 1.6  2004/06/20 08:55:28  florian
   * logs truncated
}
