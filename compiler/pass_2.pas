{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit handles the codegeneration pass

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
unit pass_2;

{$i fpcdefs.inc}

interface

uses
   node;

    type
       tenumflowcontrol = (fc_exit,fc_break,fc_continue);
       tflowcontrol = set of tenumflowcontrol;

    var
       flowcontrol : tflowcontrol;
{ produces assembler for the expression in variable p }
{ and produces an assembler node at the end        }
procedure generatecode(var p : tnode);

{ produces the actual code }
function do_secondpass(var p : tnode) : boolean;
procedure secondpass(var p : tnode);


implementation

   uses
{$ifdef EXTDEBUG}
     cutils,
{$endif}
     globtype,systems,verbose,
     cclasses,globals,
     symconst,symbase,symtype,symsym,paramgr,
     aasmbase,aasmtai,
     pass_1,cpubase,cgbase,regvars,nflw,rgobj;

{*****************************************************************************
                              SecondPass
*****************************************************************************}

{$ifdef EXTDEBUG}
     procedure logsecond(ht:tnodetype; entry: boolean);
       const
         secondnames: array[tnodetype] of string[13] =
            ('<emptynode>',
             'add-addn',  {addn}
             'add-muln',  {muln}
             'add-subn',  {subn}
             'moddiv-divn',      {divn}
             'add-symdifn',      {symdifn}
             'moddiv-modn',      {modn}
             'assignment',  {assignn}
             'load',        {loadn}
             'nothing-range',     {range}
             'add-ltn',  {ltn}
             'add-lten',  {lten}
             'add-gtn',  {gtn}
             'add-gten',  {gten}
             'add-equaln',  {equaln}
             'add-unequaln',  {unequaln}
             'in',    {inn}
             'add-orn',  {orn}
             'add-xorn',  {xorn}
             'shlshr-shrn',      {shrn}
             'shlshr-shln',      {shln}
             'add-slashn',  {slashn}
             'add-andn',  {andn}
             'subscriptn',  {subscriptn}
             'dderef',       {derefn}
             'addr',        {addrn}
             'doubleaddr',  {doubleaddrn}
             'ordconst',    {ordconstn}
             'typeconv',    {typeconvn}
             'calln',       {calln}
             'noth-callpar',{callparan}
             'realconst',   {realconstn}
             'unaryminus',  {unaryminusn}
             'asm',         {asmn}
             'vecn',        {vecn}
             'pointerconst',{pointerconstn}
             'stringconst', {stringconstn}
             'funcret',     {funcretn}
             'selfn',       {selfn}
             'not',         {notn}
             'inline',      {inlinen}
             'niln',        {niln}
             'error',       {errorn}
             'nothing-typen',     {typen}
             'hnewn',       {hnewn}
             'hdisposen',   {hdisposen}
             'setelement',  {setelementn}
             'setconst',    {setconstn}
             'blockn',      {blockn}
             'statement',   {statementn}
             'ifn',         {ifn}
             'breakn',      {breakn}
             'continuen',   {continuen}
             'while_repeat', {whilerepeatn}
             'for',         {forn}
             'exitn',       {exitn}
             'with',        {withn}
             'case',        {casen}
             'label',       {labeln}
             'goto',        {goton}
             'tryexcept',   {tryexceptn}
             'raise',       {raisen}
             'tryfinally',  {tryfinallyn}
             'on',    {onn}
             'is',    {isn}
             'as',    {asn}
             'error-caret',       {caretn}
             'fail',        {failn}
             'add-starstar',  {starstarn}
             'procinline',  {procinlinen}
             'arrayconstruc', {arrayconstructn}
             'noth-arrcnstr',     {arrayconstructrangen}
             'tempcreaten',
             'temprefn',
             'tempdeleten',
             'addoptn',
             'nothing-nothg',     {nothingn}
             'loadvmt',      {loadvmtn}
             'guidconstn',
             'rttin'
             );
      var
        p: pchar;
      begin
        if entry then
          p := strpnew('second '+secondnames[ht]+' (entry)')
        else
          p := strpnew('second '+secondnames[ht]+' (exit)');
        exprasmlist.concat(tai_comment.create(p));
        WriteLn(p);
      end;
{$endif EXTDEBUG}

     procedure secondpass(var p : tnode);
      var
         oldcodegenerror  : boolean;
         oldlocalswitches : tlocalswitches;
         oldpos    : tfileposinfo;
{$ifdef TEMPREGDEBUG}
         prevp : pptree;
{$endif TEMPREGDEBUG}
{$ifdef EXTDEBUG}
         oldloc : tloc;
{$endif EXTDEBUG}
      begin
         if not assigned(p) then
          internalerror(200208221);
         if not(nf_error in p.flags) then
          begin
            oldcodegenerror:=codegenerror;
            oldlocalswitches:=aktlocalswitches;
            oldpos:=aktfilepos;
{$ifdef TEMPREGDEBUG}
            testregisters32;
            prevp:=curptree;
            curptree:=@p;
            p^.usableregs:=usablereg32;
{$endif TEMPREGDEBUG}
            aktfilepos:=p.fileinfo;
            aktlocalswitches:=p.localswitches;
            codegenerror:=false;
{$ifdef EXTDEBUG}
            oldloc:=p.location.loc;
            p.location.loc:=LOC_INVALID;
            if (cs_asm_nodes in aktglobalswitches) then
              logsecond(p.nodetype,true);
{$endif EXTDEBUG}
            p.pass_2;
{$ifdef EXTDEBUG}
            if (cs_asm_nodes in aktglobalswitches) then
              logsecond(p.nodetype,false);
            if (not codegenerror) and
               (oldloc<>LOC_INVALID) and
               (p.location.loc=LOC_INVALID) then
             Comment(V_Fatal,'Location not set in secondpass: '+nodetype2str[p.nodetype]);
{$endif EXTDEBUG}
            if codegenerror then
              include(p.flags,nf_error);

            codegenerror:=codegenerror or oldcodegenerror;
            aktlocalswitches:=oldlocalswitches;
            aktfilepos:=oldpos;
{$ifdef TEMPREGDEBUG}
            curptree:=prevp;
{$endif TEMPREGDEBUG}
{$ifdef EXTTEMPREGDEBUG}
            if p.usableregs-usablereg32>p.reallyusedregs then
              p.reallyusedregs:=p.usableregs-usablereg32;
            if p.reallyusedregs<p.registers32 then
              Comment(V_Debug,'registers32 overestimated '+tostr(p^.registers32)+
                '>'+tostr(p^.reallyusedregs));
{$endif EXTTEMPREGDEBUG}
          end
         else
           codegenerror:=true;
      end;


    function do_secondpass(var p : tnode) : boolean;
      begin
         codegenerror:=false;
         if not(nf_error in p.flags) then
           secondpass(p);
         do_secondpass:=codegenerror;
      end;

    procedure clearrefs(p : tnamedindexitem;arg:pointer);

      begin
         if (tsym(p).typ=varsym) then
           if tvarsym(p).refs>1 then
             tvarsym(p).refs:=1;
      end;

    procedure generatecode(var p : tnode);
      begin
         rg.cleartempgen;
         flowcontrol:=[];
         { when size optimization only count occurrence }
         if cs_littlesize in aktglobalswitches then
           rg.t_times:=1
         else
           { reference for repetition is 100 }
           rg.t_times:=100;
         { clear register count }
         rg.clearregistercount;
         use_esp_stackframe:=false;
         symtablestack.foreach_static({$ifdef FPCPROCVAR}@{$endif}clearrefs,nil);
         symtablestack.next.foreach_static({$ifdef FPCPROCVAR}@{$endif}clearrefs,nil);
         { firstpass everything }
         do_firstpass(p);
         { only do secondpass if there are no errors }
         if ErrorCount=0 then
           begin
{$ifdef OMITSTACKFRAME}
             if (cs_regalloc in aktglobalswitches) and
                ((procinfo^.flags and (pi_uses_asm or pi_uses_exceptions))=0) then
               begin
                 { can we omit the stack frame ? }
                 { conditions:
                   1. procedure (not main block)
                   2. no constructor or destructor
                   3. no call to other procedures
                   4. no interrupt handler
                 }
                 {!!!!!! this doesn work yet, because of problems with
                    with linux and windows
                 }
                 (*
                 if assigned(aktprocsym) then
                   begin
                     if not(assigned(procinfo^._class)) and
                        not(aktprocdef.proctypeoption in [potype_constructor,potype_destructor]) and
                        not(po_interrupt in aktprocdef.procoptions) and
                        ((procinfo^.flags and pi_do_call)=0) and
                        (lexlevel>=normal_function_level) then
                       begin
                        { use ESP as frame pointer }
                         procinfo^.framepointer:=STACK_POINTER_REG;
                         use_esp_stackframe:=true;

                        { calc parameter distance new }
                         dec(procinfo^.framepointer_offset,4);
                         dec(procinfo^.selfpointer_offset,4);

                        { is this correct ???}
                        { retoffset can be negativ for results in eax !! }
                        { the value should be decreased only if positive }
                         if procinfo.retoffset>=0 then
                           dec(procinfo.retoffset,4);

                         dec(procinfo.para_offset,4);
                         aktprocdef.parast.address_fixup:=procinfo.para_offset;
                       end;
                   end;
                  *)
                end;
{$endif OMITSTACKFRAME}

              { assign parameter locations }
{$ifndef i386}
              setparalocs(procinfo.procdef);
{$endif i386}

              procinfo.after_pass1;

              { process register variable stuff (JM) }
              assign_regvars(p);
              load_regvars(procinfo.aktentrycode,p);

              { for the i386 it must be done in genexitcode because it has  }
              { to add 'fstp' instructions when using fpu regvars and those }
              { must come after the "exitlabel" (JM)                        }
{$ifndef i386}
              cleanup_regvars(procinfo.aktexitcode);
{$endif i386}

              do_secondpass(p);

              if assigned(procinfo.procdef) then
                procinfo.procdef.fpu_used:=p.registersfpu;

           end;
         procinfo.aktproccode.concatlist(exprasmlist);
      end;

end.
{
  $Log$
  Revision 1.40  2002-12-21 23:21:47  mazen
  + added support for the shift nodes
  + added debug output on screen with -an command line option

  Revision 1.39  2002/08/23 16:14:49  peter
    * tempgen cleanup
    * tt_noreuse temp type added that will be used in genentrycode

  Revision 1.38  2002/08/20 16:55:38  peter
    * don't write (stabs)line info when inlining a procedure

  Revision 1.37  2002/08/19 19:36:44  peter
    * More fixes for cross unit inlining, all tnodes are now implemented
    * Moved pocall_internconst to po_internconst because it is not a
      calling type at all and it conflicted when inlining of these small
      functions was requested

  Revision 1.36  2002/08/18 20:06:24  peter
    * inlining is now also allowed in interface
    * renamed write/load to ppuwrite/ppuload
    * tnode storing in ppu
    * nld,ncon,nbas are already updated for storing in ppu

  Revision 1.35  2002/08/17 09:23:38  florian
    * first part of procinfo rewrite

  Revision 1.34  2002/08/15 19:10:35  peter
    * first things tai,tnode storing in ppu

  Revision 1.33  2002/07/30 20:50:44  florian
    * the code generator knows now if parameters are in registers

  Revision 1.32  2002/07/19 11:41:36  daniel
  * State tracker work
  * The whilen and repeatn are now completely unified into whilerepeatn. This
    allows the state tracker to change while nodes automatically into
    repeat nodes.
  * Resulttypepass improvements to the notn. 'not not a' is optimized away and
    'not(a>b)' is optimized into 'a<=b'.
  * Resulttypepass improvements to the whilerepeatn. 'while not a' is optimized
    by removing the notn and later switchting the true and falselabels. The
    same is done with 'repeat until not a'.

  Revision 1.31  2002/07/01 18:46:25  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.30  2002/05/18 13:34:11  peter
    * readded missing revisions

  Revision 1.29  2002/05/16 19:46:42  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.27  2002/05/12 16:53:08  peter
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

  Revision 1.26  2002/04/21 19:02:04  peter
    * removed newn and disposen nodes, the code is now directly
      inlined from pexpr
    * -an option that will write the secondpass nodes to the .s file, this
      requires EXTDEBUG define to actually write the info
    * fixed various internal errors and crashes due recent code changes

  Revision 1.25  2002/04/20 21:32:24  carl
  + generic FPC_CHECKPOINTER
  + first parameter offset in stack now portable
  * rename some constants
  + move some cpu stuff to other units
  - remove unused constents
  * fix stacksize for some targets
  * fix generic size problems which depend now on EXTEND_SIZE constant

  Revision 1.24  2002/04/07 13:30:13  carl
  - removed unused variable

  Revision 1.23  2002/04/02 17:11:29  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.22  2002/03/31 20:26:35  jonas
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
