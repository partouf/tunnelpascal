{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

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
{$ifdef TP}
  {$E+,F+,N+}
{$endif}
unit pass_2;
interface

uses
  tree;

{ produces assembler for the expression in variable p }
{ and produces an assembler node at the end           }
procedure generatecode(var p : ptree);

{ produces the actual code }
function do_secondpass(var p : ptree) : boolean;
procedure secondpass(var p : ptree);


implementation

   uses
     globtype,systems,
     cobjects,comphook,verbose,globals,files,
     symtable,types,aasm,scanner,
     pass_1,hcodegen,temp_gen
{$ifdef GDB}
     ,gdb
{$endif}
{$ifdef i386}
{$ifdef Ag386Bin}
     ,i386base,i386asm
{$else}
     ,i386
{$endif}
     ,tgeni386,cgai386
     ,cg386con,cg386mat,cg386cnv,cg386set,cg386add
     ,cg386mem,cg386cal,cg386ld,cg386flw,cg386inl
{$endif}
{$ifdef m68k}
     ,m68k,tgen68k,cga68k
     ,cg68kcon,cg68kmat,cg68kcnv,cg68kset,cg68kadd
     ,cg68kmem,cg68kcal,cg68kld,cg68kflw,cg68kinl
{$endif}
     ;

{*****************************************************************************
                              SecondPass
*****************************************************************************}

    type
       secondpassproc = procedure(var p : ptree);

    procedure secondnothing(var p : ptree);

      begin
      end;

    procedure seconderror(var p : ptree);

      begin
         p^.error:=true;
         codegenerror:=true;
      end;


    procedure secondstatement(var p : ptree);

      var
         hp : ptree;
         oldrl : plinkedlist;

      begin
         hp:=p;
         while assigned(hp) do
          begin
            if assigned(hp^.right) then
             begin
               cleartempgen;
               oldrl:=temptoremove;
               temptoremove:=new(plinkedlist,init);
               secondpass(hp^.right);
               { release temp. ansi strings }
               removetemps(exprasmlist,temptoremove);
               dispose(temptoremove,done);
               temptoremove:=oldrl;
             end;
            hp:=hp^.left;
          end;
      end;


    procedure secondblockn(var p : ptree);
      begin
      { do second pass on left node }
        if assigned(p^.left) then
         secondpass(p^.left);
      end;


    procedure secondasm(var p : ptree);
      begin
         exprasmlist^.concatlist(p^.p_asm);
         if not p^.object_preserved then
          begin
{$ifdef i386}
            maybe_loadesi;
{$endif}
{$ifdef m68k}
            maybe_loada5;
{$endif}
          end;
       end;


     procedure secondpass(var p : ptree);
       const
         procedures : array[ttreetyp] of secondpassproc =
            (secondadd,         {addn}
             secondadd,         {muln}
             secondadd,         {subn}
             secondmoddiv,      {divn}
             secondadd,         {symdifn}
             secondmoddiv,      {modn}
             secondassignment,  {assignn}
             secondload,        {loadn}
             secondnothing,     {range}
             secondadd,         {ltn}
             secondadd,         {lten}
             secondadd,         {gtn}
             secondadd,         {gten}
             secondadd,         {equaln}
             secondadd,         {unequaln}
             secondin,          {inn}
             secondadd,         {orn}
             secondadd,         {xorn}
             secondshlshr,      {shrn}
             secondshlshr,      {shln}
             secondadd,         {slashn}
             secondadd,         {andn}
             secondsubscriptn,  {subscriptn}
             secondderef,       {derefn}
             secondaddr,        {addrn}
             seconddoubleaddr,  {doubleaddrn}
             secondordconst,    {ordconstn}
             secondtypeconv,    {typeconvn}
             secondcalln,       {calln}
             secondnothing,     {callparan}
             secondrealconst,   {realconstn}
             secondfixconst,    {fixconstn}
             secondumminus,     {umminusn}
             secondasm,         {asmn}
             secondvecn,        {vecn}
             secondstringconst, {stringconstn}
             secondfuncret,     {funcretn}
             secondselfn,       {selfn}
             secondnot,         {notn}
             secondinline,      {inlinen}
             secondniln,        {niln}
             seconderror,       {errorn}
             secondnothing,     {typen}
             secondhnewn,       {hnewn}
             secondhdisposen,   {hdisposen}
             secondnewn,        {newn}
             secondsimplenewdispose, {simpledisposen}
             secondsetelement,  {setelementn}
             secondsetconst,    {setconstn}
             secondblockn,      {blockn}
             secondstatement,   {statementn}
             secondnothing,     {loopn}
             secondifn,         {ifn}
             secondbreakn,      {breakn}
             secondcontinuen,   {continuen}
             second_while_repeatn, {repeatn}
             second_while_repeatn, {whilen}
             secondfor,         {forn}
             secondexitn,       {exitn}
             secondwith,        {withn}
             secondcase,        {casen}
             secondlabel,       {labeln}
             secondgoto,        {goton}
             secondsimplenewdispose, {simplenewn}
             secondtryexcept,   {tryexceptn}
             secondraise,       {raisen}
             secondnothing,     {switchesn}
             secondtryfinally,  {tryfinallyn}
             secondon,          {onn}
             secondis,          {isn}
             secondas,          {asn}
             seconderror,       {caretn}
             secondfail,        {failn}
             secondadd,         {starstarn}
             secondprocinline,  {procinlinen}
             secondarrayconstruct, {arrayconstructn}
             secondnothing,     {arrayconstructrangen}
             secondnothing,     {nothingn}
             secondloadvmt      {loadvmtn}
             );
      var
         oldcodegenerror  : boolean;
         oldlocalswitches : tlocalswitches;
         oldpos           : tfileposinfo;
      begin
         if not(p^.error) then
          begin
            oldcodegenerror:=codegenerror;
            oldlocalswitches:=aktlocalswitches;
            oldpos:=aktfilepos;

            aktfilepos:=p^.fileinfo;
            aktlocalswitches:=p^.localswitches;
            codegenerror:=false;
            procedures[p^.treetype](p);
            p^.error:=codegenerror;

            codegenerror:=codegenerror or oldcodegenerror;
            aktlocalswitches:=oldlocalswitches;
            aktfilepos:=oldpos;
          end
         else
           codegenerror:=true;
      end;


    function do_secondpass(var p : ptree) : boolean;
      begin
         codegenerror:=false;
         if not(p^.error) then
           secondpass(p);
         do_secondpass:=codegenerror;
      end;



    var
       regvars : array[1..maxvarregs] of pvarsym;
       regvars_para : array[1..maxvarregs] of boolean;
       regvars_refs : array[1..maxvarregs] of longint;
       parasym : boolean;

    procedure searchregvars(p : psym);
      var
         i,j,k : longint;
      begin
         if (p^.typ=varsym) and ((pvarsym(p)^.var_options and vo_regable)<>0) then
           begin
              { walk through all momentary register variables }
              for i:=1 to maxvarregs do
                begin
                   { free register ? }
                   if regvars[i]=nil then
                     begin
                        regvars[i]:=pvarsym(p);
                        regvars_para[i]:=parasym;
                        break;
                     end;
                   { else throw out a variable ? }
                       j:=pvarsym(p)^.refs;
                   { parameter get a less value }
                   if parasym then
                     begin
                        if cs_littlesize in aktglobalswitches  then
                          dec(j,1)
                        else
                          dec(j,100);
                     end;
                   if (j>regvars_refs[i]) and (j>0) then
                     begin
                        for k:=maxvarregs-1 downto i do
                          begin
                             regvars[k+1]:=regvars[k];
                             regvars_para[k+1]:=regvars_para[k];
                          end;
                        { calc the new refs
                        pvarsym(p)^.refs:=j; }
                        regvars[i]:=pvarsym(p);
                        regvars_para[i]:=parasym;
                        regvars_refs[i]:=j;
                        break;
                     end;
                end;
           end;
      end;


    procedure generatecode(var p : ptree);
      var
         i       : longint;
         regsize : topsize;
         hr      : preference;
      label
         nextreg;
      begin
         temptoremove:=nil;
         cleartempgen;
         { when size optimization only count occurrence }
         if cs_littlesize in aktglobalswitches then
           t_times:=1
         else
           { reference for repetition is 100 }
           t_times:=100;
         { clear register count }
         clearregistercount;
         use_esp_stackframe:=false;

         if not(do_firstpass(p)) then
           begin
              { max. optimizations     }
              { only if no asm is used }
              { and no try statement   }
              if (cs_regalloc in aktglobalswitches) and
                ((procinfo.flags and (pi_uses_asm or pi_uses_exceptions))=0) then
                begin
                   { can we omit the stack frame ? }
                   { conditions:
                     1. procedure (not main block)
                     2. no constructor or destructor
                     3. no call to other procedures
                     4. no interrupt handler
                   }
                   if assigned(aktprocsym) then
                     begin
                       if (aktprocsym^.definition^.options and
                        (poconstructor+podestructor{+poinline}+pointerrupt)=0) and
                        ((procinfo.flags and pi_do_call)=0) and
                        (lexlevel>=normal_function_level) then
                       begin
                         { use ESP as frame pointer }
                         procinfo.framepointer:=stack_pointer;
                         use_esp_stackframe:=true;

                         { calc parameter distance new }
                         dec(procinfo.framepointer_offset,4);
                         dec(procinfo.ESI_offset,4);

                         { is this correct ???}
                         { retoffset can be negativ for results in eax !! }
                         { the value should be decreased only if positive }
                         if procinfo.retoffset>=0 then
                           dec(procinfo.retoffset,4);

                         dec(procinfo.call_offset,4);
                         aktprocsym^.definition^.parast^.call_offset:=procinfo.call_offset;
                       end;
                     end;
                   if (p^.registers32<4) then
                       begin
                        for i:=1 to maxvarregs do
                          regvars[i]:=nil;
                        parasym:=false;
                      {$ifdef tp}
                        symtablestack^.foreach(searchregvars);
                      {$else}
                        symtablestack^.foreach(@searchregvars);
                      {$endif}
                        { copy parameter into a register ? }
                        parasym:=true;
                      {$ifdef tp}
                        symtablestack^.next^.foreach(searchregvars);
                      {$else}
                        symtablestack^.next^.foreach(@searchregvars);
                      {$endif}
                        { hold needed registers free }
                        for i:=maxvarregs downto maxvarregs-p^.registers32+1 do
                          regvars[i]:=nil;
                        { now assign register }
                        for i:=1 to maxvarregs-p^.registers32 do
                          begin
                             if assigned(regvars[i]) then
                               begin
                                  { it is nonsens, to copy the variable to }
                                  { a register because we need then much   }
                                  { too pushes ?                           }
                                  if reg_pushes[varregs[i]]>=regvars[i]^.refs then
                                    begin
                                       regvars[i]:=nil;
                                       goto nextreg;
                                    end;

                                  { register is no longer available for }
                                  { expressions                         }
                                  { search the register which is the most }
                                  { unused                                }
                                  usableregs:=usableregs-[varregs[i]];
                                  is_reg_var[varregs[i]]:=true;
                                  dec(c_usableregs);

                                  { possibly no 32 bit register are needed }
                                  { call by reference/const ? }
                                  if (regvars[i]^.varspez=vs_var) or
                                     ((regvars[i]^.varspez=vs_const) and
                                       push_addr_param(regvars[i]^.definition)) then
                                    begin
                                       regvars[i]^.reg:=varregs[i];
                                       regsize:=S_L;
                                    end
                                  else
                                   if (regvars[i]^.definition^.deftype=orddef) and
                                      (porddef(regvars[i]^.definition)^.size=1) then
                                    begin
{$ifdef i386}
                                       regvars[i]^.reg:=reg32toreg8(varregs[i]);
{$endif}
                                       regsize:=S_B;
                                    end
                                  else
                                   if (regvars[i]^.definition^.deftype=orddef) and
                                      (porddef(regvars[i]^.definition)^.size=2) then
                                    begin
{$ifdef i386}
                                       regvars[i]^.reg:=reg32toreg16(varregs[i]);
{$endif}
                                       regsize:=S_W;
                                    end
                                  else
                                    begin
                                       regvars[i]^.reg:=varregs[i];
                                       regsize:=S_L;
                                    end;
                                  { parameter must be load }
                                  if regvars_para[i] then
                                    begin
                                       { procinfo is there actual,      }
                                       { because we can't never be in a }
                                       { nested procedure               }
                                       { when loading parameter to reg  }
                                       new(hr);
                                       reset_reference(hr^);
                                       hr^.offset:=pvarsym(regvars[i])^.address+procinfo.call_offset;
                                       hr^.base:=procinfo.framepointer;
{$ifdef i386}
                                       procinfo.aktentrycode^.concat(new(pai386,op_ref_reg(A_MOV,regsize,
                                         hr,regvars[i]^.reg)));
{$endif i386}
{$ifdef m68k}
                                       procinfo.aktentrycode^.concat(new(pai68k,op_ref_reg(A_MOVE,regsize,
                                         hr,regvars[i]^.reg)));
{$endif m68k}
                                       unused:=unused - [regvars[i]^.reg];
                                    end;
                                  { procedure uses this register }
{$ifdef i386}
                                  usedinproc:=usedinproc or ($80 shr byte(varregs[i]));
{$endif i386}
{$ifdef m68k}
                                  usedinproc:=usedinproc or ($800 shr word(varregs[i]));
{$endif m68k}
                               end;
                             nextreg:
                               { dummy }
                               regsize:=S_W;
                          end;
                        if (status.verbosity and v_debug)=v_debug then
                          begin
                             for i:=1 to maxvarregs do
                               begin
                                  if assigned(regvars[i]) then
                                   Message3(cg_d_register_weight,reg2str(regvars[i]^.reg),
                                           tostr(regvars[i]^.refs),regvars[i]^.name);
                               end;
                          end;
                     end;
                end;
              if assigned(aktprocsym) and
                 ((aktprocsym^.definition^.options and poinline)<>0) then
                make_const_global:=true;
              do_secondpass(p);

              if assigned(procinfo.def) then
                procinfo.def^.fpu_used:=p^.registersfpu;

              { all registers can be used again }
              resetusableregisters;
           end;
         procinfo.aktproccode^.concatlist(exprasmlist);
         make_const_global:=false;
      end;

end.
{
  $Log$
  Revision 1.16  1999-03-24 23:17:11  peter
    * fixed bugs 212,222,225,227,229,231,233

  Revision 1.15  1999/02/22 02:15:25  peter
    * updates for ag386bin

  Revision 1.14  1999/01/23 23:29:37  florian
    * first running version of the new code generator
    * when compiling exceptions under Linux fixed

  Revision 1.13  1998/12/30 13:41:09  peter
    * released valuepara

  Revision 1.12  1998/12/19 00:23:51  florian
    * ansistring memory leaks fixed

  Revision 1.11  1998/12/11 00:03:28  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.10  1998/11/18 15:44:14  peter
    * VALUEPARA for tp7 compatible value parameters

  Revision 1.9  1998/11/13 15:40:21  pierre
    + added -Se in Makefile cvstest target
    + lexlevel cleanup
      normal_function_level main_program_level and unit_init_level defined
    * tins_cache grown to A_EMMS (gave range check error in asm readers)
      (test added in code !)
    * -Un option was wrong
    * _FAIL and _SELF only keyword inside
      constructors and methods respectively

  Revision 1.8  1998/10/29 15:42:49  florian
    + partial disposing of temp. ansistrings

  Revision 1.7  1998/10/26 22:58:19  florian
    * new introduded problem with classes fix, the parent class wasn't set
      correct, if the class was defined forward before

  Revision 1.6  1998/09/23 09:58:52  peter
    * first working array of const things

  Revision 1.5  1998/09/21 10:01:06  peter
    * check if procinfo.def is assigned before storing registersfpu

  Revision 1.4  1998/09/21 08:45:16  pierre
    + added vmt_offset in tobjectdef.write for fututre use
      (first steps to have objects without vmt if no virtual !!)
    + added fpu_used field for tabstractprocdef  :
      sets this level to 2 if the functions return with value in FPU
      (is then set to correct value at parsing of implementation)
      THIS MIGHT refuse some code with FPU expression too complex
      that were accepted before and even in some cases
      that don't overflow in fact
      ( like if f : float; is a forward that finally in implementation
       only uses one fpu register !!)
      Nevertheless I think that it will improve security on
      FPU operations !!
    * most other changes only for UseBrowser code
      (added symtable references for record and objects)
      local switch for refs to args and local of each function
      (static symtable still missing)
      UseBrowser still not stable and probably broken by
      the definition hash array !!

  Revision 1.3  1998/09/17 09:42:40  peter
    + pass_2 for cg386
    * Message() -> CGMessage() for pass_1/pass_2

  Revision 1.2  1998/09/07 18:46:07  peter
    * update smartlinking, uses getdatalabel
    * renamed ptree.value vars to value_str,value_real,value_set

  Revision 1.1  1998/09/01 09:07:12  peter
    * m68k fixes, splitted cg68k like cgi386

}
