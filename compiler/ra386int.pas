{
    $Id$
    Copyright (c) 1997-98 by Carl Eric Codere

    Does the parsing process for the intel styled inline assembler.

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
  {$E+,N+}
{$endif}
Unit Ra386int;
Interface

uses
  tree;

   function assemble: ptree;



Implementation

Uses
  globtype,
  strings,cobjects,systems,verbose,globals,
  files,aasm,types,scanner,hcodegen,symtable
  ,i386base
  ,rautils,ra386;


type
  tasmtoken = (
    AS_NONE,AS_LABEL,AS_LLABEL,AS_STRING,AS_INTNUM,
    AS_COMMA,AS_LBRACKET,AS_RBRACKET,AS_LPAREN,
    AS_RPAREN,AS_COLON,AS_DOT,AS_PLUS,AS_MINUS,AS_STAR,
    AS_SEPARATOR,AS_ID,AS_REGISTER,AS_OPCODE,AS_SLASH,
     {------------------ Assembler directives --------------------}
    AS_DB,AS_DW,AS_DD,AS_END,
     {------------------ Assembler Operators  --------------------}
    AS_BYTE,AS_WORD,AS_DWORD,AS_QWORD,AS_TBYTE,AS_NEAR,AS_FAR,
    AS_HIGH,AS_LOW,AS_OFFSET,AS_SEG,AS_TYPE,AS_PTR,AS_MOD,AS_SHL,AS_SHR,AS_NOT,
    AS_AND,AS_OR,AS_XOR);

  tasmkeyword = string[6];
const
   { These tokens should be modified accordingly to the modifications }
   { in the different enumerations.                                   }
   firstdirective = AS_DB;
   lastdirective  = AS_END;
   firstoperator  = AS_BYTE;
   lastoperator   = AS_XOR;
   firstsreg      = R_CS;
   lastsreg       = R_SS;

   _count_asmdirectives = longint(lastdirective)-longint(firstdirective);
   _count_asmoperators  = longint(lastoperator)-longint(firstoperator);
   _count_asmprefixes   = 5;
   _count_asmspecialops = 25;
   _count_asmoverrides  = 3;

   _asmdirectives : array[0.._count_asmdirectives] of tasmkeyword =
   ('DB','DW','DD','END');

   { problems with shl,shr,not,and,or and xor, they are }
   { context sensitive.                                 }
   _asmoperators : array[0.._count_asmoperators] of tasmkeyword = (
    'BYTE','WORD','DWORD','QWORD','TBYTE','NEAR','FAR','HIGH',
    'LOW','OFFSET','SEG','TYPE','PTR','MOD','SHL','SHR','NOT','AND',
    'OR','XOR');

  token2str : array[tasmtoken] of string[10] = (
    '','Label','LLabel','String','Integer',
    ',','[',']','(',
    ')',':','.','+','-','*',
    ';','identifier','register','opcode','/',
    '','','','END',
    '','','','','','','','',
    '','','','type','ptr','mod','shl','shr','not',
    'and','or','xor'
  );

const
  newline = #10;
  firsttoken : boolean = TRUE;
var
  _asmsorted     : boolean;
  inexpression   : boolean;
  curlist        : paasmoutput;
  c              : char;
  prevasmtoken   : tasmtoken;
  actasmtoken    : tasmtoken;
  actasmpattern  : string;
  actasmregister : tregister;
  actopcode      : tasmop;
  actopsize      : topsize;
  actcondition   : tasmcond;
  iasmops        : ^op2strtable;
  iasmregs       : ^reg2strtable;


Procedure SetupTables;
{ creates uppercased symbol tables for speed access }
var
  i : tasmop;
  j : tregister;
Begin
  { opcodes }
  new(iasmops);
  for i:=firstop to lastop do
   iasmops^[i] := upper(int_op2str[i]);
  { registers }
  new(iasmregs);
  for j:=firstreg to lastreg do
   iasmregs^[j] := upper(int_reg2str[j]);
end;


  {---------------------------------------------------------------------}
  {                     Routines for the tokenizing                     }
  {---------------------------------------------------------------------}


   function is_asmopcode(const s: string):boolean;
   var
     i: tasmop;
     cond : string[4];
     cnd : tasmcond;
     j: longint;
   Begin
     is_asmopcode:=FALSE;

     actopcode:=A_None;
     actcondition:=C_None;
     actopsize:=S_NO;

     for i:=firstop to lastop do
      if s=iasmops^[i] then
       begin
         actopcode:=i;
         actasmtoken:=AS_OPCODE;
         is_asmopcode:=TRUE;
         exit;
       end;
     { not found yet, check condition opcodes }
     j:=0;
     while (j<CondAsmOps) do
      begin
        if Copy(s,1,Length(CondAsmOpStr[j]))=CondAsmOpStr[j] then
         begin
           cond:=Copy(s,Length(CondAsmOpStr[j])+1,255);
           if cond<>'' then
            begin
              for cnd:=low(TasmCond) to high(TasmCond) do
               if Cond=Upper(cond2str[cnd]) then
                begin
                  actopcode:=CondASmOp[j];
                  actcondition:=cnd;
                  is_asmopcode:=TRUE;
                  actasmtoken:=AS_OPCODE;
                  exit
                end;
            end;
         end;
        inc(j);
      end;
   end;


function is_asmoperator(const s: string):boolean;
var
  i : longint;
Begin
  for i:=0 to _count_asmoperators do
   if s=_asmoperators[i] then
    begin
      actasmtoken:=tasmtoken(longint(firstoperator)+i);
      is_asmoperator:=true;
      exit;
    end;
  is_asmoperator:=false;
end;


Function is_asmdirective(const s: string):boolean;
var
  i : longint;
Begin
  for i:=0 to _count_asmdirectives do
   if s=_asmdirectives[i] then
    begin
      actasmtoken:=tasmtoken(longint(firstdirective)+i);
      is_asmdirective:=true;
      exit;
    end;
  is_asmdirective:=false;
end;


Function is_register(const s: string):boolean;
Var
  i : tregister;
Begin
  actasmregister:=R_NO;
  for i:=firstreg to lastreg do
   if s=iasmregs^[i] then
    begin
      actasmtoken:=AS_REGISTER;
      actasmregister:=i;
      is_register:=true;
      exit;
    end;
  is_register:=false;
end;


function is_locallabel(const s:string):boolean;
begin
  is_locallabel:=(length(s)>1) and (s[1]='@');
end;


Procedure GetToken;
var
  len : longint;
  forcelabel : boolean;
begin
  { save old token and reset new token }
  prevasmtoken:=actasmtoken;
  actasmtoken:=AS_NONE;
  { reset }
  forcelabel:=FALSE;
  actasmpattern:='';
  { while space and tab , continue scan... }
  while (c in [' ',#9]) do
    c:=current_scanner^.asmgetchar;
  { get token pos }
  if not (c in [newline,#13,'{',';']) then
    current_scanner^.gettokenpos;
{ Local Label, Label, Directive, Prefix or Opcode }
  if firsttoken and not (c in [newline,#13,'{',';']) then
   begin
     firsttoken:=FALSE;
     len:=0;
     while c in ['A'..'Z','a'..'z','0'..'9','_','@'] do
      begin
        { if there is an at_sign, then this must absolutely be a label }
        if c = '@' then
         forcelabel:=TRUE;
        inc(len);
        actasmpattern[len]:=c;
        c:=current_scanner^.asmgetchar;
      end;
     actasmpattern[0]:=chr(len);
     uppervar(actasmpattern);
     { label ? }
     if c = ':' then
      begin
        if actasmpattern[1]='@' then
          actasmtoken:=AS_LLABEL
        else
          actasmtoken:=AS_LABEL;
        { let us point to the next character }
        c:=current_scanner^.asmgetchar;
        exit;
      end;
     { Are we trying to create an identifier with }
     { an at-sign...?                             }
     if forcelabel then
      Message(asmr_e_none_label_contain_at);
     { opcode ? }
     If is_asmopcode(actasmpattern) then
      Begin
        { check if we are in an expression  }
        { then continue with asm directives }
        if not inexpression then
         exit;
      end;
     if is_asmdirective(actasmpattern) then
      exit;
     actasmtoken:=AS_NONE;
     exit;
   end
  else { else firsttoken }
   begin
     case c of
       '@' : { possiblities : - local label reference , such as in jmp @local1 }
             {                - @Result, @Code or @Data special variables.     }
         begin
           actasmpattern:=c;
           c:=current_scanner^.asmgetchar;
           while c in  ['A'..'Z','a'..'z','0'..'9','_','@'] do
            begin
              actasmpattern:=actasmpattern + c;
              c:=current_scanner^.asmgetchar;
            end;
           uppervar(actasmpattern);
           actasmtoken:=AS_ID;
           exit;
         end;

       'A'..'Z','a'..'z','_': { identifier, register, opcode, prefix or directive }
         begin
           actasmpattern:=c;
           c:=current_scanner^.asmgetchar;
           while c in  ['A'..'Z','a'..'z','0'..'9','_'] do
            begin
              actasmpattern:=actasmpattern + c;
              c:=current_scanner^.asmgetchar;
            end;
           uppervar(actasmpattern);
           { after prefix we allow also a new opcode }
           If is_prefix(actopcode) and is_asmopcode(actasmpattern) then
            Begin
              { if we are not in a constant }
              { expression than this is an  }
              { opcode.                     }
              if not inexpression then
               exit;
            end;
           if is_register(actasmpattern) then
            exit;
           if is_asmdirective(actasmpattern) then
            exit;
           if is_asmoperator(actasmpattern) then
            exit;
           actasmtoken:=AS_ID;
           exit;
         end;

       '&' : { override operator... not supported }
         begin
           Message(asmr_w_override_op_not_supported);
           c:=current_scanner^.asmgetchar;
           actasmtoken:=AS_NONE;
         end;

       '''' : { string or character }
         begin
           actasmpattern:='';
           repeat
             if c = '''' then
              begin
                c:=current_scanner^.asmgetchar;
                if c=newline then
                 begin
                   Message(scan_f_string_exceeds_line);
                   break;
                 end;
                repeat
                  if c='''' then
                   begin
                     c:=current_scanner^.asmgetchar;
                     if c='''' then
                      begin
                        actasmpattern:=actasmpattern+'''';
                        c:=current_scanner^.asmgetchar;
                        if c=newline then
                         begin
                           Message(scan_f_string_exceeds_line);
                           break;
                         end;
                      end
                     else
                      break;
                   end
                  else
                   begin
                     actasmpattern:=actasmpattern+c;
                     c:=current_scanner^.asmgetchar;
                     if c=newline then
                      begin
                        Message(scan_f_string_exceeds_line);
                        break
                      end;
                   end;
                until false; { end repeat }
              end
             else
              break; { end if }
           until false;
           actasmtoken:=AS_STRING;
           exit;
         end;

       '"' : { string or character }
         begin
           actasmpattern:='';
           repeat
             if c = '"' then
              begin
                c:=current_scanner^.asmgetchar;
                if c=newline then
                 begin
                   Message(scan_f_string_exceeds_line);
                   break;
                 end;
                repeat
                  if c='"' then
                   begin
                     c:=current_scanner^.asmgetchar;
                     if c='"' then
                      begin
                        actasmpattern:=actasmpattern+'"';
                        c:=current_scanner^.asmgetchar;
                        if c=newline then
                         begin
                           Message(scan_f_string_exceeds_line);
                           break;
                         end;
                      end
                     else
                      break;
                   end
                  else
                   begin
                     actasmpattern:=actasmpattern+c;
                     c:=current_scanner^.asmgetchar;
                     if c=newline then
                      begin
                        Message(scan_f_string_exceeds_line);
                        break
                      end;
                   end;
                until false; { end repeat }
              end
             else
              break; { end if }
           until false;
           actasmtoken:=AS_STRING;
           exit;
         end;

       '$' :
         begin
           c:=current_scanner^.asmgetchar;
           while c in ['0'..'9','A'..'F','a'..'f'] do
            begin
              actasmpattern:=actasmpattern + c;
              c:=current_scanner^.asmgetchar;
            end;
           actasmpattern:=tostr(ValHexaDecimal(actasmpattern));
           actasmtoken:=AS_INTNUM;
           exit;
         end;

       ',' :
         begin
           actasmtoken:=AS_COMMA;
           c:=current_scanner^.asmgetchar;
           exit;
         end;

       '[' :
         begin
           actasmtoken:=AS_LBRACKET;
           c:=current_scanner^.asmgetchar;
           exit;
         end;

       ']' :
         begin
           actasmtoken:=AS_RBRACKET;
           c:=current_scanner^.asmgetchar;
           exit;
         end;

       '(' :
         begin
           actasmtoken:=AS_LPAREN;
           c:=current_scanner^.asmgetchar;
           exit;
         end;

       ')' :
         begin
           actasmtoken:=AS_RPAREN;
           c:=current_scanner^.asmgetchar;
           exit;
         end;

       ':' :
         begin
           actasmtoken:=AS_COLON;
           c:=current_scanner^.asmgetchar;
           exit;
         end;

       '.' :
         begin
           actasmtoken:=AS_DOT;
           c:=current_scanner^.asmgetchar;
           exit;
         end;

       '+' :
         begin
           actasmtoken:=AS_PLUS;
           c:=current_scanner^.asmgetchar;
           exit;
         end;

       '-' :
         begin
           actasmtoken:=AS_MINUS;
           c:=current_scanner^.asmgetchar;
           exit;
         end;

       '*' :
         begin
           actasmtoken:=AS_STAR;
           c:=current_scanner^.asmgetchar;
           exit;
         end;

       '/' :
         begin
           actasmtoken:=AS_SLASH;
           c:=current_scanner^.asmgetchar;
           exit;
         end;

       '0'..'9':
         begin
           actasmpattern:=c;
           c:=current_scanner^.asmgetchar;
           { Get the possible characters }
           while c in ['0'..'9','A'..'F','a'..'f'] do
            begin
              actasmpattern:=actasmpattern + c;
              c:=current_scanner^.asmgetchar;
            end;
           { Get ending character }
           uppervar(actasmpattern);
           c:=upcase(c);
           { possibly a binary number. }
           if (actasmpattern[length(actasmpattern)] = 'B') and (c <> 'H') then
            Begin
              { Delete the last binary specifier }
              delete(actasmpattern,length(actasmpattern),1);
              actasmpattern:=tostr(ValBinary(actasmpattern));
              actasmtoken:=AS_INTNUM;
              exit;
            end
           else
            Begin
              case c of
                'O' :
                  Begin
                    actasmpattern:=tostr(ValOctal(actasmpattern));
                    actasmtoken:=AS_INTNUM;
                    c:=current_scanner^.asmgetchar;
                    exit;
                  end;
                'H' :
                  Begin
                    actasmpattern:=tostr(ValHexaDecimal(actasmpattern));
                    actasmtoken:=AS_INTNUM;
                    c:=current_scanner^.asmgetchar;
                    exit;
                  end;
                else { must be an integer number }
                  begin
                    actasmpattern:=tostr(ValDecimal(actasmpattern));
                    actasmtoken:=AS_INTNUM;
                    exit;
                  end;
              end;
            end;
         end;

       ';','{',#13,newline :
         begin
           c:=current_scanner^.asmgetchar;
           firsttoken:=TRUE;
           actasmtoken:=AS_SEPARATOR;
           exit;
         end;

        else
           Begin
             Message(scan_f_illegal_char);
           end;
     end;
   end;
end;


function consume(t : tasmtoken):boolean;
begin
  Consume:=true;
  if t<>actasmtoken then
   begin
     Message2(scan_f_syn_expected,token2str[t],token2str[actasmtoken]);
     Consume:=false;
   end;
  repeat
    gettoken;
  until actasmtoken<>AS_NONE;
end;


procedure RecoverConsume(allowcomma:boolean);
begin
  While not (actasmtoken in [AS_SEPARATOR,AS_END]) do
   begin
     if allowcomma and (actasmtoken=AS_COMMA) then
      break;
     Consume(actasmtoken);
   end;
end;


{*****************************************************************************
                               Parsing Helpers
*****************************************************************************}

Procedure BuildRecordOffsetSize(const expr: string;var offset:longint;var size:longint);
{ Description: This routine builds up a record offset after a AS_DOT }
{ token is encountered.                                              }
{ On entry actasmtoken should be equal to AS_DOT                     }
var
  s : string;
Begin
  offset:=0;
  size:=0;
  s:=expr;
  while (actasmtoken=AS_DOT) do
   begin
     Consume(AS_DOT);
     if actasmtoken=AS_ID then
      s:=s+'.'+actasmpattern;
     if not Consume(AS_ID) then
      begin
        RecoverConsume(true);
        break;
      end;
   end;
  if not GetRecordOffsetSize(s,offset,size) then
   Message(asmr_e_building_record_offset);
end;


Procedure BuildConstSymbolExpression(needofs:boolean;var value:longint;var asmsym:string);
var
  tempstr,expr,hs : string;
  parenlevel,l,k : longint;
  errorflag : boolean;
  prevtok : tasmtoken;
  hl : PAsmLabel;
  sym : psym;
Begin
  { reset }
  value:=0;
  asmsym:='';
  errorflag:=FALSE;
  tempstr:='';
  expr:='';
  inexpression:=TRUE;
  parenlevel:=0;
  Repeat
    Case actasmtoken of
      AS_LPAREN:
        Begin
          Consume(AS_LPAREN);
          expr:=expr + '(';
          inc(parenlevel);
        end;
      AS_RPAREN:
        Begin
          Consume(AS_RPAREN);
          expr:=expr + ')';
          dec(parenlevel);
        end;
      AS_SHL:
        Begin
          Consume(AS_SHL);
          expr:=expr + '<';
        end;
      AS_SHR:
        Begin
          Consume(AS_SHR);
          expr:=expr + '>';
        end;
      AS_SLASH:
        Begin
          Consume(AS_SLASH);
          expr:=expr + '/';
        end;
      AS_MOD:
        Begin
          Consume(AS_MOD);
          expr:=expr + '%';
        end;
      AS_STAR:
        Begin
          Consume(AS_STAR);
          expr:=expr + '*';
        end;
      AS_PLUS:
        Begin
          Consume(AS_PLUS);
          expr:=expr + '+';
        end;
      AS_MINUS:
        Begin
          Consume(AS_MINUS);
          expr:=expr + '-';
        end;
      AS_AND:
        Begin
          Consume(AS_AND);
          expr:=expr + '&';
        end;
      AS_NOT:
        Begin
          Consume(AS_NOT);
          expr:=expr + '~';
        end;
      AS_XOR:
        Begin
          Consume(AS_XOR);
          expr:=expr + '^';
        end;
      AS_OR:
        Begin
          Consume(AS_OR);
          expr:=expr + '|';
        end;
      AS_INTNUM:
        Begin
          expr:=expr + actasmpattern;
          Consume(AS_INTNUM);
        end;
      AS_OFFSET:
        begin
          Consume(AS_OFFSET);
          if actasmtoken<>AS_ID then
           Message(asmr_e_offset_without_identifier);
        end;
      AS_ID:
        Begin
          tempstr:=actasmpattern;
          prevtok:=prevasmtoken;
          consume(AS_ID);
          if actasmtoken=AS_DOT then
           begin
             BuildRecordOffsetSize(tempstr,l,k);
             str(l, tempstr);
             expr:=expr + tempstr;
           end
          else
           if SearchIConstant(tempstr,l) then
            begin
              str(l, tempstr);
              expr:=expr + tempstr;
            end
          else
           begin
             hs:='';
             if is_locallabel(tempstr) then
              begin
                CreateLocalLabel(tempstr,hl,false);
                hs:=hl^.name
              end
             else
              if SearchLabel(tempstr,hl,false) then
               hs:=hl^.name
             else
              begin
                getsym(tempstr,false);
                sym:=srsym;
                if assigned(sym) then
                 begin
                   if sym^.owner^.symtabletype in [localsymtable,parasymtable] then
                     Message(asmr_e_no_local_or_para_allowed);
                   case srsym^.typ of
                     varsym :
                       hs:=pvarsym(srsym)^.mangledname;
                     typedconstsym :
                       hs:=ptypedconstsym(srsym)^.mangledname;
                     procsym :
                       hs:=pprocsym(srsym)^.mangledname;
                     else
                       Message(asmr_e_wrong_sym_type);
                   end;
                 end
                else
                 Message1(sym_e_unknown_id,tempstr);
              end;
             { symbol found? }
             if hs<>'' then
              begin
                if needofs and (prevtok<>AS_OFFSET) then
                 Message(asmr_e_need_offset);
                if asmsym='' then
                 asmsym:=hs
                else
                 Message(asmr_e_cant_have_multiple_relocatable_symbols);
                if (expr='') or (expr[length(expr)]='+') then
                 begin
                   delete(expr,length(expr),1);
                   if not(actasmtoken in [AS_MINUS,AS_PLUS,AS_COMMA,AS_SEPARATOR,AS_END]) then
                    Message(asmr_e_only_add_relocatable_symbol);
                 end
                else
                 Message(asmr_e_only_add_relocatable_symbol);
              end;
           end;
        end;
      AS_RBRACKET,
      AS_SEPARATOR,
      AS_COMMA:
        Begin
          break;
        end;
    else
      Begin
        { write error only once. }
        if not errorflag then
          Message(asmr_e_invalid_constant_expression);
        { consume tokens until we find COMMA or SEPARATOR }
        Consume(actasmtoken);
        errorflag:=TRUE;
      end;
    end;
  Until false;
  { calculate expression }
  if not ErrorFlag then
    value:=CalculateExpression(expr)
  else
    value:=0;
  { no longer in an expression }
  inexpression:=FALSE;
end;



Function BuildConstExpression:longint;
var
  l : longint;
  hs : string;
begin
  BuildConstSymbolExpression(false,l,hs);
  if hs<>'' then
   Message(asmr_e_relocatable_symbol_not_allowed);
  BuildConstExpression:=l;
end;


{****************************************************************************
                               T386IntelOperand
****************************************************************************}

type
  P386IntelOperand=^T386IntelOperand;
  T386IntelOperand=object(T386Operand)
    Procedure BuildOperand;virtual;
  private
    Procedure BuildReference;
    Procedure BuildConstant;
  end;



Procedure T386IntelOperand.BuildReference;
var
  l : longint;
  hs : string;
  code : integer;
  hreg,
  oldbase : tregister;
  GotPlus,Negative : boolean;
Begin
  Consume(AS_LBRACKET);
  InitRef;
  GotPlus:=true;
  Negative:=false;
  repeat
    Case actasmtoken of

      AS_ID: { Constant reference expression OR variable reference expression }
        Begin
          if not GotPlus then
            Message(asmr_e_invalid_reference_syntax);
          if actasmpattern[1] = '@' then
           Message(asmr_e_local_symbol_not_allowed_as_ref);
          if SearchIConstant(actasmpattern,l) then
           begin
             l:=BuildConstExpression;
             if actasmtoken=AS_STAR then
              opr.ref.scalefactor:=l
             else
              begin
                if negative then
                  Dec(opr.ref.offset,l)
                else
                  Inc(opr.ref.offset,l);
              end;
           end
          else
           Begin
             if hasvar then
               Message(asmr_e_cant_have_multiple_relocatable_symbols);
             if negative then
               Message(asmr_e_only_add_relocatable_symbol);
             oldbase:=opr.ref.base;
             opr.ref.base:=R_NO;
             if not SetupVar(actasmpattern) then
               Message1(sym_e_unknown_id,actasmpattern);
             { is the base register loaded by the var ? }
             if (opr.ref.base<>R_NO) then
              begin
                { check if we can move the old base to the index register }
                if (opr.ref.index<>R_NO) then
                 Message(asmr_e_wrong_base_index)
                else
                 opr.ref.index:=oldbase;
              end
             else
              opr.ref.base:=oldbase;
             { we can't have a Constant here so add the constant value to the
               offset }
             if opr.typ=OPR_CONSTANT then
              begin
                opr.typ:=OPR_REFERENCE;
                inc(opr.ref.offset,opr.val);
              end;
             Consume(AS_ID);
           end;
          GotPlus:=false;
        end;

      AS_PLUS :
        Begin
          Consume(AS_PLUS);
          Negative:=false;
          GotPlus:=true;
        end;

      AS_MINUS :
        begin
          Consume(AS_MINUS);
          Negative:=true;
          GotPlus:=true;
        end;

      AS_STAR : { Scaling }
        begin
          Consume(AS_STAR);
          hs:='';
          l:=0;
          case actasmtoken of
            AS_LPAREN :
              l:=BuildConstExpression;
            AS_INTNUM:
              Begin
                hs:=actasmpattern;
                Consume(AS_INTNUM);
              end;
            AS_REGISTER :
              begin
                if opr.ref.scalefactor=0 then
                 Message(asmr_e_wrong_scale_factor);
              end;
            else
              Message(asmr_e_invalid_reference_syntax);
          end;
          if actasmtoken<>AS_REGISTER then
           begin
             if hs<>'' then
              val(hs,l,code);
             opr.ref.scalefactor:=l
           end;
          GotPlus:=false;
        end;

      AS_REGISTER :
        begin
          if (not GotPlus) and (actasmtoken<>AS_STAR) then
            Message(asmr_e_invalid_reference_syntax);
          hreg:=actasmregister;
          Consume(AS_REGISTER);
          { this register will be the index }
          if (actasmtoken=AS_STAR) or
             (opr.ref.base<>R_NO) then
           begin
             if (opr.ref.index<>R_NO) then
              Message(asmr_e_multiple_index);
             opr.ref.index:=hreg;
           end
          else
           opr.ref.base:=hreg;
          GotPlus:=false;
        end;

      AS_NOT,
      AS_INTNUM,
      AS_LPAREN : { Constant reference expression }
        begin
          if not GotPlus then
            Message(asmr_e_invalid_reference_syntax);
          l:=BuildConstExpression;
          if actasmtoken=AS_STAR then
           opr.ref.scalefactor:=l
          else
           begin
             if negative then
               Dec(opr.ref.offset,l)
             else
               Inc(opr.ref.offset,l);
           end;
          GotPlus:=false;
        end;

      AS_RBRACKET :
        begin
          if GotPlus then
            Message(asmr_e_invalid_reference_syntax);
          Consume(AS_RBRACKET);
          break;
        end;

      else
        Begin
          Message(asmr_e_invalid_reference_syntax);
          RecoverConsume(true);
          break;
        end;
    end;
  until false;
end;


Procedure T386IntelOperand.BuildConstant;
var
  l : longint;
  tempstr : string;
begin
  BuildConstSymbolExpression(true,l,tempstr);
  if tempstr<>'' then
   begin
     opr.typ:=OPR_SYMBOL;
     opr.symofs:=l;
     opr.symbol:=newasmsymbol(tempstr);
   end
  else
   begin
     opr.typ:=OPR_CONSTANT;
     opr.val:=l;
   end;
end;


Procedure T386IntelOperand.BuildOperand;

  procedure AddLabelOperand(hl:pasmlabel);
  begin
    if is_calljmp(actopcode) then
     begin
       opr.typ:=OPR_SYMBOL;
       opr.symbol:=hl;
     end
    else
     begin
       InitRef;
       opr.ref.symbol:=hl;
     end;
  end;

var
  expr,
  tempstr : string;
  tempreg : tregister;
  l,
  toffset,
  tsize   : longint;
  hl      : PAsmLabel;
Begin
  tempstr:='';
  expr:='';
  case actasmtoken of

    AS_OFFSET,
    AS_INTNUM,
    AS_PLUS,
    AS_MINUS,
    AS_NOT,
    AS_LPAREN :
      Begin
        if not (opr.typ in [OPR_NONE,OPR_CONSTANT]) then
          Message(asmr_e_invalid_operand_type);
        BuildConstant;
      end;

    AS_STRING :
      Begin
        if not (opr.typ in [OPR_NONE]) then
          Message(asmr_e_invalid_operand_type);
        if not PadZero(actasmpattern,4) then
          Message1(asmr_e_invalid_string_as_opcode_operand,actasmpattern);
        opr.typ:=OPR_CONSTANT;
        opr.val:=ord(actasmpattern[4]) + ord(actasmpattern[3]) shl 8 +
                 Ord(actasmpattern[2]) shl 16 + ord(actasmpattern[1]) shl 24;
        Consume(AS_STRING);
      end;


    AS_ID : { A constant expression, or a Variable ref. }
      Begin
        { Label or Special symbol reference? }
        if actasmpattern[1] = '@' then
         Begin
           if actasmpattern = '@RESULT' then
            Begin
              InitRef;
              SetupResult;
            end
           else
            if (actasmpattern = '@CODE') or (actasmpattern = '@DATA') then
              Message(asmr_w_CODE_and_DATA_not_supported)
           else
            { Local Label }
            begin
              CreateLocalLabel(actasmpattern,hl,false);
              Consume(AS_ID);
              AddLabelOperand(hl);
              if not (actasmtoken in [AS_SEPARATOR,AS_COMMA]) then
               Message(asmr_e_syntax_error);
            end;
         end
        else
        { support result for delphi modes }
         if (m_objpas in aktmodeswitches) and (actasmpattern='RESULT') then
          begin
            InitRef;
            SetUpResult;
            Consume(AS_ID);
          end
        { probably a variable or normal expression }
        { or a procedure (such as in CALL ID)      }
        else
         Begin
           { is it a constant ? }
           if SearchIConstant(actasmpattern,l) then
            Begin
              if not (opr.typ in [OPR_NONE,OPR_CONSTANT]) then
               Message(asmr_e_invalid_operand_type);
              BuildConstant;
            end
           else
            { Check for pascal label }
            if SearchLabel(actasmpattern,hl,false) then
             begin
               Consume(AS_ID);
               AddLabelOperand(hl);
               if not (actasmtoken in [AS_SEPARATOR,AS_COMMA]) then
                Message(asmr_e_syntax_error);
             end
            else
            { is it a normal variable ? }
             Begin
               InitRef;
               if not SetupVar(actasmpattern) then
                Begin
                  { not a variable, check special variables.. }
                  if actasmpattern = 'SELF' then
                   SetupSelf
                  else
                   Message1(sym_e_unknown_id,actasmpattern);
                end;
               l:=0;
               expr:=actasmpattern;
               Consume(AS_ID);
               if actasmtoken=AS_LBRACKET then
                begin
                  opr.typ:=OPR_REFERENCE;
                  reset_reference(opr.Ref);
                  BuildReference;
                end;
               if actasmtoken=AS_DOT then
                begin
                  if expr='' then
                   Message(asmr_e_no_var_type_specified)
                  else
                   begin
                     BuildRecordOffsetSize(expr,toffset,tsize);
                     inc(l,toffset);
                     SetSize(tsize);
                   end;
                end;
               if actasmtoken in [AS_PLUS,AS_MINUS] then
                inc(l,BuildConstExpression);
               if opr.typ=OPR_REFERENCE then
                inc(opr.ref.offset,l)
               else
                inc(opr.val,l);
             end;
         end;
      end;

    AS_REGISTER : { Register, a variable reference or a constant reference }
      Begin
        { save the type of register used. }
        tempreg:=actasmregister;
        Consume(AS_REGISTER);
        if actasmtoken = AS_COLON then
         Begin
           Consume(AS_COLON);
           InitRef;
           opr.ref.segment:=tempreg;
           BuildReference;
         end
        else
        { Simple register }
         begin
           if not (opr.typ in [OPR_NONE,OPR_REGISTER]) then
            Message(asmr_e_invalid_operand_type);
           opr.typ:=OPR_REGISTER;
           opr.reg:=tempreg;
           size:=reg_2_opsize[opr.reg];
         end;
      end;

    AS_LBRACKET: { a variable reference, register ref. or a constant reference }
      Begin
        BuildReference;
      end;

    AS_SEG :
      Begin
        Message(asmr_e_seg_not_supported);
        Consume(actasmtoken);
      end;

    AS_SEPARATOR,
    AS_COMMA: ;

    else
      Message(asmr_e_syn_operand);
  end;
  if not(actasmtoken in [AS_END,AS_SEPARATOR,AS_COMMA]) then
   begin
     Message(asmr_e_syntax_error);
     RecoverConsume(true);
   end;
end;


{*****************************************************************************
                                T386IntelInstruction
*****************************************************************************}

type
  P386IntelInstruction=^T386IntelInstruction;
  T386IntelInstruction=object(T386Instruction)
    procedure InitOperands;virtual;
    procedure BuildOpcode;virtual;
  end;

procedure T386IntelInstruction.InitOperands;
var
  i : longint;
begin
  for i:=1to 3 do
   Operands[i]:=new(P386IntelOperand,Init);
end;


Procedure T386IntelInstruction.BuildOpCode;
var
  PrefixOp,OverrideOp: tasmop;
  expr : string;
  size : topsize;
  operandnum : longint;
Begin
  expr:='';
  PrefixOp:=A_None;
  OverrideOp:=A_None;
  { prefix seg opcode / prefix opcode }
  repeat
    if is_prefix(actopcode) then
     begin
       PrefixOp:=ActOpcode;
       opcode:=ActOpcode;
       condition:=ActCondition;
       opsize:=ActOpsize;
       ConcatInstruction(curlist);
       Consume(AS_OPCODE);
     end
    else
     if is_override(actopcode) then
      begin
        OverrideOp:=ActOpcode;
        opcode:=ActOpcode;
        condition:=ActCondition;
        opsize:=ActOpsize;
        ConcatInstruction(curlist);
        Consume(AS_OPCODE);
      end
    else
     break;
  until (actasmtoken<>AS_OPCODE);
  { opcode }
  if (actasmtoken <> AS_OPCODE) then
   Begin
     Message(asmr_e_invalid_or_missing_opcode);
     RecoverConsume(false);
     exit;
   end;
  { Fill the instr object with the current state }
  Opcode:=ActOpcode;
  condition:=ActCondition;
  opsize:=ActOpsize;
  { Valid combination of prefix/override and instruction ?  }
  if (prefixop<>A_NONE) and (NOT CheckPrefix(PrefixOp,actopcode)) then
    Message1(asmr_e_invalid_prefix_and_opcode,actasmpattern);
  if (overrideop<>A_NONE) and (NOT CheckOverride(OverrideOp,ActOpcode)) then
    Message1(asmr_e_invalid_override_and_opcode,actasmpattern);
  { We are reading operands, so opcode will be an AS_ID }
  operandnum:=1;
  Consume(AS_OPCODE);
  { Zero operand opcode ?  }
  if actasmtoken in [AS_SEPARATOR,AS_END] then
   begin
     operandnum:=0;
     exit;
   end;
  { Read Operands }
  repeat
    case actasmtoken of

      { End of asm operands for this opcode }
      AS_END,
      AS_SEPARATOR :
        break;

      { Operand delimiter }
      AS_COMMA :
        Begin
          if operandnum > MaxOperands then
            Message(asmr_e_too_many_operands)
          else
            Inc(operandnum);
          Consume(AS_COMMA);
        end;

      { Typecast, Constant Expression, Type Specifier }
      AS_DWORD,
      AS_BYTE,
      AS_WORD,
      AS_TBYTE,
      AS_QWORD :
        Begin
          { load the size in a temp variable, so it can be set when the
            operand is read }
          Case actasmtoken of
            AS_DWORD : size:=S_L;
            AS_WORD  : size:=S_W;
            AS_BYTE  : size:=S_B;
            AS_QWORD : size:=S_IQ;
            AS_TBYTE : size:=S_FX;
          end;
          Consume(actasmtoken);
          if actasmtoken=AS_PTR then
           begin
             Consume(AS_PTR);
             Operands[operandnum]^.InitRef;
           end;
          Operands[operandnum]^.BuildOperand;
          { now set the size which was specified by the override }
          Operands[operandnum]^.size:=size;
        end;

      { Type specifier }
      AS_NEAR,
      AS_FAR :
        Begin
          if actasmtoken = AS_NEAR then
            Message(asmr_w_near_ignored)
          else
            Message(asmr_w_far_ignored);
          Consume(actasmtoken);
          if actasmtoken=AS_PTR then
           begin
             Consume(AS_PTR);
             Operands[operandnum]^.InitRef;
           end;
          Operands[operandnum]^.BuildOperand;
        end;

      else
        Operands[operandnum]^.BuildOperand;
    end; { end case }
  until false;
  Ops:=operandnum;
end;


Procedure BuildConstant(maxvalue: longint);
var
 strlength: byte;
 asmsym,
 expr: string;
 value : longint;
Begin
  strlength:=0; { assume it is a DB }
  Repeat
    Case actasmtoken of
      AS_STRING:
        Begin
          if maxvalue = $ffff then
            strlength:=2
          else
            if maxvalue = $ffffffff then
              strlength:=4;
          { DD and DW cases }
          if strlength <> 0 then
           Begin
             if Not PadZero(actasmpattern,strlength) then
              Message(scan_f_string_exceeds_line);
           end;
          expr:=actasmpattern;
          Consume(AS_STRING);
          Case actasmtoken of
            AS_COMMA:
              Consume(AS_COMMA);
            AS_SEPARATOR: ;
            else
              Message(asmr_e_invalid_string_expression);
          end;
          ConcatString(curlist,expr);
        end;
      AS_PLUS,
      AS_MINUS,
      AS_LPAREN,
      AS_NOT,
      AS_INTNUM,
      AS_ID :
        Begin
          BuildConstSymbolExpression(false,value,asmsym);
          if asmsym<>'' then
           begin
             if maxvalue<>$ffffffff then
               Message(asmr_w_const32bit_for_address);
             ConcatConstSymbol(curlist,asmsym,value)
           end
          else
           ConcatConstant(curlist,value,maxvalue);
        end;
      AS_COMMA:
        Consume(AS_COMMA);
      AS_SEPARATOR:
        break;
      else
        begin
          Message(asmr_e_syn_constant);
          RecoverConsume(false);
        end
    end;
  Until false;
end;


Function Assemble: Ptree;
Var
  hl : PAsmLabel;
  instr : T386IntelInstruction;
Begin
  Message1(asmr_d_start_reading,'intel');
  inexpression:=FALSE;
  firsttoken:=TRUE;
  if assigned(procinfo.retdef) and
     (is_fpu(procinfo.retdef) or
     ret_in_acc(procinfo.retdef)) then
    procinfo.funcret_is_valid:=true;
 { sets up all opcode and register tables in uppercase }
  if not _asmsorted then
   Begin
     SetupTables;
     _asmsorted:=TRUE;
   end;
  curlist:=new(paasmoutput,init);
  { setup label linked list }
  new(LocalLabelList,Init);
  { start tokenizer }
  c:=current_scanner^.asmgetchar;
  gettoken;
  { main loop }
  repeat
    case actasmtoken of
      AS_LLABEL:
        Begin
          if CreateLocalLabel(actasmpattern,hl,true) then
            ConcatLabel(curlist,hl);
          Consume(AS_LLABEL);
        end;

      AS_LABEL:
        Begin
          if SearchLabel(upper(actasmpattern),hl,true) then
           ConcatLabel(curlist,hl)
          else
           Message1(asmr_e_unknown_label_identifier,actasmpattern);
          Consume(AS_LABEL);
        end;

      AS_DW :
        Begin
          inexpression:=true;
          Consume(AS_DW);
          BuildConstant($ffff);
          inexpression:=false;
        end;

      AS_DB :
        Begin
          inexpression:=true;
          Consume(AS_DB);
          BuildConstant($ff);
          inexpression:=false;
        end;

      AS_DD :
        Begin
          inexpression:=true;
          Consume(AS_DD);
          BuildConstant($ffffffff);
          inexpression:=false;
        end;

      AS_OPCODE :
        Begin
          instr.init;
          instr.BuildOpcode;
          { We need AT&T style operands }
          instr.SwapOperands;
          instr.AddReferenceSizes;
          instr.SetInstructionOpsize;
          instr.CheckOperandSizes;
          instr.ConcatInstruction(curlist);
          instr.done;
        end;

      AS_SEPARATOR :
        Begin
          Consume(AS_SEPARATOR);
        end;

      AS_END :
        break; { end assembly block }

      else
        Begin
          Message(asmr_e_syntax_error);
          { error recovery }
          Consume(actasmtoken);
        end;
    end; { end case }
  until false;
  { Check LocalLabelList }
  LocalLabelList^.CheckEmitted;
  dispose(LocalLabelList,Done);
  { Return the list in an asmnode }
  assemble:=genasmnode(curlist);
  Message1(asmr_d_finish_reading,'intel');
end;


{*****************************************************************************
                                     Initialize
*****************************************************************************}

var
  old_exit : pointer;

procedure ra386int_exit;{$ifndef FPC}far;{$endif}
begin
  if assigned(iasmops) then
    dispose(iasmops);
  if assigned(iasmregs) then
    dispose(iasmregs);
  exitproc:=old_exit;
end;


begin
   old_exit:=exitproc;
   exitproc:=@ra386int_exit;
end.
{
  $Log$
  Revision 1.36  1999-06-01 19:56:37  peter
    * fixed llabel with delete the first @

  Revision 1.35  1999/05/27 19:44:59  peter
    * removed oldasm
    * plabel -> pasmlabel
    * -a switches to source writing automaticly
    * assembler readers OOPed
    * asmsymbol automaticly external
    * jumptables and other label fixes for asm readers

  Revision 1.34  1999/05/21 13:55:16  peter
    * NEWLAB for label as symbol

  Revision 1.33  1999/05/05 22:22:03  peter
    * updated messages

  Revision 1.32  1999/05/04 21:45:02  florian
    * changes to compile it with Delphi 4.0

  Revision 1.31  1999/05/01 13:48:41  peter
    * merged nasm compiler

  Revision 1.6  1999/04/26 23:26:18  peter
    * redesigned record offset parsing to support nested records
    * normal compiler uses the redesigned createvarinstr()

  Revision 1.5  1999/04/20 11:01:24  peter
    * better tokenpos info

  Revision 1.4  1999/04/14 09:07:46  peter
    * asm reader improvements

  Revision 1.3  1999/03/06 17:24:27  peter
    * rewritten intel parser a lot, especially reference reading
    * size checking added for asm parsers

  Revision 1.2  1999/03/02 02:56:31  peter
    + stabs support for binary writers
    * more fixes and missing updates from the previous commit :(

  Revision 1.1  1999/03/01 15:46:26  peter
    * ag386bin finally make cycles correct
    * prefixes are now also normal opcodes
}
