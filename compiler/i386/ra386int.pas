{
    $Id$
    Copyright (c) 1998-2002 by Carl Eric Codere and Peter Vreman

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
Unit Ra386int;

{$i fpcdefs.inc}

  interface

    uses
      cclasses,
      cpubase,
      rasm,
      rax86;

  type
    tasmtoken = (
      AS_NONE,AS_LABEL,AS_LLABEL,AS_STRING,AS_INTNUM,
      AS_COMMA,AS_LBRACKET,AS_RBRACKET,AS_LPAREN,
      AS_RPAREN,AS_COLON,AS_DOT,AS_PLUS,AS_MINUS,AS_STAR,
      AS_SEPARATOR,AS_ID,AS_REGISTER,AS_OPCODE,AS_SLASH,
       {------------------ Assembler directives --------------------}
      AS_DB,AS_DW,AS_DD,AS_END,
       {------------------ Assembler Operators  --------------------}
      AS_BYTE,AS_WORD,AS_DWORD,AS_QWORD,AS_TBYTE,AS_DQWORD,AS_NEAR,AS_FAR,
      AS_HIGH,AS_LOW,AS_OFFSET,AS_SEG,AS_TYPE,AS_PTR,AS_MOD,AS_SHL,AS_SHR,AS_NOT,
      AS_AND,AS_OR,AS_XOR);

    type
       ti386intreader = class(tasmreader)
         actasmtoken : tasmtoken;
         prevasmtoken : tasmtoken;
         ActOpsize : topsize;
         constructor create;override;
         destructor destroy;override;
         function is_asmopcode(const s: string):boolean;
         function is_asmoperator(const s: string):boolean;
         function is_asmdirective(const s: string):boolean;
         function is_register(const s:string):boolean;
         function is_locallabel(const s:string):boolean;
         function Assemble: tlinkedlist;override;
         procedure GetToken;
         function consume(t : tasmtoken):boolean;
         procedure RecoverConsume(allowcomma:boolean);
         procedure BuildRecordOffsetSize(const expr: string;var offset:longint;var size:longint);
         procedure BuildConstSymbolExpression(needofs,isref:boolean;var value:longint;var asmsym:string);
         function BuildConstExpression:longint;
         function BuildRefConstExpression:longint;
         procedure BuildReference(oper : tx86operand);
         procedure BuildOperand(oper: tx86operand);
         procedure BuildConstantOperand(oper: tx86operand);
         procedure BuildOpCode(instr : tx86instruction);
         procedure BuildConstant(maxvalue: longint);
       end;


  implementation

    uses
       { common }
       cutils,
       { global }
       globtype,globals,verbose,
       systems,
       { aasm }
       cpuinfo,aasmbase,aasmtai,aasmcpu,
       { symtable }
       symconst,symbase,symtype,symsym,symtable,
       { parser }
       scanner,
       { register allocator }
       rabase,rautils,itx86int,
       { codegen }
       cgbase,cgobj,procinfo
       ;

    type
      tasmkeyword = string[6];


    const
       { These tokens should be modified accordingly to the modifications }
       { in the different enumerations.                                   }
       firstdirective = AS_DB;
       lastdirective  = AS_END;
       firstoperator  = AS_BYTE;
       lastoperator   = AS_XOR;

       _count_asmdirectives = longint(lastdirective)-longint(firstdirective);
       _count_asmoperators  = longint(lastoperator)-longint(firstoperator);

       _asmdirectives : array[0.._count_asmdirectives] of tasmkeyword =
       ('DB','DW','DD','END');

       { problems with shl,shr,not,and,or and xor, they are }
       { context sensitive.                                 }
       _asmoperators : array[0.._count_asmoperators] of tasmkeyword = (
        'BYTE','WORD','DWORD','QWORD','TBYTE','DQWORD','NEAR','FAR','HIGH',
        'LOW','OFFSET','SEG','TYPE','PTR','MOD','SHL','SHR','NOT','AND',
        'OR','XOR');

      token2str : array[tasmtoken] of string[10] = (
        '','Label','LLabel','String','Integer',
        ',','[',']','(',
        ')',':','.','+','-','*',
        ';','identifier','register','opcode','/',
        '','','','END',
        '','','','','','','','','',
        '','','','type','ptr','mod','shl','shr','not',
        'and','or','xor'
      );

    var
      inexpression   : boolean;

    constructor ti386intreader.create;
      var
        i : tasmop;
        str2opentry: tstr2opentry;
      Begin
        inherited create;
        { opcodes }
        { creates uppercased symbol tables for speed access }
        iasmops:=tdictionary.create;
        iasmops.delete_doubles:=true;
        for i:=firstop to lastop do
          begin
            str2opentry:=tstr2opentry.createname(upper(std_op2str[i]));
            str2opentry.op:=i;
            iasmops.insert(str2opentry);
          end;
      end;


    destructor ti386intreader.destroy;
      begin
        if assigned(iasmops) then
          iasmops.Free;
      end;

{---------------------------------------------------------------------}
{                     Routines for the tokenizing                     }
{---------------------------------------------------------------------}


     function ti386intreader.is_asmopcode(const s: string):boolean;
       var
         str2opentry: tstr2opentry;
         cond : string[4];
         cnd : tasmcond;
         j: longint;
       Begin
         is_asmopcode:=FALSE;

         actopcode:=A_None;
         actcondition:=C_None;
         actopsize:=S_NO;

         str2opentry:=tstr2opentry(iasmops.search(s));
         if assigned(str2opentry) then
           begin
             actopcode:=str2opentry.op;
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


    function ti386intreader.is_asmoperator(const s: string):boolean;
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


    Function ti386intreader.is_asmdirective(const s: string):boolean;
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


    function ti386intreader.is_register(const s:string):boolean;
      begin
        is_register:=false;
        actasmregister:=masm_regnum_search(lower(s));
        if actasmregister<>NR_NO then
          begin
            is_register:=true;
            actasmtoken:=AS_REGISTER;
          end;
      end;


    function ti386intreader.is_locallabel(const s:string):boolean;
      begin
        is_locallabel:=(length(s)>1) and (s[1]='@');
      end;


    Procedure ti386intreader.GetToken;
      var
        len : longint;
        forcelabel : boolean;
        srsym : tsym;
        srsymtable : tsymtable;
      begin
        { save old token and reset new token }
        prevasmtoken:=actasmtoken;
        actasmtoken:=AS_NONE;
        { reset }
        forcelabel:=FALSE;
        actasmpattern:='';
        { while space and tab , continue scan... }
        while (c in [' ',#9]) do
          c:=current_scanner.asmgetchar;
        { get token pos }
        if not (c in [#10,#13,'{',';']) then
          current_scanner.gettokenpos;
      { Local Label, Label, Directive, Prefix or Opcode }
        if firsttoken and not (c in [#10,#13,'{',';']) then
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
              c:=current_scanner.asmgetchar;
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
              c:=current_scanner.asmgetchar;
              firsttoken:=true;
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
           message1(asmr_e_unknown_opcode,actasmpattern);
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
                 c:=current_scanner.asmgetchar;
                 while c in  ['A'..'Z','a'..'z','0'..'9','_','@'] do
                  begin
                    actasmpattern:=actasmpattern + c;
                    c:=current_scanner.asmgetchar;
                  end;
                 uppervar(actasmpattern);
                 actasmtoken:=AS_ID;
                 exit;
               end;

             'A'..'Z','a'..'z','_': { identifier, register, opcode, prefix or directive }
               begin
                 actasmpattern:=c;
                 c:=current_scanner.asmgetchar;
                 while c in  ['A'..'Z','a'..'z','0'..'9','_'] do
                  begin
                    actasmpattern:=actasmpattern + c;
                    c:=current_scanner.asmgetchar;
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
                 { support st(X) for fpu registers }
                 if (actasmpattern = 'ST') and (c='(') then
                  Begin
                    actasmpattern:=actasmpattern+c;
                    c:=current_scanner.asmgetchar;
                    if c in ['0'..'7'] then
                     actasmpattern:=actasmpattern + c
                    else
                     Message(asmr_e_invalid_fpu_register);
                    c:=current_scanner.asmgetchar;
                    if c <> ')' then
                     Message(asmr_e_invalid_fpu_register)
                    else
                     Begin
                       actasmpattern:=actasmpattern + c;
                       c:=current_scanner.asmgetchar;
                     end;
                  end;
                 if is_register(actasmpattern) then
                  exit;
                 if is_asmdirective(actasmpattern) then
                  exit;
                 if is_asmoperator(actasmpattern) then
                  exit;
                 { if next is a '.' and this is a unitsym then we also need to
                   parse the identifier }
                 if (c='.') then
                  begin
                    searchsym(actasmpattern,srsym,srsymtable);
                    if assigned(srsym) and
                       (srsym.typ=unitsym) and
                       (srsym.owner.unitid=0) then
                     begin
                       { Add . to create System.Identifier }
                       actasmpattern:=actasmpattern+c;
                       c:=current_scanner.asmgetchar;
                       { Delphi allows System.@Halt, just ignore the @ }
                       if c='@' then
                         c:=current_scanner.asmgetchar;
                       while c in  ['A'..'Z','a'..'z','0'..'9','_','$'] do
                        begin
                          actasmpattern:=actasmpattern + upcase(c);
                          c:=current_scanner.asmgetchar;
                        end;
                     end;
                  end;
                 actasmtoken:=AS_ID;
                 exit;
               end;

             '''' : { string or character }
               begin
                 actasmpattern:='';
                 current_scanner.in_asm_string:=true;
                 repeat
                   if c = '''' then
                    begin
                      c:=current_scanner.asmgetchar;
                      if c in [#10,#13] then
                       begin
                         Message(scan_f_string_exceeds_line);
                         break;
                       end;
                      repeat
                        if c='''' then
                         begin
                           c:=current_scanner.asmgetchar;
                           if c='''' then
                            begin
                              actasmpattern:=actasmpattern+'''';
                              c:=current_scanner.asmgetchar;
                              if c in [#10,#13] then
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
                           c:=current_scanner.asmgetchar;
                           if c in [#10,#13] then
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
                 current_scanner.in_asm_string:=false;
                 actasmtoken:=AS_STRING;
                 exit;
               end;

             '"' : { string or character }
               begin
                 current_scanner.in_asm_string:=true;
                 actasmpattern:='';
                 repeat
                   if c = '"' then
                    begin
                      c:=current_scanner.asmgetchar;
                      if c in [#10,#13] then
                       begin
                         Message(scan_f_string_exceeds_line);
                         break;
                       end;
                      repeat
                        if c='"' then
                         begin
                           c:=current_scanner.asmgetchar;
                           if c='"' then
                            begin
                              actasmpattern:=actasmpattern+'"';
                              c:=current_scanner.asmgetchar;
                              if c in [#10,#13] then
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
                           c:=current_scanner.asmgetchar;
                           if c in [#10,#13] then
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
                 current_scanner.in_asm_string:=false;
                 actasmtoken:=AS_STRING;
                 exit;
               end;

             '$' :
               begin
                 c:=current_scanner.asmgetchar;
                 while c in ['0'..'9','A'..'F','a'..'f'] do
                  begin
                    actasmpattern:=actasmpattern + c;
                    c:=current_scanner.asmgetchar;
                  end;
                 actasmpattern:=tostr(ValHexaDecimal(actasmpattern));
                 actasmtoken:=AS_INTNUM;
                 exit;
               end;

             ',' :
               begin
                 actasmtoken:=AS_COMMA;
                 c:=current_scanner.asmgetchar;
                 exit;
               end;

             '[' :
               begin
                 actasmtoken:=AS_LBRACKET;
                 c:=current_scanner.asmgetchar;
                 exit;
               end;

             ']' :
               begin
                 actasmtoken:=AS_RBRACKET;
                 c:=current_scanner.asmgetchar;
                 exit;
               end;

             '(' :
               begin
                 actasmtoken:=AS_LPAREN;
                 c:=current_scanner.asmgetchar;
                 exit;
               end;

             ')' :
               begin
                 actasmtoken:=AS_RPAREN;
                 c:=current_scanner.asmgetchar;
                 exit;
               end;

             ':' :
               begin
                 actasmtoken:=AS_COLON;
                 c:=current_scanner.asmgetchar;
                 exit;
               end;

             '.' :
               begin
                 actasmtoken:=AS_DOT;
                 c:=current_scanner.asmgetchar;
                 exit;
               end;

             '+' :
               begin
                 actasmtoken:=AS_PLUS;
                 c:=current_scanner.asmgetchar;
                 exit;
               end;

             '-' :
               begin
                 actasmtoken:=AS_MINUS;
                 c:=current_scanner.asmgetchar;
                 exit;
               end;

             '*' :
               begin
                 actasmtoken:=AS_STAR;
                 c:=current_scanner.asmgetchar;
                 exit;
               end;

             '/' :
               begin
                 actasmtoken:=AS_SLASH;
                 c:=current_scanner.asmgetchar;
                 exit;
               end;

             '0'..'9':
               begin
                 actasmpattern:=c;
                 c:=current_scanner.asmgetchar;
                 { Get the possible characters }
                 while c in ['0'..'9','A'..'F','a'..'f'] do
                  begin
                    actasmpattern:=actasmpattern + c;
                    c:=current_scanner.asmgetchar;
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
                          c:=current_scanner.asmgetchar;
                          exit;
                        end;
                      'H' :
                        Begin
                          actasmpattern:=tostr(ValHexaDecimal(actasmpattern));
                          actasmtoken:=AS_INTNUM;
                          c:=current_scanner.asmgetchar;
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
             ';','{',#13,#10 :
               begin
                 c:=current_scanner.asmgetchar;
                 firsttoken:=TRUE;
                 actasmtoken:=AS_SEPARATOR;
                 exit;
               end;

              else
                 current_scanner.illegal_char(c);
           end;
         end;
      end;


  function ti386intreader.consume(t : tasmtoken):boolean;
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


  procedure ti386intreader.RecoverConsume(allowcomma:boolean);
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

    { This routine builds up a record offset after a AS_DOT
      token is encountered.
      On entry actasmtoken should be equal to AS_DOT                     }
    Procedure ti386intreader.BuildRecordOffsetSize(const expr: string;var offset:longint;var size:longint);
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


    Procedure ti386intreader.BuildConstSymbolExpression(needofs,isref:boolean;var value:longint;var asmsym:string);
      var
        tempstr,expr,hs : string;
        parenlevel,l,k : longint;
        hasparen,
        errorflag : boolean;
        prevtok : tasmtoken;
        hl : tasmlabel;
        sym : tsym;
        srsymtable : tsymtable;
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
                if isref and (actasmtoken=AS_REGISTER) then
                 break;
                expr:=expr + '*';
              end;
            AS_PLUS:
              Begin
                Consume(AS_PLUS);
                if isref and (actasmtoken=AS_REGISTER) then
                 break;
                expr:=expr + '+';
              end;
            AS_LBRACKET:
              begin
                { Support ugly delphi constructs like: [ECX].1+2[EDX] }
                if isref then
                  break;
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
            AS_TYPE:
              begin
                l:=0;
                hasparen:=false;
                Consume(AS_TYPE);
                if actasmtoken=AS_LPAREN then
                  begin
                    hasparen:=true;
                    Consume(AS_LPAREN);
                  end;
                if actasmtoken<>AS_ID then
                 Message(asmr_e_type_without_identifier)
                else
                 begin
                   tempstr:=actasmpattern;
                   Consume(AS_ID);
                   if actasmtoken=AS_DOT then
                    BuildRecordOffsetSize(tempstr,k,l)
                   else
                    begin
                      searchsym(tempstr,sym,srsymtable);
                      if assigned(sym) then
                       begin
                         case sym.typ of
                           varsym :
                             l:=tvarsym(sym).getsize;
                           typedconstsym :
                             l:=ttypedconstsym(sym).getsize;
                           typesym :
                             l:=ttypesym(sym).restype.def.size;
                           else
                             Message(asmr_e_wrong_sym_type);
                         end;
                       end
                      else
                       Message1(sym_e_unknown_id,tempstr);
                    end;
                 end;
                str(l, tempstr);
                expr:=expr + tempstr;
                if hasparen then
                  Consume(AS_RPAREN);
              end;
            AS_STRING:
              Begin
                l:=0;
                case Length(actasmpattern) of
                 1 :
                  l:=ord(actasmpattern[1]);
                 2 :
                  l:=ord(actasmpattern[2]) + ord(actasmpattern[1]) shl 8;
                 3 :
                  l:=ord(actasmpattern[3]) +
                     Ord(actasmpattern[2]) shl 8 + ord(actasmpattern[1]) shl 16;
                 4 :
                  l:=ord(actasmpattern[4]) + ord(actasmpattern[3]) shl 8 +
                     Ord(actasmpattern[2]) shl 16 + ord(actasmpattern[1]) shl 24;
                else
                  Message1(asmr_e_invalid_string_as_opcode_operand,actasmpattern);
                end;
                str(l, tempstr);
                expr:=expr + tempstr;
                Consume(AS_STRING);
              end;
            AS_ID:
              Begin
                hs:='';
                tempstr:=actasmpattern;
                prevtok:=prevasmtoken;
                consume(AS_ID);
                if SearchIConstant(tempstr,l) then
                 begin
                   str(l, tempstr);
                   expr:=expr + tempstr;
                 end
                else
                 begin
                   if is_locallabel(tempstr) then
                    begin
                      CreateLocalLabel(tempstr,hl,false);
                      hs:=hl.name
                    end
                   else
                    if SearchLabel(tempstr,hl,false) then
                     hs:=hl.name
                   else
                    begin
                      searchsym(tempstr,sym,srsymtable);
                      if assigned(sym) then
                       begin
                         case sym.typ of
                           varsym :
                             begin
                               if sym.owner.symtabletype in [localsymtable,parasymtable] then
                                Message(asmr_e_no_local_or_para_allowed);
                               hs:=tvarsym(sym).mangledname;
                             end;
                           typedconstsym :
                             hs:=ttypedconstsym(sym).mangledname;
                           procsym :
                             begin
                               if Tprocsym(sym).procdef_count>1 then
                                Message(asmr_w_calling_overload_func);
                               hs:=tprocsym(sym).first_procdef.mangledname;
                             end;
                           typesym :
                             begin
                               if not(ttypesym(sym).restype.def.deftype in [recorddef,objectdef]) then
                                Message(asmr_e_wrong_sym_type);
                             end;
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
                         { don't remove the + if there could be a record field }
                         if actasmtoken<>AS_DOT then
                          delete(expr,length(expr),1);
                       end
                      else
                       Message(asmr_e_only_add_relocatable_symbol);
                    end;
                   if actasmtoken=AS_DOT then
                    begin
                      BuildRecordOffsetSize(tempstr,l,k);
                      str(l, tempstr);
                      expr:=expr + tempstr;
                    end
                   else
                    begin
                      if (expr='') or (expr[length(expr)] in ['+','-','/','*']) then
                       delete(expr,length(expr),1);
                    end;
                 end;
                { check if there are wrong operator used like / or mod etc. }
                if (hs<>'') and not(actasmtoken in [AS_MINUS,AS_PLUS,AS_COMMA,AS_SEPARATOR,AS_END,AS_RBRACKET]) then
                 Message(asmr_e_only_add_relocatable_symbol);
              end;
            AS_END,
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


    Function ti386intreader.BuildConstExpression:longint;
      var
        l : longint;
        hs : string;
      begin
        BuildConstSymbolExpression(false,false,l,hs);
        if hs<>'' then
         Message(asmr_e_relocatable_symbol_not_allowed);
        BuildConstExpression:=l;
      end;


    Function ti386intreader.BuildRefConstExpression:longint;
      var
        l : longint;
        hs : string;
      begin
        BuildConstSymbolExpression(false,true,l,hs);
        if hs<>'' then
         Message(asmr_e_relocatable_symbol_not_allowed);
        BuildRefConstExpression:=l;
      end;


    procedure ti386intreader.BuildReference(oper : tx86operand);
      var
        k,l,scale : longint;
        tempstr,hs : string;
        typesize : longint;
        code : integer;
        hreg : tregister;
        GotStar,GotOffset,HadVar,
        GotPlus,Negative : boolean;
      Begin
        Consume(AS_LBRACKET);
        if not(oper.opr.typ in [OPR_LOCAL,OPR_REFERENCE]) then
          oper.InitRef;
        GotStar:=false;
        GotPlus:=true;
        GotOffset:=false;
        Negative:=false;
        Scale:=0;
        repeat
          if GotOffset and (actasmtoken<>AS_ID) then
            Message(asmr_e_invalid_reference_syntax);

          Case actasmtoken of

            AS_ID: { Constant reference expression OR variable reference expression }
              Begin
                if not GotPlus then
                  Message(asmr_e_invalid_reference_syntax);
                if actasmpattern[1] = '@' then
                 Message(asmr_e_local_label_not_allowed_as_ref);
                GotStar:=false;
                GotPlus:=false;
                if SearchIConstant(actasmpattern,l) or
                   SearchRecordType(actasmpattern) then
                 begin
                   l:=BuildRefConstExpression;
                   GotPlus:=(prevasmtoken=AS_PLUS);
                   GotStar:=(prevasmtoken=AS_STAR);
                   case oper.opr.typ of
                     OPR_LOCAL :
                       begin
                         if GotStar then
                           Message(asmr_e_invalid_reference_syntax);
                         if negative then
                           Dec(oper.opr.localsymofs,l)
                         else
                           Inc(oper.opr.localsymofs,l);
                       end;
                     OPR_REFERENCE :
                       begin
                         if GotStar then
                          oper.opr.ref.scalefactor:=l
                         else
                          begin
                            if negative then
                              Dec(oper.opr.ref.offset,l)
                            else
                              Inc(oper.opr.ref.offset,l);
                          end;
                        end;
                   end;
                 end
                else
                 Begin
                   if oper.hasvar and not GotOffset then
                     Message(asmr_e_cant_have_multiple_relocatable_symbols);
                   HadVar:=oper.hasvar and GotOffset;
                   if negative then
                     Message(asmr_e_only_add_relocatable_symbol);
                   tempstr:=actasmpattern;
                   Consume(AS_ID);
                   { typecasting? }
                   if (actasmtoken=AS_LPAREN) and
                      SearchType(tempstr,typesize) then
                    begin
                      oper.hastype:=true;
                      Consume(AS_LPAREN);
                      BuildOperand(oper);
                      Consume(AS_RPAREN);
                      if oper.opr.typ in [OPR_REFERENCE,OPR_LOCAL] then
                        oper.SetSize(typesize,true);
                    end
                   else
                    if not oper.SetupVar(tempstr,GotOffset) then
                     Message1(sym_e_unknown_id,tempstr);
                   { record.field ? }
                   if actasmtoken=AS_DOT then
                    begin
                      BuildRecordOffsetSize(tempstr,l,k);
                      case oper.opr.typ of
                        OPR_LOCAL :
                          inc(oper.opr.localsymofs,l);
                        OPR_REFERENCE :
                          inc(oper.opr.ref.offset,l);
                      end;
                    end;
                   if GotOffset then
                    begin
                      if oper.hasvar and (oper.opr.ref.base=current_procinfo.framepointer) then
                       begin
                         if (oper.opr.typ=OPR_REFERENCE) then
                           oper.opr.ref.base:=NR_NO;
                         oper.hasvar:=hadvar;
                       end
                      else
                       begin
                         if oper.hasvar and hadvar then
                          Message(asmr_e_cant_have_multiple_relocatable_symbols);
                         { should we allow ?? }
                       end;
                    end;
                 end;
                GotOffset:=false;
              end;

            AS_PLUS :
              Begin
                Consume(AS_PLUS);
                Negative:=false;
                GotPlus:=true;
                GotStar:=false;
                Scale:=0;
              end;

            AS_MINUS :
              begin
                Consume(AS_MINUS);
                Negative:=true;
                GotPlus:=true;
                GotStar:=false;
                Scale:=0;
              end;

            AS_STAR : { Scaling, with eax*4 order }
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
                      case oper.opr.typ of
                        OPR_REFERENCE :
                          begin
                            if oper.opr.ref.scalefactor=0 then
                              begin
                                if scale<>0 then
                                  begin
                                    oper.opr.ref.scalefactor:=scale;
                                    scale:=0;
                                  end
                                else
                                 Message(asmr_e_wrong_scale_factor);
                              end
                            else
                              Message(asmr_e_invalid_reference_syntax);
                          end;
                        OPR_LOCAL :
                          begin
                            if oper.opr.localscale=0 then
                              begin
                                if scale<>0 then
                                  begin
                                    oper.opr.localscale:=scale;
                                    scale:=0;
                                  end
                                else
                                 Message(asmr_e_wrong_scale_factor);
                              end
                            else
                              Message(asmr_e_invalid_reference_syntax);
                          end;
                      end;
                    end;
                  else
                    Message(asmr_e_invalid_reference_syntax);
                end;
                if actasmtoken<>AS_REGISTER then
                  begin
                    if hs<>'' then
                      val(hs,l,code);
                    case oper.opr.typ of
                      OPR_REFERENCE :
                        oper.opr.ref.scalefactor:=l;
                      OPR_LOCAL :
                        oper.opr.localscale:=l;
                    end;
                    if l>9 then
                      Message(asmr_e_wrong_scale_factor);
                  end;
                GotPlus:=false;
                GotStar:=false;
              end;

            AS_REGISTER :
              begin
                if not((GotPlus and (not Negative)) or
                       GotStar) then
                  Message(asmr_e_invalid_reference_syntax);
                hreg:=actasmregister;
                Consume(AS_REGISTER);
                { this register will be the index:
                   1. just read a *
                   2. next token is a *
                   3. base register is already used }
                case oper.opr.typ of
                  OPR_LOCAL :
                    begin
                      if (oper.opr.localindexreg<>NR_NO) then
                        Message(asmr_e_multiple_index);
                      oper.opr.localindexreg:=hreg;
                      if scale<>0 then
                        begin
                          oper.opr.localscale:=scale;
                          scale:=0;
                        end;
                    end;
                  OPR_REFERENCE :
                    begin
                      if (GotStar) or
                         (actasmtoken=AS_STAR) or
                         (oper.opr.ref.base<>NR_NO) then
                       begin
                         if (oper.opr.ref.index<>NR_NO) then
                          Message(asmr_e_multiple_index);
                         oper.opr.ref.index:=hreg;
                         if scale<>0 then
                           begin
                             oper.opr.ref.scalefactor:=scale;
                             scale:=0;
                           end;
                       end
                      else
                       oper.opr.ref.base:=hreg;
                    end;
                end;
                GotPlus:=false;
                GotStar:=false;
              end;

            AS_OFFSET :
              begin
                Consume(AS_OFFSET);
                GotOffset:=true;
              end;

            AS_TYPE,
            AS_NOT,
            AS_STRING,
            AS_INTNUM,
            AS_LPAREN : { Constant reference expression }
              begin
                if not GotPlus and not GotStar then
                  Message(asmr_e_invalid_reference_syntax);
                BuildConstSymbolExpression(true,true,l,tempstr);

                if tempstr<>'' then
                 begin
                   if GotStar then
                    Message(asmr_e_only_add_relocatable_symbol);
                   if not assigned(oper.opr.ref.symbol) then
                    oper.opr.ref.symbol:=objectlibrary.newasmsymbol(tempstr,AB_EXTERNAL,AT_FUNCTION)
                   else
                    Message(asmr_e_cant_have_multiple_relocatable_symbols);
                 end;
                case oper.opr.typ of
                  OPR_REFERENCE :
                    begin
                      if GotStar then
                       oper.opr.ref.scalefactor:=l
                      else if (prevasmtoken = AS_STAR) then
                       begin
                         if scale<>0 then
                           scale:=l*scale
                         else
                           scale:=l;
                       end
                      else
                       begin
                         if negative then
                           Dec(oper.opr.ref.offset,l)
                         else
                           Inc(oper.opr.ref.offset,l);
                       end;
                    end;
                  OPR_LOCAL :
                    begin
                      if GotStar then
                       oper.opr.localscale:=l
                      else if (prevasmtoken = AS_STAR) then
                       begin
                         if scale<>0 then
                           scale:=l*scale
                         else
                           scale:=l;
                       end
                      else
                       begin
                         if negative then
                           Dec(oper.opr.localsymofs,l)
                         else
                           Inc(oper.opr.localsymofs,l);
                       end;
                    end;
                end;
                GotPlus:=(prevasmtoken=AS_PLUS) or
                         (prevasmtoken=AS_MINUS);
                if GotPlus then
                  negative := prevasmtoken = AS_MINUS;
                GotStar:=(prevasmtoken=AS_STAR);
              end;

            AS_RBRACKET :
              begin
                if GotPlus or GotStar then
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


    Procedure ti386intreader.BuildConstantOperand(oper: tx86operand);
      var
        l : longint;
        tempstr : string;
      begin
        if not (oper.opr.typ in [OPR_NONE,OPR_CONSTANT]) then
          Message(asmr_e_invalid_operand_type);
        BuildConstSymbolExpression(true,false,l,tempstr);
        if tempstr<>'' then
         begin
           oper.opr.typ:=OPR_SYMBOL;
           oper.opr.symofs:=l;
           oper.opr.symbol:=objectlibrary.newasmsymbol(tempstr,AB_EXTERNAL,AT_FUNCTION);
         end
        else
         begin
           if oper.opr.typ=OPR_NONE then
             begin
               oper.opr.typ:=OPR_CONSTANT;
               oper.opr.val:=l;
             end
           else
             inc(oper.opr.val,l);
         end;
      end;


    Procedure ti386intreader.BuildOperand(oper: tx86operand);

        procedure AddLabelOperand(hl:tasmlabel);
        begin
          if is_calljmp(actopcode) then
           begin
             oper.opr.typ:=OPR_SYMBOL;
             oper.opr.symbol:=hl;
           end
          else
           begin
             oper.InitRef;
             oper.opr.ref.symbol:=hl;
           end;
        end;

      var
        expr    : string;
        tempreg : tregister;
        typesize,
        l       : longint;
        hl      : tasmlabel;
        toffset,
        tsize   : longint;
      Begin
        expr:='';
        repeat
          if actasmtoken=AS_DOT then
            begin
              if expr<>'' then
                begin
                  BuildRecordOffsetSize(expr,toffset,tsize);
                  oper.SetSize(tsize,true);
                  case oper.opr.typ of
                    OPR_LOCAL :
                      begin
                        { don't allow direct access to fields of parameters, becuase that
                          will generate buggy code. Allow it only for explicit typecasting
                          and when the parameter is in a register (delphi compatible) }
                        if (not oper.hastype) and
                           (tvarsym(oper.opr.localsym).owner.symtabletype=parasymtable) and
                           (current_procinfo.procdef.proccalloption<>pocall_register) then
                          Message(asmr_e_cannot_access_field_directly_for_parameters);
                        inc(oper.opr.localsymofs,toffset)
                      end;
                    OPR_CONSTANT :
                      inc(oper.opr.val,toffset);
                    OPR_REFERENCE :
                      inc(oper.opr.ref.offset,toffset);
                    OPR_NONE :
                      begin
                        oper.opr.typ:=OPR_CONSTANT;
                        oper.opr.val:=toffset;
                      end;
                    else
                      internalerror(200309222);
                  end;
                  expr:='';
                end
              else
                begin
                  { See it as a separator }
                  Consume(AS_DOT);
                end;
           end;

          case actasmtoken of

            AS_OFFSET,
            AS_TYPE,
            AS_NOT,
            AS_STRING :
              Begin
                BuildConstantOperand(oper);
              end;

            AS_PLUS,
            AS_MINUS,
            AS_LPAREN,
            AS_INTNUM :
              begin
                case oper.opr.typ of
                  OPR_REFERENCE :
                    inc(oper.opr.ref.offset,BuildRefConstExpression);
                  OPR_LOCAL :
                    inc(oper.opr.localsymofs,BuildConstExpression);
                  OPR_NONE,
                  OPR_CONSTANT :
                    BuildConstantOperand(oper);
                  else
                    Message(asmr_e_invalid_operand_type);
                end;
              end;

            AS_ID : { A constant expression, or a Variable ref. }
              Begin
                { Label or Special symbol reference? }
                if actasmpattern[1] = '@' then
                 Begin
                   if actasmpattern = '@RESULT' then
                    Begin
                      oper.SetupResult;
                      Consume(AS_ID);
                    end
                   else
                    if (actasmpattern = '@CODE') or (actasmpattern = '@DATA') then
                     begin
                       Message(asmr_w_CODE_and_DATA_not_supported);
                       Consume(AS_ID);
                     end
                   else
                    { Local Label }
                    begin
                      CreateLocalLabel(actasmpattern,hl,false);
                      Consume(AS_ID);
                      AddLabelOperand(hl);
                      if not (actasmtoken in [AS_END,AS_SEPARATOR,AS_COMMA]) then
                       Message(asmr_e_syntax_error);
                    end;
                 end
                else
                { support result for delphi modes }
                 if (m_objpas in aktmodeswitches) and (actasmpattern='RESULT') then
                  begin
                    oper.SetUpResult;
                    Consume(AS_ID);
                  end
                { probably a variable or normal expression }
                { or a procedure (such as in CALL ID)      }
                else
                 Begin
                   { is it a constant ? }
                   if SearchIConstant(actasmpattern,l) then
                    Begin
                      case oper.opr.typ of
                        OPR_REFERENCE :
                          inc(oper.opr.ref.offset,BuildRefConstExpression);
                        OPR_LOCAL :
                          inc(oper.opr.localsymofs,BuildRefConstExpression);
                        OPR_NONE,
                        OPR_CONSTANT :
                          BuildConstantOperand(oper);
                        else
                          Message(asmr_e_invalid_operand_type);
                      end;
                    end
                   else
                    { Check for pascal label }
                    if SearchLabel(actasmpattern,hl,false) then
                     begin
                       Consume(AS_ID);
                       AddLabelOperand(hl);
                       if not (actasmtoken in [AS_END,AS_SEPARATOR,AS_COMMA]) then
                        Message(asmr_e_syntax_error);
                     end
                    else
                    { is it a normal variable ? }
                     Begin
                       expr:=actasmpattern;
                       Consume(AS_ID);
                       { typecasting? }
                       if SearchType(expr,typesize) then
                        begin
                          oper.hastype:=true;
                          if (actasmtoken=AS_LPAREN) then
                            begin
                              Consume(AS_LPAREN);
                              BuildOperand(oper);
                              Consume(AS_RPAREN);
                              if oper.opr.typ in [OPR_REFERENCE,OPR_LOCAL] then
                                oper.SetSize(typesize,true);
                            end;
                        end
                       else
                        begin
                          if not oper.SetupVar(expr,false) then
                            Begin
                              { not a variable, check special variables.. }
                              if expr = 'SELF' then
                                oper.SetupSelf
                              else
                                Message1(sym_e_unknown_id,expr);
                              expr:='';
                            end;
                         end;
                     end;
                 end;
              end;

            AS_REGISTER : { Register, a variable reference or a constant reference }
              begin
                { save the type of register used. }
                tempreg:=actasmregister;
                Consume(AS_REGISTER);
                if actasmtoken = AS_COLON then
                 Begin
                   Consume(AS_COLON);
                   oper.InitRef;
                   oper.opr.ref.segment:=tempreg;
                   BuildReference(oper);
                 end
                else
                { Simple register }
                 begin
                   if not (oper.opr.typ in [OPR_NONE,OPR_REGISTER]) then
                     Message(asmr_e_invalid_operand_type);
                   oper.opr.typ:=OPR_REGISTER;
                   oper.opr.reg:=tempreg;
                   oper.SetSize(tcgsize2size[cg.reg_cgsize(oper.opr.reg)],true);
                 end;
              end;

            AS_LBRACKET: { a variable reference, register ref. or a constant reference }
              Begin
                BuildReference(oper);
              end;

            AS_SEG :
              Begin
                Message(asmr_e_seg_not_supported);
                Consume(actasmtoken);
              end;

            AS_SEPARATOR,
            AS_END,
            AS_COMMA:
              break;

            else
              Message(asmr_e_syn_operand);
          end;
        until not(actasmtoken in [AS_DOT,AS_PLUS,AS_LBRACKET]);
        if not((actasmtoken in [AS_END,AS_SEPARATOR,AS_COMMA]) or
               (oper.hastype and (actasmtoken=AS_RPAREN))) then
         begin
           Message(asmr_e_syntax_error);
           RecoverConsume(true);
         end;
      end;


    Procedure ti386intreader.BuildOpCode(instr : tx86instruction);
      var
        PrefixOp,OverrideOp: tasmop;
        size,
        operandnum : longint;
      Begin
        PrefixOp:=A_None;
        OverrideOp:=A_None;
        { prefix seg opcode / prefix opcode }
        repeat
          if is_prefix(actopcode) then
           begin
             with instr do
               begin
                 OpOrder:=op_intel;
                 PrefixOp:=ActOpcode;
                 opcode:=ActOpcode;
                 condition:=ActCondition;
                 opsize:=ActOpsize;
                 ConcatInstruction(curlist);
               end;
             Consume(AS_OPCODE);
           end
          else
           if is_override(actopcode) then
            begin
              with instr do
                begin
                  OpOrder:=op_intel;
                  OverrideOp:=ActOpcode;
                  opcode:=ActOpcode;
                  condition:=ActCondition;
                  opsize:=ActOpsize;
                  ConcatInstruction(curlist);
                end;
              Consume(AS_OPCODE);
            end
          else
           break;
          { allow for newline after prefix or override }
          while actasmtoken=AS_SEPARATOR do
            Consume(AS_SEPARATOR);
        until (actasmtoken<>AS_OPCODE);
        { opcode }
        if (actasmtoken <> AS_OPCODE) then
         Begin
           Message(asmr_e_invalid_or_missing_opcode);
           RecoverConsume(false);
           exit;
         end;
        { Fill the instr object with the current state }
        with instr do
          begin
            OpOrder:=op_intel;
            Opcode:=ActOpcode;
            condition:=ActCondition;
            opsize:=ActOpsize;

            { Valid combination of prefix/override and instruction ?  }
            if (prefixop<>A_NONE) and (NOT CheckPrefix(PrefixOp,actopcode)) then
              Message1(asmr_e_invalid_prefix_and_opcode,actasmpattern);
            if (overrideop<>A_NONE) and (NOT CheckOverride(OverrideOp,ActOpcode)) then
              Message1(asmr_e_invalid_override_and_opcode,actasmpattern);
          end;
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
                if operandnum > Max_Operands then
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
            AS_DQWORD,
            AS_QWORD :
              Begin
                { load the size in a temp variable, so it can be set when the
                  operand is read }
                size:=0;
                Case actasmtoken of
                  AS_DWORD : size:=4;
                  AS_WORD  : size:=2;
                  AS_BYTE  : size:=1;
                  AS_QWORD : size:=8;
                  AS_DQWORD : size:=16;
                  AS_TBYTE : size:=extended_size;
                end;
                Consume(actasmtoken);
                if actasmtoken=AS_PTR then
                 begin
                   Consume(AS_PTR);
                   instr.Operands[operandnum].InitRef;
                 end;
                BuildOperand(instr.Operands[operandnum] as tx86operand);
                { now set the size which was specified by the override }
                instr.Operands[operandnum].setsize(size,true);
              end;

            { Type specifier }
            AS_NEAR,
            AS_FAR :
              Begin
                if actasmtoken = AS_NEAR then
                  begin
                    Message(asmr_w_near_ignored);
                    instr.opsize:=S_NEAR;
                  end
                else
                  begin
                    Message(asmr_w_far_ignored);
                    instr.opsize:=S_FAR;
                  end;
                Consume(actasmtoken);
                if actasmtoken=AS_PTR then
                 begin
                   Consume(AS_PTR);
                   instr.Operands[operandnum].InitRef;
                 end;
                BuildOperand(instr.Operands[operandnum] as tx86operand);
              end;
            else
              BuildOperand(instr.Operands[operandnum] as tx86operand);
          end; { end case }
        until false;
        instr.Ops:=operandnum;
      end;


    Procedure ti386intreader.BuildConstant(maxvalue: longint);
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
                  if maxvalue = longint($ffffffff) then
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
                  AS_END,
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
                BuildConstSymbolExpression(false,false,value,asmsym);
                if asmsym<>'' then
                 begin
                   if maxvalue<>longint($ffffffff) then
                     Message1(asmr_w_const32bit_for_address,asmsym);
                   ConcatConstSymbol(curlist,asmsym,value)
                 end
                else
                 ConcatConstant(curlist,value,maxvalue);
              end;
            AS_COMMA:
              Consume(AS_COMMA);
            AS_END,
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


  function ti386intreader.Assemble: tlinkedlist;
    Var
      hl : tasmlabel;
      instr : Tx86Instruction;
    Begin
      Message1(asmr_d_start_reading,'intel');
      inexpression:=FALSE;
      firsttoken:=TRUE;
     { sets up all opcode and register tables in uppercase
       done in the construtor now
      if not _asmsorted then
       Begin
         SetupTables;
         _asmsorted:=TRUE;
       end;
      }
      curlist:=TAAsmoutput.Create;
      { setup label linked list }
      LocalLabelList:=TLocalLabelList.Create;
      { start tokenizer }
      c:=current_scanner.asmgetcharstart;
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
              BuildConstant(longint($ffffffff));
              inexpression:=false;
            end;

          AS_OPCODE :
            Begin
              instr:=Tx86Instruction.Create(Tx86Operand);
              BuildOpcode(instr);
              with instr do
                begin
                  { We need AT&T style operands }
                  Swapoperands;
                  { Must be done with args in ATT order }
                  CheckNonCommutativeOpcodes;
                  AddReferenceSizes;
                  SetInstructionOpsize;
                  CheckOperandSizes;
                  ConcatInstruction(curlist);
                end;
              instr.Free;
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
              RecoverConsume(false);
            end;
        end; { end case }
      until false;
      { Check LocalLabelList }
      LocalLabelList.CheckEmitted;
      LocalLabelList.Free;
      { Return the list in an asmnode }
      assemble:=curlist;
      Message1(asmr_d_finish_reading,'intel');
    end;


{*****************************************************************************
                               Initialize
*****************************************************************************}

const
  asmmode_i386_intel_info : tasmmodeinfo =
          (
            id    : asmmode_i386_intel;
            idtxt : 'INTEL';
            casmreader : ti386intreader;
          );

begin
  RegisterAsmMode(asmmode_i386_intel_info);
end.
{
  $Log$
  Revision 1.71  2004-03-02 17:32:12  florian
    * make cycle fixed
    + pic support for darwin
    + support of importing vars from shared libs on darwin implemented

  Revision 1.70  2004/03/02 00:36:33  olle
    * big transformation of Tai_[const_]Symbol.Create[data]name*

  Revision 1.69  2004/01/14 23:39:05  florian
    * another bunch of x86-64 fixes mainly calling convention and
      assembler reader related

  Revision 1.68  2003/11/29 20:13:25  florian
    * fixed several pi_do_call problems

  Revision 1.67  2003/11/29 15:53:06  florian
    + nasmelf mode for BeOS
    + DQWORD directive in intel assembler mode

  Revision 1.66  2003/11/29 14:41:02  peter
    * support type()

  Revision 1.65  2003/11/12 16:05:39  florian
    * assembler readers OOPed
    + typed currency constants
    + typed 128 bit float constants if the CPU supports it

  Revision 1.64  2003/11/10 19:08:32  peter
    * line numbering is now only done when #10, #10#13 is really parsed
      instead of when it is the next character

  Revision 1.63  2003/10/30 19:59:00  peter
    * support scalefactor for opr_local
    * support reference with opr_local set, fixes tw2631

  Revision 1.62  2003/10/29 16:47:18  peter
    * fix field offset in reference

  Revision 1.61  2003/10/29 15:40:20  peter
    * support indexing and offset retrieval for locals

  Revision 1.60  2003/10/27 15:29:43  peter
    * fixed trec.field to return constant

  Revision 1.59  2003/10/24 17:39:03  peter
    * more intel parser updates

  Revision 1.58  2003/10/23 17:19:44  peter
    * typecasting fixes
    * reference building more delphi compatible

  Revision 1.57  2003/10/21 18:17:40  peter
    * ignore @ in Unit.@Proc

  Revision 1.56  2003/10/10 17:48:14  peter
    * old trgobj moved to x86/rgcpu and renamed to trgx86fpu
    * tregisteralloctor renamed to trgobj
    * removed rgobj from a lot of units
    * moved location_* and reference_* to cgobj
    * first things for mmx register allocation

  Revision 1.55  2003/10/07 18:21:18  peter
    * fix crash
    * allow parameter subscription for register parameters

  Revision 1.54  2003/10/02 21:17:38  peter
    * fix operand order when a prefix opcode is supplied

  Revision 1.53  2003/10/01 20:34:49  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.52  2003/09/23 20:37:53  peter
    * fix global var+offset

  Revision 1.51  2003/09/23 17:56:06  peter
    * locals and paras are allocated in the code generation
    * tvarsym.localloc contains the location of para/local when
      generating code for the current procedure

  Revision 1.50  2003/09/03 15:55:01  peter
    * NEWRA branch merged

  Revision 1.49.2.2  2003/08/31 15:46:26  peter
    * more updates for tregister

  Revision 1.49.2.1  2003/08/28 18:35:08  peter
    * tregister changed to cardinal

  Revision 1.49  2003/06/06 14:41:59  peter
    * use setsize for size specifier

  Revision 1.48  2003/05/30 23:57:08  peter
    * more sparc cleanup
    * accumulator removed, splitted in function_return_reg (called) and
      function_result_reg (caller)

  Revision 1.47  2003/04/30 15:45:35  florian
    * merged more x86-64/i386 code

  Revision 1.46  2003/04/27 11:21:35  peter
    * aktprocdef renamed to current_procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.45  2003/04/21 20:05:10  peter
    * removed some ie checks

  Revision 1.44  2003/03/28 19:16:57  peter
    * generic constructor working for i386
    * remove fixed self register
    * esi added as address register for i386

  Revision 1.43  2003/03/18 18:15:53  peter
    * changed reg2opsize to function

  Revision 1.42  2003/03/17 21:32:52  peter
    * allow character constants in reference declaration

  Revision 1.41  2003/02/26 22:57:44  daniel
    * Changed no longer correct fillchar of reference into location_reset

  Revision 1.40  2003/02/19 22:00:16  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.39  2003/01/08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.38  2002/12/14 15:02:03  carl
    * maxoperands -> max_operands (for portability in rautils.pas)
    * fix some range-check errors with loadconst
    + add ncgadd unit to m68k
    * some bugfix of a_param_reg with LOC_CREFERENCE

  Revision 1.37  2002/12/01 22:08:34  carl
    * some small cleanup (remove some specific operators which are not supported)

  Revision 1.36  2002/11/15 01:58:59  peter
    * merged changes from 1.0.7 up to 04-11
      - -V option for generating bug report tracing
      - more tracing for option parsing
      - errors for cdecl and high()
      - win32 import stabs
      - win32 records<=8 are returned in eax:edx (turned off by default)
      - heaptrc update
      - more info for temp management in .s file with EXTDEBUG

  Revision 1.35  2002/09/16 19:07:00  peter
    * support [eax].constant as reference

  Revision 1.34  2002/09/03 16:26:28  daniel
    * Make Tprocdef.defs protected

  Revision 1.33  2002/08/17 09:23:47  florian
    * first part of procinfo rewrite

  Revision 1.32  2002/08/13 18:01:52  carl
    * rename swatoperands to swapoperands
    + m68k first compilable version (still needs a lot of testing):
        assembler generator, system information , inline
        assembler reader.

  Revision 1.31  2002/08/11 14:32:31  peter
    * renamed current_library to objectlibrary

  Revision 1.30  2002/08/11 13:24:17  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.29  2002/07/01 18:46:34  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.28  2002/05/18 13:34:26  peter
    * readded missing revisions

  Revision 1.27  2002/05/16 19:46:52  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.25  2002/04/20 21:37:07  carl
  + generic FPC_CHECKPOINTER
  + first parameter offset in stack now portable
  * rename some constants
  + move some cpu stuff to other units
  - remove unused constents
  * fix stacksize for some targets
  * fix generic size problems which depend now on EXTEND_SIZE constant
  * removing frame pointer in routines is only available for : i386,m68k and vis targets

  Revision 1.24  2002/04/15 19:44:22  peter
    * fixed stackcheck that would be called recursively when a stack
      error was found
    * generic changeregsize(reg,size) for i386 register resizing
    * removed some more routines from cga unit
    * fixed returnvalue handling
    * fixed default stacksize of linux and go32v2, 8kb was a bit small :-)

  Revision 1.23  2002/04/15 19:12:09  carl
  + target_info.size_of_pointer -> pointer_size
  + some cleanup of unused types/variables
  * move several constants from cpubase to their specific units
    (where they are used)
  + att_Reg2str -> gas_reg2str
  + int_reg2str -> std_reg2str

  Revision 1.22  2002/04/04 19:06:13  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.21  2002/04/02 17:11:39  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.20  2002/01/24 18:25:53  peter
   * implicit result variable generation for assembler routines
   * removed m_tp modeswitch, use m_tp7 or not(m_fpc) instead

}
