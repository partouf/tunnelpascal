{
    $Id$
    Copyright (c) 1998 by Florian Klaempfl

    Does parsing of expression for Free Pascal

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
unit pexpr;

  interface

    uses symtable,tree;

    { reads a whole expression }
    function expr : ptree;

    { reads an expression without assignements and .. }
    function comp_expr(accept_equal : boolean):Ptree;

    { reads a single factor }
    function factor(getaddr : boolean) : ptree;

    { the ID token has to be consumed before calling this function }
    procedure do_member_read(getaddr : boolean;const sym : psym;var p1 : ptree;
      var pd : pdef;var again : boolean);

    function get_intconst:longint;

    function get_stringconst:string;

  implementation

    uses
       globtype,systems,tokens,
       cobjects,globals,scanner,aasm,pass_1,
       hcodegen,types,verbose,strings
       { parser specific stuff }
       ,pbase,pdecl
       { processor specific stuff }
{$ifdef i386}
{$ifdef ag386bin}
       ,i386base
{$else}
       ,i386
{$endif}
{$endif}
{$ifdef m68k}
       ,m68k
{$endif}
       ;

    const allow_type : boolean = true;

    function parse_paras(_colon,in_prop_paras : boolean) : ptree;

      var
         p1,p2 : ptree;
         end_of_paras : ttoken;

      begin
         if in_prop_paras  then
           end_of_paras:=RECKKLAMMER
         else
           end_of_paras:=RKLAMMER;
         if token=end_of_paras then
           begin
              parse_paras:=nil;
              exit;
           end;
         p2:=nil;
         inc(parsing_para_level);
         while true do
           begin
              p1:=comp_expr(true);
              p2:=gencallparanode(p1,p2);

              { it's for the str(l:5,s); }
              if _colon and (token=COLON) then
                begin
                   consume(COLON);
                   p1:=comp_expr(true);
                   p2:=gencallparanode(p1,p2);
                   p2^.is_colon_para:=true;
                   if token=COLON then
                     begin
                        consume(COLON);
                        p1:=comp_expr(true);
                        p2:=gencallparanode(p1,p2);
                        p2^.is_colon_para:=true;
                     end
                end;
              if token=COMMA then
                consume(COMMA)
              else
                break;
           end;
         dec(parsing_para_level);
         parse_paras:=p2;
      end;


    function statement_syssym(l : longint;var pd : pdef) : ptree;
      var
        p1,p2,paras  : ptree;
        prev_in_args : boolean;
        Store_valid  : boolean;
      begin
        prev_in_args:=in_args;
        Store_valid:=Must_be_valid;
        case l of
          in_ord_x :
            begin
              consume(LKLAMMER);
              in_args:=true;
              Must_be_valid:=true;
              p1:=comp_expr(true);
              consume(RKLAMMER);
              do_firstpass(p1);
              p1:=geninlinenode(in_ord_x,false,p1);
              do_firstpass(p1);
              statement_syssym := p1;
              pd:=p1^.resulttype;
            end;

          in_break :
            begin
              statement_syssym:=genzeronode(breakn);
              pd:=voiddef;
            end;

          in_continue :
            begin
              statement_syssym:=genzeronode(continuen);
              pd:=voiddef;
            end;

          in_typeof_x :
            begin
              consume(LKLAMMER);
              in_args:=true;
              {allow_type:=true;}
              p1:=comp_expr(true);
              {allow_type:=false;}
              consume(RKLAMMER);
              pd:=voidpointerdef;
              if p1^.treetype=typen then
               begin
                 if (p1^.typenodetype=nil) then
                  begin
                    Message(type_e_mismatch);
                    statement_syssym:=genzeronode(errorn);
                  end
                 else
                  if p1^.typenodetype^.deftype=objectdef then
                   begin
                      { we can use resulttype in pass_2 (PM) }
                      p1^.resulttype:=p1^.typenodetype;
                      statement_syssym:=geninlinenode(in_typeof_x,false,p1);
                   end
                 else
                  begin
                    Message(type_e_mismatch);
                    disposetree(p1);
                    statement_syssym:=genzeronode(errorn);
                  end;
               end
              else { not a type node }
               begin
                 Must_be_valid:=false;
                 do_firstpass(p1);
                 if (p1^.resulttype=nil) then
                  begin
                    Message(type_e_mismatch);
                    disposetree(p1);
                    statement_syssym:=genzeronode(errorn)
                  end
                 else
                  if p1^.resulttype^.deftype=objectdef then
                   statement_syssym:=geninlinenode(in_typeof_x,false,p1)
                 else
                  begin
                    Message(type_e_mismatch);
                    statement_syssym:=genzeronode(errorn);
                    disposetree(p1);
                  end;
               end;
            end;

          in_sizeof_x :
            begin
              consume(LKLAMMER);
              in_args:=true;
              {allow_type:=true;}
              p1:=comp_expr(true);
              {allow_type:=false; }
              consume(RKLAMMER);
              pd:=s32bitdef;
              if p1^.treetype=typen then
               begin
                 statement_syssym:=genordinalconstnode(p1^.typenodetype^.size,pd);
                 { p1 not needed !}
                 disposetree(p1);
               end
              else
               begin
                 Must_be_valid:=false;
                 do_firstpass(p1);
                 if ((p1^.resulttype^.deftype=objectdef) and
                     ((pobjectdef(p1^.resulttype)^.options and oo_hasconstructor)<>0)) or
                    is_open_array(p1^.resulttype) or
                    is_open_string(p1^.resulttype) then
                  statement_syssym:=geninlinenode(in_sizeof_x,false,p1)
                 else
                  begin
                    statement_syssym:=genordinalconstnode(p1^.resulttype^.size,pd);
                    { p1 not needed !}
                    disposetree(p1);
                  end;
               end;
            end;

          in_assigned_x :
            begin
              consume(LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              Must_be_valid:=true;
              do_firstpass(p1);
              case p1^.resulttype^.deftype of
           pointerdef,
           procvardef,
          classrefdef : ;
            objectdef : if not(pobjectdef(p1^.resulttype)^.isclass) then
                         Message(parser_e_illegal_parameter_list);
              else
                Message(parser_e_illegal_parameter_list);
              end;
              p2:=gencallparanode(p1,nil);
              p2:=geninlinenode(in_assigned_x,false,p2);
              consume(RKLAMMER);
              pd:=booldef;
              statement_syssym:=p2;
            end;

          in_ofs_x :
            begin
              consume(LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              p1:=gensinglenode(addrn,p1);
              Must_be_valid:=false;
              do_firstpass(p1);
              { Ofs() returns a longint, not a pointer }
              p1^.resulttype:=u32bitdef;
              pd:=p1^.resulttype;
              consume(RKLAMMER);
              statement_syssym:=p1;
            end;

          in_seg_x :
            begin
              consume(LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              do_firstpass(p1);
              if p1^.location.loc<>LOC_REFERENCE then
                Message(cg_e_illegal_expression);
              p1:=genordinalconstnode(0,s32bitdef);
              Must_be_valid:=false;
              pd:=s32bitdef;
              consume(RKLAMMER);
              statement_syssym:=p1;
            end;

          in_high_x,
          in_low_x :
            begin
              consume(LKLAMMER);
              in_args:=true;
              {allow_type:=true;}
              p1:=comp_expr(true);
              {allow_type:=false;}
              do_firstpass(p1);
              if p1^.treetype=typen then
                p1^.resulttype:=p1^.typenodetype;
              Must_be_valid:=false;
              p2:=geninlinenode(l,false,p1);
              consume(RKLAMMER);
              pd:=s32bitdef;
              statement_syssym:=p2;
            end;

          in_succ_x,
          in_pred_x :
            begin
              consume(LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              do_firstpass(p1);
              Must_be_valid:=false;
              p2:=geninlinenode(l,false,p1);
              consume(RKLAMMER);
              pd:=p1^.resulttype;
              statement_syssym:=p2;
            end;

          in_inc_x,
          in_dec_x :
            begin
              consume(LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              Must_be_valid:=false;
              if token=COMMA then
               begin
                 consume(COMMA);
                 p2:=gencallparanode(comp_expr(true),nil);
               end
              else
               p2:=nil;
              p2:=gencallparanode(p1,p2);
              statement_syssym:=geninlinenode(l,false,p2);
              consume(RKLAMMER);
              pd:=voiddef;
            end;

          in_concat_x :
            begin
              consume(LKLAMMER);
              in_args:=true;
              p2:=nil;
              while true do
               begin
                 p1:=comp_expr(true);
                 Must_be_valid:=true;
                 do_firstpass(p1);
                 if not((p1^.resulttype^.deftype=stringdef) or
                        ((p1^.resulttype^.deftype=orddef) and
                         (porddef(p1^.resulttype)^.typ=uchar))) then
                   Message(parser_e_illegal_parameter_list);
                 if p2<>nil then
                  p2:=gennode(addn,p2,p1)
                 else
                  p2:=p1;
                 if token=COMMA then
                  consume(COMMA)
                 else
                  break;
               end;
              consume(RKLAMMER);
              pd:=cshortstringdef;
              statement_syssym:=p2;
            end;

          in_read_x,
          in_readln_x :
            begin
              if token=LKLAMMER then
               begin
                 consume(LKLAMMER);
                 in_args:=true;
                 Must_be_valid:=false;
                 paras:=parse_paras(false,false);
                 consume(RKLAMMER);
               end
              else
               paras:=nil;
              pd:=voiddef;
              p1:=geninlinenode(l,false,paras);
              do_firstpass(p1);
              statement_syssym := p1;
            end;

          in_write_x,
          in_writeln_x :
            begin
              if token=LKLAMMER then
               begin
                 consume(LKLAMMER);
                 in_args:=true;
                 Must_be_valid:=true;
                 paras:=parse_paras(true,false);
                 consume(RKLAMMER);
               end
              else
               paras:=nil;
              pd:=voiddef;
              p1 := geninlinenode(l,false,paras);
              do_firstpass(p1);
              statement_syssym := p1;
            end;

          in_str_x_string :
            begin
              consume(LKLAMMER);
              in_args:=true;
              paras:=parse_paras(true,false);
              consume(RKLAMMER);
              p1 := geninlinenode(l,false,paras);
              do_firstpass(p1);
              statement_syssym := p1;
              pd:=voiddef;
            end;

{$IfDef ValIntern}
          in_val_x:
            Begin
              consume(LKLAMMER);
              in_args := true;
              p1:= gencallparanode(comp_expr(true), nil);
              Must_be_valid := False;
              consume(COMMA);
              p2 := gencallparanode(comp_expr(true),p1);
              if (token = COMMA) then
                Begin
                  consume(COMMA);
                  p2 := gencallparanode(comp_expr(true),p2)
                End;
              consume(RKLAMMER);
              p2 := geninlinenode(l,false,p2);
              do_firstpass(p2);
              statement_syssym := p2;
              pd := voiddef;
            End;
{$EndIf ValIntern}


          in_include_x_y,
          in_exclude_x_y :
            begin
              consume(LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              Must_be_valid:=false;
              consume(COMMA);
              p2:=comp_expr(true);
              statement_syssym:=geninlinenode(l,false,gencallparanode(p1,gencallparanode(p2,nil)));
              consume(RKLAMMER);
              pd:=voiddef;
            end;

          in_assert_x_y :
            begin
              consume(LKLAMMER);
              in_args:=true;
              p1:=comp_expr(true);
              if token=COMMA then
               begin
                 consume(COMMA);
                 p2:=comp_expr(true);
               end
              else
               begin
                 { then insert an empty string }
                 p2:=genstringconstnode('');
               end;
              statement_syssym:=geninlinenode(l,false,gencallparanode(p1,gencallparanode(p2,nil)));
              consume(RKLAMMER);
              pd:=voiddef;
            end;

          else
            internalerror(15);

        end;
        in_args:=prev_in_args;
        Must_be_valid:=Store_valid;
      end;


    { reads the parameter for a subroutine call }
    procedure do_proc_call(getaddr : boolean;var again : boolean;var p1:Ptree;var pd:Pdef);
      var
         prev_in_args : boolean;
         prevafterassn : boolean;
      begin
         prev_in_args:=in_args;
         prevafterassn:=afterassignment;
         afterassignment:=false;
         { want we only determine the address of }
         { a subroutine ?                        }
         if not(getaddr) then
           begin
              if token=LKLAMMER then
                begin
                   consume(LKLAMMER);
                   in_args:=true;
                   p1^.left:=parse_paras(false,false);
                   consume(RKLAMMER);
                end
              else p1^.left:=nil;

              { do firstpass because we need the  }
              { result type                       }
              do_firstpass(p1);
           end
         else
           begin
              { address operator @: }
              p1^.left:=nil;
              { forget pd }
              pd:=nil;
              { no postfix operators }
              again:=false;
           end;
         pd:=p1^.resulttype;
         in_args:=prev_in_args;
         afterassignment:=prevafterassn;
      end;


    { the following procedure handles the access to a property symbol }
    procedure handle_propertysym(sym : psym;st : psymtable;var p1 : ptree;
      var pd : pdef);

      var
         paras : ptree;
         p2 : ptree;

      begin
         paras:=nil;
         { property parameters? }
         if token=LECKKLAMMER then
           begin
              consume(LECKKLAMMER);
              paras:=parse_paras(false,true);
              consume(RECKKLAMMER);
           end;
         { indexed property }
         if (ppropertysym(sym)^.options and ppo_indexed)<>0 then
           begin
              p2:=genordinalconstnode(ppropertysym(sym)^.index,s32bitdef);
              paras:=gencallparanode(p2,paras);
           end;
         if not(afterassignment) and not(in_args) then
           begin
              { write property: }
              { no result }
              pd:=voiddef;
              if assigned(ppropertysym(sym)^.writeaccesssym) then
                begin
                   if ppropertysym(sym)^.writeaccesssym^.typ=procsym then
                     begin
                        { generate the method call }
                        p1:=genmethodcallnode(pprocsym(
                          ppropertysym(sym)^.writeaccesssym),st,p1);
                        { we know the procedure to call, so
                          force the usage of that procedure }
                        p1^.procdefinition:=pprocdef(ppropertysym(sym)^.writeaccessdef);
                        p1^.left:=paras;
                        consume(ASSIGNMENT);
                        { read the expression }
                        p2:=comp_expr(true);
                        p1^.left:=gencallparanode(p2,p1^.left);
                     end
                   else if ppropertysym(sym)^.writeaccesssym^.typ=varsym then
                     begin
                        if assigned(paras) then
                          message(parser_e_no_paras_allowed);
                        { subscribed access? }
                        if p1=nil then
                          begin
                             p1:=genloadnode(pvarsym(ppropertysym(sym)^.readaccesssym),st);
                          end
                        else
                          p1:=gensubscriptnode(pvarsym(
                            ppropertysym(sym)^.readaccesssym),p1);
                        consume(ASSIGNMENT);
                        { read the expression }
                        p2:=comp_expr(true);
                        p1:=gennode(assignn,p1,p2);
                     end
                   else
                     begin
                        p1:=genzeronode(errorn);
                        Message(parser_e_no_procedure_to_access_property);
                     end;
                end
              else
                begin
                   p1:=genzeronode(errorn);
                   Message(parser_e_no_procedure_to_access_property);
                end;
           end
         else
           begin
              { read property: }
              pd:=ppropertysym(sym)^.proptype;
              if assigned(ppropertysym(sym)^.readaccesssym) then
                begin
                   if ppropertysym(sym)^.readaccesssym^.typ=varsym then
                     begin
                        if assigned(paras) then
                          message(parser_e_no_paras_allowed);
                        { subscribed access? }
                        if p1=nil then
                          begin
                             p1:=genloadnode(pvarsym(
                              ppropertysym(sym)^.readaccesssym),st);
                          end
                        else
                          p1:=gensubscriptnode(pvarsym(
                            ppropertysym(sym)^.readaccesssym),p1);
                     end
                   else if ppropertysym(sym)^.readaccesssym^.typ=procsym then
                     begin
                        { generate the method call }
                        p1:=genmethodcallnode(pprocsym(
                          ppropertysym(sym)^.readaccesssym),st,p1);
                        { we know the procedure to call, so
                          force the usage of that procedure }
                        p1^.procdefinition:=pprocdef(ppropertysym(sym)^.readaccessdef);
                        { insert paras }
                        p1^.left:=paras;
                        { if we should be delphi compatible  }
                        { then force type conversion         }
                        {                                    }
                        { isn't neccessary, the result types }
                        { have to match excatly              }
                        {if cs_delphi2_compatible in aktswitches then
                          p1:=gentypeconvnode(p1,pd);
                        }
                     end
                   else
                     begin
                        p1:=genzeronode(errorn);
                        Message(type_e_mismatch);
                     end;
                end
              else
                begin
                   { error, no function to read property }
                   p1:=genzeronode(errorn);
                   Message(parser_e_no_procedure_to_access_property);
                end;
           end;
      end;


    { the ID token has to be consumed before calling this function }
    procedure do_member_read(getaddr : boolean;const sym : psym;var p1 : ptree;
      var pd : pdef;var again : boolean);

      var
         static_name : string;
         isclassref : boolean;

      begin
         if sym=nil then
           begin
              { pattern is still valid unless
              there is another ID just after the ID of sym }
              Message1(sym_e_id_no_member,pattern);
              disposetree(p1);
              p1:=genzeronode(errorn);
              { try to clean up }
              pd:=generrordef;
              again:=false;
           end
         else
           begin
              isclassref:=pd^.deftype=classrefdef;

              { check protected and private members        }
              { please leave this code as it is,           }
              { it has now the same behaviaor as TP/Delphi }
              if ((sym^.properties and sp_private)<>0) and
                (pobjectdef(pd)^.owner^.symtabletype=unitsymtable) then
               Message(parser_e_cant_access_private_member);

              if ((sym^.properties and sp_protected)<>0) and
                 (pobjectdef(pd)^.owner^.symtabletype=unitsymtable) then
                begin
                  if assigned(aktprocsym^.definition^._class) then
                    begin
                       if not aktprocsym^.definition^._class^.isrelated(
                          pobjectdef(sym^.owner^.defowner)) then
                         Message(parser_e_cant_access_protected_member);
                    end
                  else
                    Message(parser_e_cant_access_protected_member);
                end;

              { we assume, that only procsyms and varsyms are in an object }
              { symbol table, for classes, properties are allowed          }
              case sym^.typ of
                 procsym:
                   begin
                      p1:=genmethodcallnode(pprocsym(sym),srsymtable,p1);
                      do_proc_call(getaddr or
                        (getprocvar and
                        proc_to_procvar_equal(getprocvardef,pprocsym(sym)^.definition))
                        ,again,p1,pd);
                      { now we know the real method e.g. we can check for }
                      { a class method                                    }
                      if isclassref and ((p1^.procdefinition^.options and (poclassmethod or poconstructor))=0) then
                        Message(parser_e_only_class_methods_via_class_ref);
                   end;
                 varsym:
                   begin
                      if isclassref then
                        Message(parser_e_only_class_methods_via_class_ref);
                      if (sym^.properties and sp_static)<>0 then
                        begin
                           { static_name:=lower(srsymtable^.name^)+'_'+sym^.name;
                             this is wrong for static field in with symtable (PM) }
                           static_name:=lower(srsym^.owner^.name^)+'_'+sym^.name;
                           getsym(static_name,true);
                           disposetree(p1);
                           p1:=genloadnode(pvarsym(srsym),srsymtable);
                        end
                      else
                        p1:=gensubscriptnode(pvarsym(sym),p1);
                      pd:=pvarsym(sym)^.definition;
                   end;
                 propertysym:
                   begin
                      if isclassref then
                        Message(parser_e_only_class_methods_via_class_ref);
                      handle_propertysym(sym,srsymtable,p1,pd);
                   end;
                 else internalerror(16);
              end;
           end;
      end;


{****************************************************************************
                               Factor
****************************************************************************}

    function factor(getaddr : boolean) : ptree;
      var
         l        : longint;
         oldp1,
         p1,p2,p3 : ptree;
         code     : integer;
         pd,pd2   : pdef;
         possible_error,
         unit_specific,
         again    : boolean;
         sym      : pvarsym;
         classh   : pobjectdef;
         d        : bestreal;
         static_name : string;
         propsym  : ppropertysym;
         filepos  : tfileposinfo;

         {---------------------------------------------
                         Is_func_ret
         ---------------------------------------------}

        function is_func_ret(sym : psym) : boolean;
        var
           p : pprocinfo;
           storesymtablestack : psymtable;

        begin
          is_func_ret:=false;
          if (sym^.typ<>funcretsym) and ((procinfo.flags and pi_operator)=0) then
            exit;
          p:=@procinfo;
          while system.assigned(p) do
            begin
               { is this an access to a function result ? }
               if assigned(p^.funcretsym) and
                  ((pfuncretsym(sym)=p^.funcretsym) or
                   ((pvarsym(sym)=opsym) and
                    ((p^.flags and pi_operator)<>0))) and
                  (p^.retdef<>pdef(voiddef)) and
                  (token<>LKLAMMER) and
                  (not ((m_tp in aktmodeswitches) and
                  (afterassignment or in_args))) then
                 begin
                    p1:=genzeronode(funcretn);
                    pd:=p^.retdef;
                    p1^.funcretprocinfo:=p;
                    p1^.retdef:=pd;
                    is_func_ret:=true;
                    exit;
                 end;
               p:=p^.parent;
            end;
          { we must use the function call }
          if(sym^.typ=funcretsym) then
            begin
               storesymtablestack:=symtablestack;
               symtablestack:=srsymtable^.next;
               getsym(sym^.name,true);
               if srsym^.typ<>procsym then
                 Message(cg_e_illegal_expression);
               symtablestack:=storesymtablestack;
            end;
        end;

         {---------------------------------------------
                         Factor_read_id
         ---------------------------------------------}

       procedure factor_read_id;
         begin
           { allow post fix operators }
           again:=true;
           if (m_result in aktmodeswitches) and
              (pattern='RESULT') and
              assigned(aktprocsym) and
              (procinfo.retdef<>pdef(voiddef)) then
            begin
              consume(ID);
              p1:=genzeronode(funcretn);
              pd:=procinfo.retdef;
              p1^.funcretprocinfo:=pointer(@procinfo);
              p1^.retdef:=pd;
            end
           else
            begin
              if lastsymknown then
               begin
                 srsym:=lastsrsym;
                 srsymtable:=lastsrsymtable;
                 lastsymknown:=false;
               end
              else
               getsym(pattern,true);
              consume(ID);
               if not is_func_ret(srsym) then
              { else it's a normal symbol }
                begin
                { is it defined like UNIT.SYMBOL ? }
                  if srsym^.typ=unitsym then
                   begin
                     consume(POINT);
                     getsymonlyin(punitsym(srsym)^.unitsymtable,pattern);
                     unit_specific:=true;
                     consume(ID);
                   end
                  else
                   unit_specific:=false;
                  if not assigned(srsym) then
                   Begin
                     p1:=genzeronode(errorn);
                     { try to clean up }
                     pd:=generrordef;
                   end
                  else
                   Begin
                     { check semantics of private }
                     if (srsym^.typ in [propertysym,procsym,varsym]) and
                        (srsymtable^.symtabletype=objectsymtable) then
                      begin
                         if ((srsym^.properties and sp_private)<>0) and
                            (pobjectdef(srsym^.owner^.defowner)^.owner^.symtabletype=unitsymtable) then
                            Message(parser_e_cant_access_private_member);
                      end;
                     case srsym^.typ of
              absolutesym : begin
                              p1:=genloadnode(pvarsym(srsym),srsymtable);
                              pd:=pabsolutesym(srsym)^.definition;
                            end;
                   varsym : begin
                              { are we in a class method ? }
                              if (srsymtable^.symtabletype=objectsymtable) and
                                 assigned(aktprocsym) and
                                 ((aktprocsym^.definition^.options and poclassmethod)<>0) then
                                Message(parser_e_only_class_methods);
                              if (srsym^.properties and sp_static)<>0 then
                               begin
                                 static_name:=lower(srsym^.owner^.name^)+'_'+srsym^.name;
                                 getsym(static_name,true);
                               end;
                              p1:=genloadnode(pvarsym(srsym),srsymtable);
                              if pvarsym(srsym)^.is_valid=0 then
                               begin
                                 p1^.is_first := true;
                                 { set special between first loaded until checked in firstpass }
                                 pvarsym(srsym)^.is_valid:=2;
                               end;
                              pd:=pvarsym(srsym)^.definition;
                            end;
            typedconstsym : begin
                              p1:=gentypedconstloadnode(ptypedconstsym(srsym),srsymtable);
                              pd:=ptypedconstsym(srsym)^.definition;
                            end;
                   syssym : p1:=statement_syssym(psyssym(srsym)^.number,pd);
                  typesym : begin
                              pd:=ptypesym(srsym)^.definition;
                              if not assigned(pd) then
                               begin
                                 pd:=generrordef;
                                 again:=false;
                               end
                              else
                              { if we read a type declaration  }
                              { we have to return the type and }
                              { nothing else                   }
                               if block_type=bt_type then
                                begin
                                  p1:=gentypenode(pd);
                                  { here we can also set resulttype !! }
                                  p1^.resulttype:=pd;
                                  pd:=voiddef;
                                end
                              else { not type block }
                               begin
                                 if token=LKLAMMER then
                                  begin
                                    consume(LKLAMMER);
                                    p1:=comp_expr(true);
                                    consume(RKLAMMER);
                                    p1:=gentypeconvnode(p1,pd);
                                    p1^.explizit:=true;
                                  end
                                 else { not LKLAMMER}
                                  if (token=POINT) and
                                     (pd^.deftype=objectdef) and
                                     ((pobjectdef(pd)^.options and oo_is_class)=0) then
                                    begin
                                      consume(POINT);
                                      if assigned(procinfo._class) then
                                       begin
                                         if procinfo._class^.isrelated(pobjectdef(pd)) then
                                          begin
                                            p1:=gentypenode(pd);
                                            p1^.resulttype:=pd;
                                            srsymtable:=pobjectdef(pd)^.publicsyms;
                                            sym:=pvarsym(srsymtable^.search(pattern));
                                            { search also in inherited methods }
                                            while sym=nil do
                                             begin
                                               pd:=pobjectdef(pd)^.childof;
                                               srsymtable:=pobjectdef(pd)^.publicsyms;
                                               sym:=pvarsym(srsymtable^.search(pattern));
                                             end;
                                            consume(ID);
                                            do_member_read(false,sym,p1,pd,again);
                                          end
                                         else
                                          begin
                                            Message(parser_e_no_super_class);
                                            pd:=generrordef;
                                            again:=false;
                                          end;
                                       end
                                      else
                                       begin
                                         { allows @TObject.Load }
                                         { also allows static methods and variables }
                                         p1:=genzeronode(typen);
                                         p1^.resulttype:=pd;
                                         { srsymtable:=pobjectdef(pd)^.publicsyms;
                                           sym:=pvarsym(srsymtable^.search(pattern)); }

                                         { TP allows also @TMenu.Load if Load is only }
                                         { defined in an anchestor class              }
                                         sym:=pvarsym(search_class_member(pobjectdef(pd),pattern));
                                         if not assigned(sym) then
                                           Message1(sym_e_id_no_member,pattern)
                                         else if not(getaddr) and ((sym^.properties and sp_static)=0) then
                                           Message(sym_e_only_static_in_static)
                                         else
                                          begin
                                            consume(ID);
                                            do_member_read(getaddr,sym,p1,pd,again);
                                          end;
                                       end;
                                    end
                                  else
                                    begin
                                       { class reference ? }
                                       if (pd^.deftype=objectdef)
                                         and ((pobjectdef(pd)^.options and oo_is_class)<>0) then
                                         begin
                                            p1:=gentypenode(pd);
                                            p1^.resulttype:=pd;
                                            pd:=new(pclassrefdef,init(pd));
                                            p1:=gensinglenode(loadvmtn,p1);
                                            p1^.resulttype:=pd;
                                         end
                                       else
                                         begin
                                            { generate a type node }
                                            { (for typeof etc)     }
                                            if allow_type then
                                              begin
                                                 p1:=gentypenode(pd);
                                                 { here we must use typenodetype explicitly !! PM
                                                 p1^.resulttype:=pd; }
                                                 pd:=voiddef;
                                              end
                                            else
                                              Message(parser_e_no_type_not_allowed_here);
                                         end;
                                    end;
                               end;
                            end;
                  enumsym : begin
                              p1:=genenumnode(penumsym(srsym));
                              pd:=p1^.resulttype;
                            end;
                 constsym : begin
                              case pconstsym(srsym)^.consttype of
                               constint : p1:=genordinalconstnode(pconstsym(srsym)^.value,s32bitdef);
                            conststring : p1:=genstringconstnode(pstring(pconstsym(srsym)^.value)^);
                              constchar : p1:=genordinalconstnode(pconstsym(srsym)^.value,cchardef);
                              constreal : p1:=genrealconstnode(pbestreal(pconstsym(srsym)^.value)^);
                              constbool : p1:=genordinalconstnode(pconstsym(srsym)^.value,booldef);
                               constset : p1:=gensetconstnode(pconstset(pconstsym(srsym)^.value),
                                                psetdef(pconstsym(srsym)^.definition));
                               constord : p1:=genordinalconstnode(pconstsym(srsym)^.value,
                                                pconstsym(srsym)^.definition);
                               constnil : p1:=genzeronode(niln);
                              end;
                              pd:=p1^.resulttype;
                            end;
                  procsym : begin
                              { are we in a class method ? }
                              possible_error:=(srsymtable^.symtabletype=objectsymtable) and
                                              assigned(aktprocsym) and
                                              ((aktprocsym^.definition^.options and poclassmethod)<>0);
                              p1:=gencallnode(pprocsym(srsym),srsymtable);
                              p1^.unit_specific:=unit_specific;
                              do_proc_call(getaddr or
                                (getprocvar and
                                proc_to_procvar_equal(getprocvardef,pprocsym(srsym)^.definition)),
                                again,p1,pd);
                              if possible_error and
                                 ((p1^.procdefinition^.options and poclassmethod)=0) then
                               Message(parser_e_only_class_methods);
                            end;
              propertysym : begin
                              { access to property in a method }
                              { are we in a class method ? }
                              if (srsymtable^.symtabletype=objectsymtable) and
                                 assigned(aktprocsym) and
                                 ((aktprocsym^.definition^.options and poclassmethod)<>0) then
                               Message(parser_e_only_class_methods);
                              { no method pointer }
                              p1:=nil;
                              handle_propertysym(srsym,srsymtable,p1,pd);
                            end;
                 errorsym : begin
                              p1:=genzeronode(errorn);
                              pd:=generrordef;
                              if token=LKLAMMER then
                               begin
                                 consume(LKLAMMER);
                                 parse_paras(false,false);
                                 consume(RKLAMMER);
                               end;
                            end;
                     else
                       begin
                         p1:=genzeronode(errorn);
                         pd:=generrordef;
                         Message(cg_e_illegal_expression);
                       end;
                     end; { end case }
                   end;
                end;
            end;
         end;

         {---------------------------------------------
                         Factor_Read_Set
         ---------------------------------------------}

         { Read a set between [] }
         function factor_read_set:ptree;
         var
           p1,
           lastp,
           buildp : ptree;
         begin
           buildp:=nil;
         { be sure that a least one arrayconstructn is used, also for an
           empty [] }
           if token=RECKKLAMMER then
            buildp:=gennode(arrayconstructn,nil,buildp)
           else
            begin
              while true do
               begin
                 p1:=comp_expr(true);
                 if token=POINTPOINT then
                  begin
                    consume(POINTPOINT);
                    p2:=comp_expr(true);
                    p1:=gennode(arrayconstructrangen,p1,p2);
                  end;
               { insert at the end of the tree, to get the correct order }
                 if not assigned(buildp) then
                  begin
                    buildp:=gennode(arrayconstructn,p1,nil);
                    lastp:=buildp;
                  end
                 else
                  begin
                    lastp^.right:=gennode(arrayconstructn,p1,nil);
                    lastp:=lastp^.right;
                  end;
               { there could be more elements }
                 if token=COMMA then
                   consume(COMMA)
                 else
                   break;
               end;
            end;
           factor_read_set:=buildp;
         end;

         {---------------------------------------------
                           Helpers
         ---------------------------------------------}

        procedure check_tokenpos;
        begin
          if (p1<>oldp1) then
           begin
             if assigned(p1) then
              set_tree_filepos(p1,filepos);
             oldp1:=p1;
             filepos:=tokenpos;
           end;
        end;



         {---------------------------------------------
                        PostFixOperators
         ---------------------------------------------}

      procedure postfixoperators;
        var
           store_static : boolean;

        { p1 and p2 must contain valid value_str }
        begin
          check_tokenpos;
          while again do
           begin
             { prevent crashes with unknown types }
             if not assigned(pd) then
              begin
                { try to recover }
                repeat
                  case token of
                   CARET:
                     consume(CARET);

                   POINT:
                     begin
                        consume(POINT);
                        consume(ID);
                     end;

                   LECKKLAMMER:
                     begin
                        repeat
                          consume(token);
                          until token in [RECKKLAMMER,SEMICOLON];
                     end;
                  else
                    break;
                  end;
                until false;
                exit;
              end;
           { handle token }
             case token of
                CARET:
                  begin
                    consume(CARET);
                    if not(pd^.deftype in [pointerdef,farpointerdef]) then
                      begin
                         { ^ as binary operator is a problem!!!! (FK) }
                         again:=false;
                         Message(cg_e_invalid_qualifier);
                         disposetree(p1);
                         p1:=genzeronode(errorn);
                      end
                    else
                      begin
                         p1:=gensinglenode(derefn,p1);
                         pd:=ppointerdef(pd)^.definition;
                      end;
                  end;

                LECKKLAMMER:
                  begin
                    if (pd^.deftype=objectdef) and pobjectdef(pd)^.isclass then
                      begin
                        { default property }
                        propsym:=search_default_property(pobjectdef(pd));
                        if not(assigned(propsym)) then
                          begin
                             disposetree(p1);
                             p1:=genzeronode(errorn);
                             again:=false;
                             message(parser_e_no_default_property_available);
                          end
                        else
                          begin
                             p1:=nil;
                             handle_propertysym(propsym,propsym^.owner,p1,pd);
                          end;
                      end
                    else
                      begin
                        consume(LECKKLAMMER);
                        repeat
                          case pd^.deftype of
                            pointerdef:
                                begin
                                   p2:=comp_expr(true);
                                   p1:=gennode(vecn,p1,p2);
                                   pd:=ppointerdef(pd)^.definition;
                                 end;

                     stringdef : begin
                                   p2:=comp_expr(true);
                                   p1:=gennode(vecn,p1,p2);
                                   pd:=cchardef
                                 end;
                      arraydef : begin
                                   p2:=comp_expr(true);
                                 { support SEG:OFS for go32v2 Mem[] }
                                   if (target_info.target=target_i386_go32v2) and
                                      (p1^.treetype=loadn) and
                                      assigned(p1^.symtableentry) and
                                      assigned(p1^.symtableentry^.owner^.name) and
                                      (p1^.symtableentry^.owner^.name^='SYSTEM') and
                                      ((p1^.symtableentry^.name='MEM') or
                                       (p1^.symtableentry^.name='MEMW') or
                                       (p1^.symtableentry^.name='MEML')) then
                                     begin
                                       if (token=COLON) then
                                        begin
                                          consume(COLON);
                                          p3:=gennode(muln,genordinalconstnode($10,s32bitdef),p2);
                                          p2:=comp_expr(true);
                                          p2:=gennode(addn,p2,p3);
                                          p1:=gennode(vecn,p1,p2);
                                          p1^.memseg:=true;
                                          p1^.memindex:=true;
                                        end
                                       else
                                        begin
                                          p1:=gennode(vecn,p1,p2);
                                          p1^.memindex:=true;
                                        end;
                                     end
                                   else
                                     p1:=gennode(vecn,p1,p2);
                                   pd:=parraydef(pd)^.definition;
                                 end;
                          else
                            begin
                              Message(cg_e_invalid_qualifier);
                              disposetree(p1);
                              p1:=genzeronode(errorn);
                              again:=false;
                            end;
                          end;
                          if token=COMMA then
                            consume(COMMA)
                          else
                            break;
                        until false;
                        consume(RECKKLAMMER);
                      end;
                  end;
          POINT : begin
                    consume(POINT);
                    if (pd^.deftype=pointerdef) and
                      (m_autoderef in aktmodeswitches) then
                      begin
                         p1:=gensinglenode(derefn,p1);
                         pd:=ppointerdef(pd)^.definition;
                      end;
                    case pd^.deftype of
                       recorddef:
                         begin
                            sym:=pvarsym(precdef(pd)^.symtable^.search(pattern));
                            if sym=nil then
                              begin
                                Message1(sym_e_illegal_field,pattern);
                                disposetree(p1);
                                p1:=genzeronode(errorn);
                              end
                            else
                              begin
                                p1:=gensubscriptnode(sym,p1);
                                pd:=sym^.definition;
                              end;
                            consume(ID);
                          end;

                        classrefdef:
                          begin
                             classh:=pobjectdef(pclassrefdef(pd)^.definition);
                             sym:=nil;
                             while assigned(classh) do
                              begin
                                sym:=pvarsym(classh^.publicsyms^.search(pattern));
                                srsymtable:=classh^.publicsyms;
                                if assigned(sym) then
                                 break;
                                classh:=classh^.childof;
                              end;
                             consume(ID);
                             do_member_read(false,sym,p1,pd,again);
                           end;

                         objectdef:
                           begin
                             classh:=pobjectdef(pd);
                             sym:=nil;
                             store_static:=allow_only_static;
                             allow_only_static:=false;
                             while assigned(classh) do
                              begin
                                sym:=pvarsym(classh^.publicsyms^.search(pattern));
                                srsymtable:=classh^.publicsyms;
                                if assigned(sym) then
                                 break;
                                classh:=classh^.childof;
                              end;
                             allow_only_static:=store_static;
                             consume(ID);
                             do_member_read(false,sym,p1,pd,again);
                           end;
                         pointerdef:
                           begin
                             Message(cg_e_invalid_qualifier);
                             if ppointerdef(pd)^.definition^.deftype in [recorddef,objectdef,classrefdef] then
                              Message(parser_h_maybe_deref_caret_missing);
                           end;
                    else
                      begin
                        Message(cg_e_invalid_qualifier);
                        disposetree(p1);
                        p1:=genzeronode(errorn);
                      end;
                    end;
                  end;
             else
               begin
               { is this a procedure variable ? }
                 if assigned(pd) then
                  begin
                    if (pd^.deftype=procvardef) then
                     begin
                       if getprocvar and proc_to_procvar_equal(pprocvardef(pd),getprocvardef) then
                         again:=false
                       else
                         if (token=LKLAMMER) or
                            ((pprocvardef(pd)^.para1=nil) and
                             (not((token in [ASSIGNMENT,UNEQUAL,EQUAL]))) and
                             (not afterassignment) and
                             (not in_args)) then
                           begin
                              { do this in a strange way  }
                              { it's not a clean solution }
                              p2:=p1;
                              p1:=gencallnode(nil,nil);
                              p1^.right:=p2;
                              p1^.unit_specific:=unit_specific;
                              p1^.symtableprocentry:=sym;
                              if token=LKLAMMER then
                                begin
                                   consume(LKLAMMER);
                                   p1^.left:=parse_paras(false,false);
                                   consume(RKLAMMER);
                                end;
                              pd:=pprocvardef(pd)^.retdef;
                           { proc():= is never possible }
                              if token=ASSIGNMENT then
                               begin
                                 Message(cg_e_illegal_expression);
                                 p1:=genzeronode(errorn);
                                 again:=false;
                               end;
                              p1^.resulttype:=pd;
                           end
                       else
                         again:=false;
                       p1^.resulttype:=pd;
                     end
                    else
                     again:=false;
                  end
                 else
                  again:=false;
                end;
             end;
             check_tokenpos;
           end; { while again }
        end;


      {---------------------------------------------
                      Factor (Main)
      ---------------------------------------------}

      begin
        oldp1:=nil;
        p1:=nil;
        filepos:=tokenpos;
        if token=ID then
         begin
           factor_read_id;
           { handle post fix operators }
           postfixoperators;
         end
        else
         case token of
        _NEW : begin
                 consume(_NEW);
                 consume(LKLAMMER);
                 {allow_type:=true;}
                 p1:=factor(false);
                 {allow_type:=false;}
                 if p1^.treetype<>typen then
                  begin
                    Message(type_e_type_id_expected);
                    disposetree(p1);
                    pd:=generrordef;
                  end
                 else
                  pd:=p1^.typenodetype;
                 pd2:=pd;

                 if (pd^.deftype<>pointerdef) then
                   Message(type_e_pointer_type_expected)
                 else if {(ppointerdef(pd)^.definition^.deftype<>objectdef)}
                       token=RKLAMMER then
                  begin
                    if (ppointerdef(pd)^.definition^.deftype=objectdef) and
                       ((pobjectdef(ppointerdef(pd)^.definition)^.options and oo_hasvmt) <> 0)  then
                     Message(parser_w_use_extended_syntax_for_objects);
                    p1:=gensinglenode(newn,nil);
                    p1^.resulttype:=pd2;
                    consume(RKLAMMER);
                    (*Message(parser_e_pointer_to_class_expected);
                    { if an error occurs, read til the end of
                      the new statement }
                    p1:=genzeronode(errorn);
                    l:=1;
                    while true do
                     begin
                       case token of
                        LKLAMMER : inc(l);
                        RKLAMMER : dec(l);
                       end;
                       consume(token);
                       if l=0 then
                        break;
                     end;*)
                  end
                 else
                  begin
                    disposetree(p1);
                    p1:=genzeronode(hnewn);
                    p1^.resulttype:=ppointerdef(pd)^.definition;
                    consume(COMMA);
                    afterassignment:=false;
                    { determines the current object defintion }
                    classh:=pobjectdef(ppointerdef(pd)^.definition);
                    { check for an abstract class }
                    if (classh^.options and oo_is_abstract)<>0 then
                      Message(sym_e_no_instance_of_abstract_object);

                    { search the constructor also in the symbol tables of
                      the parents }

                    { no constructor found }
                    sym:=nil;
                    while assigned(classh) do
                     begin
                       sym:=pvarsym(classh^.publicsyms^.search(pattern));
                       srsymtable:=classh^.publicsyms;
                       if assigned(sym) then
                        break;
                       classh:=classh^.childof;
                     end;

                    consume(ID);
                    do_member_read(false,sym,p1,pd,again);
                    if (p1^.treetype<>calln) or
                       (assigned(p1^.procdefinition) and
                       ((p1^.procdefinition^.options and poconstructor)=0)) then
                      Message(parser_e_expr_have_to_be_constructor_call);
                    p1:=gensinglenode(newn,p1);
                    { set the resulttype }
                    p1^.resulttype:=pd2;
                    consume(RKLAMMER);
                  end;
               end;
       _SELF : begin
                 again:=true;
                 consume(_SELF);
                 if not assigned(procinfo._class) then
                  begin
                    p1:=genzeronode(errorn);
                    pd:=generrordef;
                    again:=false;
                    Message(parser_e_self_not_in_method);
                  end
                 else
                  begin
                    if (aktprocsym^.definition^.options and poclassmethod)<>0 then
                     begin
                       { self in class methods is a class reference type }
                       pd:=new(pclassrefdef,init(procinfo._class));
                       p1:=genselfnode(pd);
                       p1^.resulttype:=pd;
                     end
                    else
                     begin
                       p1:=genselfnode(procinfo._class);
                       p1^.resulttype:=procinfo._class;
                     end;
                    pd:=p1^.resulttype;
                    postfixoperators;
                  end;
               end;
  _INHERITED : begin
                 again:=true;
                 consume(_INHERITED);
                 if assigned(procinfo._class) then
                  begin
                    classh:=procinfo._class^.childof;
                    while assigned(classh) do
                     begin
                       srsymtable:=pobjectdef(classh)^.publicsyms;
                       sym:=pvarsym(srsymtable^.search(pattern));
                       if assigned(sym) then
                        begin
                          p1:=genzeronode(typen);
                          p1^.resulttype:=classh;
                          pd:=p1^.resulttype;
                          consume(ID);
                          do_member_read(false,sym,p1,pd,again);
                          break;
                        end;
                       classh:=classh^.childof;
                     end;
                    if classh=nil then
                     begin
                       Message1(sym_e_id_no_member,pattern);
                       again:=false;
                       pd:=generrordef;
                       p1:=genzeronode(errorn);
                     end;
                  end
                 else
                   begin
                      Message(parser_e_generic_methods_only_in_methods);
                      again:=false;
                      pd:=generrordef;
                      p1:=genzeronode(errorn);
                   end;
                 postfixoperators;
               end;
    INTCONST : begin
                 valint(pattern,l,code);
                 if code<>0 then
                  begin
                    val(pattern,d,code);
                    if code<>0 then
                     begin
                       Message(cg_e_invalid_integer);
                       consume(INTCONST);
                       l:=1;
                       p1:=genordinalconstnode(l,s32bitdef);
                     end
                    else
                     begin
                       consume(INTCONST);
                       p1:=genrealconstnode(d);
                     end;
                  end
                 else
                  begin
                    consume(INTCONST);
                    p1:=genordinalconstnode(l,s32bitdef);
                  end;
               end;
  REALNUMBER : begin
                 val(pattern,d,code);
                 if code<>0 then
                  begin
                    Message(parser_e_error_in_real);
                    d:=1.0;
                  end;
                 consume(REALNUMBER);
                 p1:=genrealconstnode(d);
               end;
     _STRING : begin
                 pd:=stringtype;
                 { STRING can be also a type cast }
                 if token=LKLAMMER then
                  begin
                    consume(LKLAMMER);
                    p1:=comp_expr(true);
                    consume(RKLAMMER);
                    p1:=gentypeconvnode(p1,pd);
                    p1^.explizit:=true;
                    { handle postfix operators here e.g. string(a)[10] }
                    again:=true;
                    postfixoperators;
                  end
                 else
                  p1:=gentypenode(pd);
               end;
       _FILE : begin
                 pd:=cfiledef;
                 consume(_FILE);
                 { FILE can be also a type cast }
                 if token=LKLAMMER then
                  begin
                    consume(LKLAMMER);
                    p1:=comp_expr(true);
                    consume(RKLAMMER);
                    p1:=gentypeconvnode(p1,pd);
                    p1^.explizit:=true;
                    { handle postfix operators here e.g. string(a)[10] }
                    again:=true;
                    postfixoperators;
                  end
                 else
                  p1:=gentypenode(pd);
               end;
     CSTRING : begin
                 p1:=genstringconstnode(pattern);
                 consume(CSTRING);
               end;
       CCHAR : begin
                 p1:=genordinalconstnode(ord(pattern[1]),cchardef);
                 consume(CCHAR);
               end;
 KLAMMERAFFE : begin
                 consume(KLAMMERAFFE);
                 p1:=factor(true);
                 p1:=gensinglenode(addrn,p1);
               end;
    LKLAMMER : begin
                 consume(LKLAMMER);
                 p1:=comp_expr(true);
                 consume(RKLAMMER);
                 { it's not a good solution        }
                 { but (a+b)^ makes some problems  }
                 if token in [CARET,POINT,LECKKLAMMER] then
                  begin
                    { we need the resulttype  }
                    { of the expression in pd }
                    do_firstpass(p1);
                    pd:=p1^.resulttype;
                    again:=true;
                    postfixoperators;
                  end;
               end;
 LECKKLAMMER : begin
                 consume(LECKKLAMMER);
                 p1:=factor_read_set;
                 consume(RECKKLAMMER);
               end;
        PLUS : begin
                 consume(PLUS);
                 p1:=factor(false);
               end;
       MINUS : begin
                 consume(MINUS);
                 p1:=factor(false);
                 p1:=gensinglenode(umminusn,p1);
               end;
        _NOT : begin
                 consume(_NOT);
                 p1:=factor(false);
                 p1:=gensinglenode(notn,p1);
               end;
       _TRUE : begin
                 consume(_TRUE);
                 p1:=genordinalconstnode(1,booldef);
               end;
      _FALSE : begin
                 consume(_FALSE);
                 p1:=genordinalconstnode(0,booldef);
               end;
        _NIL : begin
                 consume(_NIL);
                 p1:=genzeronode(niln);
               end;
        else
          begin
            p1:=genzeronode(errorn);
            consume(token);
            Message(cg_e_illegal_expression);
          end;
        end;
        { generate error node if no node is created }
        if not assigned(p1) then
          p1:=genzeronode(errorn);
        factor:=p1;
        check_tokenpos;
      end;


{****************************************************************************
                             Sub_Expr
****************************************************************************}

    type
      Toperator_precedence=(opcompare,opaddition,opmultiply);
      Ttok2nodeRec=record
        tok : ttoken;
        nod : ttreetyp;
      end;

    const
      tok2nodes=23;
      tok2node:array[1..tok2nodes] of ttok2noderec=(
        (tok:PLUS    ;nod:addn),
        (tok:MINUS   ;nod:subn),
        (tok:STAR    ;nod:muln),
        (tok:SLASH   ;nod:slashn),
        (tok:EQUAL   ;nod:equaln),
        (tok:GT      ;nod:gtn),
        (tok:LT      ;nod:ltn),
        (tok:GTE     ;nod:gten),
        (tok:LTE     ;nod:lten),
        (tok:SYMDIF  ;nod:symdifn),
        (tok:STARSTAR;nod:starstarn),
        (tok:CARET   ;nod:caretn),
        (tok:UNEQUAL ;nod:unequaln),
        (tok:_AS     ;nod:asn),
        (tok:_IN     ;nod:inn),
        (tok:_IS     ;nod:isn),
        (tok:_OR     ;nod:orn),
        (tok:_AND    ;nod:andn),
        (tok:_DIV    ;nod:divn),
        (tok:_MOD    ;nod:modn),
        (tok:_SHL    ;nod:shln),
        (tok:_SHR    ;nod:shrn),
        (tok:_XOR    ;nod:xorn)
      );
      operator_levels:array[Toperator_precedence] of set of Ttoken=
         ([LT,LTE,GT,GTE,EQUAL,UNEQUAL,_IN,_IS],
          [PLUS,MINUS,_OR,_XOR],
          [CARET,SYMDIF,STARSTAR,STAR,SLASH,_DIV,_MOD,_AND,_SHL,_SHR,_AS]);

    function sub_expr(pred_level:Toperator_precedence;accept_equal : boolean):Ptree;
    {Reads a subexpression while the operators are of the current precedence
     level, or any higher level. Replaces the old term, simpl_expr and
     simpl2_expr.}
      var
        low,high,mid : longint;
        p1,p2   : Ptree;
        oldt    : Ttoken;
        filepos : tfileposinfo;
      begin
        if pred_level=opmultiply then
          p1:=factor(false)
        else
          p1:=sub_expr(succ(pred_level),true);
        repeat
          if (token in operator_levels[pred_level]) and
             ((token<>EQUAL) or accept_equal) then
           begin
             oldt:=token;
             filepos:=tokenpos;
             consume(token);
             if pred_level=opmultiply then
               p2:=factor(false)
             else
               p2:=sub_expr(succ(pred_level),true);
             low:=1;
             high:=tok2nodes;
             while (low<high) do
              begin
                mid:=(low+high+1) shr 1;
                if oldt<tok2node[mid].tok then
                 high:=mid-1
                else
                 low:=mid;
              end;
             if tok2node[high].tok=oldt then
              p1:=gennode(tok2node[high].nod,p1,p2)
             else
              p1:=gennode(nothingn,p1,p2);
             set_tree_filepos(p1,filepos);
           end
          else
           break;
        until false;
        sub_expr:=p1;
      end;


    function comp_expr(accept_equal : boolean):Ptree;
      var
         oldafterassignment : boolean;

      begin
         oldafterassignment:=afterassignment;
         afterassignment:=true;
         comp_expr:=sub_expr(opcompare,accept_equal);
         afterassignment:=oldafterassignment;
      end;

    function expr : ptree;

      var
         p1,p2 : ptree;
         oldafterassignment : boolean;
         oldp1 : ptree;
         filepos : tfileposinfo;

      begin
         oldafterassignment:=afterassignment;
         p1:=sub_expr(opcompare,true);
         if token in [ASSIGNMENT,_PLUSASN,_MINUSASN,_STARASN,_SLASHASN] then
           afterassignment:=true;
         filepos:=tokenpos;
         oldp1:=p1;
         case token of
            POINTPOINT : begin
                            consume(POINTPOINT);
                            p2:=sub_expr(opcompare,true);
                            p1:=gennode(rangen,p1,p2);
                         end;
            ASSIGNMENT : begin
                            consume(ASSIGNMENT);
                            { avoid a firstpass of a procedure if
                            it must be assigned to a procvar }
                            { should be recursive for a:=b:=c !!! }
                            if (p1^.resulttype<>nil) and (p1^.resulttype^.deftype=procvardef) then
                              begin
                                 getprocvar:=true;
                                 getprocvardef:=pprocvardef(p1^.resulttype);
                              end;
                            p2:=sub_expr(opcompare,true);
                            if getprocvar and (p2^.treetype=calln) then
                              begin
                                 if ((getprocvardef^.options and pomethodpointer)<>0) then
                                   begin
                                      if (p2^.methodpointer^.resulttype^.deftype=objectdef) and
                                         (pobjectdef(p2^.methodpointer^.resulttype)^.isclass) and
                                         (proc_to_procvar_equal(getprocvardef,pprocsym(p2^.symtableentry)^.definition)) then
                                        begin
                                           p2^.treetype:=loadn;
                                           p2^.disposetyp:=dt_left;
                                           p2^.left:=p2^.methodpointer;
                                           p2^.resulttype:=pprocsym(p2^.symtableprocentry)^.definition;
                                           p2^.symtableentry:=pvarsym(p2^.symtableprocentry);
                                        end
                                      else
                                        Message(type_e_mismatch);
                                   end
                                 else if (proc_to_procvar_equal(getprocvardef,pprocsym(p2^.symtableentry)^.definition)) then
                                   begin
                                      p2^.treetype:=loadn;
                                      p2^.resulttype:=pprocsym(p2^.symtableprocentry)^.definition;
                                      p2^.symtableentry:=p2^.symtableprocentry;
                                   end;
                              end;
                            getprocvar:=false;
                            p1:=gennode(assignn,p1,p2);
                         end;
                         { this is the code for C like assignements }
                         { from an improvement of Peter Schaefer    }
            _PLUSASN   : begin
                            consume(_PLUSASN  );
                            p2:=sub_expr(opcompare,true);
                            p1:=gennode(assignn,p1,gennode(addn,getcopy(p1),p2));
                            { was first
                              p1:=gennode(assignn,p1,gennode(addn,p1,p2));
                              but disposetree assumes that we have a real
                              *** tree *** }
                         end;

            _MINUSASN   : begin
                            consume(_MINUSASN  );
                            p2:=sub_expr(opcompare,true);
                            p1:=gennode(assignn,p1,gennode(subn,getcopy(p1),p2));
                         end;
            _STARASN   : begin
                            consume(_STARASN  );
                            p2:=sub_expr(opcompare,true);
                            p1:=gennode(assignn,p1,gennode(muln,getcopy(p1),p2));
                         end;
            _SLASHASN   : begin
                            consume(_SLASHASN  );
                            p2:=sub_expr(opcompare,true);
                            p1:=gennode(assignn,p1,gennode(slashn,getcopy(p1),p2));
                         end;
         end;
         afterassignment:=oldafterassignment;
         if p1<>oldp1 then
           set_tree_filepos(p1,filepos);
         expr:=p1;
      end;


    function get_intconst:longint;
    {Reads an expression, tries to evalute it and check if it is an integer
     constant. Then the constant is returned.}
    var
      p:Ptree;
    begin
      p:=comp_expr(true);
      do_firstpass(p);
      if (p^.treetype<>ordconstn) and
         (p^.resulttype^.deftype=orddef) and
         not(Porddef(p^.resulttype)^.typ in [uvoid,uchar,bool8bit,bool16bit,bool32bit]) then
        Message(cg_e_illegal_expression)
      else
        get_intconst:=p^.value;
      disposetree(p);
    end;


    function get_stringconst:string;
    {Reads an expression, tries to evaluate it and checks if it is a string
     constant. Then the constant is returned.}
    var
      p:Ptree;
    begin
      get_stringconst:='';
      p:=comp_expr(true);
      do_firstpass(p);
      if p^.treetype<>stringconstn then
        begin
          if (p^.treetype=ordconstn) and is_char(p^.resulttype) then
            get_stringconst:=char(p^.value)
          else
            Message(cg_e_illegal_expression);
        end
      else
        get_stringconst:=strpas(p^.value_str);
      disposetree(p);
    end;

end.
{
  $Log$
  Revision 1.87  1999-03-16 17:52:52  jonas
    * changes for internal Val code (do a "make cycle OPT=-dvalintern" to test)
    * in cgi386inl: also range checking for subrange types (compile with "-dreadrangecheck")
    * in cgai386: also small fixes to emitrangecheck

  Revision 1.86  1999/03/04 13:55:44  pierre
    * some m68k fixes (still not compilable !)
    * new(tobj) does not give warning if tobj has no VMT !

  Revision 1.85  1999/02/22 15:09:39  florian
    * behaviaor of PROTECTED and PRIVATE fixed, works now like TP/Delphi

  Revision 1.84  1999/02/22 02:15:26  peter
    * updates for ag386bin

  Revision 1.83  1999/02/11 09:46:25  pierre
    * fix for normal method calls inside static methods :
      WARNING there were both parser and codegen errors !!
      added static_call boolean to calln tree

  Revision 1.82  1999/01/28 14:06:47  florian
    * small fix for method pointers
    * found the annoying strpas bug, mainly nested call to type cast which
      use ansistrings crash

  Revision 1.81  1999/01/27 00:13:55  florian
    * "procedure of object"-stuff fixed

  Revision 1.80  1999/01/21 16:41:01  pierre
   * fix for constructor inside with statements

  Revision 1.79  1998/12/30 22:15:48  peter
    + farpointer type
    * absolutesym now also stores if its far

  Revision 1.78  1998/12/11 00:03:32  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.77  1998/12/04 10:18:09  florian
    * some stuff for procedures of object added
    * bug with overridden virtual constructors fixed (reported by Italo Gomes)

  Revision 1.76  1998/11/27 14:50:40  peter
    + open strings, $P switch support

  Revision 1.75  1998/11/25 19:12:51  pierre
    * var:=new(pointer_type) support added

  Revision 1.74  1998/11/13 10:18:11  peter
    + nil constants

  Revision 1.73  1998/11/05 12:02:52  peter
    * released useansistring
    * removed -Sv, its now available in fpc modes

  Revision 1.72  1998/11/04 10:11:41  peter
    * ansistring fixes

  Revision 1.71  1998/10/22 23:57:29  peter
    * fixed filedef for typenodetype

  Revision 1.70  1998/10/21 15:12:54  pierre
    * bug fix for IOCHECK inside a procedure with iocheck modifier
    * removed the GPF for unexistant overloading
      (firstcall was called with procedinition=nil !)
    * changed typen to what Florian proposed
      gentypenode(p : pdef) sets the typenodetype field
      and resulttype is only set if inside bt_type block !

  Revision 1.69  1998/10/20 15:10:19  pierre
    * type ptree only allowed inside expression
      if following sizeof typeof low high or as first arg of new !!

  Revision 1.68  1998/10/20 11:15:44  pierre
   * calling of private method allowed inside child object method

  Revision 1.67  1998/10/19 08:54:57  pierre
    * wrong stabs info corrected once again !!
    + variable vmt offset with vmt field only if required
      implemented now !!!

  Revision 1.66  1998/10/15 15:13:28  pierre
    + added oo_hasconstructor and oo_hasdestructor
      for objects options

  Revision 1.65  1998/10/13 13:10:24  peter
    * new style for m68k/i386 infos and enums

  Revision 1.64  1998/10/12 12:20:55  pierre
    + added tai_const_symbol_offset
      for r : pointer = @var.field;
    * better message for different arg names on implementation
      of function

  Revision 1.63  1998/10/12 10:28:30  florian
    + auto dereferencing of pointers to structured types in delphi mode

  Revision 1.62  1998/10/12 10:05:41  peter
    * fixed mem leak with arrayconstrutor

  Revision 1.61  1998/10/05 13:57:15  peter
    * crash preventions

  Revision 1.60  1998/10/05 12:32:46  peter
    + assert() support

  Revision 1.59  1998/10/01 14:56:24  peter
    * crash preventions

  Revision 1.58  1998/09/30 07:40:35  florian
    * better error recovering

  Revision 1.57  1998/09/28 16:18:16  florian
    * two fixes to get ansi strings work

  Revision 1.56  1998/09/26 17:45:36  peter
    + idtoken and only one token table

  Revision 1.55  1998/09/24 23:49:10  peter
    + aktmodeswitches

  Revision 1.54  1998/09/23 15:46:39  florian
    * problem with with and classes fixed

  Revision 1.53  1998/09/23 09:58:54  peter
    * first working array of const things

  Revision 1.52  1998/09/20 09:38:45  florian
    * hasharray for defs fixed
    * ansistring code generation corrected (init/final, assignement)

  Revision 1.51  1998/09/18 16:03:43  florian
    * some changes to compile with Delphi

  Revision 1.50  1998/09/17 13:41:18  pierre
  sizeof(TPOINT) problem

  Revision 1.49.2.1  1998/09/17 08:42:31  pierre
  TPOINT sizeof fix

  Revision 1.49  1998/09/09 11:50:53  pierre
    * forward def are not put in record or objects
    + added check for forwards also in record and objects
    * dummy parasymtable for unit initialization removed from
    symtable stack

  Revision 1.48  1998/09/07 22:25:53  peter
    * fixed str(boolean,string) which was allowed
    * fixed write(' ':<int expression>) only constants where allowed :(

  Revision 1.47  1998/09/07 18:46:10  peter
    * update smartlinking, uses getdatalabel
    * renamed ptree.value vars to value_str,value_real,value_set

  Revision 1.46  1998/09/04 08:42:03  peter
    * updated some error messages

  Revision 1.45  1998/09/01 17:39:49  peter
    + internal constant functions

  Revision 1.44  1998/08/28 10:54:24  peter
    * fixed smallset generation from elements, it has never worked before!

  Revision 1.43  1998/08/23 16:07:24  florian
    * internalerror with mod/div fixed

  Revision 1.42  1998/08/21 14:08:50  pierre
    + TEST_FUNCRET now default (old code removed)
      works also for m68k (at least compiles)

  Revision 1.41  1998/08/20 21:36:39  peter
    * fixed 'with object do' bug

  Revision 1.40  1998/08/20 09:26:41  pierre
    + funcret setting in underproc testing
      compile with _dTEST_FUNCRET

  Revision 1.39  1998/08/18 16:48:48  pierre
    * bug for -So proc assignment to p^rocvar fixed

  Revision 1.38  1998/08/18 14:17:09  pierre
    * bug about assigning the return value of a function to
      a procvar fixed : warning
      assigning a proc to a procvar need @ in FPC mode !!
    * missing file/line info restored

  Revision 1.37  1998/08/18 09:24:43  pierre
    * small warning position bug fixed
    * support_mmx switches splitting was missing
    * rhide error and warning output corrected

  Revision 1.36  1998/08/15 16:50:29  peter
    * fixed proc()=expr which was not allowed anymore by my previous fix

  Revision 1.35  1998/08/14 18:18:46  peter
    + dynamic set contruction
    * smallsets are now working (always longint size)

  Revision 1.34  1998/08/13 11:00:12  peter
    * fixed procedure<>procedure construct

  Revision 1.33  1998/08/11 15:31:39  peter
    * write extended to ppu file
    * new version 0.99.7

  Revision 1.32  1998/08/11 14:05:32  peter
    * fixed sizeof(array of char)

  Revision 1.31  1998/08/10 14:50:11  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.30  1998/07/28 21:52:54  florian
    + implementation of raise and try..finally
    + some misc. exception stuff

  Revision 1.29  1998/07/27 21:57:13  florian
    * fix to allow tv like stream registration:
        @tmenu.load doesn't work if load had parameters or if load was only
        declared in an anchestor class of tmenu

  Revision 1.28  1998/07/14 21:46:51  peter
    * updated messages file

  Revision 1.27  1998/06/25 14:04:23  peter
    + internal inc/dec

  Revision 1.26  1998/06/09 16:01:46  pierre
    + added procedure directive parsing for procvars
      (accepted are popstack cdecl and pascal)
    + added C vars with the following syntax
      var C calias 'true_c_name';(can be followed by external)
      reason is that you must add the Cprefix

      which is target dependent

  Revision 1.25  1998/06/05 14:37:33  pierre
    * fixes for inline for operators
    * inline procedure more correctly restricted

  Revision 1.24  1998/06/04 23:51:52  peter
    * m68k compiles
    + .def file creation moved to gendef.pas so it could also be used
      for win32

  Revision 1.23  1998/06/04 09:55:40  pierre
    * demangled name of procsym reworked to become independant of the mangling scheme

  Revision 1.22  1998/06/02 17:03:03  pierre
    *  with node corrected for objects
    * small bugs for SUPPORT_MMX fixed

  Revision 1.21  1998/05/27 19:45:05  peter
    * symtable.pas splitted into includefiles
    * symtable adapted for $ifdef NEWPPU

  Revision 1.20  1998/05/26 07:53:59  pierre
    * bug fix for empty sets (nil pd was dereferenced )

  Revision 1.19  1998/05/25 17:11:43  pierre
    * firstpasscount bug fixed
      now all is already set correctly the first time
      under EXTDEBUG try -gp to skip all other firstpasses
      it works !!
    * small bug fixes
      - for smallsets with -dTESTSMALLSET
      - some warnings removed (by correcting code !)

  Revision 1.18  1998/05/23 01:21:20  peter
    + aktasmmode, aktoptprocessor, aktoutputformat
    + smartlink per module $SMARTLINK-/+ (like MMX) and moved to aktswitches
    + $LIBNAME to set the library name where the unit will be put in
    * splitted cgi386 a bit (codeseg to large for bp7)
    * nasm, tasm works again. nasm moved to ag386nsm.pas

  Revision 1.17  1998/05/22 12:37:03  carl
    * crash bugfix (patched msanually to main branch)

  Revision 1.16  1998/05/21 19:33:32  peter
    + better procedure directive handling and only one table

  Revision 1.15  1998/05/20 09:42:35  pierre
    + UseTokenInfo now default
    * unit in interface uses and implementation uses gives error now
    * only one error for unknown symbol (uses lastsymknown boolean)
      the problem came from the label code !
    + first inlined procedures and function work
      (warning there might be allowed cases were the result is still wrong !!)
    * UseBrower updated gives a global list of all position of all used symbols
      with switch -gb

  Revision 1.14  1998/05/11 13:07:56  peter
    + $ifdef NEWPPU for the new ppuformat
    + $define GDB not longer required
    * removed all warnings and stripped some log comments
    * no findfirst/findnext anymore to remove smartlink *.o files

  Revision 1.13  1998/05/06 08:38:45  pierre
    * better position info with UseTokenInfo
      UseTokenInfo greatly simplified
    + added check for changed tree after first time firstpass
      (if we could remove all the cases were it happen
      we could skip all firstpass if firstpasscount > 1)
      Only with ExtDebug

  Revision 1.12  1998/05/05 12:05:42  florian
    * problems with properties fixed
    * crash fixed:  i:=l when i and l are undefined, was a problem with
      implementation of private/protected

  Revision 1.11  1998/05/04 11:22:26  florian
    * problem with DOM solved: it crashes when accessing a property in a method

  Revision 1.10  1998/05/01 16:38:45  florian
    * handling of private and protected fixed
    + change_keywords_to_tp implemented to remove
      keywords which aren't supported by tp
    * break and continue are now symbols of the system unit
    + widestring, longstring and ansistring type released

  Revision 1.9  1998/04/29 10:33:58  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.8  1998/04/14 23:27:03  florian
    + exclude/include with constant second parameter added

  Revision 1.7  1998/04/09 23:02:15  florian
    * small problems solved to get remake3 work

  Revision 1.6  1998/04/09 22:16:35  florian
    * problem with previous REGALLOC solved
    * improved property support

  Revision 1.5  1998/04/08 10:26:09  florian
    * correct error handling of virtual constructors
    * problem with new type declaration handling fixed

  Revision 1.4  1998/04/07 22:45:05  florian
    * bug0092, bug0115 and bug0121 fixed
    + packed object/class/array

  Revision 1.3  1998/04/07 13:19:46  pierre
    * bugfixes for reset_gdb_info
      in MEM parsing for go32v2
      better external symbol creation
      support for rhgdb.exe (lowercase file names)
}
