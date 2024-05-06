{
    Copyright (c) 2014 by Florian Klaempfl

    Symbol table overrides for AVR

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
unit symcpu;

{$i fpcdefs.inc}

interface

uses
  symtype,symdef,symsym,symconst;

type
  { defs }
  tcpufiledef = class(tfiledef)
  end;
  tcpufiledefclass = class of tcpufiledef;

  tcpuvariantdef = class(tvariantdef)
  end;
  tcpuvariantdefclass = class of tcpuvariantdef;

  tcpuformaldef = class(tformaldef)
  end;
  tcpuformaldefclass = class of tcpuformaldef;

  tcpuforwarddef = class(tforwarddef)
  end;
  tcpuforwarddefclass = class of tcpuforwarddef;

  tcpuundefineddef = class(tundefineddef)
  end;
  tcpuundefineddefclass = class of tcpuundefineddef;

  tcpuerrordef = class(terrordef)
  end;
  tcpuerrordefclass = class of tcpuerrordef;

  tcpupointerdef = class(tpointerdef)
  protected
    procedure ppuload_platform(ppufile:tcompilerppufile); override;
    procedure ppuwrite_platform(ppufile:tcompilerppufile); override;
  public
    constructor create(def:tdef); override;
    function compatible_with_pointerdef_size(ptr:tpointerdef): boolean; override;
    function getcopy:tstoreddef; override;
    function GetTypeName:string; override;
  end;
  tcpupointerdefclass = class of tcpupointerdef;

  tcpurecorddef = class(trecorddef)
  protected
    procedure ppuload_platform(ppufile:tcompilerppufile); override;
    procedure ppuwrite_platform(ppufile:tcompilerppufile); override;
  public
    function getcopy:tstoreddef; override;
    function GetTypeName:string; override;
  end;
  tcpurecorddefclass = class of tcpurecorddef;

  tcpuimplementedinterface = class(timplementedinterface)
  end;
  tcpuimplementedinterfaceclass = class of tcpuimplementedinterface;

  tcpuobjectdef = class(tobjectdef)
  end;
  tcpuobjectdefclass = class of tcpuobjectdef;

  tcpuclassrefdef = class(tclassrefdef)
  end;
  tcpuclassrefdefclass = class of tcpuclassrefdef;

  tcpuarraydef = class(tarraydef)
  protected
    procedure ppuload_platform(ppufile:tcompilerppufile); override;
    procedure ppuwrite_platform(ppufile:tcompilerppufile); override;
  public
    function getcopy:tstoreddef; override;
    function GetTypeName:string; override;
  end;
  tcpuarraydefclass = class of tcpuarraydef;

  tcpuorddef = class(torddef)
  protected
    procedure ppuload_platform(ppufile:tcompilerppufile); override;
    procedure ppuwrite_platform(ppufile:tcompilerppufile); override;
  public
    function getcopy:tstoreddef; override;
    function GetTypeName:string; override;
  end;
  tcpuorddefclass = class of tcpuorddef;

  tcpufloatdef = class(tfloatdef)
  protected
    procedure ppuload_platform(ppufile:tcompilerppufile); override;
    procedure ppuwrite_platform(ppufile:tcompilerppufile); override;
  public
    function getcopy:tstoreddef; override;
    function GetTypeName:string; override;
  end;
  tcpufloatdefclass = class of tcpufloatdef;

  tcpuprocvardef = class(tprocvardef)
  end;
  tcpuprocvardefclass = class of tcpuprocvardef;

  tcpuprocdef = class(tprocdef)
  end;
  tcpuprocdefclass = class of tcpuprocdef;

  tcpustringdef = class(tstringdef)
  protected
    procedure ppuload_platform(ppufile:tcompilerppufile); override;
    procedure ppuwrite_platform(ppufile:tcompilerppufile); override;
  public
    function getcopy:tstoreddef; override;
    function GetTypeName:string; override;
  end;
  tcpustringdefclass = class of tcpustringdef;

  tcpuenumdef = class(tenumdef)
  end;
  tcpuenumdefclass = class of tcpuenumdef;

  tcpusetdef = class(tsetdef)
  end;
  tcpusetdefclass = class of tcpusetdef;

  { syms }
  tcpulabelsym = class(tlabelsym)
  end;
  tcpulabelsymclass = class of tcpulabelsym;

  tcpuunitsym = class(tunitsym)
  end;
  tcpuunitsymclass = class of tcpuunitsym;

  tcpuprogramparasym = class(tprogramparasym)
  end;
  tcpuprogramparasymclass = class(tprogramparasym);

  tcpunamespacesym = class(tnamespacesym)
  end;
  tcpunamespacesymclass = class of tcpunamespacesym;

  tcpuprocsym = class(tprocsym)
  end;
  tcpuprocsymclass = class of tcpuprocsym;

  tcputypesym = class(ttypesym)
  end;
  tcpuypesymclass = class of tcputypesym;

  tcpufieldvarsym = class(tfieldvarsym)
  end;
  tcpufieldvarsymclass = class of tcpufieldvarsym;

  tcpulocalvarsym = class(tlocalvarsym)
  end;
  tcpulocalvarsymclass = class of tcpulocalvarsym;

  tcpuparavarsym = class(tparavarsym)
  end;
  tcpuparavarsymclass = class of tcpuparavarsym;

  tcpustaticvarsym = class(tstaticvarsym)
  end;
  tcpustaticvarsymclass = class of tcpustaticvarsym;

  tcpuabsolutevarsym = class(tabsolutevarsym)
  end;
  tcpuabsolutevarsymclass = class of tcpuabsolutevarsym;

  tcpupropertysym = class(tpropertysym)
  end;
  tcpupropertysymclass = class of tcpupropertysym;

  tcpuconstsym = class(tconstsym)
  end;
  tcpuconstsymclass = class of tcpuconstsym;

  tcpuenumsym = class(tenumsym)
  end;
  tcpuenumsymclass = class of tcpuenumsym;

  tcpusyssym = class(tsyssym)
  end;
  tcpusyssymclass = class of tcpusyssym;


const
  pbestrealtype : ^tdef = @s64floattype;


implementation

procedure tcpustringdef.ppuload_platform(ppufile: tcompilerppufile);
begin
  inherited ppuload_platform(ppufile);
  symsection:=tsymsection(ppufile.getbyte);
end;

procedure tcpustringdef.ppuwrite_platform(ppufile: tcompilerppufile);
begin
  inherited ppuwrite_platform(ppufile);
  ppufile.putbyte(byte(symsection));
end;

function tcpustringdef.getcopy: tstoreddef;
begin
  Result:=inherited getcopy;
  Result.symsection:=symsection;
end;

function tcpustringdef.GetTypeName: string;
begin
  result:=inherited GetTypeName;
  case symsection of
    ss_eeprom: result:=result+'.EEPROM';
    ss_progmem: result:=result+'.PROGMEM';
  else
    ;
  end;
end;

procedure tcpuorddef.ppuload_platform(ppufile: tcompilerppufile);
begin
  inherited ppuload_platform(ppufile);
  symsection:=tsymsection(ppufile.getbyte);
end;

procedure tcpuorddef.ppuwrite_platform(ppufile: tcompilerppufile);
begin
  inherited ppuwrite_platform(ppufile);
  ppufile.putbyte(byte(symsection));
end;

function tcpuorddef.getcopy: tstoreddef;
begin
  Result:=inherited getcopy;
  Result.symsection:=symsection;
end;

function tcpuorddef.GetTypeName: string;
begin
  result:=inherited GetTypeName;
  case symsection of
    ss_eeprom: result:=result+'.EEPROM';
    ss_progmem: result:=result+'.PROGMEM';
  else
    ;
  end;
end;

procedure tcpuarraydef.ppuload_platform(ppufile: tcompilerppufile);
begin
  inherited ppuload_platform(ppufile);
  symsection:=tsymsection(ppufile.getbyte);
end;

procedure tcpuarraydef.ppuwrite_platform(ppufile: tcompilerppufile);
begin
  inherited ppuwrite_platform(ppufile);
  ppufile.putbyte(byte(symsection));
end;

function tcpuarraydef.getcopy: tstoreddef;
begin
  Result:=inherited getcopy;
  Result.symsection:=symsection;
end;

function tcpuarraydef.GetTypeName: string;
begin
  result:=inherited GetTypeName;
  case symsection of
    ss_eeprom: result:=result+'.EEPROM';
    ss_progmem: result:=result+'.PROGMEM';
  else
    ;
  end;
end;

procedure tcpufloatdef.ppuload_platform(ppufile: tcompilerppufile);
begin
  inherited ppuload_platform(ppufile);
  symsection:=tsymsection(ppufile.getbyte);
end;

procedure tcpufloatdef.ppuwrite_platform(ppufile: tcompilerppufile);
begin
  inherited ppuwrite_platform(ppufile);
  ppufile.putbyte(byte(symsection));
end;

function tcpufloatdef.getcopy: tstoreddef;
begin
  Result:=inherited getcopy;
  Result.symsection:=symsection;
end;

function tcpufloatdef.GetTypeName: string;
begin
  result:=inherited GetTypeName;
  case symsection of
    ss_eeprom: result:=result+'.EEPROM';
    ss_progmem: result:=result+'.PROGMEM';
  else
    ;
  end;
end;

procedure tcpurecorddef.ppuload_platform(ppufile: tcompilerppufile);
begin
  inherited ppuload_platform(ppufile);
  symsection:=tsymsection(ppufile.getbyte);
end;

procedure tcpurecorddef.ppuwrite_platform(ppufile: tcompilerppufile);
begin
  inherited ppuwrite_platform(ppufile);
  ppufile.putbyte(byte(symsection));
end;

function tcpurecorddef.getcopy: tstoreddef;
begin
  Result:=inherited getcopy;
  tcpurecorddef(result).symsection:=symsection;
end;

function tcpurecorddef.GetTypeName: string;
begin
  result:=inherited GetTypeName;
  case symsection of
    ss_eeprom: result:=result+'.EEPROM';
    ss_progmem: result:=result+'.PROGMEM';
  else
    ;
  end;
end;

procedure tcpupointerdef.ppuload_platform(ppufile:tcompilerppufile);
begin
  inherited ppuload_platform(ppufile);
  symsection:=tsymsection(ppufile.getbyte);
end;

procedure tcpupointerdef.ppuwrite_platform(ppufile:tcompilerppufile);
begin
  inherited ppuwrite_platform(ppufile);
  ppufile.putbyte(byte(symsection));
end;

constructor tcpupointerdef.create(def:tdef);
begin
  inherited create(def);
  symsection:=def.symsection;
end;

function tcpupointerdef.compatible_with_pointerdef_size(ptr:tpointerdef
  ): boolean;
begin
  result:=inherited and
    (tcpupointerdef(ptr).symsection=symsection);
end;

function tcpupointerdef.getcopy:tstoreddef;
begin
  result:=inherited getcopy;
  tcpupointerdef(result).symsection:=symsection;
end;

function tcpupointerdef.GetTypeName:string;
begin
  result:=inherited GetTypeName;
  case symsection of
    ss_eeprom: result:=result+'.EEPROM';
    ss_progmem: result:=result+'.PROGMEM';
  else
    ;
  end;
end;

begin
  { used tdef classes }
  cfiledef:=tcpufiledef;
  cvariantdef:=tcpuvariantdef;
  cformaldef:=tcpuformaldef;
  cforwarddef:=tcpuforwarddef;
  cundefineddef:=tcpuundefineddef;
  cerrordef:=tcpuerrordef;
  cpointerdef:=tcpupointerdef;
  crecorddef:=tcpurecorddef;
  cimplementedinterface:=tcpuimplementedinterface;
  cobjectdef:=tcpuobjectdef;
  cclassrefdef:=tcpuclassrefdef;
  carraydef:=tcpuarraydef;
  corddef:=tcpuorddef;
  cfloatdef:=tcpufloatdef;
  cprocvardef:=tcpuprocvardef;
  cprocdef:=tcpuprocdef;
  cstringdef:=tcpustringdef;
  cenumdef:=tcpuenumdef;
  csetdef:=tcpusetdef;

  { used tsym classes }
  clabelsym:=tcpulabelsym;
  cunitsym:=tcpuunitsym;
  cprogramparasym:=tcpuprogramparasym;
  cnamespacesym:=tcpunamespacesym;
  cprocsym:=tcpuprocsym;
  ctypesym:=tcputypesym;
  cfieldvarsym:=tcpufieldvarsym;
  clocalvarsym:=tcpulocalvarsym;
  cparavarsym:=tcpuparavarsym;
  cstaticvarsym:=tcpustaticvarsym;
  cabsolutevarsym:=tcpuabsolutevarsym;
  cpropertysym:=tcpupropertysym;
  cconstsym:=tcpuconstsym;
  cenumsym:=tcpuenumsym;
  csyssym:=tcpusyssym;
end.

