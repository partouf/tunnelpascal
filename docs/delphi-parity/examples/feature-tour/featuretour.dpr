(* tunnelpascal Delphi-parity feature tour — driver.

   Each unit demonstrates one group of features that upstream fpc trunk either
   rejects outright or requires an explicit modeswitch for.

   Delphi mode comes from -Mdelphi on the command line rather than a directive
   in every file. Build against a built fork tree, from the repo root:

     ./compiler/ppc3 -Mdelphi \
                     -Fu./rtl/units/x86_64-linux \
                     -Fu./packages/rtl-objpas/units/x86_64-linux \
                     -Fu./docs/delphi-parity/examples/feature-tour \
                     -FE<outdir> docs/delphi-parity/examples/feature-tour/featuretour.dpr

   Against a dotted-units build (the System.* release variant, which is what
   Compiler Explorer ships) add -dFPC_DOTTEDUNITS. The compiler does not define
   that itself, so tourrtti.pp keys its uses clause off it. *)

program featuretour;

uses
  tourdelphi13,
  tourgenerics,
  tourhelpers,
  tourclosures,
  tourrtti;

begin
  WriteLn('=== tunnelpascal Delphi-parity feature tour ===');
  tourdelphi13.Run;
  tourgenerics.Run;
  tourhelpers.Run;
  tourclosures.Run;
  tourrtti.Run;
end.
