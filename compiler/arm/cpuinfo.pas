{
    Copyright (c) 1998-2002 by the Free Pascal development team

    Basic Processor information for the ARM

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Unit CPUInfo;

Interface

  uses
    globtype;

Type
   bestreal = double;
   ts32real = single;
   ts64real = double;
   ts80real = type extended;
   ts128real = type extended;
   ts64comp = comp;

   pbestreal=^bestreal;

   { possible supported processors for this target }
   tcputype =
      (cpu_none,
       cpu_armv3,
       cpu_armv4,
       cpu_armv4t,
       cpu_armv5,
       cpu_armv5t,
       cpu_armv5te,
       cpu_armv5tej,
       cpu_armv6,
       cpu_armv6k,
       cpu_armv6t2,
       cpu_armv6z,
       cpu_armv6m,
       cpu_armv7,
       cpu_armv7a,
       cpu_armv7r,
       cpu_armv7m,
       cpu_armv7em
      );

   tinstructionset = (is_thumb,is_arm);

Type
   tfputype =
     (fpu_none,
      fpu_soft,
      fpu_libgcc,
      fpu_fpa,
      fpu_fpa10,
      fpu_fpa11,
      fpu_vfpv2,
      fpu_vfpv3,
      fpu_vfpv3_d16,
      fpu_fpv4_s16
     );

   tcontrollertype =
     (ct_none,

      { Phillips }
      ct_lpc810m021fn8,
      ct_lpc811m001fdh16,
      ct_lpc812m101fdh16,
      ct_lpc812m101fd20,
      ct_lpc812m101fdh20,
      ct_lpc1110fd20,
      ct_lpc1111fdh20_002,
      ct_lpc1111fhn33_101,
      ct_lpc1111fhn33_102,
      ct_lpc1111fhn33_103,
      ct_lpc1111fhn33_201,
      ct_lpc1111fhn33_202,
      ct_lpc1111fhn33_203,
      ct_lpc1112fd20_102,
      ct_lpc1112fdh20_102,
      ct_lpc1112fdh28_102,
      ct_lpc1112fhn33_101,
      ct_lpc1112fhn33_102,
      ct_lpc1112fhn33_103,
      ct_lpc1112fhn33_201,
      ct_lpc1112fhn24_202,
      ct_lpc1112fhn33_202,
      ct_lpc1112fhn33_203,
      ct_lpc1112fhi33_202,
      ct_lpc1112fhi33_203,
      ct_lpc1113fhn33_201,
      ct_lpc1113fhn33_202,
      ct_lpc1113fhn33_203,
      ct_lpc1113fhn33_301,
      ct_lpc1113fhn33_302,
      ct_lpc1113fhn33_303,
      ct_lpc1113bfd48_301,
      ct_lpc1113bfd48_302,
      ct_lpc1113bfd48_303,
      ct_lpc1114fdh28_102,
      ct_lpc1114fn28_102,
      ct_lpc1114fhn33_201,
      ct_lpc1114fhn33_202,
      ct_lpc1114fhn33_203,
      ct_lpc1114fhn33_301,
      ct_lpc1114fhn33_302,
      ct_lpc1114fhn33_303,
      ct_lpc1114fhn33_333,
      ct_lpc1114fhi33_302,
      ct_lpc1114fhi33_303,
      ct_lpc1114bfd48_301,
      ct_lpc1114bfd48_302,
      ct_lpc1114bfd48_303,
      ct_lpc1114bfd48_323,
      ct_lpc1114bfd48_333,
      ct_lpc1115bfd48_303,
      ct_lpc11c12fd48_301,
      ct_lpc11c14fd48_301,
      ct_lpc11c22fd48_301,
      ct_lpc11c24fd48_301,
      ct_lpc11d24fd48_301,
      ct_lpc1224fbd48_101,
      ct_lpc1224fbd48_121,
      ct_lpc1224fbd64_101,
      ct_lpc1224fbd64_121,
      ct_lpc1225fbd48_301,
      ct_lpc1225fbd48_321,
      ct_lpc1225fbd64_301,
      ct_lpc1225fbd64_321,
      ct_lpc1226fbd48_301,
      ct_lpc1226fbd64_301,
      ct_lpc1227fbd48_301,
      ct_lpc1227fbd64_301,
      ct_lpc12d27fbd100_301,
      ct_lpc1311fhn33,
      ct_lpc1311fhn33_01,
      ct_lpc1313fhn33,
      ct_lpc1313fhn33_01,
      ct_lpc1313fbd48,
      ct_lpc1313fbd48_01,
      ct_lpc1315fhn33,
      ct_lpc1315fbd48,
      ct_lpc1316fhn33,
      ct_lpc1316fbd48,
      ct_lpc1317fhn33,
      ct_lpc1317fbd48,
      ct_lpc1317fbd64,
      ct_lpc1342fhn33,
      ct_lpc1342fbd48,
      ct_lpc1343fhn33,
      ct_lpc1343fbd48,
      ct_lpc1345fhn33,
      ct_lpc1345fbd48,
      ct_lpc1346fhn33,
      ct_lpc1346fbd48,
      ct_lpc1347fhn33,
      ct_lpc1347fbd48,
      ct_lpc1347fbd64,

      ct_lpc2114,
      ct_lpc2124,
      ct_lpc2194,
      ct_lpc1754,
      ct_lpc1756,
      ct_lpc1758,
      ct_lpc1764,
      ct_lpc1766,
      ct_lpc1768,

      { ATMEL }
      ct_at91sam7s256,
      ct_at91sam7se256,
      ct_at91sam7x256,
      ct_at91sam7xc256,

      { STMicroelectronics }
      ct_stm32f100x4, // LD&MD value line, 4=16,6=32,8=64,b=128
      ct_stm32f100x6,
      ct_stm32f100x8,
      ct_stm32f100xB,
      ct_stm32f100xC, // HD value line, r=512,d=384,c=256
      ct_stm32f100xD,
      ct_stm32f100xE,
      ct_stm32f101x4, // LD Access line, 4=16,6=32
      ct_stm32f101x6,
      ct_stm32f101x8, // MD Access line, 8=64,B=128
      ct_stm32f101xB,
      ct_stm32f101xC, // HD Access line, C=256,D=384,E=512
      ct_stm32f101xD,
      ct_stm32f101xE,
      ct_stm32f101xF, // XL Access line, F=768,G=1M
      ct_stm32f101xG,
      ct_stm32f102x4, // LD usb access line, 4=16,6=32
      ct_stm32f102x6,
      ct_stm32f102x8, // MD usb access line, 8=64,B=128
      ct_stm32f102xB,
      ct_stm32f103x4, // LD performance line, 4=16,6=32
      ct_stm32f103x6,
      ct_stm32f103x8, // MD performance line, 8=64,B=128
      ct_stm32f103xB,
      ct_stm32f103xC, // HD performance line, C=256,D=384,E=512
      ct_stm32f103xD,
      ct_stm32f103xE,
      ct_stm32f103xF, // XL performance line, F=768,G=1M
      ct_stm32f103xG,
      ct_stm32f107x8, // MD and HD connectivity line, 8=64,B=128,C=256
      ct_stm32f107xB,
      ct_stm32f107xC,

      { TI - Fury Class - 64 K Flash, 16 K SRAM Devices }
      ct_lm3s1110,
      ct_lm3s1133,
      ct_lm3s1138,
      ct_lm3s1150,
      ct_lm3s1162,
      ct_lm3s1165,
      ct_lm3s1166,
      ct_lm3s2110,
      ct_lm3s2139,
      ct_lm3s6100,
      ct_lm3s6110,

      { TI - Fury Class - 128K Flash, 32K SRAM devices }
      ct_lm3s1601,
      ct_lm3s1608,
      ct_lm3s1620,
      ct_lm3s1635,
      ct_lm3s1636,
      ct_lm3s1637,
      ct_lm3s1651,
      ct_lm3s2601,
      ct_lm3s2608,
      ct_lm3s2620,
      ct_lm3s2637,
      ct_lm3s2651,
      ct_lm3s6610,
      ct_lm3s6611,
      ct_lm3s6618,
      ct_lm3s6633,
      ct_lm3s6637,
      ct_lm3s8630,

      { TI - Fury Class - 256K Flash, 64K SRAM devices }
      ct_lm3s1911,
      ct_lm3s1918,
      ct_lm3s1937,
      ct_lm3s1958,
      ct_lm3s1960,
      ct_lm3s1968,
      ct_lm3s1969,
      ct_lm3s2911,
      ct_lm3s2918,
      ct_lm3s2919,
      ct_lm3s2939,
      ct_lm3s2948,
      ct_lm3s2950,
      ct_lm3s2965,
      ct_lm3s6911,
      ct_lm3s6918,
      ct_lm3s6938,
      ct_lm3s6950,
      ct_lm3s6952,
      ct_lm3s6965,
      ct_lm3s8930,
      ct_lm3s8933,
      ct_lm3s8938,
      ct_lm3s8962,
      ct_lm3s8970,
      ct_lm3s8971,

      { TI - Tempest Tempest - 256 K Flash, 64 K SRAM }
      ct_lm3s5951,
      ct_lm3s5956,
      ct_lm3s1b21,
      ct_lm3s2b93,
      ct_lm3s5b91,
      ct_lm3s9b81,
      ct_lm3s9b90,
      ct_lm3s9b92,
      ct_lm3s9b95,
      ct_lm3s9b96,

      ct_lm3s5d51,

      { TI Stellaris }
      ct_lm4f120h5,
      
      { SAMSUNG }
      ct_sc32442b,
      
      { Infineon }
      ct_xmc4500x1024,
      ct_xmc4500x768,
      ct_xmc4502x768,
      ct_xmc4504x512,

      // generic Thumb2 target
      ct_thumb2bare
     );


Const
   {# Size of native extended floating point type }
   extended_size = 12;
   {# Size of a multimedia register               }
   mmreg_size = 16;
   { target cpu string (used by compiler options) }
   target_cpu_string = 'arm';

   { calling conventions supported by the code generator }
   supported_calling_conventions : tproccalloptions = [
     pocall_internproc,
     pocall_safecall,
     pocall_stdcall,
     { same as stdcall only different name mangling }
     pocall_cdecl,
     { same as stdcall only different name mangling }
     pocall_cppdecl,
     { same as stdcall but floating point numbers are handled like equal sized integers }
     pocall_softfloat,
     { same as stdcall (requires that all const records are passed by
       reference, but that's already done for stdcall) }
     pocall_mwpascal,
     { used for interrupt handling }
     pocall_interrupt
   ];

   cputypestr : array[tcputype] of string[8] = ('',
     'ARMV3',
     'ARMV4',
     'ARMV4T',
     'ARMV5',
     'ARMV5T',
     'ARMV5TE',
     'ARMV5TEJ',
     'ARMV6',
     'ARMV6K',
     'ARMV6T2',
     'ARMV6Z',
     'ARMV6M',
     'ARMV7',
     'ARMV7A',
     'ARMV7R',
     'ARMV7M',
     'ARMV7EM'
   );

   fputypestr : array[tfputype] of string[9] = ('',
     'SOFT',
     'LIBGCC',
     'FPA',
     'FPA10',
     'FPA11',
     'VFPV2',
     'VFPV3',
     'VFPV3_D16',
     'FPV4_S16'
   );


    { We know that there are fields after sramsize
      but we don't care about this warning }
    {$WARN 3177 OFF}

   embedded_controllers : array [tcontrollertype] of tcontrollerdatatype =
   (
      (controllertypestr:'';		controllerunitstr:'';	flashbase:0;	flashsize:0;	srambase:0;	sramsize:0),

      { LPC 8xx Series}
      (controllertypestr:'LPC810M021FN8';	controllerunitstr:'LPC8xx';	flashbase:$00000000;	flashsize:$00001000;	srambase:$10000000;	sramsize:$00000400),
      (controllertypestr:'LPC811M001FDH16';	controllerunitstr:'LPC8xx';	flashbase:$00000000;	flashsize:$00002000;	srambase:$10000000;	sramsize:$00000800),
      (controllertypestr:'LPC812M101FDH16';	controllerunitstr:'LPC8xx';	flashbase:$00000000;	flashsize:$00004000;	srambase:$10000000;	sramsize:$00001000),
      (controllertypestr:'LPC812M101FD20';	controllerunitstr:'LPC8xx';	flashbase:$00000000;	flashsize:$00004000;	srambase:$10000000;	sramsize:$00001000),
      (controllertypestr:'LPC812M101FDH20';	controllerunitstr:'LPC8xx';	flashbase:$00000000;	flashsize:$00004000;	srambase:$10000000;	sramsize:$00001000),

      { LPC 11xx Series}
      (controllertypestr:'LPC1110FD20';		controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00001000;	srambase:$10000000;	sramsize:$00000400),
      (controllertypestr:'LPC1111FDH20/002';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00002000;	srambase:$10000000;	sramsize:$00000800),
      (controllertypestr:'LPC1111FHN33/101';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00002000;	srambase:$10000000;	sramsize:$00000800),
      (controllertypestr:'LPC1111FHN33/102';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00002000;	srambase:$10000000;	sramsize:$00000800),
      (controllertypestr:'LPC1111FHN33/103';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00002000;	srambase:$10000000;	sramsize:$00000800),
      (controllertypestr:'LPC1111FHN33/201';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00002000;	srambase:$10000000;	sramsize:$00001000),
      (controllertypestr:'LPC1111FHN33/202';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00002000;	srambase:$10000000;	sramsize:$00001000),
      (controllertypestr:'LPC1111FHN33/203';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00002000;	srambase:$10000000;	sramsize:$00001000),

      (controllertypestr:'LPC1112FD20/102';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00004000;	srambase:$10000000;	sramsize:$00001000),
      (controllertypestr:'LPC1112FDH20/102';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00004000;	srambase:$10000000;	sramsize:$00001000),
      (controllertypestr:'LPC1112FDH28/102';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00004000;	srambase:$10000000;	sramsize:$00001000),
      (controllertypestr:'LPC1112FHN33/101';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00004000;	srambase:$10000000;	sramsize:$00000800),
      (controllertypestr:'LPC1112FHN33/102';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00004000;	srambase:$10000000;	sramsize:$00000800),
      (controllertypestr:'LPC1112FHN33/103';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00004000;	srambase:$10000000;	sramsize:$00000800),
      (controllertypestr:'LPC1112FHN33/201';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00004000;	srambase:$10000000;	sramsize:$00001000),
      (controllertypestr:'LPC1112FHN24/202';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00004000;	srambase:$10000000;	sramsize:$00001000),
      (controllertypestr:'LPC1112FHN33/202';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00004000;	srambase:$10000000;	sramsize:$00001000),
      (controllertypestr:'LPC1112FHN33/203';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00004000;	srambase:$10000000;	sramsize:$00001000),
      (controllertypestr:'LPC1112FHI33/202';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00004000;	srambase:$10000000;	sramsize:$00001000),
      (controllertypestr:'LPC1112FHI33/203';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00004000;	srambase:$10000000;	sramsize:$00001000),

      (controllertypestr:'LPC1113FHN33/201';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00006000;	srambase:$10000000;	sramsize:$00001000),
      (controllertypestr:'LPC1113FHN33/202';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00006000;	srambase:$10000000;	sramsize:$00001000),
      (controllertypestr:'LPC1113FHN33/203';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00006000;	srambase:$10000000;	sramsize:$00001000),
      (controllertypestr:'LPC1113FHN33/301';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00006000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC1113FHN33/302';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00006000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC1113FHN33/303';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00006000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC1113FBD48/301';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00006000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC1113FBD48/302';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00006000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC1113FBD48/303';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00006000;	srambase:$10000000;	sramsize:$00002000),

      (controllertypestr:'LPC1114FDH28/102';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00008000;	srambase:$10000000;	sramsize:$00001000),
      (controllertypestr:'LPC1114FN28/102';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00008000;	srambase:$10000000;	sramsize:$00001000),
      (controllertypestr:'LPC1114FHN33/201';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00008000;	srambase:$10000000;	sramsize:$00001000),
      (controllertypestr:'LPC1114FHN33/202';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00008000;	srambase:$10000000;	sramsize:$00001000),
      (controllertypestr:'LPC1114FHN33/203';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00008000;	srambase:$10000000;	sramsize:$00001000),
      (controllertypestr:'LPC1114FHN33/301';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00008000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC1114FHN33/302';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00008000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC1114FHN33/303';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00008000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC1114FHN33/333';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$0000E000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC1114FHI33/302';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00008000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC1114FHI33/303';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00008000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC1114FBD48/301';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00008000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC1114FBD48/302';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00008000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC1114FBD48/303';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00008000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC1114FBD48/323';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$0000C000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC1114FBD48/333';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$0000E000;	srambase:$10000000;	sramsize:$00002000),

      (controllertypestr:'LPC1115FBD48/303';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00010000;	srambase:$10000000;	sramsize:$00002000),

      (controllertypestr:'LPC11C12FBD48/301';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00004000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC11C14FBD48/301';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00008000;	srambase:$10000000;	sramsize:$00002000),

      (controllertypestr:'LPC11C22FBD48/301';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00004000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC11C24FBD48/301';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00008000;	srambase:$10000000;	sramsize:$00002000),

      (controllertypestr:'LPC11D14FBD100/302';	controllerunitstr:'LPC11XX';	flashbase:$00000000;	flashsize:$00008000;	srambase:$10000000;	sramsize:$00002000),

      {LPC 122x Series}
      (controllertypestr:'LPC1224FBD48/101';	controllerunitstr:'LPC122X';	flashbase:$00000000;	flashsize:$00008000;	srambase:$10000000;	sramsize:$00001000),
      (controllertypestr:'LPC1224FBD48/121';	controllerunitstr:'LPC122X';	flashbase:$00000000;	flashsize:$0000C000;	srambase:$10000000;	sramsize:$00001000),
      (controllertypestr:'LPC1224FBD64/101';	controllerunitstr:'LPC122X';	flashbase:$00000000;	flashsize:$00008000;	srambase:$10000000;	sramsize:$00001000),
      (controllertypestr:'LPC1224FBD64/121';	controllerunitstr:'LPC122X';	flashbase:$00000000;	flashsize:$0000C000;	srambase:$10000000;	sramsize:$00001000),

      (controllertypestr:'LPC1225FBD48/301';	controllerunitstr:'LPC122X';	flashbase:$00000000;	flashsize:$00010000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC1225FBD48/321';	controllerunitstr:'LPC122X';	flashbase:$00000000;	flashsize:$00014000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC1225FBD64/301';	controllerunitstr:'LPC122X';	flashbase:$00000000;	flashsize:$00010000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC1225FBD64/321';	controllerunitstr:'LPC122X';	flashbase:$00000000;	flashsize:$00014000;	srambase:$10000000;	sramsize:$00002000),

      (controllertypestr:'LPC1226FBD48/301';	controllerunitstr:'LPC122X';	flashbase:$00000000;	flashsize:$00018000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC1226FBD64/301';	controllerunitstr:'LPC122X';	flashbase:$00000000;	flashsize:$00018000;	srambase:$10000000;	sramsize:$00002000),

      (controllertypestr:'LPC1227FBD48/301';	controllerunitstr:'LPC122X';	flashbase:$00000000;	flashsize:$00020000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC1227FBD64/301';	controllerunitstr:'LPC122X';	flashbase:$00000000;	flashsize:$00020000;	srambase:$10000000;	sramsize:$00002000),

      (controllertypestr:'LPC12D27FBD100/301';	controllerunitstr:'LPC122X';	flashbase:$00000000;	flashsize:$00020000;	srambase:$10000000;	sramsize:$00002000),


      (controllertypestr:'LPC1311FHN33';	controllerunitstr:'LPC13XX';	flashbase:$00000000;	flashsize:$00002000;	srambase:$10000000;	sramsize:$00001000),
      (controllertypestr:'LPC1311FHN33/01';	controllerunitstr:'LPC13XX';	flashbase:$00000000;	flashsize:$00002000;	srambase:$10000000;	sramsize:$00001000),

      (controllertypestr:'LPC1313FHN33';	controllerunitstr:'LPC13XX';	flashbase:$00000000;	flashsize:$00008000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC1313FHN33/01';	controllerunitstr:'LPC13XX';	flashbase:$00000000;	flashsize:$00008000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC1313FBD48';	controllerunitstr:'LPC13XX';	flashbase:$00000000;	flashsize:$00008000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC1313FBD48/01';	controllerunitstr:'LPC13XX';	flashbase:$00000000;	flashsize:$00008000;	srambase:$10000000;	sramsize:$00002000),

      (controllertypestr:'LPC1315FHN33';	controllerunitstr:'LPC13XX';	flashbase:$00000000;	flashsize:$00008000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC1315FBD48';	controllerunitstr:'LPC13XX';	flashbase:$00000000;	flashsize:$00008000;	srambase:$10000000;	sramsize:$00002000),

      (controllertypestr:'LPC1316FHN33';	controllerunitstr:'LPC13XX';	flashbase:$00000000;	flashsize:$0000C000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC1316FBD48';	controllerunitstr:'LPC13XX';	flashbase:$00000000;	flashsize:$0000C000;	srambase:$10000000;	sramsize:$00002000),

      (controllertypestr:'LPC1317FHN33';	controllerunitstr:'LPC13XX';	flashbase:$00000000;	flashsize:$00010000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC1317FBD48';	controllerunitstr:'LPC13XX';	flashbase:$00000000;	flashsize:$00010000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC1317FBD64';	controllerunitstr:'LPC13XX';	flashbase:$00000000;	flashsize:$00010000;	srambase:$10000000;	sramsize:$00002000),

      (controllertypestr:'LPC1342FHN33';	controllerunitstr:'LPC13XX';	flashbase:$00000000;	flashsize:$00004000;	srambase:$10000000;	sramsize:$00001000),
      (controllertypestr:'LPC1342FBD48';	controllerunitstr:'LPC13XX';	flashbase:$00000000;	flashsize:$00004000;	srambase:$10000000;	sramsize:$00001000),

      (controllertypestr:'LPC1343FHN33';	controllerunitstr:'LPC13XX';	flashbase:$00000000;	flashsize:$00008000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC1343FBD48';	controllerunitstr:'LPC13XX';	flashbase:$00000000;	flashsize:$00008000;	srambase:$10000000;	sramsize:$00002000),

      (controllertypestr:'LPC1345FHN33';	controllerunitstr:'LPC13XX';	flashbase:$00000000;	flashsize:$00008000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC1345FBD48';	controllerunitstr:'LPC13XX';	flashbase:$00000000;	flashsize:$00008000;	srambase:$10000000;	sramsize:$00002000),

      (controllertypestr:'LPC1346FHN33';	controllerunitstr:'LPC13XX';	flashbase:$00000000;	flashsize:$0000C000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC1346FBD48';	controllerunitstr:'LPC13XX';	flashbase:$00000000;	flashsize:$0000C000;	srambase:$10000000;	sramsize:$00002000),

      (controllertypestr:'LPC1347FHN33';	controllerunitstr:'LPC13XX';	flashbase:$00000000;	flashsize:$00010000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC1347FBD48';	controllerunitstr:'LPC13XX';	flashbase:$00000000;	flashsize:$00010000;	srambase:$10000000;	sramsize:$00002000),
      (controllertypestr:'LPC1347FBD64';	controllerunitstr:'LPC13XX';	flashbase:$00000000;	flashsize:$00010000;	srambase:$10000000;	sramsize:$00002000),

      (controllertypestr:'LPC2114';	controllerunitstr:'LPC21x4';	flashbase:$00000000;	flashsize:$00040000;	srambase:$40000000;	sramsize:$00004000),
      (controllertypestr:'LPC2124';	controllerunitstr:'LPC21x4';	flashbase:$00000000;	flashsize:$00040000;	srambase:$40000000;	sramsize:$00004000),
      (controllertypestr:'LPC2194';	controllerunitstr:'LPC21x4';	flashbase:$00000000;	flashsize:$00040000;	srambase:$40000000;	sramsize:$00004000),
      (controllertypestr:'LPC1754';	controllerunitstr:'LPC1754';	flashbase:$00000000;	flashsize:$00020000;	srambase:$10000000;	sramsize:$00004000),
      (controllertypestr:'LPC1756';	controllerunitstr:'LPC1756';	flashbase:$00000000;	flashsize:$00040000;	srambase:$10000000;	sramsize:$00004000),
      (controllertypestr:'LPC1758';	controllerunitstr:'LPC1758';	flashbase:$00000000;	flashsize:$00080000;	srambase:$10000000;	sramsize:$00008000),
      (controllertypestr:'LPC1764';	controllerunitstr:'LPC1764';	flashbase:$00000000;	flashsize:$00020000;	srambase:$10000000;	sramsize:$00004000),
      (controllertypestr:'LPC1766';	controllerunitstr:'LPC1766';	flashbase:$00000000;	flashsize:$00040000;	srambase:$10000000;	sramsize:$00008000),
      (controllertypestr:'LPC1768';	controllerunitstr:'LPC1768';	flashbase:$00000000;	flashsize:$00080000;	srambase:$10000000;	sramsize:$00008000),

      { AT91 }
      (controllertypestr:'AT91SAM7S256';	controllerunitstr:'AT91SAM7x256';	flashbase:$00000000;	flashsize:$00040000;	srambase:$00200000;	sramsize:$00010000),
      (controllertypestr:'AT91SAM7SE256';	controllerunitstr:'AT91SAM7x256';	flashbase:$00000000;	flashsize:$00040000;	srambase:$00200000;	sramsize:$00010000),
      (controllertypestr:'AT91SAM7X256';	controllerunitstr:'AT91SAM7x256';	flashbase:$00000000;	flashsize:$00040000;	srambase:$00200000;	sramsize:$00010000),
      (controllertypestr:'AT91SAM7XC256';	controllerunitstr:'AT91SAM7x256';	flashbase:$00000000;	flashsize:$00040000;	srambase:$00200000;	sramsize:$00010000),

      { STM32F1 series }
      (controllertypestr:'STM32F100X4';     controllerunitstr:'STM32F10X_LD';     flashbase:$08000000; flashsize:$00004000; srambase:$20000000; sramsize:$00001000),
      (controllertypestr:'STM32F100X6';     controllerunitstr:'STM32F10X_LD';     flashbase:$08000000; flashsize:$00008000; srambase:$20000000; sramsize:$00001000),
      (controllertypestr:'STM32F100X8';     controllerunitstr:'STM32F10X_MD';     flashbase:$08000000; flashsize:$00010000; srambase:$20000000; sramsize:$00002000),
      (controllertypestr:'STM32F100XB';     controllerunitstr:'STM32F10X_MD';     flashbase:$08000000; flashsize:$00020000; srambase:$20000000; sramsize:$00002000),
      (controllertypestr:'STM32F100XC';     controllerunitstr:'STM32F10X_HD';     flashbase:$08000000; flashsize:$00040000; srambase:$20000000; sramsize:$00006000),
      (controllertypestr:'STM32F100XD';     controllerunitstr:'STM32F10X_HD';     flashbase:$08000000; flashsize:$00060000; srambase:$20000000; sramsize:$00008000),
      (controllertypestr:'STM32F100XE';     controllerunitstr:'STM32F10X_HD';     flashbase:$08000000; flashsize:$00080000; srambase:$20000000; sramsize:$00008000),
      (controllertypestr:'STM32F101X4';     controllerunitstr:'STM32F10X_LD';     flashbase:$08000000; flashsize:$00004000; srambase:$20000000; sramsize:$00001000),
      (controllertypestr:'STM32F101X6';     controllerunitstr:'STM32F10X_LD';     flashbase:$08000000; flashsize:$00008000; srambase:$20000000; sramsize:$00001800),
      (controllertypestr:'STM32F101X8';     controllerunitstr:'STM32F10X_MD';     flashbase:$08000000; flashsize:$00010000; srambase:$20000000; sramsize:$00002800),
      (controllertypestr:'STM32F101XB';     controllerunitstr:'STM32F10X_MD';     flashbase:$08000000; flashsize:$00020000; srambase:$20000000; sramsize:$00004000),
      (controllertypestr:'STM32F101XC';     controllerunitstr:'STM32F10X_HD';     flashbase:$08000000; flashsize:$00040000; srambase:$20000000; sramsize:$00008000),
      (controllertypestr:'STM32F101XD';     controllerunitstr:'STM32F10X_HD';     flashbase:$08000000; flashsize:$00060000; srambase:$20000000; sramsize:$0000C000),
      (controllertypestr:'STM32F101XE';     controllerunitstr:'STM32F10X_HD';     flashbase:$08000000; flashsize:$00080000; srambase:$20000000; sramsize:$0000C000),
      (controllertypestr:'STM32F101XF';     controllerunitstr:'STM32F10X_XL';     flashbase:$08000000; flashsize:$000C0000; srambase:$20000000; sramsize:$00014000),
      (controllertypestr:'STM32F101XG';     controllerunitstr:'STM32F10X_XL';     flashbase:$08000000; flashsize:$00100000; srambase:$20000000; sramsize:$00014000),
      (controllertypestr:'STM32F102X4';     controllerunitstr:'STM32F10X_LD';     flashbase:$08000000; flashsize:$00004000; srambase:$20000000; sramsize:$00001000),
      (controllertypestr:'STM32F102X6';     controllerunitstr:'STM32F10X_LD';     flashbase:$08000000; flashsize:$00008000; srambase:$20000000; sramsize:$00001800),
      (controllertypestr:'STM32F102X8';     controllerunitstr:'STM32F10X_MD';     flashbase:$08000000; flashsize:$00010000; srambase:$20000000; sramsize:$00002800),
      (controllertypestr:'STM32F102XB';     controllerunitstr:'STM32F10X_MD';     flashbase:$08000000; flashsize:$00020000; srambase:$20000000; sramsize:$00004000),
      (controllertypestr:'STM32F103X4';     controllerunitstr:'STM32F10X_LD';     flashbase:$08000000; flashsize:$00004000; srambase:$20000000; sramsize:$00001000),
      (controllertypestr:'STM32F103X6';     controllerunitstr:'STM32F10X_LD';     flashbase:$08000000; flashsize:$00008000; srambase:$20000000; sramsize:$00002800),
      (controllertypestr:'STM32F103X8';     controllerunitstr:'STM32F10X_MD';     flashbase:$08000000; flashsize:$00010000; srambase:$20000000; sramsize:$00005000),
      (controllertypestr:'STM32F103XB';     controllerunitstr:'STM32F10X_MD';     flashbase:$08000000; flashsize:$00020000; srambase:$20000000; sramsize:$00005000),
      (controllertypestr:'STM32F103XC';     controllerunitstr:'STM32F10X_HD';     flashbase:$08000000; flashsize:$00040000; srambase:$20000000; sramsize:$0000C000),
      (controllertypestr:'STM32F103XD';     controllerunitstr:'STM32F10X_HD';     flashbase:$08000000; flashsize:$00060000; srambase:$20000000; sramsize:$00010000),
      (controllertypestr:'STM32F103XE';     controllerunitstr:'STM32F10X_HD';     flashbase:$08000000; flashsize:$00080000; srambase:$20000000; sramsize:$00010000),
      (controllertypestr:'STM32F103XF';     controllerunitstr:'STM32F10X_XL';     flashbase:$08000000; flashsize:$000C0000; srambase:$20000000; sramsize:$00018000),
      (controllertypestr:'STM32F103XG';     controllerunitstr:'STM32F10X_XL';     flashbase:$08000000; flashsize:$00100000; srambase:$20000000; sramsize:$00018000),
      (controllertypestr:'STM32F107X8';     controllerunitstr:'STM32F10X_CONN';   flashbase:$08000000; flashsize:$00010000; srambase:$20000000; sramsize:$00010000),
      (controllertypestr:'STM32F107XB';     controllerunitstr:'STM32F10X_CONN';   flashbase:$08000000; flashsize:$00020000; srambase:$20000000; sramsize:$00010000),
      (controllertypestr:'STM32F107XC';     controllerunitstr:'STM32F10X_CONN';   flashbase:$08000000; flashsize:$00040000; srambase:$20000000; sramsize:$00010000),

      (controllertypestr:'LM3S1110';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00010000;	srambase:$20000000;	sramsize:$00004000),
      (controllertypestr:'LM3S1133';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00010000;	srambase:$20000000;	sramsize:$00004000),
      (controllertypestr:'LM3S1138';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00010000;	srambase:$20000000;	sramsize:$00004000),
      (controllertypestr:'LM3S1150';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00010000;	srambase:$20000000;	sramsize:$00004000),
      (controllertypestr:'LM3S1162';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00010000;	srambase:$20000000;	sramsize:$00004000),
      (controllertypestr:'LM3S1165';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00010000;	srambase:$20000000;	sramsize:$00004000),
      (controllertypestr:'LM3S1166';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00010000;	srambase:$20000000;	sramsize:$00004000),
      (controllertypestr:'LM3S2110';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00010000;	srambase:$20000000;	sramsize:$00004000),
      (controllertypestr:'LM3S2139';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00010000;	srambase:$20000000;	sramsize:$00004000),
      (controllertypestr:'LM3S6100';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00010000;	srambase:$20000000;	sramsize:$00004000),
      (controllertypestr:'LM3S6110';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00010000;	srambase:$20000000;	sramsize:$00004000),

      { TI - 128K Flash, 32K SRAM devices }
      (controllertypestr:'LM3S1601';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00020000;	srambase:$20000000;	sramsize:$00008000),
      (controllertypestr:'LM3S1608';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00020000;	srambase:$20000000;	sramsize:$00008000),
      (controllertypestr:'LM3S1620';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00020000;	srambase:$20000000;	sramsize:$00008000),
      (controllertypestr:'LM3S1635';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00020000;	srambase:$20000000;	sramsize:$00008000),
      (controllertypestr:'LM3S1636';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00020000;	srambase:$20000000;	sramsize:$00008000),
      (controllertypestr:'LM3S1637';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00020000;	srambase:$20000000;	sramsize:$00008000),
      (controllertypestr:'LM3S1651';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00020000;	srambase:$20000000;	sramsize:$00008000),
      (controllertypestr:'LM3S2601';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00020000;	srambase:$20000000;	sramsize:$00008000),
      (controllertypestr:'LM3S2608';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00020000;	srambase:$20000000;	sramsize:$00008000),
      (controllertypestr:'LM3S2620';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00020000;	srambase:$20000000;	sramsize:$00008000),
      (controllertypestr:'LM3S2637';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00020000;	srambase:$20000000;	sramsize:$00008000),
      (controllertypestr:'LM3S2651';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00020000;	srambase:$20000000;	sramsize:$00008000),
      (controllertypestr:'LM3S6610';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00020000;	srambase:$20000000;	sramsize:$00008000),
      (controllertypestr:'LM3S6611';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00020000;	srambase:$20000000;	sramsize:$00008000),
      (controllertypestr:'LM3S6618';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00020000;	srambase:$20000000;	sramsize:$00008000),
      (controllertypestr:'LM3S6633';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00020000;	srambase:$20000000;	sramsize:$00008000),
      (controllertypestr:'LM3S6637';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00020000;	srambase:$20000000;	sramsize:$00008000),
      (controllertypestr:'LM3S8630';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00020000;	srambase:$20000000;	sramsize:$00008000),

      { TI - 256K Flash, 64K SRAM devices }
      (controllertypestr:'LM3S1911';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00010000),
      (controllertypestr:'LM3S1918';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00010000),
      (controllertypestr:'LM3S1937';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00010000),
      (controllertypestr:'LM3S1958';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00010000),
      (controllertypestr:'LM3S1960';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00010000),
      (controllertypestr:'LM3S1968';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00010000),
      (controllertypestr:'LM3S1969';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00010000),
      (controllertypestr:'LM3S2911';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00010000),
      (controllertypestr:'LM3S2918';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00010000),
      (controllertypestr:'LM3S2919';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00010000),
      (controllertypestr:'LM3S2939';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00010000),
      (controllertypestr:'LM3S2948';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00010000),
      (controllertypestr:'LM3S2950';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00010000),
      (controllertypestr:'LM3S2965';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00010000),
      (controllertypestr:'LM3S6911';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00010000),
      (controllertypestr:'LM3S6918';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00010000),
      (controllertypestr:'LM3S6938';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00010000),
      (controllertypestr:'LM3S6950';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00010000),
      (controllertypestr:'LM3S6952';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00010000),
      (controllertypestr:'LM3S6965';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00010000),
      (controllertypestr:'LM3S8930';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00010000),
      (controllertypestr:'LM3S8933';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00010000),
      (controllertypestr:'LM3S8938';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00010000),
      (controllertypestr:'LM3S8962';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00010000),
      (controllertypestr:'LM3S8970';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00010000),
      (controllertypestr:'LM3S8971';	controllerunitstr:'LM3FURY';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00010000),

      { TI - Tempest parts - up to 512 K Flash, 96 K SRAM }
      (controllertypestr:'LM3S5951';	controllerunitstr:'LM3TEMPEST';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00010000),
      (controllertypestr:'LM3S5956';	controllerunitstr:'LM3TEMPEST';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00010000),
      (controllertypestr:'LM3S1B21';	controllerunitstr:'LM3TEMPEST';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00018000),
      (controllertypestr:'LM3S2B93';	controllerunitstr:'LM3TEMPEST';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00018000),
      (controllertypestr:'LM3S5B91';	controllerunitstr:'LM3TEMPEST';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00018000),
      (controllertypestr:'LM3S9B81';	controllerunitstr:'LM3TEMPEST';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00018000),
      (controllertypestr:'LM3S9B90';	controllerunitstr:'LM3TEMPEST';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00018000),
      (controllertypestr:'LM3S9B92';	controllerunitstr:'LM3TEMPEST';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00018000),
      (controllertypestr:'LM3S9B95';	controllerunitstr:'LM3TEMPEST';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00018000),
      (controllertypestr:'LM3S9B96';	controllerunitstr:'LM3TEMPEST';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00018000),
      (controllertypestr:'LM3S5D51';	controllerunitstr:'LM3TEMPEST';	flashbase:$00000000;	flashsize:$00080000;	srambase:$20000000;	sramsize:$00018000),

      { TI }
      (controllertypestr:'LM4F120H5';	controllerunitstr:'LM4F120';	flashbase:$00000000;	flashsize:$00040000;	srambase:$20000000;	sramsize:$00008000),

      { Samsung }
      (controllertypestr:'SC32442B';	controllerunitstr:'SC32442b';	flashbase:$00000000;	flashsize:$00000000;	srambase:$00000000;	sramsize:$08000000),
      
      { Infinion }
      (controllertypestr:'XMC4500X1024';  controllerunitstr:'XMC4500'; flashbase:$08000000;	flashsize:$00100000;	srambase:$20000000;	sramsize:$00010000),
      (controllertypestr:'XMC4500X768';   controllerunitstr:'XMC4500'; flashbase:$08000000;	flashsize:$000C0000;	srambase:$20000000;	sramsize:$00010000),
      (controllertypestr:'XMC4502X768';   controllerunitstr:'XMC4502'; flashbase:$08000000;	flashsize:$000C0000;	srambase:$20000000;	sramsize:$00010000),
      (controllertypestr:'XMC4504X512';   controllerunitstr:'XMC4504'; flashbase:$08000000;	flashsize:$00080000;	srambase:$20000000;	sramsize:$00010000),

      { Bare bones }
      (controllertypestr:'THUMB2_BARE';	controllerunitstr:'THUMB2_BARE';	flashbase:$00000000;	flashsize:$00002000;	srambase:$20000000;	sramsize:$00000400)
    );

   vfp_scalar = [fpu_vfpv2,fpu_vfpv3,fpu_vfpv3_d16,fpu_fpv4_s16];

   { Supported optimizations, only used for information }
   supported_optimizerswitches = genericlevel1optimizerswitches+
                                 genericlevel2optimizerswitches+
                                 genericlevel3optimizerswitches-
                                 { no need to write info about those }
                                 [cs_opt_level1,cs_opt_level2,cs_opt_level3]+
                                 [cs_opt_regvar,cs_opt_loopunroll,cs_opt_tailrecursion,
				  cs_opt_stackframe,cs_opt_nodecse,cs_opt_reorder_fields,cs_opt_fastmath];

   level1optimizerswitches = genericlevel1optimizerswitches;
   level2optimizerswitches = genericlevel2optimizerswitches + level1optimizerswitches +
     [cs_opt_regvar,cs_opt_stackframe,cs_opt_tailrecursion,cs_opt_nodecse];
   level3optimizerswitches = genericlevel3optimizerswitches + level2optimizerswitches + [cs_opt_scheduler{,cs_opt_loopunroll}];
   level4optimizerswitches = genericlevel4optimizerswitches + level3optimizerswitches + [];

 type
   tcpuflags =
      (CPUARM_HAS_BX,         { CPU supports the BX instruction                           }
       CPUARM_HAS_BLX,        { CPU supports the BLX rX instruction                       }
       CPUARM_HAS_BLX_LABEL,  { CPU supports the BLX <label> instruction                  }
       CPUARM_HAS_CLZ,        { CPU supports the CLZ instruction                          }
       CPUARM_HAS_EDSP,       { CPU supports the PLD,STRD,LDRD,MCRR and MRRC instructions }
       CPUARM_HAS_REV,        { CPU supports the REV instruction                          }
       CPUARM_HAS_RBIT,       { CPU supports the RBIT instruction                         }
       CPUARM_HAS_DMB,        { CPU has memory barrier instructions (DMB, DSB, ISB)       }
       CPUARM_HAS_LDREX,
       CPUARM_HAS_IDIV,
       CPUARM_HAS_THUMB_IDIV,
       CPUARM_HAS_THUMB2
      );

 const
   cpu_capabilities : array[tcputype] of set of tcpuflags =
     ( { cpu_none     } [],
       { cpu_armv3    } [],
       { cpu_armv4    } [],
       { cpu_armv4t   } [CPUARM_HAS_BX],
       { cpu_armv5    } [CPUARM_HAS_BX,CPUARM_HAS_BLX,CPUARM_HAS_BLX_LABEL,CPUARM_HAS_CLZ],
       { cpu_armv5t   } [CPUARM_HAS_BX,CPUARM_HAS_BLX,CPUARM_HAS_BLX_LABEL,CPUARM_HAS_CLZ],
       { cpu_armv5te  } [CPUARM_HAS_BX,CPUARM_HAS_BLX,CPUARM_HAS_BLX_LABEL,CPUARM_HAS_CLZ,CPUARM_HAS_EDSP],
       { cpu_armv5tej } [CPUARM_HAS_BX,CPUARM_HAS_BLX,CPUARM_HAS_BLX_LABEL,CPUARM_HAS_CLZ,CPUARM_HAS_EDSP],
       { cpu_armv6    } [CPUARM_HAS_BX,CPUARM_HAS_BLX,CPUARM_HAS_BLX_LABEL,CPUARM_HAS_CLZ,CPUARM_HAS_EDSP,CPUARM_HAS_REV,CPUARM_HAS_LDREX],
       { cpu_armv6k   } [CPUARM_HAS_BX,CPUARM_HAS_BLX,CPUARM_HAS_BLX_LABEL,CPUARM_HAS_CLZ,CPUARM_HAS_EDSP,CPUARM_HAS_REV,CPUARM_HAS_LDREX],
       { cpu_armv6t2  } [CPUARM_HAS_BX,CPUARM_HAS_BLX,CPUARM_HAS_BLX_LABEL,CPUARM_HAS_CLZ,CPUARM_HAS_EDSP,CPUARM_HAS_REV,CPUARM_HAS_RBIT,CPUARM_HAS_LDREX,CPUARM_HAS_THUMB2],
       { cpu_armv6z   } [CPUARM_HAS_BX,CPUARM_HAS_BLX,CPUARM_HAS_BLX_LABEL,CPUARM_HAS_CLZ,CPUARM_HAS_EDSP,CPUARM_HAS_REV,CPUARM_HAS_LDREX],
       { cpu_armv6m   } [CPUARM_HAS_BX,CPUARM_HAS_BLX,CPUARM_HAS_REV],
       { the identifier armv7 is should not be used, it is considered being equal to armv7a }
       { cpu_armv7    } [CPUARM_HAS_BX,CPUARM_HAS_BLX,CPUARM_HAS_BLX_LABEL,CPUARM_HAS_CLZ,CPUARM_HAS_EDSP,CPUARM_HAS_REV,CPUARM_HAS_RBIT,CPUARM_HAS_LDREX,CPUARM_HAS_DMB,CPUARM_HAS_THUMB2],
       { cpu_armv7a   } [CPUARM_HAS_BX,CPUARM_HAS_BLX,CPUARM_HAS_BLX_LABEL,CPUARM_HAS_CLZ,CPUARM_HAS_EDSP,CPUARM_HAS_REV,CPUARM_HAS_RBIT,CPUARM_HAS_LDREX,CPUARM_HAS_DMB,CPUARM_HAS_THUMB2],
       { cpu_armv7r   } [CPUARM_HAS_BX,CPUARM_HAS_BLX,CPUARM_HAS_BLX_LABEL,CPUARM_HAS_CLZ,CPUARM_HAS_EDSP,CPUARM_HAS_REV,CPUARM_HAS_RBIT,CPUARM_HAS_LDREX,CPUARM_HAS_THUMB_IDIV,CPUARM_HAS_DMB,CPUARM_HAS_THUMB2],
       { cpu_armv7m   } [CPUARM_HAS_BX,CPUARM_HAS_BLX,CPUARM_HAS_CLZ,CPUARM_HAS_EDSP,CPUARM_HAS_REV,CPUARM_HAS_RBIT,CPUARM_HAS_LDREX,CPUARM_HAS_THUMB_IDIV,CPUARM_HAS_DMB,CPUARM_HAS_THUMB2],
       { cpu_armv7em  } [CPUARM_HAS_BX,CPUARM_HAS_BLX,CPUARM_HAS_CLZ,CPUARM_HAS_EDSP,CPUARM_HAS_REV,CPUARM_HAS_RBIT,CPUARM_HAS_LDREX,CPUARM_HAS_THUMB_IDIV,CPUARM_HAS_DMB,CPUARM_HAS_THUMB2]
     );

   { contains all CPU supporting any kind of thumb instruction set }
   cpu_has_thumb = [cpu_armv4t,cpu_armv5t,cpu_armv5te,cpu_armv5tej,cpu_armv6t2,cpu_armv6z,cpu_armv6m,cpu_armv7a,cpu_armv7r,cpu_armv7m,cpu_armv7em];

Implementation

end.

