    {$if declared (NVMCTRL)}
      {$push} {$optimization on}
      procedure writeEEPROMbyte(const EEPROMaddress: pointer; const val: byte);
      begin
        while (NVMCTRL.STATUS and 3 <> 0) do;
        PByte(EEPROMaddress + $1400)^ := val;
        CPU.CCP := $9D;
        // next line MUST finish within 4 clock cycles
        NVMCTRL.CTRLA := 3;
      end;

      // Not required by compiler to read EEPROM
      // Provided for compatibilty of user code with classic AVRs
      function readEEPROMbyte(const EEPROMaddress: pointer): byte;
      begin
        readEEPROMbyte := PByte(EEPROMaddress + $1400)^;
      end;
      {$pop}

    {$elseif declared(EECR)}
      {$push} {$optimization on}
      procedure writeEEPROMbyte(const EEPROMaddress: pointer; const val: byte);
      var
        oldsreg: byte;
      begin
        // Flash writes are unlikely but not impossible for a user program
        // Wait for both EEPROM write enable and SPM enable bits to clear before proceedng
        while (EECR and 2 <> 0) {$if declared(SPMCSR)} and (SPMCSR and 1 <> 0) {$endif declared SPMCSR} do;
        // An interrupt after writing to EEAR or EEDR which changes these two registers would corrupt this code
        oldsreg := avr_save;
        {$if declared(EEAR)}
        EEAR := word(EEPROMaddress);
        {$elseif declared(EEARL)}
        EEARL := byte(EEPROMaddress);
        {$endif EEAR}
        EEDR := val;
        // The next two writes to EECR should not be interrupted
        // as that will break the timing constraint
        EECR := 4;  // EEPROM master write enable
        EECR := 2;  // EEPROM write enable
        avr_restore(oldsreg);
      end;

      function readEEPROMbyte(const EEPROMaddress: pointer): byte;
      begin
        while (EECR and 2 <> 0) do;
        {$if declared(EEAR)}
        EEAR := word(EEPROMaddress);
        {$elseif declared(EEARL)}
        EEARL := byte(EEPROMaddress);
        {$endif EEAR}
        EECR := EECR or 1;
        readEEPROMbyte := EEDR;
      end;
      {$pop}
    {$endif}

