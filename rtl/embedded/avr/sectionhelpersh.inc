{$mode objfpc}
// Check if EEPROM access is supported by controller
{$if declared (NVMCTRL) or declared(EECR)}
  procedure writeEEPROMbyte(const EEPROMaddress: pointer; const val: byte); public name 'FPC_WRITE_EEPROM_BYTE';
  function readEEPROMbyte(const EEPROMaddress: pointer): byte; public name 'FPC_READ_EEPROM_BYTE';
{$endif NVMCTRL or EECR}

