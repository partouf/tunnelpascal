unit ATmega8;

interface

var
  // ANALOG_COMPARATOR
  SFIOR : byte absolute $00+$50; // Special Function IO Register
  ACSR : byte absolute $00+$28; // Analog Comparator Control And Status Register
  // SPI
  SPDR : byte absolute $00+$2F; // SPI Data Register
  SPSR : byte absolute $00+$2E; // SPI Status Register
  SPCR : byte absolute $00+$2D; // SPI Control Register
  // EXTERNAL_INTERRUPT
  GICR : byte absolute $00+$5B; // General Interrupt Control Register
  GIFR : byte absolute $00+$5A; // General Interrupt Flag Register
  MCUCR : byte absolute $00+$55; // MCU Control Register
  // TIMER_COUNTER_0
  TIMSK : byte absolute $00+$59; // Timer/Counter Interrupt Mask Register
  TIFR : byte absolute $00+$58; // Timer/Counter Interrupt Flag register
  TCCR0 : byte absolute $00+$53; // Timer/Counter0 Control Register
  TCNT0 : byte absolute $00+$52; // Timer Counter 0
  // TIMER_COUNTER_1
  TCCR1A : byte absolute $00+$4F; // Timer/Counter1 Control Register A
  TCCR1B : byte absolute $00+$4E; // Timer/Counter1 Control Register B
  TCNT1 : word absolute $00+$4C; // Timer/Counter1  Bytes
  TCNT1L : byte absolute $00+$4C; // Timer/Counter1  Bytes
  TCNT1H : byte absolute $00+$4C+1; // Timer/Counter1  Bytes
  OCR1A : word absolute $00+$4A; // Timer/Counter1 Output Compare Register  Bytes
  OCR1AL : byte absolute $00+$4A; // Timer/Counter1 Output Compare Register  Bytes
  OCR1AH : byte absolute $00+$4A+1; // Timer/Counter1 Output Compare Register  Bytes
  OCR1B : word absolute $00+$48; // Timer/Counter1 Output Compare Register  Bytes
  OCR1BL : byte absolute $00+$48; // Timer/Counter1 Output Compare Register  Bytes
  OCR1BH : byte absolute $00+$48+1; // Timer/Counter1 Output Compare Register  Bytes
  ICR1 : word absolute $00+$46; // Timer/Counter1 Input Capture Register  Bytes
  ICR1L : byte absolute $00+$46; // Timer/Counter1 Input Capture Register  Bytes
  ICR1H : byte absolute $00+$46+1; // Timer/Counter1 Input Capture Register  Bytes
  // TIMER_COUNTER_2
  TCCR2 : byte absolute $00+$45; // Timer/Counter2 Control Register
  TCNT2 : byte absolute $00+$44; // Timer/Counter2
  OCR2 : byte absolute $00+$43; // Timer/Counter2 Output Compare Register
  ASSR : byte absolute $00+$42; // Asynchronous Status Register
  // USART
  UDR : byte absolute $00+$2C; // USART I/O Data Register
  UCSRA : byte absolute $00+$2B; // USART Control and Status Register A
  UCSRB : byte absolute $00+$2A; // USART Control and Status Register B
  UCSRC : byte absolute $00+$40; // USART Control and Status Register C
  UBRRH : byte absolute $00+$40; // USART Baud Rate Register Hight Byte
  UBRRL : byte absolute $00+$29; // USART Baud Rate Register Low Byte
  // TWI
  TWBR : byte absolute $00+$20; // TWI Bit Rate register
  TWCR : byte absolute $00+$56; // TWI Control Register
  TWSR : byte absolute $00+$21; // TWI Status Register
  TWDR : byte absolute $00+$23; // TWI Data register
  TWAR : byte absolute $00+$22; // TWI (Slave) Address register
  // WATCHDOG
  WDTCR : byte absolute $00+$41; // Watchdog Timer Control Register
  // PORTB
  PORTB : byte absolute $00+$38; // Port B Data Register
  DDRB : byte absolute $00+$37; // Port B Data Direction Register
  PINB : byte absolute $00+$36; // Port B Input Pins
  // PORTC
  PORTC : byte absolute $00+$35; // Port C Data Register
  DDRC : byte absolute $00+$34; // Port C Data Direction Register
  PINC : byte absolute $00+$33; // Port C Input Pins
  // PORTD
  PORTD : byte absolute $00+$32; // Port D Data Register
  DDRD : byte absolute $00+$31; // Port D Data Direction Register
  PIND : byte absolute $00+$30; // Port D Input Pins
  // EEPROM
  EEAR : word absolute $00+$3E; // EEPROM Address Register  Bytes
  EEARL : byte absolute $00+$3E; // EEPROM Address Register  Bytes
  EEARH : byte absolute $00+$3E+1; // EEPROM Address Register  Bytes
  EEDR : byte absolute $00+$3D; // EEPROM Data Register
  EECR : byte absolute $00+$3C; // EEPROM Control Register
  // CPU
  SREG : byte absolute $00+$5F; // Status Register
  SP : word absolute $00+$5D; // Stack Pointer 
  SPL : byte absolute $00+$5D; // Stack Pointer 
  SPH : byte absolute $00+$5D+1; // Stack Pointer 
  MCUCSR : byte absolute $00+$54; // MCU Control And Status Register
  OSCCAL : byte absolute $00+$51; // Oscillator Calibration Value
  SPMCR : byte absolute $00+$57; // Store Program Memory Control Register
  // AD_CONVERTER
  ADMUX : byte absolute $00+$27; // The ADC multiplexer Selection Register
  ADCSRA : byte absolute $00+$26; // The ADC Control and Status register
  ADC : word absolute $00+$24; // ADC Data Register  Bytes
  ADCL : byte absolute $00+$24; // ADC Data Register  Bytes
  ADCH : byte absolute $00+$24+1; // ADC Data Register  Bytes

const
  // SFIOR
  ACME = 3; // Analog Comparator Multiplexer Enable
  // ACSR
  ACD = 7; // Analog Comparator Disable
  ACBG = 6; // Analog Comparator Bandgap Select
  ACO = 5; // Analog Compare Output
  ACI = 4; // Analog Comparator Interrupt Flag
  ACIE = 3; // Analog Comparator Interrupt Enable
  ACIC = 2; // Analog Comparator Input Capture Enable
  ACIS = 0; // Analog Comparator Interrupt Mode Select bits
  // SPSR
  SPIF = 7; // SPI Interrupt Flag
  WCOL = 6; // Write Collision Flag
  SPI2X = 0; // Double SPI Speed Bit
  // SPCR
  SPIE = 7; // SPI Interrupt Enable
  SPE = 6; // SPI Enable
  DORD = 5; // Data Order
  MSTR = 4; // Master/Slave Select
  CPOL = 3; // Clock polarity
  CPHA = 2; // Clock Phase
  SPR = 0; // SPI Clock Rate Selects
  // GICR
  INT = 6; // External Interrupt Request 1 Enable
  IVSEL = 1; // Interrupt Vector Select
  IVCE = 0; // Interrupt Vector Change Enable
  // GIFR
  INTF = 6; // External Interrupt Flags
  // MCUCR
  ISC1 = 2; // Interrupt Sense Control 1 Bits
  ISC0 = 0; // Interrupt Sense Control 0 Bits
  // TIMSK
  TOIE0 = 0; // Timer/Counter0 Overflow Interrupt Enable
  // TIFR
  TOV0 = 0; // Timer/Counter0 Overflow Flag
  // TCCR0
  CS02 = 2; // Clock Select0 bit 2
  CS01 = 1; // Clock Select0 bit 1
  CS00 = 0; // Clock Select0 bit 0
  // TIMSK
  TICIE1 = 5; // Timer/Counter1 Input Capture Interrupt Enable
  OCIE1A = 4; // Timer/Counter1 Output CompareA Match Interrupt Enable
  OCIE1B = 3; // Timer/Counter1 Output CompareB Match Interrupt Enable
  TOIE1 = 2; // Timer/Counter1 Overflow Interrupt Enable
  // TIFR
  ICF1 = 5; // Input Capture Flag 1
  OCF1A = 4; // Output Compare Flag 1A
  OCF1B = 3; // Output Compare Flag 1B
  TOV1 = 2; // Timer/Counter1 Overflow Flag
  // TCCR1A
  COM1A = 6; // Compare Output Mode 1A, bits
  COM1B = 4; // Compare Output Mode 1B, bits
  FOC1A = 3; // Force Output Compare 1A
  FOC1B = 2; // Force Output Compare 1B
  WGM1 = 0; // Waveform Generation Mode
  // TCCR1B
  ICNC1 = 7; // Input Capture 1 Noise Canceler
  ICES1 = 6; // Input Capture 1 Edge Select
  CS1 = 0; // Prescaler source of Timer/Counter 1
  // TIMSK
  OCIE2 = 7; // Timer/Counter2 Output Compare Match Interrupt Enable
  TOIE2 = 6; // Timer/Counter2 Overflow Interrupt Enable
  // TIFR
  OCF2 = 7; // Output Compare Flag 2
  TOV2 = 6; // Timer/Counter2 Overflow Flag
  // TCCR2
  FOC2 = 7; // Force Output Compare
  WGM20 = 6; // Waveform Genration Mode
  COM2 = 4; // Compare Output Mode bits
  WGM21 = 3; // Waveform Generation Mode
  CS2 = 0; // Clock Select bits
  // ASSR
  AS2 = 3; // Asynchronous Timer/counter2
  TCN2UB = 2; // Timer/Counter2 Update Busy
  OCR2UB = 1; // Output Compare Register2 Update Busy
  TCR2UB = 0; // Timer/counter Control Register2 Update Busy
  // SFIOR
  PSR2 = 1; // Prescaler Reset Timer/Counter2
  // UCSRA
  RXC = 7; // USART Receive Complete
  TXC = 6; // USART Transmitt Complete
  UDRE = 5; // USART Data Register Empty
  FE = 4; // Framing Error
  DOR = 3; // Data overRun
  UPE = 2; // Parity Error
  U2X = 1; // Double the USART transmission speed
  MPCM = 0; // Multi-processor Communication Mode
  // UCSRB
  RXCIE = 7; // RX Complete Interrupt Enable
  TXCIE = 6; // TX Complete Interrupt Enable
  UDRIE = 5; // USART Data register Empty Interrupt Enable
  RXEN = 4; // Receiver Enable
  TXEN = 3; // Transmitter Enable
  UCSZ2 = 2; // Character Size
  RXB8 = 1; // Receive Data Bit 8
  TXB8 = 0; // Transmit Data Bit 8
  // UCSRC
  URSEL = 7; // Register Select
  UMSEL = 6; // USART Mode Select
  UPM = 4; // Parity Mode Bits
  USBS = 3; // Stop Bit Select
  UCSZ = 1; // Character Size
  UCPOL = 0; // Clock Polarity
  // TWCR
  TWINT = 7; // TWI Interrupt Flag
  TWEA = 6; // TWI Enable Acknowledge Bit
  TWSTA = 5; // TWI Start Condition Bit
  TWSTO = 4; // TWI Stop Condition Bit
  TWWC = 3; // TWI Write Collition Flag
  TWEN = 2; // TWI Enable Bit
  TWIE = 0; // TWI Interrupt Enable
  // TWSR
  TWS = 3; // TWI Status
  TWPS = 0; // TWI Prescaler
  // TWAR
  TWA = 1; // TWI (Slave) Address register Bits
  TWGCE = 0; // TWI General Call Recognition Enable Bit
  // WDTCR
  WDCE = 4; // Watchdog Change Enable
  WDE = 3; // Watch Dog Enable
  WDP = 0; // Watch Dog Timer Prescaler bits
  // EECR
  EERIE = 3; // EEPROM Ready Interrupt Enable
  EEMWE = 2; // EEPROM Master Write Enable
  EEWE = 1; // EEPROM Write Enable
  EERE = 0; // EEPROM Read Enable
  // SREG
  I = 7; // Global Interrupt Enable
  T = 6; // Bit Copy Storage
  H = 5; // Half Carry Flag
  S = 4; // Sign Bit
  V = 3; // Two's Complement Overflow Flag
  N = 2; // Negative Flag
  Z = 1; // Zero Flag
  C = 0; // Carry Flag
  // MCUCR
  SE = 7; // Sleep Enable
  SM = 4; // Sleep Mode Select
  // MCUCSR
  WDRF = 3; // Watchdog Reset Flag
  BORF = 2; // Brown-out Reset Flag
  EXTRF = 1; // External Reset Flag
  PORF = 0; // Power-on reset flag
  // SPMCR
  SPMIE = 7; // SPM Interrupt Enable
  RWWSB = 6; // Read-While-Write Section Busy
  RWWSRE = 4; // Read-While-Write Section Read Enable
  BLBSET = 3; // Boot Lock Bit Set
  PGWRT = 2; // Page Write
  PGERS = 1; // Page Erase
  SPMEN = 0; // Store Program Memory Enable
  // SFIOR
  ADHSM = 4; // ADC High Speed Mode
  PUD = 2; // Pull-up Disable
  PSR10 = 0; // Prescaler Reset Timer/Counter1 and Timer/Counter0
  // ADMUX
  REFS = 6; // Reference Selection Bits
  ADLAR = 5; // Left Adjust Result
  MUX = 0; // Analog Channel and Gain Selection Bits
  // ADCSRA
  ADEN = 7; // ADC Enable
  ADSC = 6; // ADC Start Conversion
  ADFR = 5; // ADC  Free Running Select
  ADIF = 4; // ADC Interrupt Flag
  ADIE = 3; // ADC Interrupt Enable
  ADPS = 0; // ADC  Prescaler Select Bits

implementation

{$define RELBRANCHES}

{$i avrcommon.inc}

procedure INT0_ISR; external name 'INT0_ISR'; // Interrupt 1 External Interrupt Request 0
procedure INT1_ISR; external name 'INT1_ISR'; // Interrupt 2 External Interrupt Request 1
procedure TIMER2_COMP_ISR; external name 'TIMER2_COMP_ISR'; // Interrupt 3 Timer/Counter2 Compare Match
procedure TIMER2_OVF_ISR; external name 'TIMER2_OVF_ISR'; // Interrupt 4 Timer/Counter2 Overflow
procedure TIMER1_CAPT_ISR; external name 'TIMER1_CAPT_ISR'; // Interrupt 5 Timer/Counter1 Capture Event
procedure TIMER1_COMPA_ISR; external name 'TIMER1_COMPA_ISR'; // Interrupt 6 Timer/Counter1 Compare Match A
procedure TIMER1_COMPB_ISR; external name 'TIMER1_COMPB_ISR'; // Interrupt 7 Timer/Counter1 Compare Match B
procedure TIMER1_OVF_ISR; external name 'TIMER1_OVF_ISR'; // Interrupt 8 Timer/Counter1 Overflow
procedure TIMER0_OVF_ISR; external name 'TIMER0_OVF_ISR'; // Interrupt 9 Timer/Counter0 Overflow
procedure SPI__STC_ISR; external name 'SPI__STC_ISR'; // Interrupt 10 Serial Transfer Complete
procedure USART__RXC_ISR; external name 'USART__RXC_ISR'; // Interrupt 11 USART, Rx Complete
procedure USART__UDRE_ISR; external name 'USART__UDRE_ISR'; // Interrupt 12 USART Data Register Empty
procedure USART__TXC_ISR; external name 'USART__TXC_ISR'; // Interrupt 13 USART, Tx Complete
procedure ADC_ISR; external name 'ADC_ISR'; // Interrupt 14 ADC Conversion Complete
procedure EE_RDY_ISR; external name 'EE_RDY_ISR'; // Interrupt 15 EEPROM Ready
procedure ANA_COMP_ISR; external name 'ANA_COMP_ISR'; // Interrupt 16 Analog Comparator
procedure TWI_ISR; external name 'TWI_ISR'; // Interrupt 17 2-wire Serial Interface
procedure SPM_RDY_ISR; external name 'SPM_RDY_ISR'; // Interrupt 18 Store Program Memory Ready

procedure _FPC_start; assembler; nostackframe; noreturn; public name '_START'; section '.init';
 asm
   rjmp __dtors_end
   rjmp INT0_ISR
   rjmp INT1_ISR
   rjmp TIMER2_COMP_ISR
   rjmp TIMER2_OVF_ISR
   rjmp TIMER1_CAPT_ISR
   rjmp TIMER1_COMPA_ISR
   rjmp TIMER1_COMPB_ISR
   rjmp TIMER1_OVF_ISR
   rjmp TIMER0_OVF_ISR
   rjmp SPI__STC_ISR
   rjmp USART__RXC_ISR
   rjmp USART__UDRE_ISR
   rjmp USART__TXC_ISR
   rjmp ADC_ISR
   rjmp EE_RDY_ISR
   rjmp ANA_COMP_ISR
   rjmp TWI_ISR
   rjmp SPM_RDY_ISR

   .weak INT0_ISR
   .weak INT1_ISR
   .weak TIMER2_COMP_ISR
   .weak TIMER2_OVF_ISR
   .weak TIMER1_CAPT_ISR
   .weak TIMER1_COMPA_ISR
   .weak TIMER1_COMPB_ISR
   .weak TIMER1_OVF_ISR
   .weak TIMER0_OVF_ISR
   .weak SPI__STC_ISR
   .weak USART__RXC_ISR
   .weak USART__UDRE_ISR
   .weak USART__TXC_ISR
   .weak ADC_ISR
   .weak EE_RDY_ISR
   .weak ANA_COMP_ISR
   .weak TWI_ISR
   .weak SPM_RDY_ISR

   .set INT0_ISR, Default_IRQ_handler
   .set INT1_ISR, Default_IRQ_handler
   .set TIMER2_COMP_ISR, Default_IRQ_handler
   .set TIMER2_OVF_ISR, Default_IRQ_handler
   .set TIMER1_CAPT_ISR, Default_IRQ_handler
   .set TIMER1_COMPA_ISR, Default_IRQ_handler
   .set TIMER1_COMPB_ISR, Default_IRQ_handler
   .set TIMER1_OVF_ISR, Default_IRQ_handler
   .set TIMER0_OVF_ISR, Default_IRQ_handler
   .set SPI__STC_ISR, Default_IRQ_handler
   .set USART__RXC_ISR, Default_IRQ_handler
   .set USART__UDRE_ISR, Default_IRQ_handler
   .set USART__TXC_ISR, Default_IRQ_handler
   .set ADC_ISR, Default_IRQ_handler
   .set EE_RDY_ISR, Default_IRQ_handler
   .set ANA_COMP_ISR, Default_IRQ_handler
   .set TWI_ISR, Default_IRQ_handler
   .set SPM_RDY_ISR, Default_IRQ_handler
 end;

end.
