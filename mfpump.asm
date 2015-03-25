 title  "MRItimer - PIC12F675 pump controller"
;
;  This Program pulses a microfluidic pump 
;  and lights a green lamp for 10sec every 5 minutes
;
;  Hardware Notes:
;   PIC12F675 running at 4 MHz Using the Internal Clock
;
;  based on original by Myke Predko
;  04.11.19
;  delivered to WFS with usable feature set; ijs, 2013.07.15
;  latest revision ijs, 2013.06.19
;  modifications for selectable 30min latency; ijs, 2013.08.19
;  modifications for 60sec ON time
;  modified for 1 second on, 5 or 10 sec off; ijs, 2014.11.24
;

  LIST p=12F675			; define processor
 INCLUDE "p12f675.inc"

; Version selection
;  #define v530               ; speed switch selects 5 or 30 mins latency

; PIC12F675 Configuration Bit Settings

; CONFIG
; __config 0x3194
 __CONFIG _FOSC_INTRCIO & _WDTE_OFF & _PWRTE_OFF & _MCLRE_OFF & _BOREN_OFF & _CP_OFF & _CPD_OFF

; Pin I/O definitions
#define PumpEnable	GPIO, 0	; pin 7	output to Bartels enable line
#define	SpeedSwitch	GPIO, 1	; pin 6 weak pullup
#define Potentiometer	GPIO, 2 ; pin 5 AN2
#define RunSwitch	GPIO, 3	; pin 4 NOTE: GP3 input only, no internal pullup
#define LED_Green	GPIO, 4	; pin 3 
#define LED_Red		GPIO, 5	; pin 2

;  Variables
 CBLOCK 0x20
Dlay:2
Count1
Count2
Count3
Count4
CountOnTime
Count10OnTime
CountOffTime
OffScaleCounter
SpeedScale
SpeedCounter
SpeedFlag
CounterMin
Counter2sec
SpeedTicks
 ENDC

  PAGE
;  Mainline

 org     0

  nop                           ;  For ICD Debug
  goto	Init			;  start of program
  
  
	org	0x004			; interrupt vector
	return			; interrupt trap, return without re-enabling

Init
  call    0x3FF  	        ; retrieve factory calibration value
  bcf STATUS, RP0

  BANKSEL GPIO
  clrf    GPIO                  ;  Initialize I/O Bits to Off
  movlw   7                     ;  Turn off Comparators
  movwf   CMCON
  bsf     STATUS, RP0           ;  Replaces:    movf    STATUS, w
                                ;               iorlw   1 << 5
                                ;               movwf   STATUS
  BANKSEL TRISIO		; in TRIS register
  bcf     OPTION_REG, 7  	;  Enable PORTA Pull Ups
  movlw   B'001110'		; set input enables
  movwf   TRISIO                ;  GP1, AN2, and GP3 are inputs
  movlw	  B'000010'		; input switch lines
  movwf   WPU			; set weak pullups
  clrf    ANSEL			; all inputs digital
  bsf     ANSEL, 2		; except AN2, pin 5
  movlw   B'00100'		; select AN2
  movwf	  ANSEL			; set AN2 = pin5 as analog input

  banksel GPIO
;  bcf LED_Gnd			; set Gnd ref for PICkit-1 demo board LEDs
	movlw	B'00111'	; turn off comparator
	movwf	CMCON		;
  
StartOnCycle 
  call	PumpOn			; turn on for limited time
  movlw D'5'			; multiplier for latency timer
  movwf CountOnTime		; load the counter
OnDelayLoop
  call Delay10			; good for 186ms delay
  decfsz CountOnTime, 1		; 0.186 * 5 ~ 1 seconds
  goto OnDelayLoop 
  
StartOffCycle
  call	PumpOff			; turn off and wait out cycle time
OffDelay
  movlw D'53'			; multiplier for latency timer in 0.186 second increments
  btfss SpeedSwitch
  movlw D'26'
  movwf CountOffTime	; load the counter
OffDlyLoop
	call Delay10
	decfsz CountOffTime, 1
	goto OffDlyLoop	;
  goto    StartOnCycle            ;  Repeat entire pump cycle

PumpOn
	;movlw B'01001'		; redacted (Outputs on GP0 and GP3)
	;movwf GPIO		; turn them on
	bsf PumpEnable		; turn on pump
	bsf LED_Green		; turn on green lamp
	return
	
PumpOff				; turn off GP0 and GP3
	bcf PumpEnable		; turn off pump
	bcf LED_Green		; turn off lamp
	return

Delay10				; delay 186 milliseconds
	clrf Count1		; zero counters
	clrf Count2

Delay10a
	decfsz Count1, 1	; inner counting loop
	goto Delay10a		; 3 cycles * 256 = 768 cycles
	decfsz Count2, 1	; outer counting loop
	goto Delay10a		; * 256 = 186ms
	btfss RunSwitch		; check for priming button
	goto PrimePump		; if not pressed continue on
Delay10_exit			; 
	return

PrimePump			; while button is pressed, run pump
	call PumpOn
	clrf Count3		; zero counters
	clrf Count4
DelayPP				; prime pump switch debounce delay
	decfsz Count3, 1	; inner counting loop
	goto DelayPP
	decfsz Count4, 1	; outer counting loop
	goto DelayPP
	btfsc RunSwitch		; check for priming button
				; button released, restart cycle
	goto StartOffCycle	; with pump off and latency
	goto PrimePump		; still pressed, repeat


  end                           
