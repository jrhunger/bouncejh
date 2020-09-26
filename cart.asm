	processor 6502

	include "vcs.h"
	include "macro.h"

;;;; start constant declarations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
P0HEIGHT equ 9
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; end constant declarations

;;; $80 to $FF for variables, minus some at end if using stack
	seg.u variables
	org $80
;;;;  start variable declarations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
P0x	byte	; P0 x
P0y	byte	; P0 y
P0xdir	byte	; x velocity + / - / 0
P0ydir	byte	; y velocity + / - / 0
P0spritePtr	ds	; y-adjusted sprite pointer
CTRLPF_shadow	byte	; track content of CTRLPF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  end variables

;;; Begin code segment in ROM at $F000
	seg code
	org $F000

Start:
	CLEAN_START

;;;;  start variable initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda #50
	sta P0x
	sta P0y

	lda #1
	sta P0ydir
	sta P0xdir

;;; Set high byte of P0spritePtr (low byte updated per frame)
	lda #>P0bitmap
	sta P0spritePtr+1

;;; Initialize CTRLPF
	; D0 = REF (reflect playfield)
	; D1 - SCORE (color left/right of playfield like P0/P1)
	; D2 - PFP (1 playfield over players)
	; D4/D5 - Ball Size 00 = 1 / 01 = 2 / 10 = 4 / 11 = 8
	lda #000000000	; don't reflect playfield
	sta CTRLPF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  end variable initialization

StartFrame:
	lda #2
	sta VSYNC

;;; 3 lines of VSYNC
	sta WSYNC	; store halts until scanline complete
	sta WSYNC	; 2nd
	sta WSYNC	; 3rd

;;;; set timer for VBLANK
	LDA #44
	STA	TIM64T

	lda #0
	sta VSYNC	; turn off VSYNC

;;;;  start game vblank logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; handle input
CheckP0Up:
	lda #%00010000
	bit SWCHA
	bne CheckP0Down
	lda #1
	clc
	adc P0ydir
	sta P0ydir
	lda #%00000010	; 0 or 1 are ok, 2 is not
	bit P0ydir
	beq CheckP0Down	; if it's not 2 we are good
	lda #1		; if it is 2 set to 1
	sta P0ydir
CheckP0Down:
	lda #%00100000
	bit SWCHA
	bne CheckP0Right
	lda #%00000010	; set for -1, not for 0 or 1
	bit P0ydir
	bne CheckP0Right; -1 is the lowest so don't change it
	lda P0ydir
	clc
	adc #-1
	sta P0ydir
CheckP0Right:
	lda #%10000000
	bit SWCHA
	bne CheckP0Left
	lda #1
	clc
	adc P0xdir
	sta P0xdir
	lda #%00000010
	bit P0xdir
	beq CheckP0Left
	lda #1
	sta P0xdir
CheckP0Left:
	lda #%01000000
	bit SWCHA
	bne NoInput
	lda #-1
	lda #%00000010	; set for -1, not for 0 or 1
	bit P0xdir
	bne NoInput	; -1 is the lowest so dont change
	lda P0xdir
	clc
	adc #-1
	sta P0xdir
NoInput:
;;; calculate P0 x position
	lda P0x
	cmp #4
	beq P0xLow
	cmp #156	; 160 pixels minus 1/2 of 4-wide sprite
	beq P0xHigh
	lda P0xdir
	jmp P0xMove
P0xLow:
	lda #1
	sta P0xdir
	jmp P0xMove
P0xHigh:
	lda #-1
	sta P0xdir
P0xMove:
	clc
	adc P0x
	sta P0x
;;; update horizontal position
	ldx #0
;	lda P0x ; not needed if already in A
	jsr PosObject
	sta WSYNC
	sta HMOVE

;;; update P0 y position
	lda P0y
	cmp #8
	beq P0yLow
	cmp #192
	beq P0yHigh
	lda P0ydir
	jmp P0yMove
P0yLow:
	lda #1
	sta P0ydir
	jmp P0yMove
P0yHigh:
	lda #-1
	sta P0ydir
P0yMove:
	clc
	adc P0y
	sta P0y
	
;;; P0 y pointer
	lda #<P0bitmap+P0HEIGHT
	sec
	sbc P0y
	sta P0spritePtr

;;; Check collisions
	lda #%10000000
	bit CXP0FB	; bit 7 = P0/PF
	beq NoP0Collision
	lda #$30
	sta COLUPF
	jmp DoneCollision
NoP0Collision
	lda #0
	sta COLUPF
DoneCollision
	sta CXCLR	; clear collisions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  end game vblank logic


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  start kernel prep
	ldy #192	; counter
;;;;  end kernel prep
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Wait for rest of VBLANK
.VblankWaitLoop
	lda INTIM 	; load timer interrupt
	bne	.VblankWaitLoop
	sta WSYNC 	; wait for next wsync
	sta VBLANK	; turn off VBlank. A is zero because of bne above

;;;; kernel (192 visible scan lines)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.LoopVisible:
	sta WSYNC	; 3|0 wait for scanline at beginning so end-of
			;    loop logic is pre-scanline
;;; background (9)
;	tya		; 2| get scan line
;	asl		; 2| 192 scan lines, 128 unique colors (last bit unused)
			;    x2 means we see all of them with some repeats
;	ora #%00001000	; 2| don't use luminance < 8
			;    (this makes a tight repeat.. if only i could asl the right nibble)
	sty COLUBK	; 3| (9) set bg color to result
; 12 
;;; left playfield (21)
	lda pf1l,Y	; 4|
	sta PF1		; 3| 	after 65, before 28
	lda pf2l,Y	; 4|
	sta PF2		; 3|	after  0, before 38
; 30
;;; draw P0 (21)
	sec		; 2| set carry
	tya		; 2|
	sbc P0y		; 3|
	adc P0HEIGHT	; 3|
	bcs .DrawP0	; 2/3|
;
	nop		; 2|
	nop		; 2|
	sec		; 2|
	bcs .NoDrawP0	; 3|
.DrawP0
	lda (P0spritePtr),Y	; 5|
	sta GRP0	; 3|
.NoDrawP0
; 51

;;; right playfield (21)
	lda pf0r,Y	; 4|
	sta PF0		; 3|	after 28, before 49
	lda pf1r,Y	; 4|
	sta PF1		; 3| 	after 39, before 54
	lda pf2r,Y	; 4|
	sta PF2		; 3|	after 49, before 65

;;; left pf0
	lda pf0l+1,Y	; 4|
	sta PF0		; 3| 	after 54, before 22
;;; end loop (cycles <= 67 here to avoid wrap)
	dey		; 2| y--
	bne .LoopVisible	; 2/3/4| go back until x = 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; end kernel

;;;; set timer for OVERSCAN
	lda #2
	sta WSYNC
	sta VBLANK
	lda #36
	sta TIM64T

;;;;  start game overscan logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda #0
	sta GRP0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  end game overscan logic


;;;; Wait for rest of OVERSCAN
.OverscanWaitLoop:
	lda INTIM
	bne .OverscanWaitLoop
	lda #2
	sta WSYNC

;;; new frame
	jmp StartFrame

;;;;   start subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; PosObject from https://www.biglist.com/lists/stella/archives/200403/msg00260.html
;;; but that didn't work right so traced back to https://www.biglist.com/lists/stella/archives/200311/msg00039.html
; Positions an object horizontally
; Inputs: A = Desired position.
; X = Desired object to be positioned (0-5). *jh* (P0, P1, M0, M1, Ball)
; scanlines: If control comes on or before cycle 73 then 1 scanline is consumed.
; If control comes after cycle 73 then 2 scanlines are consumed.
; Outputs: X = unchanged
; A = Fine Adjustment value.
; Y = the "remainder" of the division by 15 minus an additional 15.
; control is returned on cycle 6 of the next scanline.
PosObject SUBROUTINE
	STA WSYNC ; 00 Sync to start of scanline.
	SEC ; 02 Set the carry flag so no borrow will be applied during the division.
.divideby15
	SBC #15 ; 04 ; Waste the necessary amount of time dividing X-pos by 15!
	BCS .divideby15 ; 06/07 - 11/16/21/26/31/36/41/46/51/56/61/66

	EOR #$0F
	ASL
	ASL
	ASL
	ASL

	ADC #$90
	STA RESP0,X
	STA WSYNC
	STA HMP0,X

	RTS

;;; end PosObject from https://www.biglist.com/lists/stella/archives/200311/msg00039.html
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   end subroutines

;;;;  start ROM lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; playfield.h should set pfdata[lr]pf[012] labels and each table should be one page
	include "playfield.h"

	org $fef6
P0bitmap:
	byte #%00000000
	byte #%00101001
	byte #%01101001
	byte #%10101001
	byte #%00101001
	byte #%00101111
	byte #%00101001
	byte #%00101001
	byte #%11101001

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  end ROM lookup tables

;;; Complete to 4kB
	org $FFFC
	.word Start
	.word Start
