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

;;; calculate P0 x position
	lda P0x
	beq P0xLow
	cmp #152	; 160 pixels minus 8-wide sprite
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
;;; clear 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  end game vblank logic

;;;; Wait for rest of VBLANK
.VblankWaitLoop
	lda INTIM 	; load timer interrupt
	bne	.VblankWaitLoop
	sta WSYNC 	; wait for next wsync
	sta VBLANK	; turn off VBlank

;;;; kernel (192 visible scan lines)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ldy #192	; counter
.LoopVisible:
;;; for rainbow background
	sty COLUBK	; set bg color to loop var

;;; draw P0
	sec	; 2 set carry
	tya	; 2
	sbc P0y	; 3
	adc P0HEIGHT	; 2
	bcs .DrawP0

	nop	; 2
	nop	; 2
	sec	; 2
	bcs .NoDrawP0	; 3
.DrawP0
	lda (P0spritePtr),Y	; 5
	sta GRP0	; 3
.NoDrawP0
	sta WSYNC	; wait for next scanline
	dey	; y--
	bne .LoopVisible	; go back until x = 0
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
	org $f0f6
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

P0color:
	byte #$00
	byte #$00
	byte #$00
	byte #$00
	byte #$00
	byte #$00
	byte #$00
	byte #$00
	byte #$00

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  end ROM lookup tables

;;; Complete to 4kB
	org $FFFC
	.word Start
	.word Start
