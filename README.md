# Atari 2600 Assembler playground - "asymmetrical playfield"

## from "bouncejh"
* rainbow background based on the scan line counter
* bouncing JH sprite
* collision detect (P0 and background)
  * change background color while colliding

## added asymmetrical playfield
* uses static fullscreen tables for each register
  * draw a screen at https://www.masswerk.at/vcs-tools/TinyPlayfieldEditor/
  * save as {filename}.tpe
    * Asymmetric
    * Right Side: Repeat
    * Kernel Height 192
    * Line Height: 1px
    * Order by: PF-Registers
    * Number Format: Bin
    * (don't Reverse Order)
  * ./tpe2h.pl {filename}.tpe > playfield.h
  * make and run
