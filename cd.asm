         keep  obj/cd
         mcopy cd.macros
****************************************************************
*
*  Console Driver 1.3
*  ------------------
*
*  This library implements a substantial subset of the console
*  driver used in Apple Pascal.  The Apple Pascal console driver
*  is the one used by APW; it is available through the text
*  tool kit.
*
*  The main reason for this driver is to provide a safe way to
*  write text from a classic desk accessory.  None of the
*  normal console drivers can be used safely, since they may
*  be in use by the program that was interupted.
*
*  A secondary reason is that a console driver such as this
*  one is easy to change to do special tricks, like reading the
*  position of the cursor.
*
*  To use the driver, start your program with
*
*        jsl   ~CDAStart
*
*  This initializes the console driver, installing it as a RAM
*  based driver to the text tool kit.  Your program can now do
*  virtually anything that it could do with the text tool kit if
*  the standard Pascal driver was selected.  This procedure assumes
*  that the screen is already saved if that is necessary.
*
*  When your program is ready to exit, make the call
*
*        jsl   ~CDAShutDown
*
*  Currently, this call does nothing, but in future releases,
*  some clean up may be necessary.
*
*  ORCA/Pascal and ORCA/C make these calls automatically when you compile
*  a Classic Desk Accessory.  This driver is fully compatible
*  with the I/O macros used by the ORCA/M macro assembler libraries.
*
*  In addition, this library has a subroutine called ~DAID, used to
*  start and shut down a user ID for use by CDAs and NDAs.
*
*  Licensing
*  ---------
*
*  This driver is a part of ORCA/Pascal and ORCA/C, and is
*  covered by the normal terms of license for those products'
*  run time library.  These terms are subject to change, but
*  at this time provide royalty free license to anyone who
*  owns ORCA/Pascal or ORCA/C, provided that proper
*  acknowledgement is given.  Contact
*
*        Byte Works, Inc.
*        4700 Irving Blvd. NW, Suite 207
*        Albuquerque, NM  87114
*        (505)-898-8183
*
*  for details and a copy of the licensing forms.
*
*  Copyright
*  ---------
*
*  Copyright 1987, 1989
*  Byte Works, Inc.
*  All Rights Reserved
*
*  Author
*  ------
*
*  Mike Westerfield
*  October 1987
*
*  Adapted from the ORCA/M 4.1 console driver.
*
****************************************************************
*
*  Changes made in V1.1 of the console driver:
*
*  1.    Inverse capitols (used for the cursor) are now mapped
*        out of the mousetext area.
*
*  2.    A form feed is performed when you write a character
*        with the cursor in column 80.
*
*  3.    If you start in 40 column mode, the console driver
*        swicthes to an 80 column display.
*
*  4.    Long addressing is now used to access bank 1, rather
*        than the old page selection switches.
*
****************************************************************
*
*  Changes made in V1.2 of the console driver:
*
*  1.  ~DAID subroutine added
*
****************************************************************
*
*  Changes made in V1.3 of the console driver:
*
*  1.  Added mouse on and mouse off support.
*
****************************************************************
*
Dummy    start
         end

****************************************************************
*
*  Common - Common Area for CRT Driver
*
****************************************************************
*
Common   privdata
;
;  Addresses of the lines on the page
;
lineAddrs anop
         dc    a'$400,$480,$500,$580,$600,$680,$700,$780'
         dc    a'$428,$4A8,$528,$5A8,$628,$6A8,$728,$7A8'
         dc    a'$450,$4D0,$550,$5D0,$650,$6D0,$750,$7D0'
;
;  ASCII character codes
;
ASCFS    equ   $1C                      forward space
ASCGS    equ   $1D                      clear to EOL
;
;  Constants
;
maxCh    equ   80                       screen size
maxCv    equ   24
;
;  Global variables
;
ch       ds    2                        horizontal cursor
chMask   dc    i'$80'                   character mask
cursorOn ds    2                        is the cursor visible?
cv       ds    2                        vertical cursor
mask     dc    i'0'                     goto mask
mouseText dc   i'0'                     mousetext enabled?
         end

****************************************************************
*
*  ~CDAShutDown - Shut Down the CDA Console Driver
*
****************************************************************
*
~CDAShutDown start

	lda	#SysCharErrout+3
	sta	>SysCharErrout+1
	lda	#SysCharOut+3
	sta	>SysCharOut+1
	lda	#SysLineIn+3
	sta	>SysLineIn+1
         rtl
         end

****************************************************************
*
*  ~CDAStart - Initialize the CDA Console Driver
*
*  Outputs:
*        ch - 0
*        cv - 0
*        chMask - $0080
*        mask - 0
*
****************************************************************
*
~CDAStart start
         using Common
col80    equ   $C00D                    enable 80 col display

         phb                            set local data bank
         phk
         plb

         short M                        turn on 80 col display
         sta   >col80
         long  M
         stz   ch                       cursor starts at top left
         stz   cv
         stz   mask                     not doing a gotoXY
         lda   #$0080                   characters are standard
         sta   chMask
         jsr   Clear                    Clear the screen
         jsr   Invert                   show the cursor
         lda   #1                       cursor is visible
         sta   cursorOn

	lda	#SysCDTextCharOut	use CD I/O
	sta	SysCharErrout+1
	sta	SysCharOut+1
	lda	#SysGraphTextLineIn
	sta	SysLineIn+1

         plb                            restore caller's data bank
         rtl
         end

****************************************************************
*
*  Clear - Clear the Screen
*
*  Notes:
*        1)  CV and CH are unchanged.
*        2)  All registers are returned intact.
*
****************************************************************
*
Clear    private
         using Common
lBasl    equ   3                        local base address

         pha                            save the registers
         phx
         phy
         pha                            make room for lBasl
         pha
         phd
         tsc
         tcd
         stz   lBasl+2                  screen is in bank 0
         lda   cv                       save the cursor
         pha
         lda   #maxCv-1                 for each line
         sta   cv
lb1      lda   cv                          set the line address
         asl   a
         tax
         lda   lineAddrs,X
         sta   lBasl
         lda   #'  '                       clear with spaces
         ora   chMask
         xba
         ora   chMask
         ldy   #maxCh/2-2                  clear the primary bank
lb2      sta   [lBasl],Y
         dey
         dey
         bpl   lb2
         ldy   #maxCh/2-2                  clear the secondary bank
         inc   lBasl+2
lb3      sta   [lBasl],Y
         dey
         dey
         bpl   lb3
         stz   lBasl+2
         dec   cv                       next line
         bpl   lb1
         pla                            restore the cursor
         sta   cv
         pld                            restore the registers
         pla
         pla
         ply
         plx
         pla
         rts
         end

****************************************************************
*
*  ControlCodes - Interpret Terminal Control Codes
*
*  Inputs:
*        A - character
*
*  Outputs:
*        C - set if a control code was interpreted
*
*  Notes:
*        1)  All registers are returned intact.
*        2)  If a GOTO was received, the next two characters
*            will also be intercepted by this routine,
*            regardless of what they are.
*
****************************************************************
*
ControlCodes private
         using Common
;..............................................................;
;                                                              ;
;  Check for control codes.                                    ;
;                                                              ;
;..............................................................;
;
;  Check for an active GOTO.
;
         asl   mask                     if msb of mask is set, this
         bcc   cc1                       char is part of a cursor
         ldx   Y                         positioning sequence
         stx   X                        roll any previous position
         sec                            convert char to disp
         sbc   #32
         sta   Y
         lda   mask                     if mask is 0, sequence is complete
         bne   cg1
         ldx   X
         ldy   Y
         jsr   GotoXY                   position the cursor
cg1      sec
         rts
;
;  Check for control codes.
;
cc1      cmp   #' '                     return if it is not a control code
         blt   cc2
         clc
         rts

cc2      pha                            save the registers
         phx
         phy
         tay                            see if this code gets interpreted
         lda   disp,Y
         and   #$00FF
         beq   cc3
         tay                            yes -> find the subroutine address
         lda   adds-2,Y
         pea   cc3-1                    do indirect call
         pha
         rts
cc3      ply                            restore the registers
         plx
         pla
         sec                            tell the caller we took it
         rts
         eject
;..............................................................;
;                                                              ;
;  Control code interpreters.                                  ;
;                                                              ;
;..............................................................;
;
;  BEL: Ring the bell.
;
BEL      _SysBeep                       sound the ProDOS tone
         rts
;
;  BS: Non-desructive back space.
;
BS       dec   ch
         bpl   bs1
         inc   ch
bs1      rts
;
;  LF: Line feed.
;
LF       ldy   cv                       check for scroll
         cpy   #maxCv-1
         blt   lf1
         brl   Scroll                   scroll screen

lf1      iny                            go to next line
         ldx   ch
         brl   GotoXY
;
;  VT: Clear to end of screen.
;
VT       jsr   GS                       clear to EOL
         lda   cv                       save the cursor
         pha
         lda   ch
         pha
vt1      inc   cv                       clear remaining lines
         ldy   cv
         cpy   #maxCv-1
         bgt   vt2
         ldx   #0
         jsr   GotoXY                   goto start of line
         jsr   GS                       clear to eol
         bra   vt1

vt2      pla                            restore the cursor
         tax
         ply
         brl   GotoXY
;
;  FF: Form feed.
;
FF       jsr   Clear
         bra   EM
;
;  CR: Carriage return.
;
CR       stz   ch
         rts
;
;  SO: Use standard characters.
;
SO       lda   #$0080
         sta   chMask
         rts
;
;  SI: Use alternate characters.
;
SI       stz   chMask
         rts
;
;  EM: Home the cursor.
;
EM       ldx   #0                       set the cursor to 0,0
         txy
         brl   GotoXY                   goto X,Y
;
;  FS: Forward space.
;
FS       lda   #maxCh-1                 check to see if at EOL
         cmp   ch
         beq   FS1                      branch if at eol
         inc   ch
         rts

FS1      jsr   CR                       if at EOL then issue a carriage return
         brl   LF                        and line feed
;
;  GS: Clear to EOL.
;
GS       ldy   ch                       print out blanks from CH to EOL
         lda   #' '
         ora   chMask
gs1      jsr   PrintHV                  print out character
         inc   ch
         ldx   #maxCh-1
         cpx   ch
         bge   gs1
         sty   ch
         rts
;
;  RS: Screen GOTO.
;
RS       lda   #$C000
         sta   mask
         rts
;
;  US: Reverse linefeed.
;
US       dec   cv
         bpl   us1
         inc   cv
us1      ldx   ch
         ldy   cv
         brl   GotoXY
;
;  ENQ: cursor on
;
ENQ      lda   #1
         sta   cursorOn
         rts
;
;  ACK: cursor off
;
ACK      stz   cursorOn
         rts
;
;  ESC: mousetext on
;
ESC      lda   #1
         sta   mouseText
         rts
;
;  CAN: mousetext off
;
CAN      stz   mouseText
         rts
         eject
;..............................................................;
;                                                              ;
;  Local data.                                                 ;
;                                                              ;
;..............................................................;
;
adds     dc    a'BEL-1'                 ring the bell
         dc    a'BS-1'                  non-destructive back space
         dc    a'LF-1'                  line feed
         dc    a'VT-1'                  clear to EOS
         dc    a'FF-1'                  form feed
         dc    a'CR-1'                  carriage return
         dc    a'SO-1'                  standard characters
         dc    a'SI-1'                  alternate characters
         dc    a'EM-1'                  home cursor
         dc    a'FS-1'                  forward space
         dc    a'GS-1'                  clear to EOL
         dc    a'RS-1'                  screen GOTO
         dc    a'US-1'                  reverse line feed
         dc    a'ENQ-1'                 cursor on
         dc    a'ACK-1'                 cursor off
         dc    a'ESC-1'                 mousetext on
         dc    a'CAN-1'                 mousetext off

disp     dc    i1'00,00,00,00,00,28,30,02' disps into ADDS for control codes
         dc    i1'04,00,06,08,10,12,14,16'
         dc    i1'00,00,00,00,00,00,00,00'
         dc    i1'34,18,00,32,20,22,24,26'

X        ds    2                        X position for GOTO
Y        ds    2                        Y position for GOTO
         end

****************************************************************
*
*  ~DAID - Get/Release a user ID for a desk accessory
*
*  Inputs:
*        flag - shut down flag:
*              0 - deallocate the user ID
*              1 - allocate the user ID
*
*  Notes:
*        A,X,Y - unchanged
*
****************************************************************
*
~DAID    start
flag     equ   4                        allocation flag

         sta   >aReg                    save the registers
         txa
         sta   >xReg
         tya
         sta   >yReg

         lda   4,S                      branch for deallocation
         beq   lb1

         lda   >~User_ID                quit if we have a user ID
         bne   lb2
         pha                            get a user ID
         ph2   #$5000
         _GetNewID
         pla                            save it
         sta   >~User_ID
         bra   lb2

lb1      lda   >~User_ID                if we have a user ID then
         beq   lb2
         jsl   ~MM_Init                   reset the memory manager
         lda   >~User_ID                  dispose of all memory
         pha
         pha
         _DisposeAll
         _DeleteID                        free up the user ID
         lda   #0                         need a new user ID if recalled
         sta   >~User_ID

lb2      phb                            remove flag from the stack
         plx
         ply
         pla
         phy
         phx
         lda   >xReg                    restore registers
         tax
         lda   >yReg
         tay
         lda   >aReg
         plb
         rtl
;
;  local data
;
aReg     ds    2                        A register storage
xReg     ds    2                        X register storage
yReg     ds    2                        Y register storage
         end

****************************************************************
*
*  GotoXY - Position the Cursor
*
*  Inputs:
*        X - column
*        Y - row
*
*  Notes:
*        1)  Row and column numbers count from 0.
*        2)  All registers are returned intact.
*
****************************************************************
*
GotoXY   private
         using Common

         cpx   #maxCh-1                 set horizontal position
         blt   lb1
         ldx   #maxCh-1
lb1      stx   ch
         cpy   #maxCv-1                 check range of vertical position
         blt   lb2
         ldy   #maxCv-1
lb2      sty   cv                       set the vertical cursor position
         rts
         end

****************************************************************
*
*  Invert - Invert the current character
*
*  Inputs:
*        CV - vertical position
*        CH - horizontal position
*
****************************************************************
*
Invert   private
         using Common
char     equ   9                        character to print
lBasl    equ   3                        base address of current line

         phx                            save registers & set up DP
         pha
         phy
         pha
         pha
         phd
         tsc
         tcd
         lda   cv                       set up the address of the line
         asl   a
         tax
         lda   lineAddrs,X
         sta   lBasl
         stz   lBasl+2
         short I,M                      use short regs
         lda   ch                       if char goes in an even column, switch
         lsr   a                         to alternate bank
         bcs   pr2
         inc   lBasl+2
pr2      tay                            invert the character
         lda   [lBasl],Y
         eor   #$80
         ldx   mouseText                if not mouseText then
         bne   pr3
         cmp   #$60                       if this char is in the mousetext area
         bge   pr3                           then
         cmp   #$40
         blt   pr3
         eor   #$40                         map the char to inverse
pr3      sta   [lBasl],Y
         stz   lBasl+2                  make sure we end up pointing to bank 0
         long  I,M                      back to long regs
         pld                            restore registers
         pla
         pla
         ply
         pla
         plx
         rts
         end

****************************************************************
*
*  PrintHV - Put a Character
*
*  Places a character on the screen without interpreting
*  control codes.
*
*  Inputs:
*        CV - vertical position
*        CH - horizontal position
*        A - character to write
*
****************************************************************
*
PrintHV  private
         using Common
char     equ   9                        character to print
lBasl    equ   3                        base address of current line

         phx                            save registers & set up DP
         pha
         phy
         pha
         pha
         phd
         tsc
         tcd
         lda   cv                       set up the address of the line
         asl   a
         tax
         lda   lineAddrs,X
         sta   lBasl
         stz   lBasl+2
         short I,M                      use short regs
         lda   ch                       if char goes in an even column, switch
         lsr   a                         to alternate bank
         bcs   pr2
         inc   lBasl+2
pr2      tay                            write the character
         lda   char
         sta   [lBasl],Y
         stz   lBasl+2                  make sure we end up pointing to bank 0
         long  I,M                      back to long regs
         pld                            restore registers
         pla
         pla
         ply
         pla
         plx
         rts
         end

****************************************************************
*
*  PutChar - Put a Character
*
*  Writes a character and advances the cursor.  All terminal
*  control codes are interpreted.
*
*  Inputs:
*        CV - vertical position
*        CH - horizontal position
*        A - character to write
*
****************************************************************
*
PutChar  private
         using Common

         ldx   cursorOn                 turn off the cursor
         beq   lb1
         jsr   Invert
lb1      jsr   PutX                     write the character
         bcs   lb2
         lda   #ASCFS                   advance the cursor
         jsr   PutX
lb2      lda   cursorOn
         beq   lb3
         brl   Invert                   turn the cursor on
lb3      rts
         end

****************************************************************
*
*  PutX - Put a Character
*
*  Writes a character without advancing the cursor.  All
*  terminal control codes are interpreted.
*
*  Inputs:
*        CV - vertical position
*        CH - horizontal position
*        A - character to write
*
*  Outputs:
*        C - set if the character writen was a control
*              character
*
****************************************************************
*
PutX     private
         using Common

         phx                            save X
         and   #$7F                     convert to ASCII
         jsr   ControlCodes             interpret control codes
         bcs   lb2
         ora   chMask                   set high bit to mask value
         ldx   mouseText                if not mouseText then
         bne   lb1
         cmp   #$40                        if ch in [$40..$5F] then
         blt   lb1
         cmp   #$60
         bge   lb1
         and   #$1F                           map it to inverse
lb1      jsr   PrintHV                  write the character
         clc
lb2      plx                            restore X
         rts
         end

****************************************************************
*
*  Scroll - Scroll the screen.
*
*  Notes:
*        1)  All registers are returned intact.
*        2)  CV and CH are unchanged.
*        3)  The last line is blanked.
*
****************************************************************
*
Scroll   private
         using Common
lBasl    equ   3                        base address pointers
lBasl2   equ   7

         pha                            save registers and cursor
         phx
         phy
         lda   ch
         pha
         lda   cv
         pha
         pha                            make room for lBasl, lBasl2
         pha
         pha
         pha
         phd
         tsc
         tcd
         lda   lineAddrs                set up initial base addresses
         sta   lBasl
         stz   lBasl+2
         stz   lBasl2+2
         stz   cv

sc1      lda   lBasl                    for each line but the last...
         sta   lBasl2
         inc   cv
         lda   cv
         asl   a
         tax
         lda   lineAddrs,X
         sta   lBasl
         ldy   #maxCh/2-2                 copy one line on page 1
sc2      lda   [lBasl],Y
         sta   [lBasl2],Y
         dey
         dey
         bpl   sc2
         inc   lBasl+2                    copy one line on page 2
         inc   lBasl2+2
         ldy   #maxCh/2-2
sc3      lda   [lBasl],Y
         sta   [lBasl2],Y
         dey
         dey
         bpl   sc3
         stz   lBasl+2
         stz   lBasl2+2
         lda   cv                       next line
         cmp   #maxCv-1
         blt   sc1
         stz   ch                       clear the bottom line
         lda   #ASCGS
         jsr   ControlCodes
         pld                            restore DP
         pla                            get rid of lBasl, lBasl2
         pla
         pla
         pla
         pla                            restore cursor and registers
         sta   cv
         pla
         sta   ch
         ply
         plx
         pla
         rts
         end

****************************************************************
*
*  SysCDTextCharOut - Single character output
*
****************************************************************
*
SysCDTextCharOut private

	phb		get the character
	plx
	ply
	pla
	phy
	phx
	plb
         and   #$007F                   convert to standard ASCII
         cmp   #10                      eat line feeds
         beq   wr2
         cmp   #13                      do cr-linefeed for RETURN char
         bne   wr1
         jsr   PutChar
         lda   #10
wr1      jsr   PutChar                  write the character
wr2      rtl
         end
