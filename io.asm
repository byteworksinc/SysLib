         keep  obj/io
         mcopy io.macros
****************************************************************
*
*  Input, ouput and integer conversion libraries
*
*  By Mike Westerfield
*
*  Copyright January 1987, All rights reserved
*  By the Byte Works, Inc.
*
****************************************************************
*
Dummy    start
         end

****************************************************************
*
*  ~GSOSIO - Common area for the GS/OS text I/O package
*
****************************************************************
*
~GSOSIO  privdata
;
;  When input comes from .CONSOLE, it is read one line at a time to allow for
;  input editing.  The line is stuffed into ~line, then scanned using
;  ~lineDisp as an index.
;
;  The end of a line is marked by chr(13), after which any character
;  read forces a new line read.
;
;  chr(0) is treated as an end of file condition.  End of file is reported
;  for the remainder of the program execution.
;
;  If an error occurs during startup, end of file is reported on the first
;  character read.
;
;  ~putback provides a one-character putback buffer.  This buffer is
;  valid whether input is comming from .CONSOLE or from a file.
;
~line    ds    256                      input line
~lineDisp ds   2                        disp in ~line for next char
~lineLength ds 2                        length of the current line
~putback ds    2                        one character putback buffer
;
;  File reference numbers
;
~stinRefnum ds 2
~stoutRefnum ds 2
~erroutRefnum ds 2
;
;  If we open a file, we should close it.  These flags tell us if we opened
;  a particular channel.
;
~opened  ds    6                        boolean flags for channels; did we open them?
;
;  This flag is true if input is comming from .CONSOLE.  It is used to decide
;  if the console driver's line input routine should be used.  If 0, .CONSOLE
;  is not in use; if non-zero, this is the device number for .CONSOLE.
;
~inputIsConsole ds 2                    is standard in from .CONSOLE?
         end

****************************************************************
*
*  ~IOCom - Common data area
*
****************************************************************
*
~IOCom   privdata
~strlen   equ   40                      length of string buffer

~string   dstr  ,~strlen                string buffer in string format
~ASCII   ds    ~strlen                  string buffer in ASCII format
         end

****************************************************************
*
*  ~CV28 - Convert 2 byte integer to 8 byte integer
*
*  Inputs:
*        A - 2 byte integer
*
*  Outputs:
*        * - 8 byte integer
*
****************************************************************
*
~CV28    start

         phb                            save return addr
         tay
         pla
         sta   >retadl
         pla
         ldx   #0                       set X to sign part
         cpy   #$8000
         blt   lb1
         dex
lb1      phx                            save number
         phx
         phx
         phy
         pha                            restore return addr
         lda   >retadl
         pha
         plb
         rtl

retadl   ds    2
         end

****************************************************************
*
*  ~CV2S - Convert 2 byte integer to a string
*
*  Inputs:
*        A - 2 byte integer
*        * - address of string buffer
*
****************************************************************
*
~CV2S    start
         using ~IOCom

         pha                            save number to convert
         ph4   #~ASCII                  set addr of output string
         ph2   #~strlen                 set max length of string
         ph2   #1                       value is signed
         _Int2Dec                       convert the number
         jmp   ~MovStr                  move the string to the string buffer
         end

****************************************************************
*
*  ~CV42 - Convert 4 byte integer to 2 byte integer
*
*  Inputs:
*        * - 4 byte integer
*
*  Outputs:
*        A - 2 byte integer
*        V - set if an overflow occurred
*
****************************************************************
*
~CV42    start

         ldx   #0                       Y = value
         lda   4,S                      X = expected most sig word
         tay
         bpl   lb1
         dex
lb1      txa                            branch if most sig word does not match
         cmp   6,S
         bne   err
         clv
         bra   out

err      sep   #%01000000               SEV
out      lda   0,S                      fix return addr
         sta   4,S
         lda   2,S
         sta   6,S
         pla
         pla
         tya                            set value
         rtl
         end

****************************************************************
*
*  ~CV48 - Convert 4 byte integer to 8 byte integer
*
*  Inputs:
*        * - 4 byte integer
*
*  Outputs:
*        * - 8 byte integer
*
****************************************************************
*
~CV48    start

         pha                            make room for result
         pha
         lda   4,S                      move return address
         sta   0,S
         lda   6,S
         sta   2,S
         lda   8,S                      move number
         sta   4,S
         ldx   #0
         lda   10,S
         sta   6,S
         bpl   lb1                      set X to sign part
         dex
lb1      txa                            save sign part
         sta   8,S
         sta   10,S
         rtl
         end

****************************************************************
*
*  ~CV4S - Convert 4 byte integer to a string
*
*  Inputs:
*        NUM1 - 4 byte integer
*
****************************************************************
*
~CV4S    start
         using ~IOCom

         lda   2,S                      swap number and return addr
         tay
         lda   1,S
         tax
         lda   4,S
         sta   1,S
         lda   6,S
         sta   3,S
         txa
         sta   5,S
         tya
         sta   6,S
         ph4   #~ASCII                  set addr of output string
         ph2   #~strlen                 set max length of string
         ph2   #1                       value is signed
         _Long2Dec                      convert the number
         jmp   ~MovStr                  move the string to the string buffer
         end

****************************************************************
*
*  ~CV82 - Convert 8 byte integer to 2 byte integer
*
*  Inputs:
*        * - 8 byte integer
*
*  Outputs:
*        A - 2 byte integer
*        V - set if an overflow occurred
*
****************************************************************
*
~CV82    start

         ldx   #0                       X = expected most sig word
         lda   4,S
         tay
         bpl   lb1
         dex
lb1      txa                            branch if most sig word does not match
         cmp   6,S
         bne   err
         cmp   8,S
         bne   err
         cmp   10,S
         bne   err
         clv
         bra   out

err      sep   #%01000000               SEV
out      lda   0,S                      fix return addr
         sta   8,S
         lda   2,S
         sta   10,S
         pla
         pla
         pla
         pla
         tya                            set value
         rtl
         end

****************************************************************
*
*  ~CV84 - Convert 8 byte integer to 4 byte integer
*
*  Inputs:
*        * - 8 byte integer
*
*  Outputs:
*        * - 4 byte integer
*        V - set if an overflow occurred
*
****************************************************************
*
~CV84    start

         ldx   #0                       X = expected most sig word
         lda   6,S
         bpl   lb1
         dex
lb1      txa                            branch if most sig word does not match
         cmp   8,S
         bne   err
         cmp   10,S
         bne   err
         clv
         bra   out

err      sep   #%01000000               SEV
out      lda   4,S                      set return value
         sta   8,S
         lda   6,S
         sta   10,S
         lda   0,S                      fix return addr
         sta   4,S
         lda   2,S
         sta   6,S
         pla
         pla
         rtl
         end

****************************************************************
*
*  ~CV8S - Convert 8 byte integer to a string
*
*  Inputs:
*        num1 - 8 byte integer
*        STR - addr of string
*
****************************************************************
*
~CV8S    start
         using ~IOCom
num1     equ   12                       number to convert
adr      equ   8                        addr of string buffer
index    equ   3                        index into ASCII string buffer
neg      equ   1                        negative?

         pea   ~strlen-1                initialize index
         pea   0                        initialize neg
         tsc                            set up DP
         phd
         tcd
         short M                        blank string
         lda   #' '
         ldx   #~strlen-2
lb1      sta   >~ASCII,X
         dbpl  X,lb1
         lda   #'0'
         sta   ~ASCII+~strlen-1         set initial value
         long  M
         lda   num1+6                   if num1 < 0 then
         bpl   lb3
         inc   neg                        neg = 1
         sec                              num1 = -num1
         lda   #0
         sbc   num1
         sta   num1
         lda   #0
         sbc   num1+2
         sta   num1+2
         lda   #0
         sbc   num1+4
         sta   num1+4
         lda   #0
         sbc   num1+6
         sta   num1+6
         bpl   lb3                        if still neg, use default
         short M
         ldx   #L:bstr-1
lb2      lda   >bstr,X
         sta   >~ASCII+~strlen-L:bstr,X
         dbpl  X,lb2
         long  M
         bra   lb7
lb3      anop                           endif
lb4      lda   num1                     while num1 <> 0 do
         ora   num1+2
         ora   num1+4
         ora   num1+6
         beq   lb5
         ph8   num1                       num1 = num1 div 10
         lda   #0                         A = num1 mod 10
         pha
         pha
         pha
         pea   10
         jsl   ~Div8
         pl8   num1
         pla
         plx
         plx
         plx
         ldx   index                      save digit
         ora   #'0'
         short M
         sta   >~ASCII,X
         long  M
         dec   index
         bra   lb4                      endwhile
lb5      lda   neg                      if neg then
         beq   lb6
         short M                          save - char
         lda   #'-'
         ldx   index
         sta   >~ASCII,X
         long  M
lb6      anop                           endif
lb7      ph4   <adr                     move the characters to the string buffer
         jsl   ~MovStr
         move4 4,num1+4                 fix return addr
         pld                            fix DP
         clc                            fix stack
         tsc
         adc   #16
         tcs
         rtl

bstr     dc    C'-9223372036854775808'
         end

****************************************************************
*
*  ~CVS2 - Convert string to 2 byte integer
*
*  Inputs:
*        adr - addr of string to convert
*
*  Outputs:
*        A - number
*        V - set if error, else clear
*
****************************************************************
*
~CVS2    start
adr      equ   4

         tsc                            set up DP
         phd
         tcd
         pha                            make space for result
         clc                            push addr of string
         lda   adr
         adc   #2
         tax
         lda   adr+2
         adc   #0
         pha
         phx
         LDY   #1                       push length of string
         lda   [adr],Y
         and   #$00FF
         pha
         pea   1                        push sign flag
         _Dec2Int                       convert to integer
         pla                            recover value
         ldx   0                        move return addr
         stx   4
         ldx   2
         stx   6
         clv                            V = C
         bcc   lb1
         sep   #$40
lb1      pld                            reset DP
         plx                            remove address
         plx
         rtl
         end

****************************************************************
*
*  ~CVS4 - Convert string to 4 byte integer
*
*  Inputs:
*        adr - address of string to convert
*
*  Outputs:
*        adr - number
*        V - set if error, else clear
*
****************************************************************
*
~CVS4    start
adr      equ   4

         tsc                            set up DP
         phd
         tcd
         pha                            make space for result
         pha
         clc                            push addr of string
         lda   adr
         adc   #2
         tax
         lda   adr+2
         adc   #0
         pha
         phx
         ldy   #1                       push length of string
         lda   [adr],Y
         and   #$00FF
         pha
         pea   1                        push sign flag
         _Dec2Long                      convert to integer
         pla                            recover value
         sta   adr                      move return addr
         pla
         sta   adr+2
         clv                            V = C
         bcc   lb1
         sep   #$40
lb1      pld                            reset DP
         rtl
         end

****************************************************************
*
*  ~CVS8 - Convert string to 8 byte integer
*
*  Inputs:
*        adr - address of string to convert
*
*  Outputs:
*        NUM - number
*        V - set if error, else clear
*
****************************************************************
*
~CVS8    start
adr      equ   18                       address of string
RETURN   equ   15                       return address
neg      equ   13                       negative?
cc       equ   11                       disp in string
maxlen   equ   9                        max disp in string
num      equ   1                        number

         tsc                            set up DP
         sec
         sbc   #14
         tcs
         phd
         tcd
         ldy   #1                       maxlen = len(adr)+2
         lda   [adr],Y
         and   #$00FF
         inc   A
         inc   A
         sta   maxlen
         stz   num                      num = 0
         stz   num+2
         stz   num+4
         stz   num+6
         stz   neg                      neg = false
         ldy   #2                       CC = 2
lb1      lda   [adr],Y                  while adr[CC] = ' ' do
         and   #$00FF
         cmp   #' '
         bne   lb2
         iny                              ++CC
         bra   lb1                      endwhile
lb2      lda   [adr],Y                  if adr[CC] = '-' then
         and   #$00FF
         cmp   #'-'
         bne   lb3
         inc   neg                        neg = true
         iny                              ++cc
lb3      sty   cc                       endif
lb4      ldy   cc                       while (cc < maxlen)
         cpy   maxlen                     and isdigit(adr[cc]) do
         bge   lb6
         lda   [adr],Y
         and   #$00FF
         cmp   #'0'
         blt   lb6
         cmp   #'9'+1
         bge   lb6
         mul8  num,#10                    num = num*10
         bvs   err                        if overflow then error
         ldy   cc                         num = num+adr[cc]
         lda   [adr],Y
         and   #$000F
         clc
         adc   num
         sta   num
         bcc   lb5
         inc   num+2
         bne   lb5
         inc   num+4
         bne   lb5
         inc   num+6
         bmi   err                        if num < 0 then error
lb5      iny                              ++cc
         sty   cc
         bra   lb4                      endwhile
lb6      lda   neg                      if neg then
         beq   lb7
         sub8  #0,num,num                 num = -num
lb7      anop                           endif
         clv
         bra   lb8
err      sep   #$40                     SEV
lb8      move4 RETURN,cc
         move4 num,RETURN-1
         move4 num+4,adr
         pld
         pla
         pla
         pla
         pla
         pla
         rtl
         end

****************************************************************
*
*  ~GET2 - Get a Two Byte Integer
*
*  Inputs:
*        X - CR flag
*
*  Outputs:
*        A - result
*        V - set for number overflow
*
****************************************************************
*
~GET2    start
         using ~IOCom

         jsl   ~GETN                    read the number
         bvs   err
         ph4   #~string                 convert string to number
         jsl   ~CVS2
err      rtl
         end

****************************************************************
*
*  ~GET4 - Get a Four Byte Integer
*
*  Inputs:
*        X - CR flag
*
*  Outputs:
*        * - result
*        V - set for number overflow
*
****************************************************************
*
~GET4    start
         using ~IOCom

         jsl   ~GETN                    read the number
         bvc   lb1
         ph4   #0
         bra   lb2
lb1      ph4   #~string                 convert string to number
         jsl   ~CVS4
lb2      php                            swap RETURN, number
         lda   6,S
         tax
         lda   7,S
         tay
         lda   4,S
         sta   7,S
         lda   2,S
         sta   5,S
         txa
         sta   2,S
         tya
         sta   3,S
         plp
         rtl
         end

****************************************************************
*
*  ~GET8 - Get an Eight Byte Integer
*
*  Inputs:
*        X - CR flag
*
*  Outputs:
*        * - result
*        V - set for number overflow
*
****************************************************************
*
~GET8    start
         using ~IOCom

         jsl   ~GETN                    read the number
         bvc   lb1
         ph8   #0
         bra   lb2
lb1      ph4   #~string                 convert string to number
         jsl   ~CVS8
lb2      php                            swap RETURN, number
         lda   10,S
         tax
         lda   11,S
         tay
         lda   8,S
         sta   11,S
         lda   6,S
         sta   9,S
         lda   4,S
         sta   7,S
         lda   2,S
         sta   5,S
         txa
         sta   2,S
         tya
         sta   3,S
         plp
         rtl
         end

****************************************************************
*
*  ~GETC - Get a Character
*
*  Inputs:
*        X - carriage return flag
*
*  Outputs:
*        A - character read
*
****************************************************************
*
~GETC    start
RETURN   equ   $0D                      RETURN key code

         phx                            save CR
         jsl   SysKeyin
         plx                            fetch CR
         cmp   #RETURN                  done if we got a CR
         beq   GC3
         txy                            done if no skip to CR needed
         beq   GC3
         pha
GC2      jsl   SysKeyin                 skip to CR (or EOF)
         tax
         beq   GC2a
         cmp   #RETURN
         bne   GC2
GC2a     pla
GC3      rtl
         end

****************************************************************
*
*  ~GETN - Get a Numeric String
*
*  Inputs:
*        X - CR flag
*
*  Outputs:
*        ~string - result
*        V - set for no string
*
*  Notes:
*        1)  The integer ends with the first non-blank, non-
*              numeric character.
*        2)  Leading signs are allowed.
*        3)  Leading white space is skipped, Pascal style
*
****************************************************************
*
~GETN    start
         using ~IOCom
CR       equ   3                        carriage return flag
cc       equ   1                        character counter
TAB      equ   9                        TAB key code
RETURN   equ   13                       RETURN key code

         phx                            CR = X
         pea   2                        cc = 2
         tsc                            set up DP
         phd
         tcd
lb1      jsl   SysKeyin                 A := SysKeyin
         cmp   #TAB                     while whitespace(A) do
         beq   lb1                        A := SysKeyin
         cmp   #RETURN                  endwhile
         beq   lb1
         cmp   #' '
         beq   lb1
         cmp   #'-'                     if A = '-' then
         bne   lb2
         ldx   cc                         ~string[cc] := A
         sta   >~string,X
         inx                              ++cc
         stx   cc
         jsl   SysKeyin                   A := SysKeyin
lb2      anop                           endif
         jsr   IsDigit                  if IsDigit(A) then
         jcc   lb9
         cmp   #'0'                       if A = '0' then
         bne   lb4
         ldx   cc                           ~string[cc] := A
         sta   >~string,X
         inx                                ++cc
         stx   cc
lb3      cmp   #'0'                         while A = '0' do
         bne   lb4
         jsl   SysKeyin                       A := SysKeyin
         bra   lb3                          endwhile
lb4      anop                             endif
lb5      ldx   cc                         while (cc < ~strlen)
         cpx   #~strlen
         bge   lb6
         jsr   IsDigit                      and IsDigit(cc) do
         bcc   lb6
         sta   >~string,X                   ~string[cc] := A
         inx                                ++cc
         stx   cc
         jsl   SysKeyin                     A := SysKeyin
         bra   lb5                        endwhile
lb6      ldx   CR                         if CR then
         beq   lb8
lb7      cmp   #RETURN                      while A <> RETURN do
         beq   lb8
         tax
         beq   lb8
         jsl   SysKeyin                       A := SysKeyin
         bra   lb7                          endwhile
lb8      anop                             endif
         pha                              SysPutback(A)
         jsl   SysPutback
         short M                          set string length
         lda   cc
         dec   A
         dec   A
         sta   ~string+1
         long  M
         clv                              clv
         bra   lb10
lb9      anop                           else
         sep   #$40                       SEV
lb10     anop                           endif
         pld                            reset DP
         pla                            pull work space off of stack
         pla
         rtl
;
;  IsDigit
;
IsDigit  cmp   #'0'
         blt   no
         cmp   #'9'+1
         bge   no
         sec
         rts
no       clc
         rts
         end

****************************************************************
*
*  ~GetRef - get a reference number
*
*  Inputs:
*        prefix - prefix number
*
*  Outputs:
*        A - reference number for a GS/OS file, or 0 for an
*            error.
*
*  Notes:  If the file is opened, ~opened[prefix] is true.
*
****************************************************************
*
~GetRef  private
         using ~GSOSIO
refnum   equ   1                        reference number
handle   equ   3                        memory handle
ptr      equ   7                        memory pointer

buffSize equ   8*1024

         lsub  (2:prefix),10

         stz   refNum                   refNum := 0 (in case of error)
         lda   prefix                   ~opened[prefix] := false
         asl   A
         tax
         stz   ~opened-20,X

         lda   prefix                   try for an opened file
         sta   gsPrefixNum
         GetStdRefnumGS gsRec
         bcs   lb1
         lda   gsRefnum
         beq   lb1
         sta   refnum
         brl   lb4

lb1      pha                            get a file buffer
         pha
         ph4   #buffSize
         lda   >~User_ID
         pha
         bne   lb1a
         pha
         pha
         pha
         ph4   #~GetRef
         _FindHandle
         _SetHandleID
lb1a     ph2   #$C010
         ph4   #0
         _NewHandle
         ply
         plx
         bcs   lb4
         stx   handle+2                 dereference the file buffer handle
         sty   handle
         ldy   #2
         lda   [handle],Y
         sta   ptr+2
         sta   gpPrefix+2
         sta   grPathname+2
         sta   opPathname+2
         lda   [handle]
         sta   ptr
         sta   gpPrefix
         inc   A
         inc   A
         sta   grPathname
         sta   opPathname
         lda   #buffSize                set the buffer size
         sta   [ptr]
         lda   prefix                   get the prefix name
         sta   gpPrefixNum
         GetPrefixGS gpRec
         bcs   lb3
         GetRefNumGS grRec              if the file is open then
         bcs   lb2
         lda   grRefnum                   return the reference number
         sta   refnum
         bne   lb3
lb2      OpenGS opRec                   if the file can be opened then
         bcs   lb3
         lda   opRefnum                   return the reference number
         sta   refNum

lb3      ph4   <handle                  dispose of the file name buffer
         _DisposeHandle

lb4      lret  2:refnum

gpRec    dc    i'2'                     GetPrefix record
gpPrefixNum ds 2
gpPrefix ds    4

gsRec    dc    i'2'                     GetStdRefNum record
gsPrefixNum ds 2
gsRefnum ds    2

grRec    dc    i'2'                     GetRefNum record
grPathname ds  4
grRefnum ds    2

opRec    dc    i'2'                     Open record
opRefnum ds    2
opPathname ds  4
         end

****************************************************************
*
*  ~GETS - String Input
*
*  Inputs:
*        adr - address of string
*
****************************************************************
*
~GETS    start
RETURN   equ   13                       RETURN key code
adr      equ   6                        address of string
len      equ   1                        length of string

         pha                            make room for len
         tsc                            set up DP
         phd
         tcd
         lda   [adr]                    get length of string
         inc4  adr
         and   #$FF
         sta   len
         short M
         beq   gt2
         ldy   #1                       place string in output buffer
gt1      long  M
         phy
         jsl   SysKeyin
         ply
         short M
         sta   [adr],Y
         cmp   #RETURN
         beq   gt3
         tax
         beq   gt3
         iny
         cpy   len
         blt   gt1
gt2      long  M
         phy
         jsl   SysKeyin                 if not at end, skip to eol
         ply
         short M
         cmp   #0
         beq   gt3
         cmp   #RETURN
         bne   gt2
gt3      dey                            set the length of the string
         tya
         sta   [adr]
         long  M
         move4 2,adr                    fix return addr
         pld                            reset DP
         pla                            remove work space from stack
         pla
         pla
         rtl
         end

****************************************************************
*
*  ~MovStr - Move a string to its destination
*
*  Inputs:
*        ASCII - ASCII character sequence
*        * - address of string buffer
*
****************************************************************
*
~MovStr  start
         using ~IOCom
str      equ   6                        address of string buffer

         phd                            set up DP
         tsc
         tcd
         short M                        find disp to start of string
         ldx   #0
         lda   #' '
lb1      cmp   >~ASCII,X
         bne   lb2
         inx
         bra   lb1
lb2      long  M                        compute length of string
         txa
         sec
         sbc   #~strlen
         eor   #$FFFF
         inc   A
         short M                        see if string is too long
         cmp   [str]
         bgt   error
         ldy   #1                       set length of string
         sta   [str],Y
lb3      lda   >~ASCII,X                copy characters to string buffer
         iny
         sta   [str],Y
         inx
         cpx   #~strlen
         blt   lb3
         long  M
         clv                            clear error flag
         bra   lb4

error    long  M                        flag error
         sep   #%01000000               SEV

lb4      lda   2,S                      fix return addr
         sta   6,S
         lda   4,S
         sta   8,S
         pld                            fix DP
         pla                            remove extra stack stuff
         pla
         rtl
         end

****************************************************************
*
*  ~PRBL - Print Blanks
*
*  Inputs:
*        A - number of blanks to print
*        Y - error out flag
*
****************************************************************
*
~PRBL    start
count    equ   3                        # blanks to print

         pha                            save # blanks
         phd                            set up DP addressing
         tsc
         tcd

         tya                            branch to the proper print loop
         beq   lb2

lb1      pea   ' '                      print blanks to error out
         jsl   SysCharErrout
         dec   count
         bne   lb1
         bra   lb3

lb2      pea   ' '                      print blanks to standard out
         jsl   SysCharOut
         dec   count
         bne   lb2

lb3      pld                            restore everything
         pla
         rtl
         end

****************************************************************
*
*  ~PUT2 - Format a Two Byte Integer for Output
*
*  Inputs:
*        N - number to write
*        F1 - field width
*        CR - carriage return flag
*        err - error output flag
*
****************************************************************
*
~PUT2    start
         using ~IOCom
N        equ   10                       number to write
F1       equ   8                        field width
CR       equ   6                        carriage return flag
err      equ   4                        error output flag

         tsc                            set up DP
         phd
         tcd
         ph4   #~string                 convert number to string
         lda   N
         jsl   ~CV2S
         ph4   #~string                 write the string
         ph2   <F1
         ph2   <CR
         ph2   <err
         jsl   ~PUTS
         move4 0,F1                     patch return addr
         pld                            fix DP
         pla                            remove extra stack space
         pla
         pla
         pla
         rtl
         end

****************************************************************
*
*  ~PUT4 - Format a Four Byte Integer for Output
*
*  Inputs:
*        N - number to write
*        F1 - field width
*        CR - carriage return flag
*        err - error output flag
*
****************************************************************
*
~PUT4    start
         using ~IOCom
N        equ   10                       number to write
F1       equ   8                        field width
CR       equ   6                        carriage return flag
err      equ   4                        error output flag

         ph4   #~string                 push addr of string
         lda   N+6,S                    convert number to string
         pha
         lda   N+6,S
         pha
         jsl   ~CV4S
         lda   #^~string                write the string
         sta   N+2,S
         lda   #~string
         sta   N,S
         jmp   ~PUTS
         end

****************************************************************
*
*  ~PUT8 - Format an Eight Byte Integer for Output
*
*  Inputs:
*        N - number to write
*        F1 - field width
*        CR - carriage return flag
*        err - error output flag
*
****************************************************************
*
~PUT8    start
         using ~IOCom
N        equ   10                       number to write
F1       equ   8                        field width
CR       equ   6                        carriage return flag
err      equ   4                        error output flag

         tsc                            set up DP
         phd
         tcd
         ph8   N                        convert number to string
         ph4   #~string
         jsl   ~CV8S
         ph4   #~string                 write the string
         ph2   <F1
         ph2   <CR
         ph2   <err
         jsl   ~PUTS
         move4 0,N+4                    patch return addr
         pld                            fix DP
         clc                            remove extra stack space
         tsc
         adc   #14
         tcs
         rtl
         end

****************************************************************
*
*  ~PUTB - Format a Boolean Variable
*
*  Inputs:
*        N - boolean value to write
*        F1 - field width
*        CR - carriage return flag
*        err - error output flag
*
****************************************************************
*
~PUTB    start
         using ~IOCom
N        equ   10                       number to write
F1       equ   8                        field width
CR       equ   6                        carriage return flag
err      equ   4                        error output flag

         tsc                            set up DP
         phd
         tcd
         lda   N                        push addr of correct string
         beq   lb1
         ph4   #true
         bra   lb2
lb1      ph4   #false
lb2      ph2   <F1                      write the string
         ph2   <CR
         ph2   <err
         jsl   ~PUTS
         move4 0,F1                     patch return addr
         pld                            fix DP
         pla                            remove extra stack space
         pla
         pla
         pla
         rtl

true     dstr  true
false    dstr  false
         end

****************************************************************
*
*  ~PUTC - Format a Character for Output
*
*  Inputs:
*        N - character to write
*        F1 - field width
*        CR - carriage return flag
*        err - error output flag
*
****************************************************************
*
~PUTC    start
         using ~IOCom
N        equ   10                       number to write
F1       equ   8                        field width
CR       equ   6                        carriage return flag
err      equ   4                        error output flag

         tsc                            set up DP
         phd
         tcd
         lda   #1                       set up string
         sta   ~string+1
         lda   N
         sta   ~string+2
         ph4   #~string                 write the string
         ph2   <F1
         ph2   <CR
         ph2   <err
         jsl   ~PUTS
         move4 0,F1                     patch return addr
         pld                            fix DP
         pla                            remove extra stack space
         pla
         pla
         pla
         rtl
         end

****************************************************************
*
*  ~PUTS - String Output
*
*  Inputs:
*        adr - address of string to write
*        F1 - field width
*        CR - carriage return flag
*        err - error output flag
*
****************************************************************
*
~PUTS    start
         using ~IOCom
adr      equ   10                       address of string to write
F1       equ   8                        field width
CR       equ   6                        carriage return flag
err      equ   4                        error output flag

         tsc                            set up DP
         phd
         tcd
         inc4  adr
         lda   [adr]                    A = - # lead blanks
         and   #$00FF
         sec
         sbc   F1
         bpl   lb1                      if A < 0 then
         eor   #$FFFF                     A = -A
         inc   A
         ldy   err                        print A blanks
         jsl   ~PRBL
lb1      anop                           endif

         lda   [adr]                    f1 = length(adr^)
         and   #$00FF
         beq   lb4                      if f1 = 0 then
         sta   f1                         goto lb2
         inc4  adr                      ++adr
         lda   err                      if err then
         beq   lb3
lb2      lda   [adr]                      write the string to error out
         and   #$00FF
         pha
         jsl   SysCharErrout
         inc4  adr
         dec   f1
         bne   lb2
         lda   cr                         if cr then
         beq   lb4
         pea   13                           write a return character
         jsl   SysCharErrout
         bra   lb4                      else
lb3      lda   [adr]                      write the string to standard out
         and   #$00FF
         pha
         jsl   SysCharOut
         inc4  adr
         dec   f1
         bne   lb3
         lda   cr                         if cr then
         beq   lb4
         pea   13                           write a return character
         jsl   SysCharOut
lb4      anop                           endif

         move4 0,adr                    patch return addr
         pld                            fix DP
         clc                            remove extra stack space
         tsc
         adc   #10
         tcs
         rtl
         end

****************************************************************
*
*  SysCharErrout - write a character to error out
*
*  Inputs:
*        ch - character to write
*
****************************************************************
*
SysCharErrout start
         using ~GSOSIO

         jmp   trap                     used for intercepting I/O
trap     anop

         lsub  (2:ch),0

         lda   ch
         sta   >lch
         lda   >~erroutRefnum
         sta   >wrRefnum
         WriteGS wrRec

         lret

wrRec    dc    i'4'
wrRefnum ds    2
         dc    a4'lch'
         dc    i4'1'
         ds    4

lch      ds    2
         end

****************************************************************
*
*  SysCharOut - write a character to standard out
*
*  Inputs:
*        ch - character to write
*
****************************************************************
*
SysCharOut start
         using ~GSOSIO

         jmp   trap                     used for intercepting I/O
trap     anop

         lsub  (2:ch),0

         lda   ch
         sta   >lch
         lda   >~stoutRefnum
         sta   >wrRefnum
         WriteGS wrRec

         lret

wrRec    dc    i'4'
wrRefnum ds    2
         dc    a4'lch'
         dc    i4'1'
         ds    4

lch      ds    2
         end    

****************************************************************
*
*  SysGraphTextCharOut - Single character graphics output
*
****************************************************************
*
SysGraphTextCharOut private
return   equ   13                       key codes
lineFeed equ   10
ch       equ   5                        location of ch on stack

         phb
         phk
         plb
;
;  Check the pen location
;
         ph4   #cPoint                  if pen location has changed from the
         _GetPen                          last time we were called
         lda   cPoint
         cmp   lPoint
         bne   wt1
         lda   cPoint+2
         cmp   lPoint+2
         beq   wt2
wt1      move4 cPoint,rPoint              set new return value
;
;  Printable character output
;
wt2      lda   ch,S                     if this is not a CR then
         cmp   #return
         beq   wt3
         cmp   #lineFeed                  ignore lineFeed character
         beq   wt5
         pha                              write the character
         _DrawChar
         bra   wt5
;
;  CR output
;
wt3      lda   #'A'                     else handle CR
         pha
         ph4   #rect                      get the character bounds
         _CharBounds
         sec                              compute the line feed length
         lda   rect+4
         sbc   rect
         clc
         adc   rPoint
         sta   rPoint

         ph4   rPoint                     do the return
         _MoveTo
;
;  Record the new pen location
;
wt5      ph4   #lPoint                  get the last pen location
         _GetPen

         plx                            remove parameter
         ply
         pla
         phy
         phx
         plb                            restore B
         rtl
;
;  Local data
;
cPoint   ds    4                        current pen location
lPoint   ds    4                        last known pen location
rPoint   ds    4                        return point
rect     ds    8                        character bounds rect
         end

****************************************************************
*
*  SysGraphTextLineIn - Graphics line input
*
*  Outputs:
*        ~lineLength - new line length
*        ~lineDisp - 0
*        ~line - input line
*
*  Notes: This subroutine is not currently tied to the actual
*        output device.  It is also used by the CDA console
*        driver as an input system.
*
****************************************************************
*
SysGraphTextLineIn start
         using ~GSOSIO

         phb
         phk
         plb

         stz   emActive                 set the emActive flag
         pha
         _EMStatus
         pla
         bcs   gt1
         tay
         beq   gt1
         inc   emActive
gt1      KeyPressGS kpRec               set the shellActive flag
         lda   #0
         rol   A
         eor   #$0001
         sta   shellActive
         stz   ~lineDisp                ~lineDisp = 0
         stz   ~lineLength              ~lineLength = 0

gt2      jsr   GetCh                    read a character
         pha                            echo it
         pha
         jsl   SysCharOut
         pla
         short M                        save it
         ldx   ~lineLength
         sta   ~line,X
         long  M
         cpx   #255                     if there is room in the line buffer then
         bge   gt3
         inc   ~lineLength                ++~lineLength
gt3      tax                            if not ch in [0, 13] then
         beq   gt4                        loop
         cmp   #13
         bne   gt2

gt4      plb
         rtl
;
;  Read a single character
;
GetCh    lda   emActive                 if emActive then
         beq   gc3

gc1      pha                              get a key event from the event manager
         ph2   #$0028
         ph4   #evRec
         _GetNextEvent
         pla
         beq   gc1
         lda   evWhat
         cmp   #5
         beq   gc2
         cmp   #3
         bne   gc1
gc2      lda   evMessage
         and   #$00FF
         rts

gc3      lda   shellActive              else if shellActive then
         beq   gc4
         ReadKeyGS rkRec                  get a key from the shell
         lda   rkKey
         rts

gc4      short M                        else
gc5      lda   >$00C000                   get a key from the keyboard latch
         bpl   gc5
         sta   >$00C010
         long  M
         and   #$007F
         rts
;               
;  Local data
;
emActive ds    2                        is the event manager active?
shellActive ds 2                        is the ORCA/Shell active?

kpRec    dc    i'3'                     KeyPressGS record
         ds    2
         ds    2
         ds    2

rkRec    dc    i'2'                     ReadKeyGS record
rkKey    ds    2
         ds    2

evRec    anop                           event record
evWhat   ds    2
evMessage ds   4
evWhen   ds    4
evWhere  ds    4
evModifiers ds 2
         end

****************************************************************
*
*  SysGraphTextShutdown - reverse the effects of SysGraphTextStartup
*
****************************************************************
*
SysGraphTextShutdown start

         lda   #SysCharErrout+3
         sta   >SysCharErrout+1
         lda   #SysCharOut+3
         sta   >SysCharOut+1
         lda   #SysLineIn+3
         sta   >SysLineIn+1
         rtl
         end

****************************************************************
*
*  SysGraphTextStartup - send console output to the current window
*
****************************************************************
*
SysGraphTextStartup start
                               
         lda   #SysGraphTextCharOut
         sta   >SysCharErrout+1
         sta   >SysCharOut+1
         lda   #SysGraphTextLineIn
         sta   >SysLineIn+1
         rtl
         end

****************************************************************
*
*  SysIOShutdown - Shut down the GS/OS text I/O system
*
****************************************************************
*
SysIOShutDown start
         using ~GSOSIO

         phb
         phk
         plb
         lda   ~opened                  if ~opened[10] then
         beq   lb1
         lda   ~stinRefnum                Close(~stinRefnum)
         sta   clRefnum
         CloseGS clRec
lb1      lda   ~opened+2                if ~opened[11] then
         beq   lb2
         lda   ~stoutRefnum               Close(~stoutRefnum)
         sta   clRefnum
         CloseGS clRec
lb2      lda   ~opened+4                if ~opened[12] then
         beq   lb3
         lda   ~erroutRefnum              Close(~erroutRefnum)
         sta   clRefnum
         CloseGS clRec
lb3      plb
         rtl

clRec    dc    i'1'                     Close record
clRefnum ds    2
         end

****************************************************************
*
*  SysKeyAvail - see if a keypress is avaliable
*
*  Outputs:
*        A - 1 if available, else 0
*
****************************************************************
*
SysKeyAvail start
         using ~GSOSIO

         lda   ~putback                 if ~putback <> 0 then
         bne   true                       return true
         lda   ~lineLength              if ~lineLength <> 0 then
         beq   lb1
         cmp   ~lineDisp                  if ~lineLength <> ~lineDisp then
         beq   lb1                          return ~line[~lineLength] <> 0
         ldx   ~lineDisp
         lda   ~line,X
         and   #$00FF
         bne   true
         bra   false

lb1      lda   ~inputIsConsole          if ~inputIsConsole then
         beq   lb4
         pha                              if the Event Manager is active then
         _EMStatus
         pla
         bcs   lb2
         tay
         beq   lb2
         pha                                if a keypress is available then
         ph2   #$0028                         return true
         ph4   evRec                        else
         _EventAvail                          return false
         pla
         bcs   false
         tay
         beq   false
         bra   true
lb2      KeyPressGS kpRec                 else if shell is active then
         bcs   lb3
         lda   kpAvailable                  return kpAvailable
         rtl

lb3      short M                          else
         lda   >$00C000                     return [$0C000000] & $0080
         long  M
         and   #$0080
         bne   true
         bra   false
lb4      jsl   SysKeyin                 else
         sta   ~putback                   ~putback := SysKeyin
         tax                              return ~putback <> 0
         beq   false

true     lda   #1
         rtl

false    lda   #0
         rtl

evRec    ds    16                       dummy event record

kpRec    dc    i'3'
         ds    2
         ds    2
kpAvailable ds 2
         end

****************************************************************
*
*  SysKeyin - read a keypress
*
****************************************************************
*
SysKeyin start
         using ~GSOSIO

         phb
         phk
         plb

top      lda   ~putback                 if ~putback then
         beq   lb1
         stz   ~putback                   return ~putback, zeroing it
         plb
         rtl

lb1      ldx   ~lineDisp                if ~lineDisp < lineLength then
         cpx   ~lineLength
         bge   lb3
         lda   ~line,X                    return char, incrementing ~lineDisp
         and   #$00FF                       if the char is not chr(0)
         beq   lb2
         inc   ~lineDisp
lb2      plb
         rtl

lb3      jsl   SysLineIn                read an input line
         bra   top                      loop
         end

****************************************************************
*
*  SysLinein - read an input line
*
*  Outputs:
*        ~lineLength - new line length
*        ~lineDisp - 0
*        ~line - input line
*
****************************************************************
*
SysLinein start
         using ~GSOSIO

         jmp   trap                     used for intercepting I/O
trap     anop

         phb
         phk
         plb
         lda   ~inputIsConsole          if ~inputIsConsole then
         beq   lb4
         jsr   ReadLine                   read a new line
         plb                              return
         rtl

lb4      lda   ~stinRefnum              else read a char from a file
         sta   rdRefnum
         stz   ~lineDisp
         lda   #1
         sta   ~lineLength
         stz   ~line
         ReadGS rdRec
         plb
         rtl
;
;  ReadLine - read a line from .CONSOLE
;
ReadLine anop

         lda   ~inputIsConsole          set the device number
         sta   trDevnum
         sta   drDevnum
         sta   gsDevnum
         sta   frDevnum
         sta   rtDevnum
         DControlGS trRec               set the terminators
         DControlGS frRec               do a formatted read
         DReadGS drRec                  read a line
         DStatusGS gsRec                find the terminator
         ldx   drTransferCount          set the line length
         inx
         stx   ~lineLength
         short M                        put the terminator in the line
         lda   gsLastTermChar
         cmp   #$2E
         bne   rl1
         lda   #0
         bra   rl2
rl1      cmp   #13
         bne   rl2
         long  M
         phx                            write a trailing CR
         DWriteGS rtRec
         plx
         short M
         lda   #13
rl2      sta   ~line-1,X
         long  M
         stz   ~lineDisp                start the scanner at the first char
         rts
;
;  Local data
;
trRec    dc    i'5'                     DControl record for setting terminators
trDevnum ds    2
         dc    i'$8001'
         dc    a4'terminators'          control list ptr
         dc    i4'l:termList+4'         request count
         dc    i4'0'                    transfer count

terminators dc i'$827F,l:termList/2'
termList dc    i'$0200,$802E,$000D'     ctrl-@, open-apple ., RETURN

drRec    dc    i'6'                     DRead record
drDevnum ds    2
         dc    a4'~line'
         dc    i4'255'
         dc    i4'0'
         dc    i'0'
drTransferCount ds 4

gsRec    dc    i'5'                     DStatus record for reading the input port
gsDevnum ds    2
         dc    i'$8001'
         dc    a4'list'                 control list ptr
         dc    i4'17'                   request count
         dc    i4'0'                    transfer count

list     ds    1                        fill character
         ds    1                        default cursor mode
         ds    1                        current cursor mode
         ds    1                        beep flag
         ds    1                        initial entry flag
         ds    1                        exit type
         ds    1                        last char read
         ds    1                        last modifier key read
gsLastTermChar ds 1                     last terminator read
         ds    1                        last terminator modifier
         ds    1                        cursor position
         ds    1                        length of the returned string
         ds    1                        input field
         ds    1                        horizontal cursor position
         ds    2                        UIR origin.x
         ds    1                        vertical cursor position
                 
rdRec    dc    i'4'                     Read record
rdRefnum ds    2
         dc    a4'~line'
         dc    i4'1'
         ds    4

frRec    anop                           DCB for D_Control to request UIM read
         dc    i'5'                     parameter count
frDevnum ds    2                        device number
         dc    i'$8003'                 control code
         dc    a4'zero'                 control list ptr
         dc    i4'2'                    request count
         dc    i4'0'

zero     dc    i'0'

rtRec    anop                           DCB for D_Write to write a CR
         dc    i'6'                     parameter count
rtDevnum ds    2                        device number
         dc    a4'rtBuffer'             buffer containing the character
         dc    i4'1'                    number of characters to write
         dc    i4'0'                    starting block (unused)
         dc    i'0'                     block size (unused)
         ds    4                        transfer count

rtBuffer dc    i1'13'
         END

****************************************************************
*
*  SysIOStartup - Start up the GS/OS text I/O system
*
****************************************************************
*
SysIOStartup start
         using ~GSOSIO

         phb
         phk
         plb
         stz   ~line                    start with an empty input line
         stz   ~lineDisp
         stz   ~lineLength
         stz   ~putback                 nothing in the putback buffer
         ph2   #10                      set the standard in reference number
         jsl   ~GetRef
         sta   ~stinRefnum

         stz   ~inputIsConsole          ~inputIsConsole := false
         lda   ~stinRefnum              get the name for ~stinRefnum
         sta   giRefnum
         GetRefInfoGS giRec
         bcs   lb1                      if (toolerror = 0)
         lda   name+2                      and (name = '.CONSOLE') then
         cmp   #9
         bne   la1
         lda   name+12
         and   #$00FF
         cmp   #':'
         bne   lb1
         dec   name+2
la1      ldy   #8
lb0      lda   name+2,Y
         cmp   console,Y
         bne   lb1
         dey
         dey
         bpl   lb0
         GetDevNumberGS grRec               inputIsConsole := GetDevNumber(.CONSOLE)
         bcs   lb1
         lda   grRefnum
         sta   ~inputIsConsole
lb1      anop                           endif

         ph2   #11                      set the standard out reference number
         jsl   ~GetRef
         sta   ~stoutRefnum
                          
         ph2   #12                      set the error out reference number
         jsl   ~GetRef
         sta   ~erroutRefnum
         plb
         rtl

giRec    dc    i'3'                     GetRefInfo record
giRefnum ds    2
         ds    2
         dc    a4'name'

name     dc    i'13,0',9c' '            name returned by GetRefInfo

grRec    dc    i'2'                     GetDevNumber record
         dc    a4'console'
grRefnum ds    2

console  dosw  '.CONSOLE'
         end

****************************************************************
*
*  SysPutback - put a character in the putback buffer
*
*  Inputs:
*        ch - character to put back
*
****************************************************************
*
SysPutback start
         using ~GSOSIO

         phb
         plx
         ply
         pla
         sta   ~putback
         phy
         phx
         plb
         rtl
         end
