         MCOPY I2.MACROS
         KEEP  OBJ/I2
****************************************************************
*
*  Two byte integer math libraries.
*
*  By Mike Westerfield
*
*  Copyright January 1987, All rights reserved
*  By the Byte Works, Inc.
*
*  Revised July 1987 for ORCA/M 1.0b:
*
*     1. MOD2 corrected to give positive results when the first
*        argument is negative.
*     2. DIV2 corrected so it doesn't hang on divide by 0
*
****************************************************************
*
DUMMY    START
         END

****************************************************************
*
*  ~DIV2 - Two Byte Signed Integer Divide
*
*  Inputs:
*        X - denominator
*        A - numerator
*
*  Outputs:
*        A - result
*        X - remainder
*        V - set for division by zero
*
*  Notes:
*        1) Assumes long A and X on entry.
*
****************************************************************
*
~DIV2    START
NUM1     EQU   5                        numerator
NUM2     EQU   3                        denominator
SIGN     EQU   1                        sign flag

         LDY   #0                       make all arguments positive
         BIT   #$8000                    start with A
         BEQ   DV1
         EOR   #$FFFF
         INC   A
         INY
DV1      PHA                            save it
         TXA                            now do X
         BEQ   ERR
         BPL   DV2
         DEY
         EOR   #$FFFF
         INC   A
DV2      PHA                            save it
         PHY                            save sign
         TSC                            set up DP
         PHD
         TCD

         LDA   #0                       initialize the remainder
         LDY   #16                      16 bits to go
DV3      ASL   NUM1                     roll up the next number
         ROL   A
         SEC                            subtract the digit
         SBC   NUM2
         BCS   DV4
         ADC   NUM2                     digit is 0
         DEY
         BNE   DV3
         BRA   DV5
DV4      INC   NUM1                     digit is 1
         DEY
         BNE   DV3

DV5      TAX                            save the remainder
         LDA   NUM1                     get the result
         LDY   SIGN                     set the sign
         BEQ   DV6
         EOR   #$FFFF
         INC   A
DV6      CLV                            clear the error flag
DV7      PLD                            reset DP
         PLY                            clean up stack
         PLY
         PLY
         RTL

ERR      PLA
         SEP   #%01000000               SEV
         RTL
         END

****************************************************************
*
*  ~MOD2 - Two Byte Signed Integer Modulo Operation
*
*  Inputs:
*        X - denominator
*        A - numerator
*
*  Outputs:
*        A - result
*        V - set for division by zero
*        Z - set to match result
*
*  Notes:
*        1) Assumes long A and X on entry.
*
****************************************************************
*
~MOD2    START

         PHB                            set up data bank
         PHK
         PLB
         CPX   #$8000                   error if 2nd arg <= 0
         BGE   ERR
         STX   N2                       save 2nd arg
         STA   SIGN                     save sign of 1st arg
         JSL   ~DIV2                    do the integer divide
         BVS   ERR
         TXA                            if result <> 0 and
         BEQ   LB1
         LDX   SIGN                       1st arg < 0 then
         BPL   LB1
         SEC                              result := result - NUM2
         SBC   N2
         EOR   #$FFFF
         INC   A
LB1      PLB
         TAX
         RTL

ERR      SEP   #%01000000               overflow
         PLB
         RTL

N2       DS    2
SIGN     DS    2
         END

****************************************************************
*
*  ~MUL2 - Two Byte Signed Integer Multiply
*
*  Inputs:
*        A - multiplicand
*        X - multipier
*
*  Outputs:
*        A - result
*        V - set if an overflow occurred
*
****************************************************************
*
~Mul2    start
num1     equ   1
num2     equ   5
sign     equ   7
;
;  Initialization
;
         tay                            save value
         phd                            set up local space
         tsc
         sec
         sbc   #8
         tcd
         tcs
         tya                            restore value
         ldy   #0                       make all arguments positive
         bit   #$8000                     start with A
         beq   in1
         eor   #$FFFF
         inc   A
         bmi   jen32768                   special case -32768
         iny
in1      sta   num1+2
         txa                              now do X
         bpl   in2
         dey
         eor   #$FFFF
         inc   A
         bpl   in2                        special case -32768
         ldx   num1+2
jen32768 brl   en32768
in2      cmp   num1+2                   make sure num1+2 is the smaller
         bge   in3                        operand
         ldx   num1+2
         sta   num1+2
         txa
in3      sta   num2
         sty   sign                     save sign; non-zero for negative results
;
;  Do the multiply
;
         lda   #0                       set up the result
         stz   num1

         lsr   num1+2                   test the LSB
         bcc   lb1                      br if it is off
         clc                            add in partial product
         adc   num2
lb1      ror   A                        multiply answer by 2
         ror   num1

         lsr   num1+2                   test the LSB
         bcc   lb2                      br if it is off
         clc                            add in partial product
         adc   num2
lb2      ror   A                        multiply answer by 2
         ror   num1

         lsr   num1+2                   test the LSB
         bcc   lb3                      br if it is off
         clc                            add in partial product
         adc   num2
lb3      ror   A                        multiply answer by 2
         ror   num1

         lsr   num1+2                   test the LSB
         bcc   lb4                      br if it is off
         clc                            add in partial product
         adc   num2
lb4      ror   A                        multiply answer by 2
         ror   num1

         lsr   num1+2                   test the LSB
         beq   abrt4
         bcc   lb5                      br if it is off
         clc                            add in partial product
         adc   num2
lb5      ror   A                        multiply answer by 2
         ror   num1

         lsr   num1+2                   test the LSB
         bcc   lb6                      br if it is off
         clc                            add in partial product
         adc   num2
lb6      ror   A                        multiply answer by 2
         ror   num1

         lsr   num1+2                   test the LSB
         bcc   lb7                      br if it is off
         clc                            add in partial product
         adc   num2
lb7      ror   A                        multiply answer by 2
         ror   num1

         lsr   num1+2                   test the LSB
         bcc   lb8                      br if it is off
         clc                            add in partial product
         adc   num2
lb8      ror   A                        multiply answer by 2
         ror   num1
;
;  Because the operands are sorted, we can stop now
;
         ldy   num1+2                   if num1+2 isn't 0 yet, we'll overflow
         bne   ovfl
         bit   #$FF80                   check for overflow in A
         bne   ovfl
         ora   num1                     do remaining shifts
         xba
;
;  Set sign and exit
;
ss1      ldy   sign                     if result is to be neg, reverse sign
         beq   ss2
         eor   #$FFFF
         inc   A
ss2      tay                            restore stack, DP
         tsc
         clc
         adc   #8
         tcs
         pld
         tya
         clv
         rtl
;
;  Handle an input operand of -32768
;
en32768  txa                            -32768 * 0 = 0
         beq   ss2

         cmp   #1                       -32768 * 1 = -32768
         bne   ovfl                     any other result is an overflow
         lda   #$8000
         bra   ss2
;
;  Abort with num1+2 = 0 after 4 shifts
;
abrt4    bcc   aa1                      add in partial product
         clc
         adc   num2
aa1      bit   #$F800                   check for overflow
         beq   aa2
         brl   ovfl
aa2      rol   num1                     do remaining shifts
         ora   num1
         rol   A
         rol   A
         rol   A
         rol   A
         bra   ss1
;
;  Handle an overflow
;
ovfl     tsc                            restore stack, DP
         clc
         adc   #8
         tcs
         pld
         sep   #%01000000               SEV
         rtl
         end

****************************************************************
*
*  ~RANX - Pseudo Random Number Generator
*
*  Creates a 16 byte sequence of pseudo-random bits for use by
*  the format dependent random number generation routines.
*
*  Outputs:
*        ~RANX+2 - 16 byte pseudo-random bit sequence
*
*
*  Notes:
*        1)  Entry at ~RANX2 initializes the random number
*              generator.  For this call, the input is a two
*              byte seed contained in A.
*        2)  The pseudo-random sequence proceeds from most to
*              least random.
*        3)  The method is adapted from Arthur Matheny,
*              "Random Number Generation for the Apple,"
*              August 1982 Micro, pp 57-60.
*
****************************************************************
*
~RANX     START

RN1      PHP                            set to long A, X
         LONG  I,M
         CLC                            randomize the bit stream
         LDX   #12
         LDA   >~SEED+14
RN2      ADC   >~SEED,X
         STA   >~SEED,X
         DEX
         DEX
         BPL   RN2
         LDX   #14
RN3      LDA   >~SEED,X
         INC   A
         STA   >~SEED,X
         BNE   RN4
         DEX
         DEX
         BPL   RN3
RN4      PLP                            reset status flags
         RTL

~SEED    ENTRY                          random number seed
         DC    H'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA'

~RANX2   ENTRY                          initialize the random bit stream
         PHP                            set to long A, X
         LONG  I,M
         LDX   #14                      init the seed
SD1      STA   >~SEED,X
         DEX
         DEX
         BPL   SD1
         PLP                            reset status bits
         RTL
         END

****************************************************************
*
*  ~SQR2 - Two Byte Integer Square Root
*
*  Inputs:
*        A - argument
*
*  Outputs:
*        A - result
*
*  Notes:
*        1)  If the argument is negative, the result is the input,
*              and the overflow flag is set.
*        2)  Uses ~DIV2, ~MUL2.
*        3) Assumes long A and X on entry.
*
****************************************************************
*
~SQR2    START
GUESS    EQU   5
LAST     EQU   3
N        EQU   1
;
;  Create an initial guess
;
         TAX                            check for a negative argument
         BPL   CG1
ERR      SEP   #%01000000               SEV
         RTL

CG1      PHA                            set up DP
         PHA
         PHA
         TSC
         PHD
         TCD
         LDA   N                        quit if zero
         BEQ   FS2
         LDX   #0                       last = 0
         STX   LAST
CG2      INX                            the initial guess is the largest power
         LSR   A                         of two that is <= the answer
         LSR   A
         BNE   CG2
         DEX
CG3      SEC
CG4      ROL   A
         DEX
         BPL   CG4
         STA   GUESS
;
;  Iterate to a final solution
;
FS1      LDX   GUESS                    while GUESS <> LAST do
         CPX   LAST
         BEQ   FS2
         STX   LAST                        LAST = GUESS
         LDA   N                           GUESS = (GUESS+N/GUESS)/2
         JSL   ~DIV2
         CLC
         ADC   GUESS
         LSR   A
         STA   GUESS
         TAX                               if GUESS*GUESS > N then
         JSL   ~MUL2
         CMP   N
         BLT   FS1
         BEQ   FS1
         DEC   GUESS                          GUESS = GUESS-1
         BRA   FS1

FS2      TXA                            endwhile
         PLD
         PLX
         PLX
         PLX
         CLV
         RTL
         END
