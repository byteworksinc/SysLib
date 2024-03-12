         KEEP  OBJ/LM
         MCOPY LM.MACROS
****************************************************************
*
*  Four and eight byte integer math libraries.
*
*  By Mike Westerfield
*
*  Copyright December 1986, All rights reserved
*  By the Byte Works, Inc.
*
*  Revised July 1987 for ORCA/M 1.0b:
*
*     1. MOD4 and MOD8 corrected to give positive results
*        when the first argument is negative.
*
****************************************************************
*
DUMMY    START
         END

****************************************************************
*
*  ~ABS8 - Eight byte absolute value
*
*  Inputs:
*        NUM1 - argument
*
*  Outputs:
*        NUM1 - result
*
****************************************************************
*
~ABS8    START
NUM1     EQU   4

         TSC                            set up DP
         PHD
         TCD
         LDA   NUM1+6                   branch if number is positive
         BPL   LB1
         SEC                            reverse sign
         LDA   #0
         SBC   NUM1
         STA   NUM1
         LDA   #0
         SBC   NUM1+2
         STA   NUM1+2
         LDA   #0
         SBC   NUM1+4
         STA   NUM1+4
         LDA   #0
         SBC   NUM1+6
         STA   NUM1+6
LB1      PLD                            reset DP
         RTL
         END

****************************************************************
*
*  ~ADD8 - Eight byte add
*
*  Inputs:
*        NUM1 - first argument
*        NUM2 - second argument
*
*  Outputs:
*        NUM1 - result
*
****************************************************************
*
~ADD8     START
NUM1     EQU   12                       first arg, result
NUM2     EQU   4                        second arg
RETURN   EQU   0                        return address

         TSC                            set up DP
         PHD
         TCD
         CLC                            add arguments
         LDA   NUM1
         ADC   NUM2
         STA   NUM1
         LDA   NUM1+2
         ADC   NUM2+2
         STA   NUM1+2
         LDA   NUM1+4
         ADC   NUM2+4
         STA   NUM1+4
         LDA   NUM1+6
         ADC   NUM2+6
         STA   NUM1+6
         MOVE4 RETURN,NUM2+4            remove NUM2 from stack and fix DP
         PLD
         PLA
         PLA
         PLA
         PLA
         RTL
         END

****************************************************************
*
*  ~CMP8 - Eight Byte Signed Integer Compare
*
*  Inputs:
*        NUM1 - first argument
*        NUM2 - second argument
*
*  Outputs:
*        C - set if NUM1 >= NUM2, else clear
*        Z - set if NUM1 = NUM2, else clear
*
****************************************************************
*
~CMP8    START
NUM1     EQU   12                       first argument
NUM2     EQU   4                        second argument
RETURN   EQU   0                        P reg, DP reg, return addr

         TDC                            set up DP
         STA   >DP
         TSC
         TCD
         LDA   NUM1+6                   if numbers are of opposite sign then
         EOR   NUM2+6
         BPL   LB1
         LDA   NUM2+6                     reverse sense of compare
         CMP   NUM1+6
         BRA   LB2                      else
LB1      LDA   NUM1+6                     compare numbers
         CMP   NUM2+6
         BNE   LB2
         LDA   NUM1+4
         CMP   NUM2+4
         BNE   LB2
         LDA   NUM1+2
         CMP   NUM2+2
         BNE   LB2
         LDA   NUM1
         CMP   NUM2
LB2      ANOP                           endif
         PHP                            save result
         LDA   RETURN                   move P, DP and return addr
         STA   NUM1+4
         LDA   RETURN+2
         STA   NUM1+6
         CLC                            remove 16 bytes from stack
         TSC
         ADC   #16
         TCS
         LDA   >DP                      restore DP
         TCD
         PLP                            restore P
         RTL                            return

DP       DS    2
         END

****************************************************************
*
*  ~DIV4 - Four Byte Signed Integer Divide
*
*  Inputs:
*        NUM1 - denominator
*        NUM2 - numerator
*
*  Outputs:
*        ANS - result
*        REM - remainder
*        V - set for division by zero
*
*  Notes
*        1) Uses ~SIGN.
*
****************************************************************
*
~DIV4    START
SIGN     EQU   1                        sign of answer
NUM1     EQU   20                       arguments
NUM2     EQU   16
ANS      EQU   5                        answer
REM      EQU   9                        remainder
RETURN   EQU   13
;
;  Initialize
;
         TSC                            set up DP
         SEC
         SBC   #12
         TCS
         PHD
         TCD
         LDA   NUM2                     check for division by zero
         ORA   NUM2+2
         BNE   DV1

         PLD                            division by zero
         TSC
         CLC
         ADC   #12
         TCS
         SEP   #%01000000
         RTL

DV1      JSL   ~SIGN                    convert to positive numbers
         LDA   NUM1+2                   do 16 bit divides separately
         ORA   NUM2+2
         BEQ   DV5
;
;  32 BIT DIVIDE
;
         LDY   #32                      32 bits to go
DV3      ASL   ANS                      roll up the next number
         ROL   ANS+2
         ROL   ANS+4
         ROL   ANS+6
         SEC                            subtract for this digit
         LDA   ANS+4
         SBC   NUM2
         TAX
         LDA   ANS+6
         SBC   NUM2+2
         BCC   DV4                      branch if minus
         STX   ANS+4                    turn the bit on
         STA   ANS+6
         INC   ANS
DV4      DEY                            next bit
         BNE   DV3
         BEQ   DV9                      go do the sign
;
;  16 BIT DIVIDE
;
DV5      LDA   #0                       initialize the remainder
         LDY   #16                      16 bits to go
DV6      ASL   ANS                      roll up the next number
         ROL   A
         SEC                            subtract the digit
         SBC   NUM2
         BCS   DV7
         ADC   NUM2                     digit is 0
         DEY
         BNE   DV6
         BEQ   DV8
DV7      INC   ANS                      digit is 1
         DEY
         BNE   DV6

DV8      STA   ANS+4                    save the remainder
;
;  SET SIGN
;
DV9      LDA   SIGN                     branch if positive
         BEQ   DV10
         SEC                            negate the result
         LDA   #0
         SBC   ANS
         STA   ANS
         LDA   #0
         SBC   ANS+2
         STA   ANS+2
DV10     LDX   #6                       move answer, remainder to stack
DV11     LDA   ANS,X
         STA   NUM2,X
         DEX
         DBPL  X,DV11
         CLV
         PLD                            fix stack, DP
         TSC
         CLC
         ADC   #12
         TCS
         RTL
         END

****************************************************************
*
*  ~DIV8 - Eight Byte Signed Integer Divide
*
*  Inputs:
*        SIGN - denominator
*        NUM2 - numerator
*
*  Outputs:
*        ANS - result
*        REM - remainder
*        V - set for division by zero
*
*  Notes
*        1) Uses ~SIG8.
*
****************************************************************
*
~DIV8    START
SIGN     EQU   1                        sign of answer
NUM2     EQU   28
ANS      EQU   9                        answer
REM      EQU   17                       remainder
RETURN   EQU   25
;
;  Initialize
;
         TSC                            set up DP
         SEC
         SBC   #24
         TCS
         PHD
         TCD
         LDA   NUM2                     check for division by zero
         ORA   NUM2+2
         ORA   NUM2+4
         ORA   NUM2+6
         BNE   DV1

         PLD                            division by zero
         TSC
         CLC
         ADC   #24
         TCS
         SEP   #%01000000
         RTL

DV1      JSL   ~SIG8                    convert to positive numbers
;
;  64 BIT DIVIDE
;
         LDY   #64                      64 bits to go
DV3      ASL   ANS                      roll up the next number
         ROL   ANS+2
         ROL   ANS+4
         ROL   ANS+6
         ROL   ANS+8
         ROL   ANS+10
         ROL   ANS+12
         ROL   ANS+14
         SEC                            subtract for this digit
         LDA   ANS+8
         SBC   NUM2
         TAX
         LDA   ANS+10
         SBC   NUM2+2
         STA   SIGN+2
         LDA   ANS+12
         SBC   NUM2+4
         STA   SIGN+4
         LDA   ANS+14
         SBC   NUM2+6
         BCC   DV4                      branch if minus
         STX   ANS+8                    save partial numerator
         STA   ANS+14
         LDA   SIGN+2
         STA   ANS+10
         LDA   SIGN+4
         STA   ANS+12
         INC   ANS                      turn the bit on
DV4      DEY                            next bit
         BNE   DV3
;
;  SET SIGN
;
         LDA   SIGN                     branch if positive
         BEQ   DV10
         SEC                            negate the result
         LDA   #0
         SBC   ANS
         STA   ANS
         LDA   #0
         SBC   ANS+2
         STA   ANS+2
         LDA   #0
         SBC   ANS+4
         STA   ANS+4
         LDA   #0
         SBC   ANS+6
         STA   ANS+6
DV10     LDX   #14                      move answer, remainder to stack
DV11     LDA   ANS,X
         STA   NUM2,X
         DEX
         DBPL  X,DV11
         CLV
         PLD                            fix stack, DP
         TSC
         CLC
         ADC   #24
         TCS
         RTL
         END

****************************************************************
*
*  ~MOD4 - Four Byte Signed Integer Modulo Operation
*
*  Inputs:
*        NUM1 - denominator
*        NUM2 - numerator
*
*  Outputs:
*        ANS - result
*        V - set for division by zero
*
*  Notes
*        1) Uses ~DIV4, ~SIGN.
*
****************************************************************
*
~MOD4    START
NUM1     EQU   9
NUM2     EQU   5

         PHB                            set up data bank
         PHK
         PLB
         LDA   NUM2+2,S                 error if 2nd arg <= 0
         BMI   ERR
         STA   N2+2                     save 2nd arg
         LDA   NUM2,S
         STA   N2
         LDA   NUM1+2,S                 save sign of 1st arg
         STA   SIGN
         PLA                            do the integer divide
         STA   RETADDR
         PLA
         STA   RETADDR+2
         JSL   ~DIV4
         LDA   RETADDR+2
         PHA
         LDA   RETADDR
         PHA
         BVS   ERR
         LDA   SIGN                     if 1st arg < 0
         BPL   LB1
         LDA   NUM1,S                     and result <> 0 then
         ORA   NUM1+2,S
         BEQ   LB1
         SEC                              result := result - NUM2
         LDA   N2
         SBC   NUM1,S
         STA   NUM1,S
         LDA   N2+2
         SBC   NUM1+2,S
         STA   NUM1+2,S
LB1      PLB
         RTL

ERR      SEP   #%01000000               overflow
         PLB
         RTL

N2       DS    4
SIGN     DS    2
RETADDR  DS    4
         END

****************************************************************
*
*  ~MOD8 - Eight Byte Signed Integer Modulo Operation
*
*  Inputs:
*        NUM1 - denominator
*        NUM2 - numerator
*
*  Outputs:
*        NUM3 - result
*        V - set for division by zero
*
*  Notes
*        1) Uses ~DIV8, ~SIG8.
*
****************************************************************
*
~MOD8    START
NUM1     EQU   13
NUM2     EQU   5

         PHB                            set up data bank
         PHK
         PLB
         LDA   NUM2+6,S                 error if 2nd arg <= 0
         BMI   ERR
         STA   N2+6                     save 2nd arg
         LDA   NUM2+4,S
         STA   N2+4
         LDA   NUM2+2,S
         STA   N2+2
         LDA   NUM2,S
         STA   N2
         LDA   NUM1+6,S                 save sign of 1st arg
         STA   SIGN
         PLA                            do the integer divide
         STA   RETADDR
         PLA
         STA   RETADDR+2
         JSL   ~DIV8
         LDA   RETADDR+2
         PHA
         LDA   RETADDR
         PHA
         BVS   ERR
         LDA   SIGN                     if 1st arg < 0
         BPL   LB1
         LDA   NUM1,S                     and result <> 0 then
         ORA   NUM1+2,S
         ORA   NUM1+4,S
         ORA   NUM1+6,S
         BEQ   LB1
         SEC                              result := NUM2 - result
         LDA   N2
         SBC   NUM1,S
         STA   NUM1,S
         LDA   N2+2
         SBC   NUM1+2,S
         STA   NUM1+2,S
         LDA   N2+4
         SBC   NUM1+4,S
         STA   NUM1+4,S
         LDA   N2+6
         SBC   NUM1+6,S
         STA   NUM1+6,S
LB1      PLB
         RTL

ERR      SEP   #%01000000               overflow
         PLB
         RTL

N2       DS    8
SIGN     DS    2
RETADDR  DS    4
         END

****************************************************************
*
*  ~MUL4 - Four Byte Signed Integer Multiply
*
*  Inputs:
*        NUM1,NUM2 - operands
*
*  Outputs:
*        ANS - result
*        V - set for overflow
*
*  Notes
*        1) Uses ~SIGN.
*
****************************************************************
*
~MUL4    START
SIGN     EQU   1                        sign of answer
NUM1     EQU   20                       arguments
NUM2     EQU   16
ANS      EQU   5                        answer
RETURN   EQU   13
;
;  Initialize the sign and split on precision.
;
         TSC                            set up DP
         SEC
         SBC   #12
         TCS
         PHD
         TCD
         JSL   ~SIGN
         LDA   ANS+2                    special case $80000000
         ORA   NUM2+2
         JMI   NG1
         LDA   ANS+2
         BEQ   ML3
;
;  Do a 32 bit by 32 bit multiply.
;
         LDY   #32                      32 bit multiply
         JSR   ML1
         BRL   ML7
ML1      LDA   ANS                      SYSS1*SYSS1+2+SYSS1+2 -> SYSS1,SYSS1+2
         LSR   A
         BCC   ML2
         CLC                            add multiplicand to the partial product
         LDA   ANS+4
         ADC   NUM2
         STA   ANS+4
         LDA   ANS+6
         ADC   NUM2+2
         STA   ANS+6
ML2      ROR   ANS+6                    shift the interem result
         ROR   ANS+4
         ROR   ANS+2
         ROR   ANS
         DEY                            loop til done
         BNE   ML1
         RTS
;
;  Do and 16 bit by 32 bit multiply.
;
ML3      ORA   NUM2+2                   branch if 16x16 is possible
         BEQ   ML4

         LDY   #16                      set up for 16 bits
         JSR   ML1                      do the multiply
         LDA   ANS+2                    move the answer
         STA   ANS
         LDA   ANS+4
         STA   ANS+2
         LDA   ANS+6
         STA   ANS+4
         STZ   ANS+6
         BRL   ML7
;
;  Do a 16 bit by 16 bit multiply.
;
ML4      LDY   #16                      set the 16 bit counter
         LDX   ANS                      move the low word
         STX   ANS+2
ML5      LSR   ANS+2                    test the bit
         BCC   ML6                      branch if the bit is off
         CLC
         ADC   NUM2
ML6      ROR   A                        shift the answer
         ROR   ANS
         DEY                            loop
         BNE   ML5
         STA   ANS+2                    save the high word
;
;  Check for overflows and set the sign bit for all multiply precisions.
;
ML7      LDA   ANS+2                    check for an overflow
         AND   #$8000
         ORA   ANS+4
         ORA   ANS+6
         BEQ   ML8

         MOVE4 RETURN-1,NUM2            overflow
ML7A     PLD
         TSC
         CLC
         ADC   #16
         TCS
         SEP   #%01000000               sev
         RTL

ML8      LDA   SIGN                     set the sign
         BEQ   ML9
         SEC
         LDA   #0
         SBC   ANS
         STA   ANS
         LDA   #0
         SBC   ANS+2
         STA   ANS+2
ML9      MOVE4 ANS,NUM1                 normal return
         MOVE4 RETURN-1,NUM2
         PLD                            fix stack, DP
         TSC
         CLC
         ADC   #16
         TCS
         CLV
         RTL
;
;  Handle the case of either operand starting as $80000000
;
NG1      LDA   NUM2+2                   place the other operand in ANS
         BMI   NG2
         MOVE4 NUM2,ANS
NG2      LDA   ANS+2                    $80000000 * 0 = 0
         BNE   ML7A
         LDA   ANS
         BEQ   ML9
         CMP   #1                       $80000000 * 1 = $80000000
         BNE   ML7A                     any other result is an overflow
         STZ   ANS
         LDA   #$8000
         STA   ANS+2
         BRA   ML9
         END

****************************************************************
*
*  ~MUL8 - Eight Byte Signed Integer Multiply
*
*  Inputs:
*        SIGN,NUM2 - operands
*
*  Outputs:
*        ANS - result
*        V - set for overflow
*
*  Notes
*        1) Uses ~SIG8.
*
****************************************************************
*
~MUL8    START
SIGN     EQU   1                        sign of answer
NUM2     EQU   28
ANS      EQU   9                        answer
RETURN   EQU   25
;
;  Initialize the sign and split on precision.
;
         TSC                            set up DP
         SEC
         SBC   #24
         TCS
         PHD
         TCD
         JSL   ~SIG8
;
;  Do a 64 bit by 64 bit multiply.
;
         LDY   #64                      64 bit multiply
ML1      LDA   ANS
         LSR   A
         BCC   ML2
         CLC                            add multiplicand to the partial product
         LDA   ANS+8
         ADC   NUM2
         STA   ANS+8
         LDA   ANS+10
         ADC   NUM2+2
         STA   ANS+10
         LDA   ANS+12
         ADC   NUM2+4
         STA   ANS+12
         LDA   ANS+14
         ADC   NUM2+6
         STA   ANS+14
ML2      ROR   ANS+14                   shift the interem result
         ROR   ANS+12
         ROR   ANS+10
         ROR   ANS+8
         ROR   ANS+6
         ROR   ANS+4
         ROR   ANS+2
         ROR   ANS
         DEY                            loop til done
         BNE   ML1
;
;  Check for overflows and set the sign bit.
;
         LDA   ANS+6                    check for an overflow
         AND   #$8000
         ORA   ANS+8
         ORA   ANS+10
         ORA   ANS+12
         ORA   ANS+14
         BEQ   ML8
         SEP   #%01000000               overflow
         BRA   ML10

ML8      LDA   SIGN                     set the sign
         BEQ   ML9
         SEC
         LDA   #0
         SBC   ANS
         STA   ANS
         LDA   #0
         SBC   ANS+2
         STA   ANS+2
         LDA   #0
         SBC   ANS+4
         STA   ANS+4
         LDA   #0
         SBC   ANS+6
         STA   ANS+6
ML9      CLV                            normal return
         MOVE4 ANS,NUM2+8
         MOVE4 ANS+4,NUM2+12
ML10     MOVE4 RETURN-1,NUM2+4
         PLD                            fix stack, DP
         PLA
         PLA
         PLA
         PLA
         PLA
         PLA
         PLA
         PLA
         PLA
         PLA
         PLA
         PLA
         PLA
         PLA
         PLA
         PLA
         RTL
         END

****************************************************************
*
*  ~SGN4 - Four Byte Signed Integer SIGN Function
*
*  Produces a result from the following table:
*
*        input            output
*
*        <0               -1
*        0                0
*        >0               1
*
*  Inputs:
*        NUM1 - argument
*
*  Outputs:
*        NUM1 - result
*
****************************************************************
*
~SGN4    START
NUM1     EQU   4

         LDA   NUM1+2,S                 branch if number is positive
         BPL   LB1
         LDA   #$FFFF                   set to -1
         STA   NUM1,S
         STA   NUM1+2,S
         BRA   LB2
LB1      ORA   NUM1,S                   done if number is 0
         BEQ   LB2
         LDA   #0                       set to 1
         STA   NUM1+2,S
         INC   A
         STA   NUM1,S
LB2      RTL
         END

****************************************************************
*
*  ~SGN8 - Eight Byte Signed Integer SIGN Function
*
*  Produces a result from the following table:
*
*        input            output
*
*        <0               -1
*        0                0
*        >0               1
*
*  Inputs:
*        NUM1 - argument
*
*  Outputs:
*        NUM1 - result
*
****************************************************************
*
~SGN8    START
NUM1     EQU   4

         LDA   NUM1+6,S                 branch if number is positive
         BPL   LB1
         LDA   #$FFFF                   set to -1
         STA   NUM1,S
         STA   NUM1+2,S
         STA   NUM1+4,S
         STA   NUM1+6,S
         BRA   LB2
LB1      ORA   NUM1+4,S                 done if number is 0
         ORA   NUM1+2,S
         ORA   NUM1,S
         BEQ   LB2
         LDA   #0                       set to 1
         STA   NUM1+2,S
         STA   NUM1+4,S
         STA   NUM1+6,S
         INC   A
         STA   NUM1,S
LB2      RTL
         END

****************************************************************
*
*  ~SIG8 - Obtain the Sign for Eight Byte Integer Operations
*
*  Inputs:
*        NUM1 - first number
*        NUM2 - second number
*
*  Outputs:
*        ANS - ABS(NUM1)
*        NUM2 - ABS(NUM2)
*        ANS+8..ANS+15 - 0
*        SIGN - 0 if NUM1*NUM2 > 0, else non-zero
*
****************************************************************
*
~SIG8    START
SIGN     EQU   1                        sign of answer
NUM1     EQU   36                       arguments
NUM2     EQU   28
ANS      EQU   9                        answer
REM      EQU   17                       remainder
RETURN   EQU   25
;
;  Make the first number positive.
;
         LDA   NUM1+6
         BPL   SG1
         SEC
         LDA   #0
         SBC   NUM1
         STA   ANS
         LDA   #0
         SBC   NUM1+2
         STA   ANS+2
         LDA   #0
         SBC   NUM1+4
         STA   ANS+4
         LDA   #0
         SBC   NUM1+6
         STA   ANS+6
         LDA   #1
         STA   SIGN
         BRA   SG2
SG1      LDX   #6
MV1      LDA   NUM1,X
         STA   ANS,X
         DEX
         DBPL  X,MV1
;
;  Initialize
;
         STZ   SIGN
SG2      STZ   ANS+8
         STZ   ANS+10
         STZ   ANS+12
         STZ   ANS+14
;
;  Make second number positive.
;
         LDA   NUM2+6
         BPL   SG3
         SEC
         LDA   #0
         SBC   NUM2
         STA   NUM2
         LDA   #0
         SBC   NUM2+2
         STA   NUM2+2
         LDA   #0
         SBC   NUM2+4
         STA   NUM2+4
         LDA   #0
         SBC   NUM2+6
         STA   NUM2+6
         DEC   SIGN
SG3      RTL
         END

****************************************************************
*
*  ~SIGN - Obtain the Sign for Four Byte Integer Operations
*
*  Inputs:
*        NUM1 - first number
*        NUM2 - second number
*
*  Outputs:
*        ANS - ABS(NUM1)
*        NUM2 - ABS(NUM2)
*        ANS+4,ANS+6 - 0
*        SIGN - 0 if NUM1*NUM2 > 0, else non-zero
*
****************************************************************
*
~SIGN    START
SIGN     EQU   1                        sign of answer
NUM1     EQU   20                       arguments
NUM2     EQU   16
ANS      EQU   5                        answer
;
;  Make the first number positive.
;
         STZ   SIGN
         LDA   NUM1+2
         BPL   SG1
         SEC
         LDA   #0
         SBC   NUM1
         STA   ANS
         LDA   #0
         SBC   NUM1+2
         STA   ANS+2
         INC   SIGN
         BRA   SG2
SG1      STA   ANS+2
         LDA   NUM1
         STA   ANS
;
;  Initialize
;
SG2      STZ   ANS+4
         STZ   ANS+6
;
;  Make second number positive.
;
         LDA   NUM2+2
         BPL   SG3
         SEC
         LDA   #0
         SBC   NUM2
         STA   NUM2
         LDA   #0
         SBC   NUM2+2
         STA   NUM2+2
         DEC   SIGN
SG3      RTL
         END

****************************************************************
*
*  ~SQR4 - Four Byte Integer Square Root
*
*  Inputs:
*        NUM1 - argument
*
*  Outputs:
*        NUM1 - result
*        V - set if argument is negative
*
*  Notes:
*        Uses ~DIV4, ~MUL4, ~SIG4.
*
****************************************************************
*
~SQR4    START
NUM1     EQU   20                       argument
ANS      EQU   1                        result of multiply, divide operations
GUESS    EQU   5                        current guess
LAST     EQU   9                        last guess
N        EQU   13                       input number
RETURN   EQU   17                       return address
;
;  Create an initial guess
;
         TSC                            set up DP
         SEC
         SBC   #16
         TCS
         PHD
         TCD
         LDA   NUM1+2                   check for a negative argument
         BPL   CG0
         SEP   #%01000000
         BRL   FN1

CG0      ORA   NUM1                     zero is a special case
         BNE   CG1
         CLV
         BRL   FN1

CG1      STZ   GUESS                    the initial guess is the largest power
         STZ   GUESS+2                   of two that is <= the answer
         SEC
         LDA   NUM1+2
         BEQ   CG3
         ROR   GUESS+2
CG2      ASL   A
         BCS   FS0
         LSR   GUESS+2
         BRA   CG2
CG3      ROR   GUESS
         LDA   NUM1
CG4      ASL   A
         BCS   FS0
         LSR   GUESS
         BRA   CG4
;
;  Iterate to a final solution
;
FS0      STZ   LAST                     LAST = 0
         STZ   LAST+2
         MOVE4 NUM1,N                   N = input number
FS1      LDA   GUESS+2                  while GUESS <> LAST do
         CMP   LAST+2
         BNE   FS1A
         LDA   GUESS
         CMP   LAST
         BEQ   FS2
FS1A     MOVE4 GUESS,LAST                  LAST = GUESS
         DIV4  N,GUESS,ANS                 GUESS = (GUESS+N/GUESS)/2
         CLC
         LDA   ANS
         ADC   GUESS
         TAX
         LDA   ANS+2
         ADC   GUESS+2
         LSR   A
         STA   GUESS+2
         TXA
         ROR   A
         STA   GUESS
         MUL4  GUESS,GUESS,ANS             if GUESS*GUESS > N then
         LDA   ANS+2                          GUESS = GUESS-1
         CMP   N+2
         BNE   FS1D
         LDA   ANS
         CMP   N
FS1D     BLT   FS1
         BEQ   FS1
         LDA   GUESS
         BNE   FS1E
         DEC   GUESS+2
FS1E     DEC   GUESS
         BRA   FS1

FS2      MOVE4 GUESS,NUM1               endwhile
         CLV
FN1      PLD                            fix return value
         TSC
         CLC
         ADC   #16
         TCS
         RTL
         END

****************************************************************
*
*  ~SQR8 - Eight Byte Integer Square Root
*
*  Inputs:
*        MR16 - argument
*
*  Outputs:
*        MR24 - result
*        V - set if argument is negative
*
*  Notes:
*        Uses ~DIV8, ~MUL8, ~SIG8.
*
****************************************************************
*
~SQR8    START
NUM1     EQU   36                       argument
ANS      EQU   1                        result of multiply, divide operations
GUESS    EQU   9                        current guess
LAST     EQU   17                       last guess
N        EQU   25                       input number
RETURN   EQU   33                       return address
;
;  Create an initial guess
;
         TSC                            set up DP
         SEC
         SBC   #32
         TCS
         PHD
         TCD
         LDA   NUM1+6                   check for a negative argument
         BPL   CG0
         SEP   #%01000000
         BRL   FN1

CG0      ORA   NUM1                     zero is a special case
         ORA   NUM1+2
         ORA   NUM1+4
         BNE   CG1
         CLV
         BRL   FN1

CG1      STZ   GUESS                    the initial guess is the largest power
         STZ   GUESS+2                   of two that is <= the answer
         STZ   GUESS+4
         STZ   GUESS+6
         LDX   #6
CG2      LDA   NUM1,X
         BNE   CG3
         DEX
         DEX
         BRA   CG2
CG3      SEC
         ROR   GUESS,X
CG4      ASL   A
         BCS   FS0
         LSR   GUESS,X
         BRA   CG4
;
;  Iterate to a final solution
;
FS0      STZ   LAST                     LAST = 0
         STZ   LAST+2
         STZ   LAST+4
         STZ   LAST+6
         LDX   #6                       N = input number
MV1      LDA   NUM1,X
         STA   N,X
         DEX
         DBPL  X,MV1
FS1      LDA   GUESS+6                  while GUESS <> LAST do
         CMP   LAST+6
         BNE   FS1A
         LDA   GUESS+4
         CMP   LAST+4
         BNE   FS1A
         LDA   GUESS+2
         CMP   LAST+2
         BNE   FS1A
         LDA   GUESS
         CMP   LAST
         JEQ   FS2
FS1A     LDX   #6                          LAST = GUESS
FS1B     LDA   GUESS,X                     GUESS = (GUESS+N/GUESS)/2
         STA   LAST,X
         DEX
         DEX
         BPL   FS1B
         DIV8  N,GUESS,ANS
         CLC
         LDA   ANS
         ADC   GUESS
         STA   GUESS
         LDA   ANS+2
         ADC   GUESS+2
         STA   GUESS+2
         LDA   ANS+4
         ADC   GUESS+4
         STA   GUESS+4
         LDA   ANS+6
         ADC   GUESS+6
         LSR   A
         STA   GUESS+6
         ROR   GUESS+4
         ROR   GUESS+2
         ROR   GUESS
         MUL8  GUESS,GUESS,ANS            if GUESS*GUESS > N then
         LDA   ANS+6                        GUESS = GUESS-1
         CMP   N+6
         BNE   FS1D
         LDA   ANS+4
         CMP   N+4
         BNE   FS1D
         LDA   ANS+2
         CMP   N+2
         BNE   FS1D
         LDA   ANS
         CMP   N
FS1D     JLE   FS1
         LDA   GUESS
         BNE   FS1E
         LDA   GUESS+2
         BNE   FS1F
         LDA   GUESS+4
         BNE   FS1G
         DEC   GUESS+6
FS1G     DEC   GUESS+4
FS1F     DEC   GUESS+2
FS1E     DEC   GUESS
         BRL   FS1

FS2      LDX   #6                       endwhile
MV2      LDA   GUESS,X
         STA   NUM1,X
         DEX
         DBPL  X,MV2
         CLV
FN1      PLD                            fix return value
         TSC
         CLC
         ADC   #32
         TCS
         RTL
         END

****************************************************************
*
*  ~SUB8 - Eight byte subtract
*
*  Inputs:
*        NUM1 - first argument
*        NUM2 - second argument
*
*  Outputs:
*        NUM3 - result
*
****************************************************************
*
~SUB8    START
NUM1     EQU   12                       first arg, result
NUM2     EQU   4                        second arg
RETURN   EQU   0                        return address

         TSC                            set up DP
         PHD
         TCD
         SEC                            subtract arguments
         LDA   NUM1
         SBC   NUM2
         STA   NUM1
         LDA   NUM1+2
         SBC   NUM2+2
         STA   NUM1+2
         LDA   NUM1+4
         SBC   NUM2+4
         STA   NUM1+4
         LDA   NUM1+6
         SBC   NUM2+6
         STA   NUM1+6
         MOVE4 RETURN,NUM2+4            remove NUM2 from stack and fix DP
         PLD
         PLA
         PLA
         PLA
         PLA
         RTL
         END
