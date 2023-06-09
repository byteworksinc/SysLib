         keep  obj/nl
         mcopy nl.macros
****************************************************************
*
*  Native Code Libraries
*
*  These libraries are common routines used by the 65816 ORCA
*  native code compilers.
*
*  Copyright 1987, 1990
*  Byte Works, Inc.
*  All rights reserved.
*
*  By Mike Westerfield and Phil Montoya
*  May 1987
*
****************************************************************
*
Dummy    start
         end

****************************************************************
*
*  SystemErrorLocation - Stop execution due to a run time error
*
****************************************************************
*
SystemErrorLocation start
         longa on
         longi on

         phk                            set data bank
         plb
         lda   ~LineNumber              if there is a non-zero line number then
         jeq   lb3
         puts  #'Error occurred at line ',errout=t print location of error
         put2  ~LineNumber,#1,errout=t
         puts  #' in procedure ',errout=t
         puts  ~ProcName-1,errout=t,cr=t
         jsl   ~ResetName                 pop proclist
         lda   ~ProcList                  if ~ProcList <> nil then
         ora   ~ProcList+2
         jeq   lb2
         putcr errout=t                     print traceback header
         puts  #'  Line  Name',cr=t,errout=t
         puts  #'  ----  ----',cr=t,errout=t
lb1      lda   ~ProcList                    while ~ProcList <> nil do
         ora   ~ProcList+2
         beq   lb2
         put2  ~LineNumber,#6,errout=t        print location of call
         puts  #'  ',errout=t
         puts  ~ProcName-1,errout=t,cr=t
         jsl   ~ResetName                     pop proclist
         bra   lb1                            endwhile
lb2      anop                             endif
lb3      anop                           endif

         lda   #$FFFF                   quit
         brl   ~Quit
         end

****************************************************************
*
*  SystemError - Handle run time errors
*
*  Inputs:
*        4,S - error number
*
****************************************************************
*
SystemError start
         longa on
         longi on
errorNumber equ 4                       error number

         lda   errorNumber,S
         pha
         jsl   SystemPrintError
         jml   SystemErrorLocation
         end

****************************************************************
*
*  SystemPrintError - Write a run time error message to errout
*
*  Inputs:
*        4,S - error number
*
****************************************************************
*
SystemPrintError start
         longa on
         longi on
errorNumber equ 10                      error number
address  equ   1                        address of the error message

         phd                            save DP
         ph4   #errorMessages           set initial string addr
         tsc                            set DP
         tcd
         ldx   errorNumber              find correct string
lb1      dex
         beq   lb2
         sec
         lda   [address]
         and   #$00FF
         adc   address
         sta   address
         bra   lb1
lb2      dec   address                  write it to error out
         ph2   #0
         ph2   #1
         ph2   #1
         jsl   ~Puts
         pld                            restore DP
         lda   2,s                      fix stack for return
         sta   4,s
         pla
         sta   1,s
         rtl

errorMessages anop                      error messages
         dw    'Subrange exceeded'
         dw    'File is not open'
         dw    'Read while at end of file'
         dw    'I/O error'
         dw    'Out of memory'
         dw    'EOLN while at end of file'
         dw    'Set overflow'
         dw    'Jump to undefined case statement label'
         dw    'Integer math error'
         dw    'Real math error'
         dw    'Underflow'
         dw    'Overflow'
         dw    'Divide by zero'
         dw    'Inexact'
         dw    'Stack overflow'
         dw    'Stack error'
         end

****************************************************************
*
*  ~_BWCommon - Global data for the compiler
*
****************************************************************
*
~_BWCommon start
;
;  Misc. variables
;
~CommandLine entry                      address of the shell command line
         ds    4
~EOFInput entry                         end of file flag for input
         ds    2
~EOLNInput entry                        end of line flag for input
         ds    2
ErrorOutput entry                       error output file variable
         dc    a4'~ErrorOutputChar'
~ErrorOutputChar entry                  error output file buffer
         ds    2
Input    entry                          standard input file variable
         dc    a4'~InputChar'
~InputChar entry                        standard input file buffer
         ds    2
~MinStack entry                         lowest reserved bank zero address
         ds    2
Output   entry                          standard output file variable
         dc    a4'~OutputChar'
~OutputChar entry                       standard output file buffer
         ds    2
~RealVal entry                          last real value returned by a function
         ds    10
~SANEStarted entry                      did we start SANE?
         dc    i'0'
~ThisFile entry                         pointer to current file variable
         ds    4
~StringList entry                       string buffer list
         ds    4
~callerStack entry                      caller's stack location
         ds    2
;
;  Traceback variables
;
~ProcList entry                         traceback list head
         ds    4
~LineNumber entry                       current line number
         ds    2
~ProcName entry                         current procedure name
         ds    32
         end

****************************************************************
*
*  ~_IOCommon - Global data for the IO routines
*
****************************************************************
*
~_IOCommon data

sDevType ds    2                        standard output device type
sDevPointer ds 4                        standard output device pointer

eDevType ds    2                        error output device type
eDevPointer ds 4                        error output device pointer

~ssRef   ds    4                        StartUpTools reference
         end

****************************************************************
*
*  ~_BWStartUp - Compiler initialization
*
*  Inputs:
*        A - user ID
*        X-Y - address of the command line
*        D - lowest reserved bank zero address
*        4,S - amount of stack space to reserve
*
*  Outputs:
*        ~User_ID - user ID
*        ~CommandLine - address of the command line
*        ~MinStack - lowest reserved bank zero address
*        ~InputChar - set to ' '
*
****************************************************************
*
~_BWStartUp start
;
;  Set up initial registers
;
         phk                            set the data bank register
         plb
         ora   #$0100                   use local user ID
         sta   >~User_ID                save the user ID for memory manager use
         stx   ~CommandLine+2           save the address of the command line
         sty   ~CommandLine
;
;  Allocate bank zero space for stack frames
;
         tsc                            save the caller's stack
         clc
         adc   #5
         sta   ~callerStack
         ph4   #0                       allocate a bank zero
         ph2   #0
         lda   10,S
         pha
         ph2   >~User_ID
         ph2   #$C105
         ph4   #0
         _NewHandle
         bcs   oom
         tsc                            get handle
         tcd
         lda   [1]                      set min stack loc.
         sta   ~MinStack
         clc                            calculate stack address
         adc   8,S
         dec   A
         tax
         plx                            remove handle
         plx
         phb                            recover return addresses
         plx
         ply
         tcs                            set stack pointer
         phy                            reset return addresses
         phx
         plb
;
;  Initialize the environment
;
         jsl   SystemSANEInit           Start up SANE
         jsl   SystemEnvironmentInit    Set up the various environment variables
         jsl   SysIOStartup             Start the GS/OS text I/O system
         rtl
;
;  Flag an out of stack memory error
;
oom      puts  #'Insufficient bank zero memory',cr=t,errout=t
         lda   #-1
         brl   ~Quit
         end

****************************************************************
*
*  ~_BWStartUp2 - Compiler initialization for RTL pragma programs
*
*  Inputs:
*        A - user ID
*        4,S - amount of stack space to reserve (may be 0)
*
*  Outputs:
*        ~User_ID - user ID
*        ~CommandLine - set to NULL
*        ~InputChar - set to ' '
*
****************************************************************
*
~_BWStartUp2 start
;
;  Set up initial registers
;
         phb                            set the data bank register
         phk
         plb
         ora   #$0100                   use local user ID
         sta   >~User_ID                save the user ID for memory manager use
         stz   ~CommandLine+2           set the address of the command line
         stz   ~CommandLine              to NULL
;
;  Allocate bank zero space for stack frames
;
         tsc                            save the caller's stack
         clc
         adc   #6
         sta   ~callerStack
         ph4   #0                       allocate a bank zero
         ph2   #0
         lda   11,S
         pha
         ph2   >~User_ID
         ph2   #$C105
         ph4   #0
         _NewHandle
         bcs   oom
         tsc                            get handle
         tcd
         lda   [1]                      set min stack loc.
         sta   ~MinStack
         clc                            calculate stack address
         adc   9,S
         dec   A
         tax
         plx                            remove handle
         plx
         plx                            recover return addresses
         ply
         tcs                            set stack pointer
         phy                            reset return addresses
         phx
;
;  Set up compiler variables
;
         lda   #' '                     reset(input)
         sta   ~InputChar
         stz   ~EOLNInput
         stz   ~EOFInput
         stz   ~LineNumber              initialize traceback info
         stz   ~ProcName
         stz   ~ProcList
         stz   ~ProcList+2
         stz   ~StringList              initialize the string buffer list
         stz   ~StringList+2
         stz   ~thisFile                initialize file lists
         stz   ~thisFile+2
         stz   ~SANEStarted             SANE not started
         plb                            reset caller's B
         rtl
;
;  Flag an out of stack memory error
;
oom      puts  #'Insufficient bank zero memory',cr=t,errout=t
         lda   #-1
         brl   ~Quit
         end

****************************************************************
*
*  ~_BWStartUp3 - Compiler initialization using stack frames
*
*  Inputs:
*        A - user ID
*        X-Y - address of the command line
*        D - lowest reserved bank zero address
*
*  Outputs:
*        ~User_ID - user ID
*        ~CommandLine - address of the command line
*        ~MinStack - lowest reserved bank zero address
*        ~InputChar - set to ' '
*
*  Notes:
*        This subroutine differs from ~_BWStartUp, which it
*        replaces, in that it uses whatever stack segment the
*        program launcher allocated.  Compilers allocate stack
*        frames of a particular size by creating their own stack
*        segment.
*
****************************************************************
*
~_BWStartUp3 start

         pha                            save the input values
         phx
         phy
         jsl   SystemUserID
         jsl   SystemMinStack           Set ~MinStack to the start of the stack segment
         jsl   SystemSANEInit           Start up SANE
         jsl   SystemEnvironmentInit    Set up the various environment variables
         jsl   SysIOStartup             Start the GS/OS text I/O system
         rtl
         end

****************************************************************
*
*  ~_BWStartUp4 - Compiler initialization for RTL pragma programs
*        using stack frames
*
*  Inputs:
*        A - user ID
*
*  Outputs:
*        ~User_ID - user ID
*        ~CommandLine - set to NULL
*        ~InputChar - set to ' '
*
****************************************************************
*
~_BWStartUp4 start

         pha                            save the user ID
         pea   0                        no command line
         pea   0
         jsl   SystemUserID
         tsc                            save the SP
         clc
         adc   #3
         sta   >~callerStack
         jsl   SystemMinStack           Set ~MinStack to the start of the stack segment
         lda   #0                       SANE is not started
         sta   >~SANEStarted
         jsl   SystemEnvironmentInit    Set up the various environment variables
         rtl
         end

****************************************************************
*
*  Pasvars - Pascal variables
*
****************************************************************
*
~PasVars start
~ToolError entry                        last error in a tool call (Pascal)
         ds    2

~User_ID entry                          user ID (Pascal, libraries)
         ds    2
         end

****************************************************************
*
*  ~AShr4 - Shift a signed long value right
*
*  Inputs:
*        A - value to shift
*        X - # bits to shift by
*
*  Outputs:
*        A - result
*
****************************************************************
*
~AShr4   start
num1     equ   8                        number to shift
num2     equ   4                        # bits to shift by

         tsc                            set up DP
         phd
         tcd
         lda   num2+2                   if num2 < 0 then
         bpl   lb2
         cmp   #$FFFF                     shift left
         bne   zero
         ldx   num2
         cpx   #-34
         blt   zero
lb1      asl   num1
         rol   num1+2
         inx
         bne   lb1
         bra   lb4
zero     stz   num1                       (result is zero)
         stz   num1+2
         bra   lb4
lb2      bne   zero                     else shift right
         ldx   num2
         beq   lb4
         cpx   #33
         bge   zero
lb3      lda   num1+2
         asl   A
         ror   num1+2
         ror   num1
         dex
         bne   lb3

lb4      lda   0                        fix stack and return
         sta   num2
         lda   2
         sta   num2+2
         pld
         pla
         pla
         rtl
         end

****************************************************************
*
*  ~CheckStack - check for a stack overflow
*
*  This routine is called on entry to each procedure or
*  function (if RangeCheck+ has been specified) to check for
*  an overflow of the run time stack.
*
*  Inputs:
*        4,S - amount of stack space to be allocated
*        ~MinStack - lowest available stack location
*
*  Notes:
*        1. X register MUST be preserved.
*
****************************************************************
*
~CheckStack start
EvalRoom equ   200                      # bytes to leave free for the
!                                        evaluation stack

         sec                            compute the lowest stack value
         tsc                             endangered by this call
         sbc   4,S
         sbc   #EvalRoom
         cmp   >~MinStack               if it is smaller than ~MinStack then
         bge   ck1
         phx
         error #15                        flag stack overflow error
         plx
ck1      lda   2,S                      remove input from stack & return
         sta   4,S
         lda   0,S
         sta   2,S
         pla
         rtl
         end

****************************************************************
*
*  ~EndDesk - Shut down QuickDraw and desktop tools; return to text mode
*  ~EndGraph - Shut down QuickDraw; return to text mode
*
****************************************************************
*
~EndDesk start
~EndGraph entry
         using ~_IOCommon

         phb
         phk
         plb

         ph2   #0
         ph4   ~ssRef
         _ShutDownTools

         jsl   SysGraphTextShutdown

         plb
         rtl
         end

****************************************************************
*
*  ~GeqL - Test two long integers for A >= B
*
*  Inputs:
*        num1 - 1st argument
*        X-A - 2nd argument
*
*  Outputs:
*        A - 1 if true, else 0
*        Z - 0 if true, else 1
*
****************************************************************
*
~GeqL    start
         longa on
         longi on
num1     equ   8                        disp of 1st arg
ret      equ   5                        return address
workDP   equ   1                        disp of 2nd argument

         ldy   #0                       assume false
         phx                            save 2nd argument
         pha
         tsc                            set up stack frame
         phd
         tcd
         txa                            if numbers have opposite sign then
         eor   num1+2
         bpl   lb1
         lda   workDP+2                   reverse sense of compare
         cmp   num1+2
         bra   lb2
lb1      lda   num1+2                   else
         cmp   workDP+2                   compare numbers
         bne   lb2
         lda   num1
         cmp   workDP
lb2      anop                           endif
         blt   lb3
         iny                            restult is true
lb3      lda   ret+1                    restore stack
         sta   num1+2
         lda   ret
         sta   num1+1
         pld
         tsc
         clc
         adc   #8
         tcs
         tya                            result -> A reg
         rtl
         end

****************************************************************
*
*  ~GrtL - Test two long integers for A > B
*
*  Inputs:
*        n1,n2: numbers
*
*  Outputs:
*        A - 1 if true, else 0
*        Z - 0 if true, else 1
*
****************************************************************
*
~GrtL    start
         longa on
         longi on
num1     equ   8                        disp of 1st arg
num2     equ   4                        disp of 2nd arg

         ldx   #0                       assume false
         lda   num1+2,S                 if numbers have opposite sign then
         eor   num2+2,S
         bpl   lb1
         lda   num2+2,S                   reverse sense of compare
         cmp   num1+2,S
         bra   lb2
lb1      lda   num1+2,S                 else
         cmp   num2+2,S                   compare numbers
         bne   lb2
         lda   num1,S
         cmp   num2,S
lb2      anop                           endif
         blt   lb3
         beq   lb3
         inx                            restult is true
lb3      phb                            restore stack
         pla
         sta   7,S
         pla
         sta   7,S
         pla
         pla
         plb
         txa                            result -> A reg
         rtl
         end

****************************************************************
*
*  ~Halt - Stop execution and return with error code
*
*  Inputs:
*        error - error code
*
****************************************************************
*
~Halt    start
error    equ   4

         lda   error,S
         brl   ~Quit
         end

****************************************************************
*
*  ~LoadTools - Load the tools from the boot disk
*  ~UnLoadTools - Unload the tools from the boot disk
*
****************************************************************
*
~LoadTools start
         longa on
         longi on

         ph4   #toolTable               try loading tools
         _LoadTools
         rts
;
;  Unload tools (uses the same table as load tools for accuracy)
;
~UnLoadTools entry
         phb
         phk
         plb
         ldx   toolTable
         ldy   #2
un1      lda   toolTable,Y
         phy
         phx
         pha
         _UnloadOneTool
         plx
         pla
         clc
         adc   #4
         tay
         dex
         bne   un1
         plb
         rts
;
;  Local data
;
toolTable dc i'13'
         dc    i'4,$0101'               quickdraw
         dc    i'5,$0100'               desk manager
         dc    i'6,$0100'               event manager
         dc    i'14,$0103'              window manager
         dc    i'15,$0103'              menu manager
         dc    i'16,$0103'              control manager
         dc    i'18,$0100'              quickdraw aux tools
         dc    i'20,$0100'              line edit
         dc    i'21,$0100'              dialog manager
         dc    i'22,$0100'              scrap manager
         dc    i'23,$0100'              standard files
         dc    i'27,$0100'              font manager
         dc    i'30,$0100'              resource manager
         end

****************************************************************
*
*  ~LongMove - move some bytes
*
*  Inputs:
*        source - pointer to source bytes
*        dest - pointer to destination bytes
*        len - number of bytes to move
*
*  Notes:
*        This subroutine leaves the destination address on the
*        stack.  It differs from ~Move in that it can move 64K
*        or more.
*
****************************************************************
*
~LongMove start

         sub   (4:len,4:source,4:dest),0

         ldx   len+2                    move whole banks
         beq   lm2
         ldy   #0
lm1      lda   [source],Y
         sta   [dest],Y
         dey
         dey
         bne   lm1
         inc   source+2
         inc   dest+2
         dex
         bne   lm1
lm2      lda   len                      move one byte if the move length is odd
         lsr   a
         bcc   lb1
         short M
         lda   [source]
         sta   [dest]
         long  M
         inc4  source
         inc4  dest
         dec   len
lb1      ldy   len                      move the bytes
         beq   lb4
         dey
         dey
         beq   lb3
lb2      lda   [source],Y
         sta   [dest],Y
         dey
         dey
         bne   lb2
lb3      lda   [source]
         sta   [dest]
lb4      return
         end

****************************************************************
*
*  ~Move - move some bytes
*
*  Inputs:
*        source - pointer to source bytes
*        dest - pointer to destination bytes
*        len - number of bytes to move
*
****************************************************************
*
~Move    start

         sub   (2:len,4:source,4:dest),0

         lda   len                      move one byte if the move length is odd
         lsr   a
         bcc   lb1
         short M
         lda   [source]
         sta   [dest]
         long  M
         inc4  source
         inc4  dest
         dec   len
lb1      ldy   len                      move the bytes
         beq   lb4
         dey
         dey
         beq   lb3
lb2      lda   [source],Y
         sta   [dest],Y
         dey
         dey
         bne   lb2
lb3      lda   [source]
         sta   [dest]
lb4      return
         end

****************************************************************
*
*  __NBACallBack - HyperStudio callback
*
*  extern pascal void __NBACALLBACK (int, HSParamPtr);
*               
*  Inputs:
*        call - call number
*        HSParamPtr - parameter block
*
****************************************************************
*
__NBACallBack start
         using ~NBACommon
call     equ   10                       callback number
HSParamPtr equ 6                        HyperStudio parameter record

         phd                            set up HyperStudio D reg
         lda   >~NBA_D
         tcd
         lda   HSParamPtr,S             set up the parameter address
         sta   0
         lda   HSParamPtr+2,S
         sta   2
         ldy   #24                      set up the callback number
         lda   call,S
         sta   [0],Y
         ldy   #16                      set up the jsl address
         lda   [0],Y
         sta   >jsl+1
         iny
         lda   [0],Y
         sta   >jsl+2
jsl      jsl   jsl
         pld                            return
         phb
         plx
         ply
         pla
         pla
         pla
         phy
         phx
         plb
         rtl
         end

****************************************************************
*
*  ~NBACommon - common area for HyperStudio New Button Action
*
****************************************************************
*
~NBACommon privdata

~NBA_B   ds    2                        caller's B register
~NBA_D   ds    2                        caller's D register
         end

****************************************************************
*
*  ~NBAShutdown - shutdown code for a HyperStudio New Button Action
*
****************************************************************
*
~NBAShutdown start
         using ~NBACommon

         jsl   ~MM_DisposeAll           shut down the compiler's memory manager
         jsl   ~MM_Init
         lda   >~NBA_B                  restore HyperStudio B
         pha
         plb
         plb
         rtl
         end

****************************************************************
*
*  ~NBAStartup - startup code for a HyperStudio New Button Action
*
****************************************************************
*
~NBAStartup start
         using ~NBACommon

         phx                            save the parameter address
         phy
         phb                            save the caller's data bank
         phb
         pla
         sta   >~NBA_B
         phk                            set up our data bank
         plb
         ora   #$0100                   set up our user ID
         sta   >~User_ID
         tdc                            save the HyperStudio DP
         sta   >~NBA_D
         jsl   SystemEnvironmentInit    Set up the various environment variables
         ply
         plx
         rtl
         end

****************************************************************
*
*  ~PDiv4 - Four Byte Signed Integer Divide
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
~PDiv4   start
sign     equ   1                        sign of answer
num1     equ   20                       arguments
num2     equ   16
ans      equ   5                        answer
rem      equ   9                        remainder
return   equ   13
;
;  Initialize
;
         tsc                            set up DP
         sec
         sbc   #12
         tcs
         phd
         tcd
         lda   num2                     check for division by zero
         ora   num2+2
         bne   dv1
         error #9                       integer math error
         brl   dv10

dv1      jsl   ~sign                    convert to positive numbers
         lda   num1+2                   do 16 bit divides separately
         ora   num2+2
         beq   dv5
;
;  32 bit divide
;
         ldy   #32                      32 bits to go
dv3      asl   ans                      roll up the next number
         rol   ans+2
         rol   ans+4
         rol   ans+6
         sec                            subtract for this digit
         lda   ans+4
         sbc   num2
         tax
         lda   ans+6
         sbc   num2+2
         bcc   dv4                      branch if minus
         stx   ans+4                    turn the bit on
         sta   ans+6
         inc   ans
dv4      dey                            next bit
         bne   dv3
         beq   dv9                      go do the sign
;
;  16 bit divide
;
dv5      lda   #0                       initialize the remainder
         ldy   #16                      16 bits to go
dv6      asl   ans                      roll up the next number
         rol   A
         sec                            subtract the digit
         sbc   num2
         bcs   dv7
         adc   num2                     digit is 0
         dey
         bne   dv6
         beq   dv8
dv7      inc   ans                      digit is 1
         dey
         bne   dv6

dv8      sta   ans+4                    save the remainder
;
;  Set sign
;
dv9      lda   sign                     branch if positive
         beq   dv10
         sec                            negate the result
         lda   #0
         sbc   ans
         sta   ans
         lda   #0
         sbc   ans+2
         sta   ans+2
dv10     lda   ans                      move answer, remainder to stack
         sta   num1
         lda   ans+2
         sta   num1+2
         lda   return-1
         sta   num2
         lda   return+1
         sta   num2+2
         clv
         pld                            fix stack, DP
         tsc
         clc
         adc   #16
         tcs
         rtl
         end

****************************************************************
*
*  ~Quit - Quit code for normal compiler shutdown
*
****************************************************************
*
~Quit    start

         pha                            save the return code
         jsl   SysIOShutdown            Shut down the GS/OS text I/O system
         jsl   SystemSANEShutDown       shut down SANE
         jsl   SystemMMShutDown         shut down the compiler's memory manager
         pla                            restore the return code
         QuitGS qt_dcb                  return to the calling shell

qt_dcb   dc    i'2'
~QuitPath entry
         dc    a4'0'
~QuitFlags entry
         dc    i'0'
         end

****************************************************************
*
*  ~ResetName - reset the name and line number
*
*  Inputs:
*        ~ProcList - head of procedure list
*
*  Outputs:
*        ~ProcList - new head of procedure list
*        ~LineNumber - old line number
*        ~ProcName - old procedure name
*
****************************************************************
*
~ResetName start
         longa on
         longi on
pRecLen  equ   38                       length of the position record
ptr      equ   1                        work pointer

         phb
         phk
         plb
         ph4   ~ProcList
         tsc
         phd
         tcd
         ldy   #pRecLen-2
lb1      lda   [ptr],Y
         sta   ~ProcList,Y
         dey
         dey
         bpl   lb1
         dispose ptr
         pld
         pla
         pla
         plb
         rtl
         end

****************************************************************
*
*  ~RTL - Exit from an RTL program
*
****************************************************************
*
~RTL     start

         pha                            save return code
         jsl   SystemMMShutDown         shut down the compiler's memory manager
         plx
         lda   >~callerStack            restore original stack
         tcs
         txa                            reset return code
         rtl                            return to caller
         end

****************************************************************
*
*  ~SetLineNumber - set the current line number
*
*  Inputs:
*        4,S - line number
*
*  Outputs:
*        ~LineNumber - current line #
*
*  Notes:
*        Must preserve registers and condition flags
*
****************************************************************
*
~SetLineNumber start

         php
         pha
         lda   7,S
         sta   >~LineNumber
         lda   5,S
         sta   7,S
         lda   3,S
         sta   5,S
         lda   1,S
         sta   3,S
         pla
         pla
         plp
         rtl
         end

****************************************************************
*
*  ~SetName - save the name and line number
*
*  Inputs:
*        ~ProcList - head of procedure list
*        ~LineNumber - line number
*        ~ProcName - procedure name
*        name - pointer to new proc name
*
*  Outputs:
*        ~ProcList - new head of procedure list
*
****************************************************************
*
~SetName start
         longa on
         longi on
pRecLen  equ   38                       length of the position record
name     equ   9                        ptr to new name
ptr      equ   1                        work pointer

         phb                            set data bank
         phk
         plb
         pha                            set up ptr
         pha
         tsc
         phd
         tcd
         new   ptr,#pRecLen             allocate space for record
         ldy   #pRecLen-2               save the record
lb1      lda   ~ProcList,Y
         sta   [ptr],Y
         dey
         dey
         bpl   lb1
         move4 ptr,~ProcList            set new head of list
         ldy   #30                      move the name into its space
lb2      lda   [name],Y
         sta   ~ProcName,Y
         dey
         dey
         bpl   lb2
         short M                        make sure the name is not too long
         lda   ~ProcName
         cmp   #32
         blt   lb3
         lda   #31
         sta   ~ProcName
lb3      long  M
         pld                            restore the stack
         pla
         pla
         plx
         ply
         pla
         pla
         phy
         phx
         plb                            reset data bank
         rtl
         end

****************************************************************
*
*  ~ShiftLeft - Shift a value left
*
*  Inputs:
*        A - value to shift
*        X - # bits to shift by
*
*  Outputs:
*        A - result
*
****************************************************************
*
~ShiftLeft start
         longa on
         longi on

         txy                            if # bits is 0, quit
         beq   rtl
         bmi   lb2                      if # bits is > 0 then
lb1      asl   A                          shift left
         dex
         bne   lb1
         bra   rtl                      else
lb2      lsr   A                          shift right
         inx
         bne   lb2
rtl      rtl
         end

****************************************************************
*
*  ~SHL4 - Shift a long value left
*
*  Inputs:
*        A - value to shift
*        X - # bits to shift by
*
*  Outputs:
*        A - result
*
****************************************************************
*
~SHL4    start
         longa on
         longi on
num1     equ   8                        number to shift
num2     equ   4                        # bits to shift by

         tsc                            set up DP
         phd
         tcd
         lda   num2+2                   if num2 < 0 then
         bpl   lb2
         cmp   #$FFFF                     shift right
         bne   zero
         ldx   num2
         cpx   #-34
         blt   zero
lb1      lda   num1+2
         asl   a
         ror   num1+2
         ror   num1
         inx
         bne   lb1
         bra   lb4
zero     stz   num1                       (result is zero)
         stz   num1+2
         bra   lb4
lb2      bne   zero                     else shift left
         ldx   num2
         beq   lb4
         cpx   #33
         bge   zero
lb3      asl   num1
         rol   num1+2
         dex
         bne   lb3

lb4      lda   0                        fix stack and return
         sta   num2
         lda   2
         sta   num2+2
         pld
         pla
         pla
         rtl
         end

****************************************************************
*
*  ~SShiftRight - Shift a signed value right
*
*  Inputs:
*        A - value to shift
*        X - # bits to shift by
*
*  Outputs:
*        A - result
*
****************************************************************
*
~SShiftRight start

         txy                            if # bits is 0, quit
         beq   rtl
         bpl   lb2                      if # bits is < 0 then
lb1      asl   A                          shift left
         inx
         bne   lb1
         bra   rtl                      else
lb2      bit   #$8000                     if value >= 0 then
         bne   lb4
lb3      lsr   A                            unsigned shift right
         dex
         bne   lb3
         rtl                              else

lb4      sec                                signed shift right
         ror   A
         dex
         bne   lb4
rtl      rtl
         end

****************************************************************
*
*  ~StartDesk - start quickdraw, and tools needed for desktop
*
*  Inputs:
*        size - horrizontal screen size
*
****************************************************************
*
~StartDesk start
         using ~_IOCommon
         longa on
         longi on
qSize    equ   20                       event manager queue size
maxY     equ   200                      max y coordinate

         sub   (2:size),0
         phb
         phk
         plb

         lda   size                     set the video mode
         lsr   A
         lsr   A
         and   #$80
         sta   ssVideoMode
         pha                            start the tools
         pha
         ph2   >~User_ID
         ph2   #0
         ph4   #ssRec
         _StartUpTools
         bcs   toolError
         pl4   ~ssRef
         _InitCursor

         jsl   SysGraphTextStartup

rts      plb
         return
;
;  Handle a tool error
;
toolError pha
         ph4   #msg                     not maskable
         _SysFailMgr
;
;  Local data
;
msg      dw    'Could not start tools: '

         dc    r'~EndDesk'              make sure EndDesk gets linked

ssRec    dc    i'0'                     flags
ssVideoMode ds 2                        video mode
         dc    i'0'
         dc    i4'0'
         dc    i'(ssEnd-ssStart)/4'
ssStart  anop
         dc    i'3,$0302'               Miscellaneos Tool Set
         dc    i'4,$0307'               QuickDraw II
         dc    i'5,$0304'               Desk Manager
         dc    i'6,$0301'               Event Manager
         dc    i'11,$0300'              Integer Math Tool Set
         dc    i'14,$0303'              Window Manager
         dc    i'15,$0303'              Menu Manager
         dc    i'16,$0303'              Control Manager
         dc    i'18,$0304'              QuickDraw II Auxiliary
         dc    i'20,$0303'              Line Edit Tool Set
         dc    i'21,$0304'              Dialog Manager
         dc    i'22,$0301'              Scrap Manager
         dc    i'23,$0303'              Standard File Operations
         dc    i'27,$0303'              Font Manager
         dc    i'28,$0303'              List Manager
         dc    i'30,$0102'              Resource Manager
ssEnd    anop
         end

****************************************************************
*
*  ~StartGraph - start quickdraw
*
*  Inputs:
*        size - horrizontal screen size
*
****************************************************************
*
~StartGraph start
         using ~_IOCommon
         longa on
         longi on
formFeed equ   $0C

         sub   (2:size),0
         phb
         phk
         plb

         lda   size                     set the video mode
         lsr   A
         lsr   A
         and   #$80
         sta   ssVideoMode
         pha                            start the tools
         pha
         ph2   >~User_ID
         ph2   #0
         ph4   #ssRec
         _StartUpTools
         jcs   toolError
         pl4   ~ssRef

         _HideCursor                    hide the cursor
         ph2   #0                       clear the screen
         _ClearScreen
         jcs   toolError
         ph2   #$FFFF                   set the pen to solid white
         _SetSolidPenPat
         jcs   toolError
         lda   #1                       set the pen to "or" mode
         pha                            (put pen size on stack now)
         pha
         pha
         _SetPenMode
         jcs   toolError
         _SetPenSize                    set the pen size to 1x1
         jcs   toolError

         ph2   #$FFFF
         _SetForeColor
         jcs   toolError
         ph2   #0
         _SetBackColor
         bcs   toolError

         jsl   SysGraphTextStartup

rts      plb
         return
;
;  Handle a tool error
;
toolError pha
         ph4   #msg                     not maskable
         _SysFailMgr
;
;  Local data
;
msg      dw    'Could not start tools: '

         dc    r'~EndGraph'             make sure EndGraph gets linked

ssRec    dc    i'0'                     flags
ssVideoMode ds 2                        video mode
         dc    i'0'
         dc    i4'0'
         dc    i'(ssEnd-ssStart)/4'
ssStart  anop
         dc    i'3,$0302'               Miscellaneos Tool Set
         dc    i'4,$0307'               QuickDraw II
ssEnd    anop
         end

****************************************************************
*
*  ~StackErr - check for stack errors
*
*  Inputs:
*        Y - expected stack location
*
****************************************************************
*
~StackErr start

         pha
         tya
         sta   >S
         tsc
         clc
         adc   #5
         cmp   >S
         beq   lb1
         phx
         ph2   #16
         jsl   SystemError
         plx
lb1      pla
         rtl

S        ds    2
         end

****************************************************************
*
*  SystemEnvironmentInit - Set up the various environment variables
*
****************************************************************
*
SystemEnvironmentInit start

         phb
         phk
         plb

         lda   #' '                     reset(input)
         sta   ~InputChar
         stz   ~EOLNInput
         stz   ~EOFInput
         stz   ~LineNumber              initialize traceback info
         stz   ~ProcName
         stz   ~ProcList
         stz   ~ProcList+2
         stz   ~StringList              initialize the string buffer list
         stz   ~StringList+2
         stz   ~thisFile                initialize file lists
         stz   ~thisFile+2

         plb
         rtl
         end

****************************************************************
*
*  SystemMinStack - set ~MinStack
*
*  Sets the system variable ~MinStack to the start of the
*  current stack segment.  ~MinStack is used by tool allocation
*  subrountines to find space for initializing tools, as well
*  as by error checking routines that check for stack overflows.
*
****************************************************************
*
SystemMinStack start

         pha                            find the stack segment handle
         pha
         pea   0
         tsc
         pha
         _FindHandle
         phd                            set up a direct page
         tsc
         tcd
         lda   [3]                      dereference the handle
         pld                            restore caller's DP
         sta   >~MinStack               set ~MinStack
         pla
         pla
         rtl
         end

****************************************************************
*
*  SystemMMShutDown - shut down the compilers' memory manager
*
****************************************************************
*
SystemMMShutDown start

         jsl   ~MM_Init                 zero the memory manager
         ph2   >~User_ID                dispose of any remaining memory
         _DisposeAll                     allocated by the memory manager
         rtl
         end

****************************************************************
*
*  SystemQuitFlags - set the Quit flags
*
*  Inputs:
*        path - GS/OS input path to quit to
*
****************************************************************
*
SystemQuitFlags start

         phb
         plx
         ply
         pla
         sta   >~QuitFlags
         phy
         phx
         plb
         rtl
         end

****************************************************************
*
*  SystemQuitPath - set the Quit pathname
*
*  Inputs:
*        path - GS/OS input path to quit to
*
****************************************************************
*
SystemQuitPath start

         phb
         plx
         ply
         pla
         sta   >~QuitPath
         pla
         sta   >~QuitPath+2
         phy
         phx
         plb
         rtl
         end

****************************************************************
*
*  SystemSANEInit - If it hasn't already been started, start SANE
*
****************************************************************
*
SystemSANEInit start

         lda   #0                       SANE has not been started by us (yet)
         sta   >~SANEStarted
         pha                            if SANE has not been started then
         _SANEStatus
         pla
         bne   sn1
         lda   >~MinStack                 get some bank zero memory for SANE
         pha
         clc
         adc   #$0100
         sta   >~MinStack
         _SANEStartUp                     initialize SANE
         lda   #1                         set the SANE startup flag
         sta   >~SANEStarted
sn1      rtl
         end

****************************************************************
*
*  SystemSANEShutDown - If we started SANE, shut it down
*
****************************************************************
*
SystemSANEShutDown start

         lda   >~SANEStarted            if we started SANE then
         beq   qt1
         _SANEShutDown                    shut it down
         lda   #0                         clear the flag (for restarts)
         sta   >~SANEStarted
qt1      rtl
         end

****************************************************************
*
*  SystemUserID - set up the user ID and command line
*
*  Inputs:
*        8,S - user ID
*        4,S - command line address
*
****************************************************************
*
SystemUserID start

         phk                            remove the return address
         plx
         ply
         pla                            save the command line address
         sta   >~CommandLine
         pla
         sta   >~CommandLine+2
         pla                            set up our user ID
         ora   #$0100
         sta   >~User_ID
         phy                            return to the caller
         phx
         plb                            (set up B, too)
         rtl
         end

****************************************************************
*
*  ~UDiv2 - Two Byte Unsigned Integer Divide
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
~UDiv2   start
num1     equ   3                        numerator
num2     equ   1                        denominator

         txy                            check for division by zero
         beq   err
         pha                            save the arguments
         phx
         tsc                            set up DP
         phd
         tcd

         lda   #0                       initialize the remainder
         ldy   #16                      16 bits to go
dv3      asl   num1                     roll up the next number
         rol   A
         sec                            subtract the digit
         sbc   num2
         bcs   dv4
         adc   num2                     digit is 0
         dey
         bne   dv3
         bra   dv5
dv4      inc   num1                     digit is 1
         dey
         bne   dv3

dv5      tax                            save the remainder
         lda   num1                     get the result
         clv                            clear the error flag
dv7      pld                            reset DP
         ply                            clean up stack
         ply
         rtl

err      pla
         sep   #%01000000               SEV
         rtl
         end

****************************************************************
*
*  ~UMul2 - unsigned multiply
*
*  Inputs:
*        X,A - operands
*
*  Outputs:
*        A - result
*
*  Notes:
*        This routine is used for array index calculations and
*        fur unsigned multiplies in C.  It does not check for
*        overflows.
*
****************************************************************
*
~UMul2   start
n1       equ   3
n2       equ   5
;
;  Initialization
;
         phx                            save the operands
         pha
         phd                            set up our DP
         tsc
         tcd
         cpx   n1                       make sure n1 is the smaller argument
         bge   in1
         lda   n1
         stx   n1
         sta   n2
in1      anop
;
;  Do the multiply
;
         lda   #0

         lsr   n1
         bcc   lb1
         clc
         adc   n2
lb1      asl   n2

         lsr   n1
         bcc   lb2
         clc
         adc   n2
lb2      asl   n2

         lsr   n1
         bcc   lb3
         clc
         adc   n2
lb3      asl   n2

         lsr   n1
         beq   abrt4
         bcc   lb4
         clc
         adc   n2
lb4      asl   n2

         lsr   n1
         bcc   lb5
         clc
         adc   n2
lb5      asl   n2

         lsr   n1
         bcc   lb6
         clc
         adc   n2
lb6      asl   n2

         lsr   n1
         bcc   lb7
         clc
         adc   n2
lb7      asl   n2

         lsr   n1
         bcc   lb8
         clc
         adc   n2
lb8      asl   n2

! Note: We're done: since n1 < n2, we either overflow or the remaining bits are
! zero.

;
;  Return the result
;
         pld
         plx
         plx
         rtl
;
;  Abort with n1 = 0 after 4 shifts
;
abrt4    bcc   aa1
         clc
         adc   n2

aa1      pld                            return the result
         plx
         plx
         rtl
         end

****************************************************************
*
*  ~UMul4 - Four byte unsigned integer multiply
*
*  Inputs:
*        num2,X-A - operands
*
*  Outputs:
*        ans - result
*
****************************************************************
*
~UMul4   start
num2     equ   12                       arguments
ans      equ   1                        answer
return   equ   9
;
;  Initialize the sign and split on precision.
;
         pea   0                        set up stack frame
         pea   0
         phx
         pha
         tsc
         phd
         tcd
         txa                            branch if the multiplier is 16 bit
         beq   ml3
;
;  Do a 32 bit by 32 bit multiply.
;
         ldy   #32                      32 bit multiply
         jsr   ml1
         brl   ml7

ml1      lda   ans                      SYSS1*SYSS1+2+SYSS1+2 -> SYSS1,SYSS1+2
         lsr   a
         bcc   ml2
         clc                            add multiplicand to the partial product
         lda   ans+4
         adc   num2
         sta   ans+4
         lda   ans+6
         adc   num2+2
         sta   ans+6
ml2      ror   ans+6                    shift the interem result
         ror   ans+4
         ror   ans+2
         ror   ans
         dey                            loop til done
         bne   ml1
         rts
;
;  Do and 16 bit by 32 bit multiply.
;
ml3      lda   num2+2                   branch if 16x16 is possible
         beq   ml4

         ldy   #16                      set up for 16 bits
         jsr   ml1                      do the multiply
         move4 ans+2,num2               move the answer
         bra   ml8
;
;  Do a 16 bit by 16 bit multiply.
;
ml4      ldy   #16                      set the 16 bit counter
         ldx   ans                      move the low word
         stx   ans+2
ml5      lsr   ans+2                    test the bit
         bcc   ml6                      branch if the bit is off
         clc
         adc   num2
ml6      ror   a                        shift the answer
         ror   ans
         dey                            loop
         bne   ml5
         sta   ans+2                    save the high word
;
;  Return the result.
;
ml7      move4 ans,num2                 fix the stack
ml8      pld                            fix stack, DP
         tsc
         clc
         adc   #8
         tcs
         rtl
         end

****************************************************************
*
*  ~UShiftRight - Shift an unsigned value right
*
*  Inputs:
*        A - value to shift
*        X - # bits to shift by
*
*  Outputs:
*        A - result
*
****************************************************************
*
~UShiftRight start

         txy                            if # bits is 0, quit
         beq   rtl
         bpl   lb2                      if # bits is < 0 then
lb1      asl   A                          shift left
         inx
         bne   lb1
         bra   rtl                      else
lb2      lsr   A                          shift right
         dex
         bne   lb2
rtl      rtl
         end

****************************************************************
*
*  ~XCMDCommon - common area for HyperCard XCMD
*
****************************************************************
*
~XCMDCommon privdata

~XCMD_B  ds    2                        caller's B register
         end

****************************************************************
*
*  ~XCMDShutdown - shutdown code for a HyperCard XCMD
*
****************************************************************
*
~XCMDShutdown start
         using ~XCMDCommon

         jsl   ~MM_DisposeAll           shut down the compiler's memory manager
         jsl   ~MM_Init
         lda   >~XCMD_B                 restore HyperCard B
         pha
         plb
         plb
         rtl
         end

****************************************************************
*
*  ~XCMDStartup - startup code for a HyperCard XCMD
*
****************************************************************
*
~XCMDStartup start
         using ~XCMDCommon

         lda   9,S                      set up the parameter for main()
         tax
         lda   7,S
         tay
         lda   5,S
         sta   9,S
         lda   4,S
         sta   8,S
         tya
         sta   4,S
         txa
         sta   6,S
         phb                            save the caller's data bank
         phb
         pla
         sta   >~XCMD_B
         phk                            set up our data bank
         plb
         ora   #$0100                   set up our user ID
         sta   >~User_ID
         jsl   SystemEnvironmentInit    Set up the various environment variables
         rtl
         end

****************************************************************
*
*  ~XJPError - indexed jump error
*
****************************************************************
*
~XJPError start
         longa on
         longi on

         error #8                       Jump to undefined case statement label
         lda   #$FFFF
         brl   ~Quit
         end
