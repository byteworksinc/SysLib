         keep  obj/mm
         mcopy mm.macros
         case  on
****************************************************************
*
*  Memory Manager
*
*  This library implements a memory manager for C, Pascal, and
*  assembly language.  It works by allocating fixed size blocks
*  from the Apple IIGS memory manager, then allocating pieces
*  of these blocks as memory is requested.
*
*  By Mike Westerfield
*
*  Copyright December 1986
*  Byte Works, Inc.
*
****************************************************************
*
test     start
         end

****************************************************************
*
*  COMMON - Common data area
*
****************************************************************
*
~common  privdata
;
;  Constants
;
~headersize gequ 4                      size of an area header
~blockheader gequ 14                    size of a block header
~blocksize gequ 4*1024                  max size of an area
~maxdisp  gequ 9*4                      max disp into list
;
;  Global variables
;
~blocks  entry
         dc    i4'0'                    pointer to first block
~list    entry
         dc    10i4'0'                  list of free area pointers, by size
!                                         (8 bytes to 4K by powers of 2)
         end

****************************************************************
*
*  calloc - Allocate and clear memory long
*
*  Inputs:
*        elt_count - number of elements to allocate
*        elt_size - size of each element
*
*  Outputs:
*        X-A - pointer to allocated memory; NULL if none
*            
****************************************************************
*
calloc   start
         using ~common
elt_count equ  4                        number of elements
elt_size equ   8                        size of each element
size     equ   4                        size of area
p        equ   8                        pointer to area

         tsc                            set up DP
         phd
         tcd
         lda   elt_count+2              if elt count or size is > $7fffffff then
         ora   elt_size+2
         bmi   lb0                        return 0
         mul4  elt_count,elt_size,size  compute size of area
         bvs   lb0                      if overflow occurred then return 0
         lda   size                     if < ~BLOCKSIZE-~HEADERSIZE then
         ldx   size+2
         bne   lb2
         cmp   #~blocksize-~headersize
         bge   lb2
         tay                              if 0 then return NULL
         bne   lb1
lb0      lda   #0
         tax
         bra   lb8
lb1      jsl   ~NEW_AREA                  allocate area
         bra   lb3                      else
lb2      jsl   ~NEW_BLOCK                 allocate block
lb3      sta   p
         stx   p+2
         ora   p+2                      branch if NULL
         beq   lb8
         phx                            save pointer
         lda   p
         pha
         lda   size                     if area is odd length then
         lsr   a
         bcc   lb4
         short m                          zero one byte
         lda   #0
         sta   [p]
         long  m
         inc4  p
         dec   size
lb4      lda   #0                       zero 64K areas
         ldx   size+2
         beq   lb6
         tay
lb5      sta   [p],y
         dey
         dey
         bne   lb5
         inc   p+2
         dbne  x,lb5
lb6      ldy   size                     zero < 64K area
         beq   lb7b
         bra   lb7a
lb7      sta   [p],y
lb7a     dey
         dey
         bne   lb7
         sta   [p]
lb7b     pla                            recover pointer
         plx
lb8      tay                            return pointer
         move4 0,8
         pld
         clc
         tsc
         adc   #8
         tcs
         tya
         rtl
         end

****************************************************************
*
*  DISPOSE - Deallocate a piece of memory
*  free - Deallocate a piece of memory
*
*  Inputs:
*        PTR - pointer to memory to deallocate
*
****************************************************************
*
~DISPOSE start
free     entry
ptr      equ   4                        pointer to area to dispose of

         tsc                            set up DP
         phd
         tcd
         ldy   ptr                      save pointer in X-Y
         ldx   ptr+2
         sec                            get address of size
         tya
         sbc   #2
         sta   ptr
         bcs   lb1
         txa                            if pointer was 0 or 1 then
         beq   lb3                        return
         dec   ptr+2
lb1      lda   [ptr]                    if no size specified then
         bne   lb2
         tya                              free a block
         jsl   ~DISPOSE_BLOCK
         bra   lb3                      else
lb2      tya                              free an area
         jsl   ~DISPOSE_AREA
lb3      lda   0                        return
         sta   4
         lda   2
         sta   6
         pld
         pla
         pla
         rtl
         end

****************************************************************
*
*  DISPOSE_AREA - Dispose of an area
*
*  Inputs:
*        X-A - pointer to area to dispose of
*
****************************************************************
*
~DISPOSE_AREA private
         using ~common
p        equ   1                        pointer to area to dispose of
p2       equ   5
size     equ   9

         pha                            reserve space for P2, size
         pha
         pha
         sec                            P1 := P-~HEADERSIZE
         sbc   #~headersize
         bcs   lb1
         dex
lb1      phx
         pha
         tsc                            set up DP
         phd
         tcd
;
;  Mark area as free
;
         lda   [p]                      quit if the area is free
         jmi   rt1
         lda   [p]                      P^ := P^|$8000
         ora   #$8000
         sta   [p]
         ldy   #2                       SIZE := P^(2)
         lda   [p],y
         sta   size
         tay                            ~MMINSERT(P,SIZE)
         lda   p
         ldx   p+2
         jsl   ~MMINSERT
;
;  Combine adjacent free nodes
;
cb1      lda   size                     if SIZE < ~BLOCKSIZE then
         cmp   #~blocksize
         jge   db1
         lda   [p]                        if P^ & SIZE then
         and   size                         {second half of block}
         beq   cb2
         sec                              P2 := P-SIZE
         lda   p
         sbc   size
         sta   p2
         lda   p+2
         sbc   #0
         sta   p2+2
         ldy   #2                         if (P2^(2) = SIZE) and
         lda   [p2],y
         cmp   size
         jne   rt1
         lda   [p2]                         (P2^ & $8000) then
         jpl   rt1                          {node is free - combine}
         ldy   size                         ~REMOVE(P,SIZE)
         ldx   p+2
         lda   p
         jsl   ~REMOVE
         ldy   size                         ~REMOVE(P2,SIZE)
         ldx   p2+2
         lda   p2
         jsl   ~REMOVE
         lda   p2                           P := P2
         sta   p
         lda   p2+2
         sta   p+2
         asl   size                         SIZE := SIZE*2
         lda   size                         P^(2) := SIZE
         ldy   #2
         sta   [p],y
         tay                                ~MMINSERT(P,SIZE)
         lda   p
         ldx   p+2
         jsl   ~MMINSERT
         bra   cb1                          COMBINE(P)
!                                         endif
cb2      clc                            else {first half of block}
         lda   p                          P2 := P+SIZE
         adc   size
         sta   p2
         lda   p+2
         adc   #0
         sta   p2+2
         ldy   #2                         if (P2^(2) = SIZE) and
         lda   [p2],y
         cmp   size
         bne   rt1
         lda   [p2]                         (P2^ & $8000) then
         bpl   rt1                          {node is free - combine}
         ldy   size                         ~REMOVE(P,SIZE)
         ldx   p+2
         lda   p
         jsl   ~REMOVE
         ldy   size                         ~REMOVE(P2,SIZE)
         ldx   p2+2
         lda   p2
         jsl   ~REMOVE
         asl   size                         SIZE := SIZE*2
         ldy   #2                           P^(2) := SIZE
         lda   size
         sta   [p],y
         ldy   size                         ~MMINSERT(P,SIZE)
         lda   p
         ldx   p+2
         jsl   ~MMINSERT
         brl   cb1                          COMBINE(P)
!                                         endif
;
;  If area is an entire block, dispose of the block
;
db1      ldy   size                     else {SIZE = ~BLOCKSIZE}
         lda   p                          ~REMOVE(P,SIZE)
         ldx   p+2
         jsl   ~REMOVE
         lda   p                          ~DISPOSE_BLOCK(P-~BLOCKHEADER)
         ldx   p+2
         jsl   ~DISPOSE_BLOCK

rt1      pld                            fix stack and return
         tsc
         clc
         adc   #10
         tcs
         rtl
         end

****************************************************************
*
*  DISPOSE_BLOCK - Deallocate a block of memory
*
*  Inputs:
*        X-A - pointer to usable part of block to deallocate
*
****************************************************************
*
~DISPOSE_BLOCK private
         using ~common
ptr      equ   1                        pointer to block to deallocate
p2       equ   5                        work pointer

         pha                            reserve space for P2
         pha
         sec                            point regesters to correct spot
         sbc   #~blockheader
         bcs   in1
         dex
in1      phx                            save pointer
         pha
         tsc                            set up DP
         phd
         tcd
         ldy   #2                       if PTR^ = NIL then
         lda   [ptr],y
         ora   [ptr]
         bne   lb1
         ldy   #4                         ~BLOCKS := PTR^(4)
         lda   [ptr],y
         sta   >~blocks
         iny
         iny
         lda   [ptr],y
         sta   >~blocks+2
         bra   lb2                      else
lb1      lda   [ptr]                      PTR^(4)^ := PTR^(4)
         sta   p2
         lda   [ptr],y
         sta   p2+2
         ldy   #4
         lda   [ptr],y
         sta   [p2],y
         iny
         iny
         lda   [ptr],y
         sta   [p2],y                   endif
lb2      ldy   #4                       if PTR^(4) <> NIL then
         lda   [ptr],y
         tax
         iny
         iny
         ora   [ptr],y
         beq   lb3
         lda   [ptr],y                    PTR^(4)^ := PTR^
         sta   p2+2
         stx   p2
         ldy   #2
         lda   [ptr]
         sta   [p2]
         lda   [ptr],y
         sta   [p2],y                   endif
lb3      ldy   #10                      dispose(PTR)
         lda   [ptr],y
         pha
         dey
         dey
         lda   [ptr],y
         pha
         _DisposeHandle
         pld                            reset DP
         pla                            dump work space
         pla
         pla
         pla
         rtl
         end

****************************************************************
*
*  FIND_DISP - Compute displacement into LIST
*
*  Inputs:
*        A - size of area
*
*  Outputs:
*        X - disp into list
*
****************************************************************
*
~FIND_DISP private

         ldx   #0
         dec   a
         lsr   a
         lsr   a
         lsr   a
         beq   lb2
lb1      inx
         inx
         inx
         inx
         lsr   a
         bne   lb1
lb2      rtl
         end

****************************************************************
*
*  INSERT - Insert an area into the area list
*
*  Inputs:
*        X-A - pointer to area
*        Y - size of area
*
****************************************************************
*
~MMINSERT private
         using ~common
ptr      equ   1                        pointer to area

         phx                            save pointer
         pha
         tsc                            set up DP
         phd
         tcd
         tya                            X = disp into list
         jsl   ~FIND_DISP
         ldy   #~headersize             PTR^(~HEADERSIZE) := ~LIST(X)
         lda   >~list,x
         sta   [ptr],y
         iny
         iny
         lda   >~list+2,x
         sta   [ptr],y
         lda   ptr                      ~LIST(X) := PTR
         sta   >~list,x
         lda   ptr+2
         sta   >~list+2,x
         pld                            fix stack and return
         pla
         pla
         rtl
         end

****************************************************************
*
*  ~MM_DISPOSEALL - dispose of all blocks
*
*  This is a slower way to reset the memory manager, used in
*  cases like XCMDs when all of the memory for the user ID
*  should not be flushed with a DisposeAll call.
*
****************************************************************
*
~MM_DISPOSEALL start
         using ~common

lb1      lda   ~blocks                  while ~blocks <> nil do begin
         ora   ~blocks+2
         beq   lb3
         lda   ~blocks                    ~DISPOSE_BLOCK(~blocks+~blockheader)
         ldx   ~blocks+2
         clc
         adc   #~blockheader
         bcc   lb2
         inx
lb2      jsl   ~DISPOSE_BLOCK
         bra   lb1
lb3      rtl
         end

****************************************************************
*
*  ~MM_INIT - initialize the memory manager for a restart
*
****************************************************************
*
~MM_INIT start
         using ~common

         phb
         phk
         plb
         stz   ~blocks
         stz   ~blocks+2
         ldx   #~maxdisp+2
lb1      stz   ~list,X
         dex
         dex
         bpl   lb1
         plb
         rtl
         end

****************************************************************
*
*  NEW - Allocate an area of memory
*  malloc - Allocate an area of memory
*
*  Inputs:
*        LEN - number of bytes to allocate
*
*  Outputs:
*        X-A - pointer to allocated area; NULL if error
*
****************************************************************
*
~NEW     start
malloc   entry
         using ~common

         lda   6,s                      fetch size of memory
         tax
         lda   4,s
         cpx   #0                       if < ~BLOCKSIZE-~HEADERSIZE
         bne   lb1
         cmp   #~blocksize-~headersize
         bge   lb1
         cmp   #0                         if 0 then RETURN NULL
         beq   lb2
         jsl   ~NEW_AREA                  allocate area
         bra   lb2                      else
lb1      jsl   ~NEW_BLOCK                 allocate block
lb2      tay                            return pointer
         lda   0,s
         sta   4,s
         lda   2,s
         sta   6,s
         pla
         pla
         tya
         rtl
         end

****************************************************************
*
*  NEW_AREA - Allocate a new area
*
*  Inputs:
*        A - size of area to allocate
*
*  Outputs:
*        X-A - pointer to usable part of area; NIL if none available
*
****************************************************************
*
~NEW_AREA private
         using ~common
p        equ   1
p2       equ   5
disp     equ   9
t        equ   11
size     equ   13

         pha                            save size
         clc                            initialize T to SIZE+~HEADERSIZE
         adc   #~headersize
         pha
         tsc                            reserve space for other parms
         sec
         sbc   #10
         tcs
         phd                            set up DP
         tcd
;
;  Set size to smallest power of 2 that is greater than or equal to the
;  requested size.
;
         lda   #8
ss1      cmp   t
         bge   ss2
         asl   a
         bra   ss1
ss2      sta   size
;
;  Set displacement into list
;
         jsl   ~FIND_DISP               DISP := ~FIND_DISP(SIZE)
         stx   disp
;
;  Make sure an area of the needed size is available
;
         stx   t                        T := DISP
fa1      ldx   t                        repeat
         lda   >~list,x                   if ~LIST[T] = NIL then
         ora   >~list+2,x
         bne   fa4
         cpx   #~maxdisp                    if T = ~MAXDISP then
         blt   fa2
         lda   #~blocksize                    P := ~NEW_BLOCK(~BLOCKSIZE)
         ldx   #0                             ~LIST[~MAXDISP] := P
         jsl   ~NEW_BLOCK
         sta   p
         sta   >~list+~maxdisp
         stx   p+2
         txa
         sta   >~list+~maxdisp+2
         ora   p                              if P = NIL then return NIL
         jeq   ra1
         lda   #$8000                         P^ := $8000
         sta   [p]
         ldy   #2                             P^(2) := ~BLOCKSIZE
         lda   #~blocksize
         sta   [p],y
         iny                                  P^(~HEADERSIZE) := NIL
         iny
         lda   #0
         sta   [p],y
         iny
         iny
         sta   [p],y
         lda   t                              if T == DISP then break;
         cmp   disp
         beq   ra0
         bra   fa6                          else
fa2      clc                                  ++T
         lda   t
         adc   #4
         sta   t
         asl   size                           SIZE *= 2
fa3      anop                               endif
         bra   fa6                        else
fa4      ldx   t                            if T > DISP then
         cpx   disp                           {split the area in half}
         ble   fa5
         lda   >~list+2,x                     P := ~LIST[T]
         sta   p+2
         lda   >~list,x
         sta   p
         ldy   size                           ~REMOVE(P,SIZE)
         ldx   p+2
;        LDA   P                                (see above - done there)
         jsl   ~REMOVE
         lsr   size                           SIZE /= 2
         clc                                  P2 := P+SIZE
         lda   p
         adc   size
         sta   p2
         lda   p+2
         adc   #0
         sta   p2+2
         ldy   #2                             P2^(2) := SIZE
         lda   size
         sta   [p2],y
         sta   [p],y                          P^(2) := SIZE
         ora   [p]                            P2^ := P^|SIZE
         sta   [p2]
         ldy   size                           ~MMINSERT(P,SIZE)
         lda   p
         ldx   p+2
         jsl   ~MMINSERT
         ldy   size                           ~MMINSERT(P,SIZE)
         lda   p2
         ldx   p2+2
         jsl   ~MMINSERT
fa5      anop                               endif
         sec                              --T
         lda   t
         sbc   #4
         sta   t
fa6      anop                             endif
         lda   t                        until T < DISP
         bmi   ra0
         cmp   disp
         jge   fa1
;
;  Return allocated area
;
ra0      ldx   disp                     P := ~LIST[DISP]
         lda   >~list,x
         sta   p
         lda   >~list+2,x
         sta   p+2
         ldy   size                     ~REMOVE(P,SIZE)
         ldx   p+2
         lda   p
         jsl   ~REMOVE
         lda   [p]                      mark as used
         and   #$7fff
         sta   [p]
ra1      lda   p                        return value
         ldx   p+2
         ora   p+2
         beq   ra2
         lda   p
         clc
         adc   #~headersize
         bcc   ra2
         inx
ra2      tay
         pld
         tsc
         clc
         adc   #14
         tcs
         tya
         rtl
         end

****************************************************************
*
*  NEW_BLOCK - Allocate a block of memory
*
*  Inputs:
*        X-A - size of block to allocate
*
*  Outputs:
*        X-A - addr of usable part of block; NIL if none
*
****************************************************************
*
~NEW_BLOCK private
         using ~common
ptr      equ   1                        pointer to block
handle   equ   5                        handle of block

         clc                            add length of header
         adc   #~blockheader
         bcc   lb1
         inx
         beq   lb1c                     if addition overflowed then return NIL
lb1      sta   >memSize                 save memory size
         txa
         sta   >memSize+2
         lda   #$0008                   set the default flag
         sta   >flag
lc1      pha                            allocate the memory
         pha
         lda   >memSize+2
         pha
         lda   >memSize
         pha
         lda   >~USER_ID
         pha
         lda   >memSize+2
         bne   lb1a
         lda   #$4010
         bra   lb1b
lb1a     lda   #$4000
lb1b     ora   >flag
         pha
         pea   0
         pea   0
         _NewHandle
         bcc   lb2                      branch if successful
         pla                            pull off garbage pointer
         pla
         lda   >flag                    if we tried for normal memory, try
         and   #$0008                    again for special memory
         beq   lb1c
         lda   #0
         sta   >flag
         bra   lc1
lb1c     lda   #0                       return NIL
         tax
         rtl

lb2      pha                            reserve space for PTR
         pha
         tsc                            set up DP
         phd
         tcd
         ldy   #2                       set up the pointer
         lda   [handle]
         sta   ptr
         lda   [handle],y
         sta   ptr+2
         lda   #0                       set back pointer to NIL
         sta   [ptr]
         sta   [ptr],y
         lda   >~blocks                 set forward pointer to ~BLOCKS
         iny
         iny
         sta   [ptr],y
         lda   >~blocks+2
         iny
         iny
         sta   [ptr],y
         iny                            save the handle
         iny
         lda   handle
         sta   [ptr],y
         iny
         iny
         lda   handle+2
         sta   [ptr],y
         iny                            set fake size to 0
         iny
         lda   #0
         sta   [ptr],y
         lda   >~blocks                 set next node's back pointer
         sta   handle
         lda   >~blocks+2
         sta   handle+2
         ora   handle
         beq   lb2a
         ldy   #2
         lda   ptr
         sta   [handle]
         lda   ptr+2
         sta   [handle],y
lb2a     lda   ptr                      set ~BLOCKS to pointer
         sta   >~blocks
         lda   ptr+2
         sta   >~blocks+2
         clc                            return PTR+~BLOCKHEADER
         lda   ptr
         adc   #~blockheader
         ldx   ptr+2
         bcc   lb3
         inx
lb3      pld                            fix stack, DP and return
         ply
         ply
         ply
         ply
         rtl

flag     ds    2
memSize  ds    4                        size of memory to allocate
         end

****************************************************************
*
*  realloc - Reallocate an area of memory
*
*  Inputs:
*        ptr - pointer to the existing area of memory
*        size - size for the new area of memory
*
*  Outputs:
*        X-A - pointer to allocated memory; NULL if none
*
****************************************************************
*
realloc  start
p        equ   1                        pointer to new area
return   equ   5                        return address
ptr      equ   8                        pointer to old area
size     equ   12                       size of new area

         pha                            reserve space for P
         pha
         tsc                            set up DP
         phd
         tcd
         phb                            use local addressing
         phk
         plb
         stz   val                      assume NULL result
         stz   val+2

         lda   size                     if size is 0 then
         ora   size+2
         bne   zr1
         ph4   <ptr                       deallocate the old area
         jsl   free
to_lb5   brl   lb5                        quit

zr1      ph4   <size                    reserve the new area
         jsl   malloc
         sta   p
         stx   p+2
         sta   val
         stx   val+2
         lda   ptr                      done if either pointer is null
         ora   ptr+2
         beq   to_lb5
         lda   p
         ora   p+2
         beq   to_lb5
         lda   ptr                      save original ptr
         ldx   ptr+2
         sta   oldPtr
         stx   oldPtr+2
         sub4  ptr,#2                   get size of old allocation
         lda   [ptr]                    ... from area header, if specified
         beq   lb0
         sec
         sbc   #~headersize   
         sta   oldSize
         stz   oldSize+2
         bra   lb0a
lb0      pha                            ... or from handle for block
         pha
         sub4  ptr,#4
         ldy   #2
         lda   [ptr],y
         pha
         lda   [ptr]
         pha
         _GetHandleSize
         pl4   oldSize
         sub4  oldSize,#~blockheader
lb0a     cmpl  oldSize,size             if old size < new size then
         bge   lb0b
         move4 oldSize,size               size to move = old size
lb0b     move4 oldPtr,ptr
         lda   size                     move old area to new:
         lsr   a                        ... if odd, move one byte
         bcc   lb1
         short m
         lda   [ptr]
         sta   [p]
         long  m
         inc4  ptr
         inc4  p
         dec   size
lb1      lda   #0                       ... move 64K areas
         ldx   size+2
         beq   lb3
         tay
lb2      lda   [ptr],y
         sta   [p],y
         dey
         dbne  y,lb2
         inc   ptr+2
         inc   p+2
         dbne  x,lb2
lb3      ldy   size                     ... move <64K area
         bne   lb4a
         bra   lb4b
lb4      lda   [ptr],y
         sta   [p],y
lb4a     dey
         dey
         bne   lb4
         lda   [ptr]
         sta   [p]
lb4b     ph4   oldPtr                   dispose of old area
         jsl   free

lb5      lda   return                   patch return address
         sta   size+1
         lda   return+1
         sta   size+2
         plb                            fix bank reg
         pld                            fix D reg
         tsc                            remove p, parms from stack
         clc
         adc   #12
         tcs
         lda   >val+2                   return val
         tax
         lda   >val
         rtl

val      ds    4                        value to return
oldPtr   ds    4                        original value of ptr
oldSize  ds    4                        size of old allocation
         end

****************************************************************
*
*  REMOVE - Remove an area from the area list
*
*  Inputs:
*        X-A - pointer to area
*        Y - size of area
*
****************************************************************
*
~REMOVE  private
         using ~common
p1       equ   1                        parent of P2
p2       equ   5                        current node
ptr      equ   9                        node to remove

         phx                            initialize pointer
         pha
         lda   #0                       reserve space for P2
         pha
         pha
         pha                            initialize P1 to NIL
         pha
         tsc                            set up DP
         phd
         tcd
         tya                            X = disp into list
         jsl   ~FIND_DISP
         lda   >~list,x                 P2 := ~LIST[X]
         sta   p2
         lda   >~list+2,x
         sta   p2+2
lb1      lda   p2                       while P2 <> PTR do
         cmp   ptr
         bne   lb2
         lda   p2+2
         cmp   ptr+2
         beq   lb3
lb2      lda   p2                         P1 := P2
         sta   p1
         lda   p2+2
         sta   p1+2
         ldy   #~headersize               P2 := P1^[~HEADERSIZE]
         lda   [p1],y
         sta   p2
         iny
         iny
         lda   [p1],y
         sta   p2+2
         bra   lb1                      endwhile
lb3      ldy   #~headersize
         lda   p1                       if P1 = NIL then
         ora   p1+1
         bne   lb4
         lda   [p2],y                     ~LIST[X] := P2^[~HEADERSIZE]
         sta   >~list,x
         iny
         iny
         lda   [p2],y
         sta   >~list+2,x
         bra   lb5                      else
lb4      lda   [p2],y                     P1^[~HEADERSIZE] := P2^[~HEADERSIZE]
         sta   [p1],y
         iny
         iny
         lda   [p2],y
         sta   [p1],y
lb5      pld                            endif
         tsc
         clc
         adc   #12
         tcs
         rtl
         end
