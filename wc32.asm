; WC32 - a Tachyon Forth inspired 32-bit system

; FOR_OS equ WINDOWS
; FOR_OS equ LINUX

match =WINDOWS, FOR_OS {
        format PE console
        include 'win32ax.inc'
        .code
}

match =LINUX, FOR_OS {
        format ELF executable 3
        segment readable executable
}

; ******************************************************************************
; Defines

TOS  equ edi         ; Top-Of-Stack
PCIP equ esi         ; Program-Counter/Instruction-Pointer
STKP equ ebp         ; Stack-Pointer

REG1 equ eax         ; Free register #1
REG2 equ ebx         ; Free register #2
REG3 equ ecx         ; Free register #3
REG4 equ edx         ; Free register #4

CELL_SZ = 4
CODE_SZ =  64*1024
DICT_SZ =  64*1024
VARS_SZ = 256*1024
TIB_SZ  = 128
xNum    = 0x70000000
numMask = 0x0FFFFFFF
LastTag equ 0

; A dictionary entry looks like this:
; Next/4, XT/4, Flags/1, Len/1, Name/?, NULL/1
DE_NEXT_OFFSET  = (0)
DE_XT_OFFSET    = (CELL_SZ)
DE_FLAGS_OFFSET = (CELL_SZ*2)
DE_LEN_OFFSET   = (CELL_SZ*2)+1
DE_NAME_OFFSET  = (CELL_SZ*2)+2

; ******************************************************************************
; MACROS
macro getTOS val { mov val, TOS }
macro setTOS val { mov TOS, val }
macro get2ND val { mov val, [STKP] }
macro set2ND val { mov [STKP], val }

macro m_push val {
       add STKP, CELL_SZ
       mov [STKP], TOS
       setTOS val
}

; ******************************************************************************
macro m_pop val {
       getTOS val
       mov TOS, [STKP]
       sub STKP, CELL_SZ
}

; ******************************************************************************
macro dictEntry XT, Flags, Len, Name, Tag {
    align CELL_SZ
    Tag: dd LastTag
         dd XT
         db Flags
         db Len
         db Name, 0
    LastTag equ Tag
}

; ******************************************************************************
; ******************************************************************************
; ******************************************************************************
entry $
main:   call    getHandles
        mov     [InitialESP], esp
        mov     ebx, THE_CODE
        mov     [HERE], ebx
        mov     ebx, THE_VARS
        mov     [VHERE], ebx

warm:   mov     eax, rStack
        mov     [rStackPtr], eax
        mov     STKP, dStack
        mov     TOS, 0
        mov     esi, THE_ROM
        cld
        jmp     wcRun
        ret

; ******************************************************************************
; ** The bread and butter of the system
; ******************************************************************************
wCall:  call    eax
wcRun:  lodsd
        cmp     eax, primEnd
        jl      wCall
        cmp     eax, xNum
        jge     .NUM
        add     [rStackPtr], CELL_SZ
        mov     edx, [rStackPtr]
        mov     [edx], esi
        mov     esi, eax
        jmp     wcRun
.NUM:   and     eax, numMask
        m_push  eax
        jmp     wcRun

; ******************************************************************************
; ******************************************************************************
EXIT: call    checkRStack
        test    edx, edx
        jz      .U
        mov     esi, [edx]
        sub     [rStackPtr], CELL_SZ
        ret
.U:     mov     esi, xWarm
        ret

; ******************************************************************************
checkRStack:
        mov     edx, [rStackPtr]
        cmp     edx, rStack
        jle     .Under
        ret
.Under: m_push '-'
        call    EMIT
        m_push 'U'
        call    EMIT
        m_push '-'
        call    EMIT
        mov     [rStackPtr], rStack
        xor     edx, edx
        ret;

; ******************************************************************************
rStackTo: ; ( N-- )
        m_pop   eax
        add     [rStackPtr], CELL_SZ
        mov     edx, [rStackPtr]
        mov     [edx], eax
        ret

; ******************************************************************************
rStackFrom: ; ( --N )
        call    checkRStack
        mov     eax, [edx]
        m_push  eax
        sub     [rStackPtr], CELL_SZ
        ret

; ******************************************************************************
rStackFetch: ; ( --N )
        call    checkRStack
        mov     eax, [edx]
        m_push  eax
        ret

; ******************************************************************************
rStackStore: ; ( N-- )
        call    checkRStack
        m_pop   eax
        mov     [edx], eax
        ret

; ******************************************************************************
doNop:  ret

; ******************************************************************************
; input:        ; eax: the number to print - destroyed
; output:       ; ecx: the start of the string
;               ; ebx: the length of the string
;
iToA:   mov     ecx, i2aBuf+63  ; output string start
        mov     ebx, 0          ; output length
        mov     BYTE [ecx], 0
        push    0               ; isNegative flag
        bt      eax, 31
        jnc     .L
        inc     BYTE [esp]
        neg     eax
.L:     push    ebx
        mov     ebx, 10
        mov     edx, 0
        div     ebx
        add     dl, '0'
        dec     ecx 
        mov     BYTE [ecx], dl
        pop     ebx
        inc     ebx
        test    eax, eax
        jnz     .L
        pop     eax             ; get the negative flag
        test    eax, eax        ; 0 means not negative
        jz      .X
        dec     ecx
        mov     BYTE [ecx], '-'
        inc     ebx
.X:     ret

; ******************************************************************************
doItoA: m_pop   eax
        call    iToA
        m_push  ecx
        m_push  ebx
        ret

; ******************************************************************************
JmpA:   lodsd
doJ:    mov     esi, eax
        ret

; ******************************************************************************
JmpZ:   m_pop  ebx
        lodsd
        test   ebx, ebx
        jz     doJ
        ret

; ******************************************************************************
JmpNZ:  m_pop  ebx
        lodsd
        test   ebx, ebx
        jnz    doJ
        ret

; ******************************************************************************
NJmpZ:  lodsd
        test   TOS, TOS
        jz     doJ
        ret

; ******************************************************************************
NJmpNZ: lodsd
        test   TOS, TOS
        jnz    doJ
        ret

; ******************************************************************************
Fetch: getTOS   edx
         setTOS   [edx]
         ret

; ******************************************************************************
doStore: m_pop  edx
         m_pop  eax
         mov    [edx], eax
         ret

; ******************************************************************************
CFetch: xor     eax, eax
        mov     al, BYTE [TOS]
        mov     TOS, eax
        ret

; ******************************************************************************
CStore: m_pop edx
        m_pop   eax
        mov     BYTE [edx], al
        ret



; ******************************************************************************
isDigit: ; Carry set means al is a digit, clear means al is NOT a digit
        cmp     al, '0'
        jl      .NO
        cmp     al, '9'
        jg      .HEX
        sub     al, '0'
        stc     ; It IS a digit
        ret
.HEX:   cmp     ecx, 16
        jne     .NO
        cmp     al, 'A'
        jl      .NO
        cmp     al, 'F'
        jg      .NO
        sub     al, 'A'
        add     al, 10
        stc     ; It is a digit
        ret
.NO:    clc     ; It is NOT a digit
        ret

; ******************************************************************************
doNumQ: ; ( addr--num 1 | 0 )
        getTOS  ebx
        mov     ecx, [BASE]
        xor     eax, eax
        xor     edx, edx
.LOOP:  mov     al, [ebx]
        test    al, al
        jz      .YES
        call    isDigit
        jnc     .NO
        imul    edx, ecx
        add     edx, eax
        inc     ebx
        jmp     .LOOP
.NO:    setTOS  0
        ret
.YES:   setTOS  edx
        m_push  1
        ret

; ******************************************************************************
; Quote
doQt:   lodsb   ; TODO
.x:     ret

; ******************************************************************************
betw:   cmp     al, bl
        jl      betF
        cmp     al, bh
        jg      betF
        mov     bl, 1
        ret
betF:   mov     bl, 0
        ret

; ******************************************************************************
doFor:  add     [lStackPtr], CELL_SZ*3
        mov     edx, [lStackPtr]
        mov     [edx], DWORD 0
        m_pop   eax
        mov     [edx-CELL_SZ], eax
        mov     [edx-(CELL_SZ*2)], esi
        ret

; ******************************************************************************
doI:    mov     edx, [lStackPtr]
        mov     eax, [edx]
        m_push  eax
        ret

; ******************************************************************************
doNext: mov     edx, [lStackPtr]
        mov     eax, [edx]
        inc     eax
        cmp     eax, [edx-CELL_SZ]
        jge     doUnloop
        mov     [edx], eax
        mov     esi, [edx-(CELL_SZ*2)]
        ret

; ******************************************************************************
doUnloop:
        mov     edx, [lStackPtr]
        sub     edx, CELL_SZ*3
        cmp     edx, lStack
        jge     .XX
        mov     edx, lStack
.XX:    mov     [lStackPtr], edx
        ret

; ******************************************************************************
match =WINDOWS, FOR_OS { include 'io-win.asm' }
match =LINUX,   FOR_OS { include 'io-lin.asm' }

; ******************************************************************************
doMult: m_pop   eax
        imul    TOS, eax
        ret

; ******************************************************************************
doSub:  m_pop   eax
        sub     TOS, eax
        ret

; ******************************************************************************
doAdd:  m_pop   eax
        add     TOS, eax
        ret

; ******************************************************************************
doMod:  m_pop   ebx
        cmp     ebx, 0
        je      .X
        m_pop   eax
        mov     edx, 0
        idiv    ebx
        m_push  edx
.X:     ret

; ******************************************************************************
doDiv:  m_pop   ebx
        cmp     ebx, 0
        je      .X
        m_pop   eax
        mov     edx, 0
        idiv    ebx
        m_push  eax
.X:     ret

; ******************************************************************************
doInc:  inc     TOS
        ret

; ******************************************************************************
doDec:  dec     TOS
        ret

; ******************************************************************************
doAnd:  m_pop   eax
        and     TOS, eax
        ret

; ******************************************************************************
doOr:   m_pop   eax
        or      TOS, eax
        ret

; ******************************************************************************
doXOR:  m_pop   eax
        xor     TOS, eax
        ret

; ******************************************************************************
doNeg:  neg     TOS
        ret

; ******************************************************************************
doInv:  not     TOS
        ret

; ******************************************************************************
doTrue: mov     TOS, 1
        ret

; ******************************************************************************
doFalse: mov     TOS, 0
        ret

; ******************************************************************************
doEQ:   m_pop   eax
        cmp     TOS, eax
        je      doTrue
        jmp     doFalse

; ******************************************************************************
doLT:   m_pop   eax
        cmp     TOS, eax
        jl      doTrue
        jmp     doFalse

; ******************************************************************************
doGT:   m_pop   eax
        cmp     TOS, eax
        jg      doTrue
        jmp     doFalse

; ******************************************************************************
doDot:  push    eax
        push    ebx
        push    ecx
        push    edx
        m_pop   eax
        call    iToA
        m_push  ecx
        m_push  ebx
        call    doType
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        ret

; ******************************************************************************
doDotS: m_push  '('
        call    printChar
        mov     eax, dStack+CELL_SZ
.L:     cmp     eax, STKP
        jg      .X
        m_push  [eax]
        call    doDot
        m_push  32
        call    printChar
        add     eax, CELL_SZ
        jmp     .L
.X:     call    doDup
        call    doDot
        m_push  ')'
        call    EMIT
        ret

; **********************************************************************
printChar:
        push    eax
        push    ebx
        push    ecx
        push    edx
        call    EMIT
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        ret

; ******************************************************************************
doDup:  getTOS  eax
        m_push  eax
        ret

; ******************************************************************************
doSwap: get2ND    eax
        set2ND    TOS
        setTOS    eax
        ret

; ******************************************************************************
doOver: get2ND      eax
        m_push      eax
        ret

; ******************************************************************************
doDrop: mov     TOS, [STKP]
        sub     STKP, CELL_SZ
        cmp     STKP, dStack
        jg      .X
        mov     STKP, dStack
.X:     ret

; ******************************************************************************
doLen:  m_pop   edx             ; ( addr--len )
        xor     ecx, ecx
.L:     cmp     [edx], BYTE 0
        je      .X
        inc     ecx
        inc     edx
        jmp     .L
.X:     m_push  ecx
        ret

; ******************************************************************************
skipWS: mov     edx, [ToIn]     ; Updates ToIn to point to the next non-whitespace char or NULL
        xor     eax, eax
.L:     mov     al, [edx]
        cmp     al, 32
        jg      .X
        test    al, al
        jz      .X
        inc     edx
        jmp     .L
.X:     mov     [ToIn], edx
        ret


; ******************************************************************************
nextWd: call    skipWS          ; ( --addr len )
        mov     ebx, buf2
        m_push  ebx
        m_push  0
.L:     mov     al, [edx]
        cmp     al, 32
        jle     .X
        mov     [ebx], al
        inc     edx
        inc     ebx
        inc     TOS
        cmp     TOS, 31
        jl      .L
.X:     mov     [ToIn], edx
        mov     [ebx], BYTE 0
        ret

; ******************************************************************************
doStrEq:       ; ( str1 str2--fl )
        m_pop   ecx
        m_pop   edx
        m_push  0               ; Default to NOT equal
.LP:    mov     al, [ecx]
        cmp     al, [edx]
        jne     .X
        test    al, al          ; End of strings?
        jz      .EQ
        inc     ecx
        inc     edx
        jmp     .LP
.EQ:    inc     TOS
.X:     ret

; ******************************************************************************
toLower: cmp    al, 'A'
         jl     .X
         cmp    al, 'Z'
         jg     .X
         add    al, 32
.X:      ret

; ******************************************************************************
doStrEqI:      ; ( str1 str2--fl )
        m_pop   ecx
        m_pop   edx
        m_push  0               ; Default to NOT equal
.LP:    mov     al, [ecx]
        call    toLower
        mov     ah, al
        mov     al, [edx]
        call    toLower
        cmp     al, ah
        jne     .X
        test    al, al          ; End of strings?
        jz      .EQ
        inc     ecx
        inc     edx
        jmp     .LP
.EQ:    inc     TOS
.X:     ret

; ******************************************************************************
Lit:  lodsd
        m_push eax
        ret

; ******************************************************************************
doComma: ; ( n-- )
        m_pop   eax
        mov     edx, [HERE]
        mov     [edx], eax
        add     edx, CELL_SZ
        mov     [HERE], edx
        ret

; ******************************************************************************
doCComma: ; ( n-- )
        m_pop   eax
        mov     edx, [HERE]
        mov     [edx], al
        inc     edx
        mov     [HERE], edx
        ret

; ******************************************************************************
; ******************************************************************************
; ******************************************************************************
primEnd:

; ******************************************************************************
; ******************************************************************************
; ******************************************************************************
match =WINDOWS, FOR_OS {
        section '.idata' import data readable writeable
        library kernel32, 'kernel32.dll', msvcrt, 'msvcrt.dll', conio, 'conio32.dll'
        import kernel32, GetTickCount, 'GetTickCount', ExitProcess, 'ExitProcess', \
                WriteConsole, 'WriteConsoleA', ReadConsole, 'ReadConsoleA', \
                GetStdHandle, 'GetStdHandle'
        import msvcrt, _getch, '_getch', _kbhit, '_kbhit'
        .data
}
match =LINUX,   FOR_OS { segment readable writable }

hStdIn      dd  ?
hStdOut     dd  ?
InitialESP  dd ?
okStr       db  " ok", 0
i2aBuf      db  64 dup ?
bytesRead   dd  ?
unkOP       db  "-unk-"
rStackPtr   dd  rStack
lStackPtr   dd  lStack
HERE        dd  THE_CODE
VHERE       dd  THE_VARS
LAST        dd  tagLast
BASE        dd  16
HERE1       dd  ?
TIB         dd  TIB_SZ dup 0
ToIn        dd  ?

buf1        db   32 dup 0       ; Buffer (used for EMIT)
dStack      dd   64 dup 0       ; Data stack
buf2        db   32 dup 0       ; Buffer (used for nextWd)
rStack      dd   64 dup 0       ; Return stack
buf3        db   32 dup 0       ; Buffer
lStack      dd   64 dup 0       ; Loop stack
buf4        db   32 dup 0       ; Buffer

; ----------------------------------------------------------------
THE_ROM:
xCold       dd xHA, xDot, xHere, xDot, xLast, xDot, xCell, xDot, doDotS
                dd xCR, xWords, doDotS, xBench
                dd xCR, xCR, xNum+10, doFor, doI, doInc, xDot, doNext, doDotS
                dd xCR, xNum+11, xNum+22, xNum+33, doDotS, doDrop, doDrop, doDrop
xWarm       dd xInterp, JmpA, xWarm
xInterp     dd xOK, xTIB, xTIBSZ, xAccept, doDrop ; , xTIB, doAdd, xNum, doSwap, CStore
                ; dd xTIB, doDup, doLen, doType, xSpace
                dd xTIB, xToIn, doStore; , doDotS, xHere, xDot
xIntLoop        dd nextWd, JmpNZ, xIntNumQ, doDrop, EXIT                ; Get next word, exit if no more words
xIntNumQ:       ; dd Lit, buf2, doDup, doLen, doType, xSpace            ; *** temp ***
                dd doNumQ, JmpZ, xIntDictQ                              ; Is it a number?
                ; dd doDup, xNum+'n', EMIT, doDot, xNum+'n', EMIT       ; *** temp - yes, it is a number! ***
                dd xCompNum, JmpA, xIntLoop                             ; Yes, it is a number!
xIntDictQ       dd Lit, buf2, xFind, JmpZ, xIntERR                      ; Is it in the dictionary?
                dd JmpNZ, xIntImmed                                     ; YES! Is it immediate?
                ; dd xNum+'C', EMIT, xNum+'-', EMIT, doDup, xDot        ; *** temp ***
                dd xExecute, JmpA, xIntLoop
                dd doComma, JmpA, xIntLoop
xIntImmed       dd xNum+'I', EMIT, xNum+'-', EMIT, doDot
                dd JmpA, xIntLoop
xIntERR         dd xNum+'?', xNum+'?', EMIT, EMIT, EXIT
xExecute    dd rStackTo, EXIT
; xExecute    dd rStackTo, EXIT
xCompNum    dd EXIT
                ; dd doDup, Lit, 0x80000000, doAnd, JmpNZ, xCompLit
                ; dd Lit, xNum, doOr, doComma, EXIT
xCompLit        dd Lit, Lit, doComma, doComma, EXIT
xDeShow     dd doDup, xDeName                   ; First char of name    ( a1--a2 )
                dd doDup, doLen, doType         ; Name length
                dd xDeNext, EXIT              ; Next entry
xDeShowVB   dd xCR, doDup, xDeNext, xDot        ; Next    ( a1--a2 )
                dd doDup, xDeXT,    xDot        ; XT
                dd doDup, xDeFlags, xDot        ; Flags
                dd doDup, xDeName               ; First char of name
                dd doDup, doLen, doType         ; Name length
                dd xDeNext, EXIT              ; Next entry
xDeNext     dd xNum+DE_NEXT_OFFSET,  doAdd, Fetch,  EXIT      ; dict entry Next  ( de--next )
xDeXT       dd xNum+DE_XT_OFFSET,    doAdd, Fetch,  EXIT      ; dict entry XT    ( de--xt )
xDeFlags    dd xNum+DE_FLAGS_OFFSET, doAdd, CFetch, EXIT      ; dict entry flags ( de--flags )
xDeLen      dd xNum+DE_LEN_OFFSET,   doAdd, CFetch, EXIT      ; dict entry len   ( de--len )
xDeName     dd xNum+DE_NAME_OFFSET,  doAdd, EXIT                ; dict entry name  ( de--addr )
xWords      dd xLast
xWdsLoop        dd xDeShowVB, xTab, doDup, JmpNZ, xWdsLoop
                dd doDrop, EXIT
xSpace      dd xNum+32, EMIT, EXIT
xCR         dd xNum+13, EMIT, xNum+10, EMIT, EXIT
xTab        dd xNum+9, EMIT, EXIT
xHA         dd Lit, HERE, EXIT
xHere       dd xHA, Fetch, EXIT
xLA         dd Lit, LAST, EXIT
xLast       dd xLA, Fetch, EXIT
xDot        dd doDot, xSpace, EXIT
xDotS       dd doDotS, EXIT
xCell       dd xNum+CELL_SZ, EXIT
xCells      dd xCell, doMult, EXIT
xOK         dd Lit, okStr, xNum+3, doType, xCR, EXIT
xTIB        dd Lit, TIB, EXIT
xTIBSZ      dd xNum+TIB_SZ, EXIT
xToIn       dd Lit, ToIn, EXIT
xAccept     dd doReadL, EXIT
xFind      dd xLast                                                 ; ( str--xt fl 1 | 0 )
xFindLoop       dd doOver, doOver, xDeName
                dd doStrEqI, JmpZ, xFindNext
                dd doSwap, doDrop, doDup, xDeXT, doSwap, xDeFlags   ; FOUND!
                dd xNum+1, EXIT
xFindNext       dd xDeNext, doDup, JmpNZ, xFindLoop
                dd doDrop, doDrop, xNum, EXIT                     ; NOT Found!
xBench      dd doTimer, Lit, 500000000, doDup, xDot, doFor, doNext
            dd doTimer, doSwap, doSub, xDot, EXIT
xIf         dd Lit, JmpZ,   doComma, xHere, xNum, doComma, EXIT
xIf0        dd Lit, JmpNZ,  doComma, xHere, xNum, doComma, EXIT
xNIf        dd Lit, NJmpZ,  doComma, xHere, xNum, doComma, EXIT
xNIf0       dd Lit, NJmpNZ, doComma, xHere, xNum, doComma, EXIT
xElse       dd EXIT ; TODO
xThen       dd xHere, doSwap, doStore, EXIT
xEmit       dd EMIT, EXIT
xBye        dd doBye, EXIT

; ----------------------------------------------------------------
THE_DICT:
        dictEntry xBye,      0, 3, "BYE",    tag0000
        dictEntry doInc,     0, 2, "1+",     tag0010
        dictEntry doDec,     0, 2, "1-",     tag0011
        dictEntry Fetch,     0, 1, "@",      tag0020
        dictEntry doStore,   0, 1, "!",      tag0021
        dictEntry CFetch,    0, 2, "C@",     tag0022
        dictEntry CStore,    0, 2, "C!",     tag0023
        dictEntry doFor,     0, 3, "FOR",    tag0030
        dictEntry doI,       0, 1, "I",      tag0031
        dictEntry doNext,    0, 4, "NEXT",   tag0032
        dictEntry xTIB,      0, 3, "TIB",    tag0060
        dictEntry xToIn,     0, 3, ">IN",    tag0061
        dictEntry xTab,      0, 3, "TAB",    tag0070
        dictEntry xCR,       0, 2, "CR",     tag0080
        dictEntry xWords,    0, 5, "WORDS",  tag0090
        dictEntry xCell,     0, 4, "CELL",   tag0100
        dictEntry xCells,    0, 4, "CELLS",  tag0101
        dictEntry doItoA,    0, 3, "I>A",    tag0110
        dictEntry xHere,     0, 4, "HERE",   tag0120
        dictEntry xHA,       0, 2, "HA",     tag0130
        dictEntry xLast,     0, 4, "LAST",   tag0140
        dictEntry xLA,       0, 2, "LA",     tag0150
        dictEntry doLen,     0, 5, "S-LEN",  tag0160
        dictEntry doStrEq,   0, 4, "S-EQ",   tag0161
        dictEntry doStrEqI,  0, 5, "S-EQI",  tag0162
        dictEntry doKey,     0, 3, "KEY",    tag0170
        dictEntry doQKey,    0, 4, "QKEY",   tag0180
        dictEntry doDot,     0, 3, "(.)",    tag0190
        dictEntry xDot,      0, 1, ".",      tag0191
        dictEntry xDotS,     0, 2, ".S",     tag0192
        dictEntry xIf,       1, 2, "IF",     tag0200
        dictEntry xIf0,      1, 3, "IF0",    tag0201
        dictEntry xNIf,      1, 3, "-IF",    tag0202
        dictEntry xNIf0,     1, 4, "-IF0",   tag0203
        dictEntry xElse,     1, 4, "ELSE",   tag0204
        dictEntry xThen,     1, 4, "THEN",   tag0205
        dictEntry xEmit,     0, 4, "EMIT",   tag0210
; TODO add more built-in dictionary entries here
        dictEntry doDup,     0, 3, "DUP",    tagLast
        rb  DICT_SZ
DICT_END:

; ----------------------------------------------------------------
THE_CODE    rd CODE_SZ
CODE_END:

; ----------------------------------------------------------------
THE_VARS    rb VARS_SZ
VARS_END:

; ----------------------------------------------------------------
