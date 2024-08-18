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

macro sPush val {
       add STKP, CELL_SZ
       mov [STKP], TOS
       setTOS val
}

; ******************************************************************************
macro sPop val {
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
        sPush    eax
        jmp     wcRun

; ******************************************************************************
; ******************************************************************************
EXIT:   call    checkRStack
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
.Under: sPush    '-'
        call    EMIT
        sPush    'U'
        call    EMIT
        sPush    '-'
        call    EMIT
        mov     [rStackPtr], rStack
        xor     edx, edx
        ret;

; ******************************************************************************
rStackTo: ; ( N-- )
        sPop     eax
        add     [rStackPtr], CELL_SZ
        mov     edx, [rStackPtr]
        mov     [edx], eax
        ret

; ******************************************************************************
rStackFrom: ; ( --N )
        call    checkRStack
        mov     eax, [edx]
        sPush    eax
        sub     [rStackPtr], CELL_SZ
        ret

; ******************************************************************************
rStackFetch: ; ( --N )
        call    checkRStack
        mov     eax, [edx]
        sPush   eax
        ret

; ******************************************************************************
rStackStore: ; ( N-- )
        call    checkRStack
        sPop    eax
        mov     [edx], eax
        ret

; ******************************************************************************
doExecute: ; ( xt-- )
        cmp     TOS, primEnd
        jg      rStackTo
        sPop    eax
        jmp     eax

; ******************************************************************************
doNop:  ret

; ******************************************************************************
; input:        ; eax: the number to print - destroyed
; output:       ; ecx: the start of the string
;               ; ebx: the length of the string
;
iToA:   mov     ecx, buf3+63  ; output string start
        mov     ebx, 0          ; output length
        mov     BYTE [ecx], 0
        push    0               ; isNegative flag
        bt      eax, 31
        jnc     .L
        inc     BYTE [esp]
        neg     eax
.L:     push    ebx
        mov     ebx, [BASE]
        mov     edx, 0
        div     ebx
        pop     ebx
        add     dl, '0'
        cmp     dl, '9'
        jle     .C
        add     dl, 7
.C:     dec     ecx 
        mov     BYTE [ecx], dl
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
doItoA: sPop    eax
        call    iToA
        sPush   ecx
        sPush   ebx
        ret

; ******************************************************************************
JmpA:   lodsd
doJmp:  mov    esi, eax
        ret

; ******************************************************************************
JmpZ:   sPop   ebx
        lodsd
        test   ebx, ebx
        jz     doJmp
        ret

; ******************************************************************************
JmpNZ:  sPop   ebx
        lodsd
        test   ebx, ebx
        jnz    doJmp
        ret

; ******************************************************************************
NJmpZ:  lodsd
        test   TOS, TOS
        jz     doJmp
        ret

; ******************************************************************************
NJmpNZ: lodsd
        test   TOS, TOS
        jnz    doJmp
        ret

; ******************************************************************************
Fetch:  getTOS   edx
        setTOS   [edx]
        ret

; ******************************************************************************
doStore: sPop   edx
         sPop   eax
         mov    [edx], eax
         ret

; ******************************************************************************
CFetch: xor     eax, eax
        mov     al, BYTE [TOS]
        mov     TOS, eax
        ret

; ******************************************************************************
CStore: sPop    edx
        sPop    eax
        mov     BYTE [edx], al
        ret

; ******************************************************************************
; A dictionary entry looks like this:
; Next/4, XT/4, Flags/1, Len/1, Name/?, NULL/1
addWord: ; ( str-- )
        call    _DUP                    ; ( str str )
        call    doLen                   ; ( str len )
        getTOS  eax
        add     eax, (2*CELL_SZ)+3
        mov     edx, [LAST]
        sub     edx, eax
        push    edx                     ; The new value for LAST
        mov     eax, [LAST]
        mov     [edx], eax              ; NEXT
        add     edx, CELL_SZ
        mov     eax, [HERE]             ; XT
        mov     [edx], eax
        add     edx, CELL_SZ
        xor     eax, eax
        mov     [edx], al               ; FLAGS
        inc     edx
        sPop    eax                     ; LEN
        mov     [edx], al
        inc     edx
        sPush   edx                     ; ( str dst )
        call    SWAP                    ; ( dst str )
        pop     edx                     ; Set LAST to the new value
        mov     [LAST], edx
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
        sPush   1
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
        sPop    eax
        mov     [edx-CELL_SZ], eax
        mov     [edx-(CELL_SZ*2)], esi
        ret

; ******************************************************************************
doI:    mov     edx, [lStackPtr]
        mov     eax, [edx]
        sPush   eax
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
CELL:   sPush   CELL_SZ
        ret

; ******************************************************************************
CELL1:  add     TOS, CELL_SZ
        ret

; ******************************************************************************
CELLS:  sPush    CELL_SZ
MULT:   sPop     eax
        imul    TOS, eax
        ret

; ******************************************************************************
MINUS:  sPop     eax
        sub     TOS, eax
        ret

; ******************************************************************************
PLUS:   sPop     eax
        add     TOS, eax
        ret

; ******************************************************************************
doSLMod:
        sPop    ebx
        cmp    ebx, 0
        je     .X
        sPop    eax
        mov    edx, 0
        idiv   ebx
        sPush   edx
        sPush   eax
.X:     ret

; ******************************************************************************
doDiv:  sPop    ebx
        cmp     ebx, 0
        je      .X
        sPop    eax
        mov     edx, 0
        idiv    ebx
        sPush   eax
.X:     ret

; ******************************************************************************
doMod:  call    doSLMod
        jmp     DROP

; ******************************************************************************
doInc4: inc     TOS
doInc3: inc     TOS
doInc2: inc     TOS
doInc:  inc     TOS
        ret

; ******************************************************************************
doDec:  dec     TOS
        ret

; ******************************************************************************
doAnd:  sPop    eax
        and     TOS, eax
        ret

; ******************************************************************************
doOr:   sPop    eax
        or      TOS, eax
        ret

; ******************************************************************************
doXOR:  sPop    eax
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
doEQ:   sPop    eax
        cmp     TOS, eax
        je      doTrue
        jmp     doFalse

; ******************************************************************************
doLT:   sPop    eax
        cmp     TOS, eax
        jl      doTrue
        jmp     doFalse

; ******************************************************************************
doGT:   sPop    eax
        cmp     TOS, eax
        jg      doTrue
        jmp     doFalse

; ******************************************************************************
doDot:  push    eax
        push    ebx
        push    ecx
        push    edx
        sPop    eax
        call    iToA
        sPush   ecx
        sPush   ebx
        call    TYPE
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        ret

; ******************************************************************************
doDotS: sPush   '('
        call    printChar
        mov     eax, dStack+CELL_SZ
.L:     cmp     eax, STKP
        jg      .X
        sPush   [eax]
        call    doDot
        sPush   32
        call    printChar
        add     eax, CELL_SZ
        jmp     .L
.X:     call    _DUP
        call    doDot
        sPush   ')'
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
_DUP:  getTOS  eax
        sPush   eax
        ret

; ******************************************************************************
SWAP:   get2ND    eax
        set2ND    TOS
        setTOS    eax
        ret

; ******************************************************************************
doOver: get2ND      eax
        sPush       eax
        ret

; ******************************************************************************
DROP: mov     TOS, [STKP]
        sub     STKP, CELL_SZ
        cmp     STKP, dStack
        jg      .X
        mov     STKP, dStack
.X:     ret

; ******************************************************************************
doLen:  sPop    edx             ; ( addr--len )
        xor     ecx, ecx
.L:     cmp     [edx], BYTE 0
        je      .X
        inc     ecx
        inc     edx
        jmp     .L
.X:     sPush   ecx
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
        sPush   ebx
        sPush   0
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
doStrCpy: ; ( dst src-- )
        sPop    ebx
        sPop    edx
.L:     mov     al, [ebx]
        mov     [edx], al
        test    al, al
        inc     edx
        inc     ebx
        jnz     .L
        ret

; ******************************************************************************
doStrEq:       ; ( str1 str2--fl )
        sPop    ecx
        sPop    edx
        sPush   0               ; Default to NOT equal
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
        sPop    ecx
        sPop    edx
        sPush   0               ; Default to NOT equal
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
        sPush eax
        ret

; ******************************************************************************
COMMA: ; ( n-- )
        sPop    eax
        mov     edx, [HERE]
        mov     [edx], eax
        add     edx, CELL_SZ
        mov     [HERE], edx
        ret

; ******************************************************************************
CCOMMA: ; ( n-- )
        sPop    eax
        mov     edx, [HERE]
        mov     [edx], al
        inc     edx
        mov     [HERE], edx
        ret

; ******************************************************************************
AVAL:   sPush   [AVAR]
        ret

; ******************************************************************************
ASET:   sPop    [AVAR]
        ret

; ******************************************************************************
AFET:   mov     eax, [AVAR]
        sPush   [eax]
        ret

; ******************************************************************************
AFET1:  mov     eax, [AVAR]
        sPush   [eax]
        add     DWORD [AVAR], CELL_SZ
        ret

; ******************************************************************************
ASTO:   sPop    ebx
        mov     eax, [AVAR]
        mov     [eax], ebx
        ret

; ******************************************************************************
ASTO1:  sPop    ebx
        mov     eax, [AVAR]
        mov     [eax], ebx
        add     DWORD [AVAR], CELL_SZ
        ret

; ******************************************************************************
BVAL:   sPush   [BVAR]
        ret

; ******************************************************************************
BSET:   sPop    [BVAR]
        ret

; ******************************************************************************
BFET:   mov     eax, [BVAR]
        sPush   [eax]
        ret

; ******************************************************************************
BFET1:  mov     eax, [BVAR]
        sPush   [eax]
        add     DWORD [BVAR], CELL_SZ
        ret

; ******************************************************************************
BSTO:   sPop    ebx
        mov     eax, [BVAR]
        mov     [eax], ebx
        ret

; ******************************************************************************
BSTO1:  sPop    ebx
        mov     eax, [BVAR]
        mov     [eax], ebx
        add     DWORD [BVAR], CELL_SZ
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
bytesRead   dd  ?
unkOP       db  "-unk-"
rStackPtr   dd  rStack
lStackPtr   dd  lStack
HERE        dd  THE_CODE
VHERE       dd  THE_VARS
LAST        dd  tagLast
BASE        dd  10
HERE1       dd  ?
AVAR        dd  0
BVAR        dd  0
TIB         dd  TIB_SZ dup 0
ToIn        dd  ?

buf1        db    4 dup 0       ; Buffer (used by EMIT)
dStack      dd   64 dup 0       ; Data stack
buf2        db   32 dup 0       ; Buffer (used by nextWd)
rStack      dd   64 dup 0       ; Return stack
buf3        db   64 dup 0       ; Buffer (used by iToA)
lStack      dd   64 dup 0       ; Loop stack
buf4        db   32 dup 0       ; Buffer

; ----------------------------------------------------------------
THE_ROM:
xCold       dd xHA, xDot, xHere, xDot, xLast, xDot, CELL, xDot, doDotS
                dd xCR, xWords, doDotS, xBench
                dd xCR, xCR, xNum+10, doFor, doI, doInc, xDot, doNext, doDotS
                dd xCR, xNum+11, xNum+22, xNum+33, doDotS, DROP, DROP, DROP
xWarm       dd xInterp, JmpA, xWarm
xInterp     dd xOK, xTIB, xTIBSZ, xAccept, DROP ; , xTIB, PLUS, xNum, SWAP, CStore
                ; dd xTIB, _DUP, doLen, TYPE, xSpace
                dd xTIB, xToIn, doStore; , doDotS, xHere, xDot
xIntLoop        dd nextWd, JmpNZ, xIntNumQ, DROP, EXIT                ; Get next word, exit if no more words
xIntNumQ:       ; dd Lit, buf2, _DUP, doLen, TYPE, xSpace              ; *** temp ***
                dd doNumQ, JmpZ, xIntDictQ                              ; Is it a number?
                ; dd _DUP, xNum+'n', EMIT, doDot, xNum+'n', EMIT       ; *** temp - yes, it is a number! ***
                dd xCompNum, JmpA, xIntLoop                             ; Yes, it is a number!
xIntDictQ       dd Lit, buf2, xFind, JmpZ, xIntERR                      ; Is it in the dictionary?
                dd JmpNZ, xIntImmed                                     ; YES! Is it immediate?
                ; dd xNum+'C', EMIT, xNum+'-', EMIT, _DUP, xDot        ; *** temp ***
                dd xExecute, JmpA, xIntLoop
                dd COMMA, JmpA, xIntLoop
xIntImmed       dd xNum+'I', EMIT, xNum+'-', EMIT, doDot
                dd JmpA, xIntLoop
xIntERR         dd xNum+'?', xNum+'?', EMIT, EMIT, EXIT
xExecute    dd doExecute, EXIT
xCompNum    dd EXIT
                ; dd _DUP, Lit, 0x80000000, doAnd, JmpNZ, xCompLit
                ; dd Lit, xNum, doOr, COMMA, EXIT
xCompLit        dd Lit, Lit, COMMA, COMMA, EXIT
xDeShow     dd _DUP, xDeName                   ; First char of name    ( a1--a2 )
                dd _DUP, doLen, TYPE           ; Name length
                dd xDeNext, EXIT                ; Next entry
xDeShowVB   dd xCR, _DUP, xDeNext, xDot        ; Next    ( a1--a2 )
                dd _DUP, xDeXT,    xDot        ; XT
                dd _DUP, xDeFlags, xDot        ; Flags
                dd _DUP, xDeName               ; First char of name
                dd _DUP, doLen, TYPE           ; Name length
                dd xDeNext, EXIT                ; Next entry
xDeNext     dd xNum+DE_NEXT_OFFSET,  PLUS, Fetch,  EXIT      ; dict entry Next  ( de--next )
xDeXT       dd xNum+DE_XT_OFFSET,    PLUS, Fetch,  EXIT      ; dict entry XT    ( de--xt )
xDeFlags    dd xNum+DE_FLAGS_OFFSET, PLUS, CFetch, EXIT      ; dict entry flags ( de--flags )
xDeLen      dd xNum+DE_LEN_OFFSET,   PLUS, CFetch, EXIT      ; dict entry len   ( de--len )
xDeName     dd xNum+DE_NAME_OFFSET,  PLUS, EXIT              ; dict entry name  ( de--addr )
xWords      dd xLast
xWdsLoop        dd xDeShow, xTab, _DUP, JmpNZ, xWdsLoop
                dd DROP, EXIT
xSpace      dd xNum+32, EMIT, EXIT
xCR         dd xNum+13, EMIT, xNum+10, EMIT, EXIT
xTab        dd xNum+9, EMIT, EXIT
xHA         dd Lit, HERE, EXIT
xHere       dd xHA, Fetch, EXIT
xLA         dd Lit, LAST, EXIT
xLast       dd xLA, Fetch, EXIT
xBASE       dd Lit, BASE, EXIT
xDot        dd doDot, xSpace, EXIT
xOK         dd Lit, okStr, xNum+3, TYPE, xCR, EXIT
xTIB        dd Lit, TIB, EXIT
xTIBSZ      dd xNum+TIB_SZ, EXIT
xToIn       dd Lit, ToIn, EXIT
xAccept     dd doReadL, EXIT
xFind      dd xLast                                                 ; ( str--xt fl 1 | 0 )
xFindLoop       dd doOver, doOver, xDeName
                dd doStrEqI, JmpZ, xFindNext
                dd SWAP, DROP, _DUP, xDeXT, SWAP, xDeFlags   ; FOUND!
                dd xNum+1, EXIT
xFindNext       dd xDeNext, _DUP, JmpNZ, xFindLoop
                dd DROP, DROP, xNum, EXIT                     ; NOT Found!
xBench      dd doTimer, Lit, 500000000, _DUP, xDot, doFor, doNext
            dd doTimer, SWAP, MINUS, xDot, EXIT
xIf         dd Lit, JmpZ,   COMMA, xHere, xNum, COMMA, EXIT
xIf0        dd Lit, JmpNZ,  COMMA, xHere, xNum, COMMA, EXIT
xNIf        dd Lit, NJmpZ,  COMMA, xHere, xNum, COMMA, EXIT
xNIf0       dd Lit, NJmpNZ, COMMA, xHere, xNum, COMMA, EXIT
xElse       dd EXIT ; TODO
xThen       dd xHere, SWAP, doStore, EXIT

; ----------------------------------------------------------------
THE_DICT:
        dictEntry doBye,     0, 3, "BYE",    tag0000
        dictEntry doInc,     0, 2, "1+",     tag0011
        dictEntry doInc2,    0, 2, "2+",     tag0012
        dictEntry doInc3,    0, 2, "3+",     tag0013
        dictEntry doInc4,    0, 2, "4+",     tag0014
        dictEntry doDec,     0, 2, "1-",     tag0015
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
        dictEntry CELL,      0, 4, "CELL",   tag0100
        dictEntry CELL1,     0, 5, "CELL+",  tag0101
        dictEntry CELLS,     0, 4, "CELLS",  tag0102
        dictEntry doItoA,    0, 3, "I>A",    tag0110
        dictEntry xHere,     0, 4, "HERE",   tag0120
        dictEntry xHA,       0, 2, "HA",     tag0121
        dictEntry xLast,     0, 4, "LAST",   tag0122
        dictEntry xLA,       0, 2, "LA",     tag0123
        dictEntry xBASE,     0, 4, "BASE",   tag0124
        dictEntry doLen,     0, 5, "S-LEN",  tag0160
        dictEntry doStrEq,   0, 4, "S-EQ",   tag0161
        dictEntry doStrEqI,  0, 5, "S-EQI",  tag0162
        dictEntry doStrCpy,  0, 5, "S-CPY",  tag0163
        dictEntry doKey,     0, 3, "KEY",    tag0170
        dictEntry doQKey,    0, 4, "QKEY",   tag0180
        dictEntry doDot,     0, 3, "(.)",    tag0190
        dictEntry xDot,      0, 1, ".",      tag0191
        dictEntry doDotS,    0, 2, ".S",     tag0192
        dictEntry xIf,       1, 2, "IF",     tag0200
        dictEntry xIf0,      1, 3, "IF0",    tag0201
        dictEntry xNIf,      1, 3, "-IF",    tag0202
        dictEntry xNIf0,     1, 4, "-IF0",   tag0203
        dictEntry xElse,     1, 4, "ELSE",   tag0204
        dictEntry xThen,     1, 4, "THEN",   tag0205
        dictEntry EMIT,      0, 4, "EMIT",   tag0210
        dictEntry TYPE,      0, 4, "TYPE",   tag0211
        dictEntry EXIT,      0, 4, "EXIT",   tag0220
        dictEntry PLUS,      0, 1, "+",      tag0231
        dictEntry MINUS,     0, 1, "-",      tag0232
        dictEntry MULT,      0, 1, "*",      tag0234
        dictEntry doDiv,     0, 1, "/",      tag0235
        dictEntry doSLMod,   0, 4, "/MOD",   tag0236
        dictEntry doMod,     0, 3, "MOD",    tag0237
        dictEntry SWAP,      0, 4, "SWAP",   tag0240
        dictEntry AVAL,      0, 2, "@A",     tag0250
        dictEntry ASET,      0, 2, "!A",     tag0251
        dictEntry AFET,      0, 2, "A@",     tag0252
        dictEntry AFET1,     0, 3, "A@+",    tag0253
        dictEntry ASTO,      0, 2, "A!",     tag0254
        dictEntry ASTO1,     0, 3, "A!+",    tag0255
        dictEntry BVAL,      0, 2, "@B",     tag0260
        dictEntry BSET,      0, 2, "!B",     tag0261
        dictEntry BFET,      0, 2, "B@",     tag0262
        dictEntry BFET1,     0, 3, "B@+",    tag0263
        dictEntry BSTO,      0, 2, "B!",     tag0264
        dictEntry BSTO1,     0, 3, "B!+",    tag0265
; TODO add more built-in dictionary entries here
        dictEntry _DUP,     0, 3, "DUP",    tagLast
        rb  DICT_SZ
DICT_END:

; ----------------------------------------------------------------
THE_CODE    rd CODE_SZ
CODE_END:

; ----------------------------------------------------------------
THE_VARS    rb VARS_SZ
VARS_END:

; ----------------------------------------------------------------
