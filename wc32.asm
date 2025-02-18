; WC32 - a Tachyon Forth inspired 32-bit system

; FOR_OS equ WINDOWS
; FOR_OS equ LINUX

; A dictionary entry looks like this:
; Next/4, XT/4, Flags/1, Len/1, Name/?, NULL/1

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
CODE_SZ = 64*1024
DICT_SZ = 64*1024
VARS_SZ = 64*1024
TIB_SZ  = 128
xNum    = $70000000
xMask   = $0FFFFFFF
LastTag equ 0

; ******************************************************************************
; MACROS
macro m_getTOS val { mov val, TOS }
macro m_setTOS val { mov TOS, val }
macro m_get2ND val { mov val, [STKP] }
macro m_set2ND val { mov [STKP], val }

macro m_push val {
       add STKP, CELL_SZ
       mov [STKP], TOS
       m_setTOS val
}

; ******************************************************************************
macro m_pop val {
       m_getTOS val
       mov TOS, [STKP]
       sub STKP, CELL_SZ
}

; ******************************************************************************
macro addDict XT, Flags, Len, Name, Tag {
    align CELL_SZ
    d_#Tag: dd LastTag
            dd XT
            db Flags, Len, Name, 0
    LastTag equ d_#Tag
}

; ******************************************************************************
; ******************************************************************************
; ******************************************************************************
entry $
match =WINDOWS, FOR_OS {
        invoke GetStdHandle, STD_INPUT_HANDLE
        mov    [hStdIn], eax
        
        invoke GetStdHandle, STD_OUTPUT_HANDLE
        mov    [hStdOut], eax
}
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
        call    doBye
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
.NUM:   and     eax, xMask
        m_push  eax
        jmp     wcRun

; ******************************************************************************
doExit: mov     eax, [rStackPtr]
        cmp     eax, rStack
        jle     .U
        mov     esi, [eax]
        sub     [rStackPtr], CELL_SZ
        ret
.U:     m_push '-'
        call    doEmit
        m_push 'U'
        call    doEmit
        m_push '-'
        call    doEmit
        ; mov     esp, [InitialESP]
        mov     esi, xWarm
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
doJmp:  m_pop  ebx
        lodsd
doJ:    mov    esi, eax
        ret

; ******************************************************************************
doJmpZ: m_pop  ebx
        lodsd
        test   ebx, ebx
        jz     doJ
        ret

; ******************************************************************************
doJmpNZ: m_pop  ebx
        lodsd
        test    ebx, ebx
        jnz     doJ
        ret

; ******************************************************************************
doFetch: m_getTOS   edx
        m_setTOS    [edx]
        ret

; ******************************************************************************
doStore: m_pop  edx
        m_pop   eax
        mov     [edx], eax
        ret

; ******************************************************************************
doCFetch: xor     eax, eax
        mov     al, BYTE [TOS]
        mov     TOS, eax
        ret

; ******************************************************************************
doCStore: m_pop edx
        m_pop   eax
        mov     BYTE [edx], al
        ret

; ******************************************************************************
; Number input
num:    sub     al, '0'
        and     eax, $FF
        mov     edx, eax
.l:     mov     al, [esi]
        mov     bx, '09'
        call    betw
        cmp     bl, 0
        je      .x
        sub     al, '0'
        imul    edx, edx, 10
        add     edx, eax
        inc     esi
        jmp     .l
.x:     m_push  edx
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
match =WINDOWS, FOR_OS {
        doBye:  invoke  ExitProcess, 0
                ret
        ; **********************************************************************
        doTimer: invoke GetTickCount
                 m_push  eax
                 ret
        ; **********************************************************************
        doEmit: m_pop   eax
                mov     [buf1], al
                invoke  WriteConsole, [hStdOut], buf1, 1, NULL, NULL
                ret
        ; **********************************************************************
        doType: m_pop   eax              ; Len ( addr len-- )
                m_pop   ebx              ; Addr
                invoke  WriteConsole, [hStdOut], ebx, eax, NULL, NULL
                ret
        ; **********************************************************************
        doReadL: m_pop  edx              ; buffer size
                 m_pop  ecx              ; buffer
                 invoke ReadConsole, [hStdIn], ecx, edx, bytesRead, 0
                 mov    eax, [bytesRead]
                 m_push eax
                 ret
        ; **********************************************************************
        doQKey: invoke _kbhit
                m_push  eax
                ret
        ; **********************************************************************
        doKey:  invoke  _getch
                m_push  eax
                ret
}

; ******************************************************************************
match =LINUX, FOR_OS {
        doBye:  ; invoke  LinuxExit, 0
                ret
        ; **********************************************************************
        doTimer: ; invoke LinuxTimer
                m_push  eax
                ret
        ; **********************************************************************
        doEmit: m_pop   eax             ; ( ch-- )
                mov     [buf1], al      ; put char in message
                mov     eax,4           ; system call number (sys_write)
                mov     ebx,1           ; file descriptor (stdout)
                mov     ecx,buf1        ; message to write
                mov     edx,1           ; message length
                int     0x80            ; call kernel
                ret
        ; **********************************************************************
        doType: m_pop   edx             ; Len ( string len-- )
                m_pop   ecx             ; String
                mov     eax,4           ; system call number (sys_write)
                mov     ebx,1           ; file descriptor (stdout)
                int     0x80
                ret
        ; **********************************************************************
        doReadL: m_pop edx              ; buffer size ( buf sz--num )
                 m_pop  ecx             ; buffer
                 mov    ebx, 0          ; stdin
                 mov    eax, 3          ; sys_read
                 int    0x80
                 m_push eax
                ret
        ; **********************************************************************
        doQKey: ; invoke LinuxKey
                m_push  DWORD 0
                ret
        doKey: ; invoke LinuxQKey
                m_push  DWORD 0
                ret
}

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
doDup:  m_push  TOS
        ret

; ******************************************************************************
doSwap: m_get2ND    eax
        m_set2ND    TOS
        m_setTOS    eax
        ret

; ******************************************************************************
doOver: m_get2ND    eax
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
doLit:  lodsd
        m_push eax
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
okStr       db  " ok", 0
i2aBuf      db  64 dup ?
bytesRead   dd  ?
unkOP       db  "-unk-"
rStackPtr   dd  rStack
lStackPtr   dd  lStack
HERE        dd  THE_CODE
VHERE       dd  THE_VARS
LAST        dd  d_tg999999
HERE1       dd  ?
TIB         dd  TIB_SZ dup 0       ; TIB

buf1        db   16 dup 0       ; Buffer
dStack      dd   64 dup 0       ; Data stack
buf2        dd    4 dup 0       ; Buffer
rStack      dd   64 dup 0       ; Return stack
buf3        dd    4 dup 0       ; Buffer
lStack      dd   64 dup 0       ; Loop stack
buf4        dd    4 dup 0       ; Buffer

; ----------------------------------------------------------------
THE_ROM:
xCold       dd xHA, xDot, xHere, xDot, xLast, xDot, xCell, xDot
                dd xCR, xWords
                dd xCR, xCR, xNum+10, doFor, doI, doInc, xDot, doNext
xWarm       dd xInterp, xBench, doJmp, xWarm
xInterp     dd xOK, xTIB, xTIBSZ, xAccept, doDec, doDec, xTIB, doAdd, xNum, doSwap, doCStore
                dd xTIB, doDup, doLen, doType, xSpace
                ; Set >IN to TIB
                ; LOOP: get the next word. If none left, exit
                ; search in dict for the word
                ; if found, compile it (or execute if immediate) and jmp to LOOP
                ; else if number, push it and jmp to LOOP
                ; else error/reset and exit
                dd doExit
xDeShow     dd doDup, xDeName                   ; First char of name    ( a1--a2 )
                dd doDup, doLen, doType         ; Name length
                dd xDeNext, doExit              ; Next entry
xDeShowVB   dd xCR, doDup, xDeNext, xDot        ; Next    ( a1--a2 )
                dd doDup, xDeXT, xDot           ; XT
                dd doDup, xDeFlags, xDot        ; Flags
                dd doDup, xDeName               ; First char of name
                dd doDup, doLen, doType         ; Name length
                dd xDeNext, doExit              ; Next entry
xDeNext     dd doFetch, doExit                                     ; dict entry Next  ( de--next )
xDeXT       dd xCell, doAdd, doFetch, doExit                       ; dict entry XT    ( de--xt )
xDeFlags    dd x2Cells, doAdd, doCFetch, doExit                    ; dict entry flags ( de--flags )
xDeLen      dd x2Cells, doAdd, xNum+1, doAdd, doCFetch, doExit     ; dict entry len   ( de--len )
xDeName     dd x2Cells, doAdd, xNum+2, doAdd, doExit               ; dict entry name  ( de--addr )
xSpace      dd xNum+32, doEmit, doExit
xCR         dd xNum+13, doEmit, xNum+10, doEmit, doExit
xTab        dd xNum+9, doEmit, doExit
xHA         dd doLit, HERE, doExit
xHere       dd xHA, doFetch, doExit
xLA         dd doLit, LAST, doExit
xLast       dd xLA, doFetch, doExit
xDot        dd doDot, xSpace, doExit
xCell       dd xNum+CELL_SZ, doExit
xCells      dd xCell, doMult, doExit
x2Cells     dd xNum+2, xCells, doExit
xOK         dd doLit, okStr, xNum+3, doType, xCR, doExit
xTIB        dd doLit, TIB, doExit
xTIBSZ      dd xNum+TIB_SZ, doExit
xAccept     dd doReadL, doExit
xWords      dd xLast
xWdsLoop        dd xDeShowVB, xTab, doDup, doJmpNZ, xWdsLoop
                dd doDrop, doExit
xBench      dd doTimer, doLit, 500000000, doDup, xDot, doFor, doNext
            dd doTimer, doSwap, doSub, xDot, doExit

; ----------------------------------------------------------------
THE_DICT:
        addDict doBye,    0, 3, "BYE",   tg000000
        addDict doInc,    0, 2, "1+",    tg200000
        addDict doDec,    0, 2, "1-",    tg200100
        addDict doFetch,  0, 1, "@",     tg200200
        addDict doFor,    0, 3, "FOR",   tg200300
        addDict doI,      0, 1, "I",     tg200400
        addDict doNext,   0, 4, "NEXT",  tg200500
        addDict xTIB,     0, 3, "TIB",   tg200600
        addDict xTab,     0, 3, "TAB",   tg200700
        addDict xCR,      0, 2, "CR",    tg200800
        addDict xWords,   0, 5, "WORDS", tg200900
        addDict xCell,    0, 4, "CELL",  tg201000
        addDict xCells,   0, 4, "CELLS", tg201001
        addDict doItoA,   0, 3, "I>A",   tg201100
        addDict xHere,    0, 4, "HERE",  tg201200
        addDict xHA,      0, 2, "HA",    tg201300
        addDict xLast,    0, 4, "LAST",  tg201400
        addDict xLA,      0, 2, "LA",    tg201500
        addDict doLen,    0, 5, "S-LEN", tg201600
        addDict doKey,    0, 3, "KEY",   tg201700
        addDict doQKey,   0, 4, "QKEY",  tg201800
        addDict doDup,    0, 3, "DUP",   tg999999
; TODO add more built-in dictionary entries here
        rb  DICT_SZ
DICT_END:

; ----------------------------------------------------------------
THE_CODE    rd CODE_SZ
CODE_END:

; ----------------------------------------------------------------
THE_VARS    rb VARS_SZ
VARS_END:
