; WC64 - A minimal 64-bit Forth system for Linux
; Using optimal register allocation for x86-64

format ELF64 executable 3

segment readable executable

entry main

; ******************************************************************************
; Register Allocation
; ******************************************************************************
; TOS  = rax         Top of data stack (caller-saved, efficient)
; STKP = rbp         Data stack pointer (callee-saved)
; PCIP = r15         Forth instruction pointer (callee-saved)
; RSP  = r13         Return stack pointer (callee-saved)
; Scratch: rbx, rcx, rdx, rsi, rdi, r8-r12, r14

; ******************************************************************************
; Constants
; ******************************************************************************
CELL_SZ = 8
CODE_SZ = 64*1024
DICT_SZ = 64*1024
VARS_SZ = 256*1024
TIB_SZ  = 128

; Numeric literal encoding (high bit tagging for speed)
xNum    = 0x8000000000000000
numMask = 0x7FFFFFFFFFFFFFFF

; Dictionary entry layout:
; Next/8, XT/8, Flags/1, Len/1, Name/?, NULL/1
DE_NEXT_OFFSET  = 0
DE_XT_OFFSET    = 8
DE_FLAGS_OFFSET = 16
DE_LEN_OFFSET   = 17
DE_NAME_OFFSET  = 18

; ******************************************************************************
; Macros
; ******************************************************************************
macro sPush val {
    add     rbp, CELL_SZ
    mov     [rbp], rax
    mov     rax, val
}

macro sPop reg {
    mov     reg, rax
    mov     rax, [rbp]
    sub     rbp, CELL_SZ
}

macro rPush val {
    add     r13, CELL_SZ
    mov     [r13], val
}

macro rPop reg {
    mov     reg, [r13]
    sub     r13, CELL_SZ
}

; ******************************************************************************
; Main Entry Point
; ******************************************************************************
main:
    ; Initialize
    mov     [InitialRSP], rsp
    mov     rbx, THE_CODE
    mov     [HERE], rbx
    
    ; Initialize stacks
    mov     r13, rStack         ; Return stack
    mov     rbp, dStack         ; Data stack
    xor     rax, rax            ; TOS = 0
    mov     r15, THE_ROM        ; Instruction pointer
    
    ; Jump to interpreter
    jmp     interpret

; ******************************************************************************
; Inner Interpreter (threaded code)
; ******************************************************************************
interpret:
    mov     rbx, [r15]          ; Fetch next instruction
    add     r15, CELL_SZ        ; Advance IP
    
    cmp     rbx, primEnd        ; Is it a primitive?
    jl      .primitive
    
    test    rbx, rbx            ; Is high bit set?
    js      .number             ; Yes = tagged number literal
    
    ; It's a colon definition - nest
    rPush   r15                 ; Save current IP
    mov     r15, rbx            ; Jump to definition
    jmp     interpret

.primitive:
    call    rbx                 ; Execute primitive
    jmp     interpret

.number:
    btr     rbx, 63             ; Clear high bit to get value
    sPush   rbx                 ; Push to stack
    jmp     interpret

; ******************************************************************************
; Primitives
; ******************************************************************************

; EXIT - Return from colon definition
pEXIT:
    cmp     r13, rStack         ; Check return stack underflow
    jle     .underflow
    rPop    r15                 ; Restore IP
    ret
.underflow:
    ; Return stack underflow - reset to warm start
    mov     r15, xWarm
    ret

; Stack manipulation
pDUP:
    sPush   rax
    ret

pDROP:
    mov     rax, [rbp]
    sub     rbp, CELL_SZ
    cmp     rbp, dStack
    jge     .ok
    mov     rbp, dStack
.ok:
    ret

pSWAP:
    mov     rbx, [rbp]
    mov     [rbp], rax
    mov     rax, rbx
    ret

pOVER:
    mov     rbx, [rbp]
    sPush   rbx
    ret

; Arithmetic
pPLUS:
    sPop    rbx
    add     rax, rbx
    ret

pMINUS:
    sPop    rbx
    sub     rax, rbx
    ret

pMULT:
    sPop    rbx
    imul    rax, rbx
    ret

pDIVMOD:
    sPop    rbx                 ; divisor
    cmp     rbx, 0
    je      .zero
    sPop    rcx                 ; dividend
    mov     rax, rcx
    cqo                         ; Sign extend rax into rdx:rax
    idiv    rbx
    sPush   rdx                 ; remainder
    ; rax already has quotient
    ret
.zero:
    ret

pINC:
    inc     rax
    ret

pDEC:
    dec     rax
    ret

pNEG:
    neg     rax
    ret

; Logical
p_AND:
    sPop    rbx
    and     rax, rbx
    ret

p_OR:
    sPop    rbx
    or      rax, rbx
    ret

p_XOR:
    sPop    rbx
    xor     rax, rbx
    ret

pINVERT:
    not     rax
    ret

; Comparison
pEQUAL:
    sPop    rbx
    cmp     rax, rbx
    mov     rax, 0
    sete    al
    ret

pLESS:
    sPop    rbx
    cmp     rax, rbx
    mov     rax, 0
    setl    al
    ret

pGREATER:
    sPop    rbx
    cmp     rax, rbx
    mov     rax, 0
    setg    al
    ret

; Memory access
pFETCH:
    mov     rax, [rax]
    ret

pSTORE:
    sPop    rbx                 ; address
    sPop    rcx                 ; value
    mov     [rbx], rcx
    ret

pCFETCH:
    movzx   rax, byte [rax]
    ret

pCSTORE:
    sPop    rbx                 ; address
    sPop    rcx                 ; value
    mov     [rbx], cl
    ret

; Return stack
pTOR:
    sPop    rbx
    rPush   rbx
    ret

pFROMR:
    rPop    rbx
    sPush   rbx
    ret

pRFETCH:
    mov     rbx, [r13]
    sPush   rbx
    ret

; Literals
pLIT:
    mov     rbx, [r15]
    add     r15, CELL_SZ
    sPush   rbx
    ret

; I/O
pEMIT:
    sPop    rbx
    mov     [charBuf], bl
    
    mov     rax, 1              ; sys_write
    mov     rdi, 1              ; stdout
    mov     rsi, charBuf
    mov     rdx, 1
    syscall
    ret

pTYPE:
    sPop    rdx                 ; length
    sPop    rsi                 ; address
    
    mov     rax, 1              ; sys_write
    mov     rdi, 1              ; stdout
    syscall
    ret

pKEY:
    mov     rax, 0              ; sys_read
    xor     rdi, rdi            ; stdin
    mov     rsi, charBuf
    mov     rdx, 1
    syscall
    
    movzx   rax, byte [charBuf]
    sPush   rax
    ret

; Dictionary
pHERE:
    mov     rbx, [HERE]
    sPush   rbx
    ret

pCOMMA:
    sPop    rbx
    mov     rcx, [HERE]
    mov     [rcx], rbx
    add     rcx, CELL_SZ
    mov     [HERE], rcx
    ret

pCCOMMA:
    sPop    rbx
    mov     rcx, [HERE]
    mov     [rcx], bl
    inc     rcx
    mov     [HERE], rcx
    ret

pLAST:
    mov     rbx, [LAST]
    sPush   rbx
    ret

pBASE:
    mov     rbx, [BASE]
    sPush   rbx
    ret

; Control flow
pBRANCH:
    mov     rbx, [r15]
    mov     r15, rbx
    ret

pZBRANCH:
    sPop    rbx
    test    rbx, rbx
    jz      pBRANCH
    add     r15, CELL_SZ
    ret

; Number output
pDOT:
    sPop    rcx                 ; number to print
    
    ; Convert number to string
    mov     rsi, numBuf + 31
    mov     byte [rsi], 0
    mov     rbx, [BASE]
    
    test    rcx, rcx
    jns     .positive
    
    neg     rcx
    push    1                   ; negative flag
    jmp     .convert
    
.positive:
    push    0                   ; not negative

.convert:
    dec     rsi
    mov     rax, rcx
    xor     rdx, rdx
    div     rbx
    mov     rcx, rax
    
    add     dl, '0'
    cmp     dl, '9'
    jle     .digit
    add     dl, 7
.digit:
    mov     [rsi], dl
    
    test    rcx, rcx
    jnz     .convert
    
    pop     rbx                 ; get negative flag
    test    rbx, rbx
    jz      .print
    
    dec     rsi
    mov     byte [rsi], '-'

.print:
    ; Calculate length
    mov     rdx, numBuf + 31
    sub     rdx, rsi
    
    ; Print the number
    mov     rax, 1              ; sys_write
    mov     rdi, 1              ; stdout
    syscall
    
    ; Print space
    mov     rax, 1
    mov     rdi, 1
    mov     rsi, spaceStr
    mov     rdx, 1
    syscall
    ret

pDOTS:
    ; Print stack depth and contents
    ; Save registers that syscalls clobber
    push    r11
    push    rcx
    
    mov     byte [charBuf], '('
    mov     rax, 1
    mov     rdi, 1
    mov     rsi, charBuf
    mov     rdx, 1
    syscall
    
    ; Restore TOS from hardware stack
    mov     rax, [rsp + 16]
    
    mov     rbx, dStack + CELL_SZ
.loop:
    cmp     rbx, rbp
    jg      .done
    
    push    rbx                 ; Save loop counter
    mov     rcx, [rbx]
    sPush   rcx
    call    pDOT
    pop     rbx                 ; Restore loop counter
    
    add     rbx, CELL_SZ
    jmp     .loop

.done:
    ; Print current TOS
    call    pDOT
    
    mov     byte [charBuf], ')'
    mov     rax, 1
    mov     rdi, 1
    mov     rsi, charBuf
    mov     rdx, 1
    syscall
    
    pop     rcx
    pop     r11
    ret

; System
pBYE:
    mov     rax, 60             ; sys_exit
    xor     rdi, rdi            ; exit code 0
    syscall
    ret

pCR:
    mov     rax, 1
    mov     rdi, 1
    mov     rsi, crStr
    mov     rdx, 1
    syscall
    ret

primEnd:

; ******************************************************************************
; High-level definitions (threaded code)
; ******************************************************************************

xCold:
    dq pHERE, pDOT, pLAST, pDOT, pCR
    dq xHello, pBYE
    
xWarm:
    dq xHello, pBYE

xHello:
    dq pLIT, helloStr, pLIT, helloLen, pTYPE, pCR, pEXIT

; ******************************************************************************
; Data segment
; ******************************************************************************
segment readable writable

InitialRSP  dq 0
HERE        dq THE_CODE
LAST        dq 0
BASE        dq 10
STATE       dq 0

charBuf     db 0
spaceStr    db ' '
crStr       db 10
numBuf      rb 32

helloStr    db 'WC64 - 64-bit Forth System'
helloLen    = $ - helloStr

dStack      rq 256
rStack      rq 256

THE_CODE:   rb CODE_SZ
THE_ROM = xCold
