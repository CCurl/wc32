; fwc64.asm - 64-bit Forth, built incrementally
; Stage 1: Inner interpreter only
;
; Register conventions (fixed for all stages):
;   rbp = DSP  (data stack pointer - cell index into dstk[])
;   r13 = RSP  (return stack pointer - cell index into rstk[])
;   r15 = PC   (program counter - cell index into code[])
;   rax/rbx/rcx/rdx = scratch
;
; Instruction encoding:
;   0..LAST_PRIM   -> primitive opcode, dispatched via primTable
;   has LIT_MASK   -> NaN-boxed literal: push (value & LIT_BITS)
;   anything else  -> colon-definition call: push PC, jump to value
;
; NaN-boxing:
;   LIT_MASK = 0x7FF8000000000000
;   LIT_BITS = 0x0007FFFFFFFFFFFF
;   encode:  n | LIT_MASK   (safe for 0 <= n < 2^51)

format ELF64 executable 3
entry main
use64

; ---------------------- constants ----------------------
SYS_WRITE  = 1
SYS_EXIT   = 60

LIT_MASK   = 0x7FF8000000000000
LIT_BITS   = 0x0007FFFFFFFFFFFF

pEXIT  = 0
pLIT   = 1      ; push next cell (for large / negative numbers)
pDUP   = 2
pDROP  = 3
pSWAP  = 4
pOVER  = 5
pADD   = 6
pSUB   = 7
pMUL   = 8
pEMIT  = 9
LAST_PRIM = 9

STK_SZ = 256

; -------------------------------------------------------
segment readable executable

; --- test program (hand-assembled) ---------------------
; Acts as code[] for stage 1.
; Tests:  65 emit  ->  'A'
;         42 dup + emit  ->  84 = 'T'
;         10 emit  ->  newline
code:
    dq 65  or LIT_MASK  ; push 65
    dq pEMIT            ; emit -> 'A'
    dq 42  or LIT_MASK  ; push 42
    dq pDUP             ; dup
    dq pADD             ; + -> 84
    dq pEMIT            ; emit -> 'T'
    dq 10  or LIT_MASK  ; push 10
    dq pEMIT            ; emit -> newline
    dq pEXIT

; --- entry point ---------------------------------------
main:
    xor     rbp, rbp        ; DSP = 0
    xor     r13, r13        ; RSP = 0
    mov     r15, 0          ; PC  = 0
    call    inner
    mov     rax, SYS_EXIT
    xor     rdi, rdi
    syscall

; --- inner interpreter ---------------------------------
inner:
.next:
    mov     rax, [code + r15*8]
    inc     r15

    test    rax, rax
    js      .check_nan
    cmp     rax, LAST_PRIM
    jle     .prim

.check_nan:
    mov     rbx, rax
    mov     rcx, LIT_MASK
    and     rbx, rcx
    cmp     rbx, rcx
    je      .literal

    ; colon-definition call
    mov     rbx, [code + r15*8]     ; peek: tail-call if next is EXIT
    cmp     rbx, pEXIT
    je      .tail
    mov     [rstk + r13*8], r15
    inc     r13
.tail:
    mov     r15, rax
    jmp     .next

.literal:
    mov     rcx, LIT_BITS
    and     rax, rcx
    mov     [dstk + rbp*8], rax
    inc     rbp
    jmp     .next

.prim:
    jmp     qword [primTable + rax*8]

; --- primitives ----------------------------------------

prim_EXIT:
    test    r13, r13
    jz      .ret
    dec     r13
    mov     r15, [rstk + r13*8]
    jmp     inner.next
.ret: ret

prim_LIT:
    mov     rax, [code + r15*8]
    inc     r15
    mov     [dstk + rbp*8], rax
    inc     rbp
    jmp     inner.next

prim_DUP:
    mov     rax, [dstk + rbp*8 - 8]
    mov     [dstk + rbp*8], rax
    inc     rbp
    jmp     inner.next

prim_DROP:
    dec     rbp
    jmp     inner.next

prim_SWAP:
    mov     rax, [dstk + rbp*8 - 8]
    mov     rbx, [dstk + rbp*8 - 16]
    mov     [dstk + rbp*8 - 8],  rbx
    mov     [dstk + rbp*8 - 16], rax
    jmp     inner.next

prim_OVER:
    mov     rax, [dstk + rbp*8 - 16]
    mov     [dstk + rbp*8], rax
    inc     rbp
    jmp     inner.next

prim_ADD:
    dec     rbp
    mov     rax, [dstk + rbp*8]
    add     [dstk + rbp*8 - 8], rax
    jmp     inner.next

prim_SUB:
    dec     rbp
    mov     rax, [dstk + rbp*8]
    sub     [dstk + rbp*8 - 8], rax
    jmp     inner.next

prim_MUL:
    dec     rbp
    mov     rax, [dstk + rbp*8]
    imul    rax, [dstk + rbp*8 - 8]
    mov     [dstk + rbp*8 - 8], rax
    jmp     inner.next

prim_EMIT:
    dec     rbp
    mov     rax, [dstk + rbp*8]
    mov     [charBuf], al
    mov     rax, SYS_WRITE
    mov     rdi, 1
    lea     rsi, [charBuf]
    mov     rdx, 1
    syscall
    jmp     inner.next

primTable:
    dq prim_EXIT, prim_LIT, prim_DUP, prim_DROP, prim_SWAP
    dq prim_OVER, prim_ADD, prim_SUB, prim_MUL,  prim_EMIT

; --- writable data -------------------------------------
segment readable writable

charBuf  db 0
dstk     rq STK_SZ + 1
rstk     rq STK_SZ + 1
