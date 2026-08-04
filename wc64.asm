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
; TSP  = r14         Temp/locals stack pointer (callee-saved)
; Scratch: rbx, rcx, rdx, rsi, rdi, r8-r12

; ******************************************************************************
; Constants
; ******************************************************************************
CELL_SZ = 8
CODE_SZ = 15*1024*1024
DICT_SZ =  1*1024*1024
TIB_SZ  = 128

; Numeric literal encoding (high bit tagging for speed)
xNum    = 0x8000000000000000
numMask = 0x7FFFFFFFFFFFFFFF

; Dictionary entry layout (fixed 32 bytes, grows downward):
; XT/8, Flags/1, Len/1, Name/22 (null-terminated, max 21 chars)
DE_SIZE         = 32
DE_XT_OFFSET    = 0
DE_FLAGS_OFFSET = 8
DE_LEN_OFFSET   = 9
DE_NAME_OFFSET  = 10
DE_MAX_NAME     = 21

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

; ******************************************************************************
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
    mov     r14, tStack         ; Temp/locals stack
    xor     rax, rax            ; TOS = 0

    ; Initialize dictionary with primitives
    call    initDict

    mov     r15, THE_ROM        ; Instruction pointer
    
    ; Jump to interpreter
    call    interpret
    call    p_BYE

; ******************************************************************************
; Inner Interpreter (threaded code)
; ******************************************************************************
primDispatch:
    call    rbx                 ; Execute primitive, fall through to interpret

interpret:
    test    r15, r15            ; Check for end of code (NULL)
    jz      .done
    mov     rbx, [r15]          ; Fetch next instruction
    add     r15, CELL_SZ        ; Advance IP

    cmp     rbx, primEnd        ; Primitive? (30% - most common non-XT exit)
    jb      primDispatch

    js      .number             ; Literal? bit63 set (10%)

    ; Colon definition (60% = 50% XT + 10% TCO) - falls through
    cmp     qword [r15], p_EXIT ; tail call?
    je      .tail
    rPush   r15                 ; Save current IP
.tail:
    mov     r15, rbx            ; Jump to definition
    jmp     interpret

.number:
    btr     rbx, 63             ; Clear high bit to get value
    sPush   rbx                 ; Push to stack
    jmp     interpret

.done:      
    mov     r13, rStack         ; Reset return stack
    ret

; ******************************************************************************
; Primitives
; ******************************************************************************

; EXIT - Return from colon definition
p_EXIT:
    cmp     r13, rStack         ; Check return stack underflow
    jng     .underflow
    rPop    r15                 ; Restore IP
    ret
.underflow:
    ; Return stack underflow
    mov     r13, rStack
    xor     r15, r15            ; NULL IP to trigger exit
    ret

; Stack manipulation
p_DUP:
    sPush   rax
    ret

p_DROP:
    mov     rax, [rbp]
    sub     rbp, CELL_SZ
    cmp     rbp, dStack
    jge     .ok
    mov     rbp, dStack
.ok:
    ret

p_SWAP:
    mov     rbx, [rbp]
    mov     [rbp], rax
    mov     rax, rbx
    ret

p_OVER:
    mov     rbx, [rbp]
    sPush   rbx
    ret

; Arithmetic
p_PLUS:
    sPop    rbx
    add     rax, rbx
    ret

p_MINUS:
    sPop    rbx
    sub     rax, rbx
    ret

p_MULT:
    sPop    rbx
    imul    rax, rbx
    ret

p_DIVMOD:
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

p_INC:
    inc     rax
    ret

p_DEC:
    dec     rax
    ret

p_NEG:
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

p_INVERT:
    not     rax
    ret

; Comparison
p_EQUAL:
    sPop    rbx
    cmp     rax, rbx
    mov     rax, 0
    sete    al
    ret

p_LESS:
    sPop    rbx
    cmp     rax, rbx
    mov     rax, 0
    setl    al
    ret

p_GREATER:
    sPop    rbx
    cmp     rax, rbx
    mov     rax, 0
    setg    al
    ret

; Memory access
p_FETCH:
    mov     rax, [rax]
    ret

p_STORE:
    sPop    rbx                 ; address
    sPop    rcx                 ; value
    mov     [rbx], rcx
    ret

p_CFETCH:
    movzx   rax, byte [rax]
    ret

p_CSTORE:
    sPop    rbx                 ; address
    sPop    rcx                 ; value
    mov     [rbx], cl
    ret

; Return stack
p_TOR:
    sPop    rbx
    rPush   rbx
    ret

p_FROMR:
    rPop    rbx
    sPush   rbx
    ret

p_RFETCH:
    mov     rbx, [r13]
    sPush   rbx
    ret

; Literals
p_LIT:
    mov     rbx, [r15]
    add     r15, CELL_SZ
    sPush   rbx
    ret

; I/O
p_EMIT:
    sPop    rbx
    mov     [charBuf], bl
    
    mov     rax, 1              ; sys_write
    mov     rdi, 1              ; stdout
    mov     rsi, charBuf
    mov     rdx, 1
    syscall
    ret

p_TYPE:
    sPop    rdx                 ; length
    sPop    rsi                 ; address
    
    mov     rax, 1              ; sys_write
    mov     rdi, 1              ; stdout
    syscall
    ret

p_KEY:
    mov     rax, 0              ; sys_read
    xor     rdi, rdi            ; stdin
    mov     rsi, charBuf
    mov     rdx, 1
    syscall
    
    movzx   rax, byte [charBuf]
    sPush   rax
    ret

; Code pointer
p_HERE:
    mov     rbx, [HERE]
    sPush   rbx
    ret

p_MEM:
    sPush   THE_CODE
    ret

p_COMMA:
    sPop    rbx
    mov     rcx, [HERE]
    mov     [rcx], rbx
    add     rcx, CELL_SZ
    mov     [HERE], rcx
    ret

; lit, ( n -- )  compile n as a literal into HERE
; if bit63 clear: tag n and compile as single cell
; if bit63 set:   compile p_LIT + n (2 cells)
p_LITCOMMA:
    test    rax, rax
    js      .twocell
    bts     rax, 63             ; tag TOS in place
    jmp     p_COMMA             ; tail call
.twocell:
    sPop    rbx                 ; save n
    sPush   p_LIT
    call    p_COMMA
    sPush   rbx
    call    p_COMMA
    ret

; Dictionary
p_LAST:
    mov     rbx, [LAST]
    sPush   rbx
    ret

p_BASE:
    mov     rbx, [BASE]
    sPush   rbx
    ret

; Add word to dictionary ( s1 -- )
; XT = HERE (captured by addDictEntry)
p_ADDDICT:
    sPop    rsi                 ; rsi = name string
    jmp     addDictEntry        ; tail call

; Control flow
p_BRANCH:
    mov     rbx, [r15]
    mov     r15, rbx
    ret

p_ZBRANCH:
    sPop    rbx
    test    rbx, rbx
    jz      p_BRANCH
    add     r15, CELL_SZ
    ret

; System
p_BYE:
    mov     rax, 60             ; sys_exit
    xor     rdi, rdi            ; exit code 0
    syscall
    ret

p_CR:
    mov     rax, 1
    mov     rdi, 1
    mov     rsi, crStr
    mov     rdx, 1
    syscall
    ret

; Locals (temp stack) frame ops
; r14 points directly to current frame's x slot; [r14]=x, [r14+8]=y, [r14+16]=z
; Each frame is 3 cells (24 bytes); +L pushes a frame, -L pops one

; +L - allocate locals frame ( -- )
p_TSPI:
    lea     rbx, [r14 + 3*CELL_SZ]
    lea     rcx, [tStack + (64-3)*CELL_SZ]
    cmp     rbx, rcx
    jg      .overflow
    mov     r14, rbx
.overflow:
    ret

; -L - free locals frame ( -- )
p_TSPD:
    lea     rbx, [r14 - 3*CELL_SZ]
    lea     rcx, [tStack]
    cmp     rbx, rcx
    jl      .underflow
    mov     r14, rbx
.underflow:
    ret

; x@ - fetch locals x slot ( -- x )
p_XFET:
    sPush   [r14]
    ret

; x! - store to locals x slot ( n -- )
p_XSTO:
    sPop    rbx
    mov     [r14], rbx
    ret

; x@+ fetch locals x slot then increment ( -- x )
p_XFETI:
    mov     rbx, [r14]
    inc     qword [r14]
    sPush   rbx
    ret

; y@ - fetch locals y slot ( -- y )
p_YFET:
    sPush   [r14 + CELL_SZ]
    ret

; y! - store to locals y slot ( n -- )
p_YSTO:
    sPop    rbx
    mov     [r14 + CELL_SZ], rbx
    ret

; y@+ fetch locals y slot then increment ( -- y )
p_YFETI:
    mov     rbx, [r14 + CELL_SZ]
    inc     qword [r14 + CELL_SZ]
    sPush   rbx
    ret

; z@ - fetch locals z slot ( -- z )
p_ZFET:
    sPush   [r14 + 2*CELL_SZ]
    ret

; z! - store to locals z slot ( n -- )
p_ZSTO:
    sPop    rbx
    mov     [r14 + 2*CELL_SZ], rbx
    ret

; z@+ fetch locals z slot then increment ( -- z )
p_ZFETI:
    mov     rbx, [r14 + 2*CELL_SZ]
    inc     qword [r14 + 2*CELL_SZ]
    sPush   rbx
    ret

; String length ( s1 -- n )
p_SLEN:
    xor     rcx, rcx
.loop:
    cmp     byte [rax + rcx], 0
    je      .done
    inc     rcx
    jmp     .loop
.done:
    mov     rax, rcx
    ret

; Make TOS lowercase ( c1 -- c2 )
p_LCASE:
    cmp     rax, 'A'
    jl      .done
    cmp     rax, 'Z'
    jg      .done
    add     rax, 32
.done:
    ret

; Case-insensitive string equal ( s1 s2 -- f )  f: -1 equal, 0 not equal
; Uses r9/r10 as string pointers - does NOT clobber rsi or rdi
p_SEQI:
    sPop    r9                  ; r9 = s2
    sPop    r10                 ; r10 = s1
    sPush   0                   ; make scratch space for use and result; set later to T/F
.loop:
    movzx   rax, byte [r9]
    call    p_LCASE
    mov     cl, al              ; cl = lowercase char from s2
    movzx   rax, byte [r10]
    call    p_LCASE             ; al = lowercase char from s1
    cmp     al, cl
    jne     .notequal
    test    al, al              ; both zero = end of strings
    jz      .equal
    inc     r9
    inc     r10
    jmp     .loop
.notequal:
    xor     rax, rax
    ret
.equal:
    mov     rax, -1
    ret

; >in ( -- a )  push address of the input pointer variable
p_TOIN:
    sPush   TOIN
    ret

; wd ( -- a )  push address of the word buffer
p_WD:
    sPush   WD
    ret

; next-word ( -- )  skip whitespace, parse next word from input into WD
; WD is a counted+null-terminated string: WD[0]=len, WD[1..len]=chars, WD[len+1]=0
p_NEXTWORD:
    mov     rsi, [TOIN]         ; rsi = current input pointer
.skip:
    cmp     byte [rsi], 0       ; end of input?
    je      .empty
    cmp     byte [rsi], 32      ; whitespace?
    jg      .collect
    inc     rsi
    jmp     .skip
.collect:
    lea     rdi, [WD+1]         ; rdi = char area (WD[1..])
    xor     rcx, rcx
.charloop:
    cmp     byte [rsi], 32      ; whitespace or null ends word
    jle     .worddone
    cmp     rcx, 30             ; cap at 30 chars (1 len + 30 chars + 1 null = 32)
    jge     .worddone
    mov     bl, [rsi]
    mov     [rdi + rcx], bl
    inc     rsi
    inc     rcx
    jmp     .charloop
.worddone:
    mov     byte [WD], cl       ; WD[0] = length
    mov     byte [rdi + rcx], 0 ; null terminate after chars
    mov     [TOIN], rsi         ; update input pointer
    ret
.empty:
    mov     byte [WD], 0        ; empty counted string
    ret

; find ( cs -- e )  search dictionary for counted string cs, return entry addr or 0
; cs points at length byte (WD); dict entries at DE_LEN_OFFSET are same format
; p_SEQI compares length bytes first - instant reject on mismatch
p_FIND:
    mov     rsi, rax            ; rsi = cs (counted string ptr)
    xor     rax, rax            ; rax = 0 (sentinel + default not-found)

    mov     rbx, [LAST]         ; rbx = current entry
    lea     rdx, [THE_DICT + DICT_SZ] ; rdx = end sentinel

.entryloop:
    cmp     rbx, rdx            ; past end of dictionary?
    jge     .notfound

    ; compare cs against entry's counted string at DE_LEN_OFFSET
    sPush   rsi                 ; s1 = search counted string (saves sentinel, TOS=rsi)
    lea     r8, [rbx + DE_LEN_OFFSET] ; r8 = s2 = entry counted string
    sPush   r8                  ; s2 on stack (saves rsi, TOS=r8)
    call    p_SEQI              ; ( s1 s2 -- f )  length byte compared first
    sPop    r8                  ; r8=result (f), rax=0 (sentinel) restored
    test    r8, r8
    jnz     .found

.next:
    add     rbx, DE_SIZE
    jmp     .entryloop

.found:
    mov     rax, rbx            ; return entry address
    ret

.notfound:
                                ; rax=0: either never matched or last p_SEQI returned 0
    ret

; is-num ( cs -- n true | false )
; cs is a counted string (WD); skips length byte, parses chars in BASE
; handles % binary, # decimal, $ hex, 'x' char literal
; returns n 1 on success, 0 on failure
p_ISNUM:
    mov     rsi, rax            ; rsi = counted string ptr
    inc     rsi                 ; skip length byte, point at chars
    mov     rbx, [BASE]         ; rbx = base
    xor     rcx, rcx            ; rcx = accumulator
    xor     rdx, rdx            ; rdx = isNeg flag

    ; char literal 'x': w[0]==39, w[2]==39, w[3]==0
    cmp     byte [rsi], 39
    jne     .not_char
    cmp     byte [rsi+2], 39
    jne     .not_char
    cmp     byte [rsi+3], 0
    jne     .not_char
    movzx   rax, byte [rsi+1]   ; push char value
    sPush   1                   ; true
    ret

.not_char:
    ; prefix overrides
    cmp     byte [rsi], '%'
    jne     .not_pct
    mov     rbx, 2
    inc     rsi
    jmp     .after_prefix
.not_pct:
    cmp     byte [rsi], '#'
    jne     .not_hash
    mov     rbx, 10
    inc     rsi
    jmp     .after_prefix
.not_hash:
    cmp     byte [rsi], '$'
    jne     .after_prefix
    mov     rbx, 16
    inc     rsi
.after_prefix:
    ; negative (base 10 only)
    cmp     rbx, 10
    jne     .after_neg
    cmp     byte [rsi], '-'
    jne     .after_neg
    mov     rdx, 1
    inc     rsi
.after_neg:
    ; must have at least one digit
    cmp     byte [rsi], 0
    je      .fail

.digitloop:
    movzx   r8, byte [rsi]
    test    r8b, r8b
    jz      .success
    ; lowercase A-Z -> a-z
    cmp     r8b, 'A'
    jl      .no_lower
    cmp     r8b, 'Z'
    jg      .no_lower
    add     r8b, 32
.no_lower:
    ; get digit value
    cmp     r8b, '0'
    jl      .fail
    cmp     r8b, '9'
    jle     .dec_digit
    cmp     r8b, 'a'
    jl      .fail
    cmp     r8b, 'f'
    jg      .fail
    sub     r8b, 'a'-10         ; 'a'=10, 'b'=11 ...
    jmp     .check_base
.dec_digit:
    sub     r8b, '0'
.check_base:
    cmp     r8, rbx
    jge     .fail
    imul    rcx, rbx
    add     rcx, r8
    inc     rsi
    jmp     .digitloop

.success:
    test    rdx, rdx
    jz      .positive
    neg     rcx
.positive:
    mov     rax, rcx            ; TOS = n (replaces s)
    sPush   1                   ; push true (saves n, TOS=1)
    ret

.fail:
    xor     rax, rax            ; TOS = false
    ret

; immediate ( -- )  set IMMED flag on most recently defined word
p_IMMEDIATE:
    mov     rbx, [LAST]
    or      byte [rbx + DE_FLAGS_OFFSET], 0x80
    ret

; count ( cs -- str len )  split counted string into addr/len pair
p_COUNT:
    movzx   rbx, byte [rax]     ; rbx = length
    inc     rax                 ; rax = char area (cs+1)
    sPush   rbx                 ; save str, TOS = len
    ret

; fopen ( name flags -- fd )  sys_open; mode=0664 used when creating
p_FOPEN:
    sPop    rsi                 ; rsi = flags
    mov     rdi, rax            ; rdi = name (will become fd after syscall)
    mov     rdx, 0x1B4          ; mode = 0664
    mov     rax, 2              ; sys_open
    syscall                     ; rax = fd (new TOS)
    ret

; fclose ( fd -- )  sys_close
p_FCLOSE:
    sPop    rdi                 ; rdi = fd
    push    rax                 ; save TOS (rax needed for syscall)
    mov     rax, 3              ; sys_close
    syscall
    pop     rax                 ; restore TOS
    ret

; fread ( buf len fd -- n )  sys_read; returns bytes read
p_FREAD:
    sPop    rdi                 ; rdi = fd
    sPop    rdx                 ; rdx = len
    mov     rsi, rax            ; rsi = buf
    mov     rax, 0              ; sys_read
    syscall                     ; rax = bytes read (new TOS)
    ret

; fwrite ( buf len fd -- n )  sys_write; returns bytes written
p_FWRITE:
    sPop    rdi                 ; rdi = fd
    sPop    rdx                 ; rdx = len
    mov     rsi, rax            ; rsi = buf
    mov     rax, 1              ; sys_write
    syscall                     ; rax = bytes written (new TOS)
    ret

; outer ( str -- )  primitive wrapper: pop string, call outer
; syscall0-6 ( a1..aN n -- r )  raw Linux syscalls
; Usage: a1 a2 a3 n syscall3  (natural left-to-right order)
; TOS=n, [rbp]=aN, ..., [rbp-(N-1)*8]=a1
p_SYSCALL0:
    syscall
    ret

p_SYSCALL1:
    mov     rdi, [rbp]              ; a1 (only arg, deepest)
    sub     rbp, CELL_SZ
    syscall
    ret

p_SYSCALL2:
    mov     rsi, [rbp]              ; a2 (last pushed)
    mov     rdi, [rbp-CELL_SZ]      ; a1 (first pushed)
    sub     rbp, 2*CELL_SZ
    syscall
    ret

p_SYSCALL3:
    mov     rdx, [rbp]              ; a3
    mov     rsi, [rbp-CELL_SZ]      ; a2
    mov     rdi, [rbp-2*CELL_SZ]    ; a1
    sub     rbp, 3*CELL_SZ
    syscall
    ret

p_SYSCALL4:
    mov     r10, [rbp]              ; a4
    mov     rdx, [rbp-CELL_SZ]      ; a3
    mov     rsi, [rbp-2*CELL_SZ]    ; a2
    mov     rdi, [rbp-3*CELL_SZ]    ; a1
    sub     rbp, 4*CELL_SZ
    syscall
    ret

p_SYSCALL5:
    mov     r8,  [rbp]              ; a5
    mov     r10, [rbp-CELL_SZ]      ; a4
    mov     rdx, [rbp-2*CELL_SZ]    ; a3
    mov     rsi, [rbp-3*CELL_SZ]    ; a2
    mov     rdi, [rbp-4*CELL_SZ]    ; a1
    sub     rbp, 5*CELL_SZ
    syscall
    ret

p_SYSCALL6:
    mov     r9,  [rbp]              ; a6
    mov     r8,  [rbp-CELL_SZ]      ; a5
    mov     r10, [rbp-2*CELL_SZ]    ; a4
    mov     rdx, [rbp-3*CELL_SZ]    ; a3
    mov     rsi, [rbp-4*CELL_SZ]    ; a2
    mov     rdi, [rbp-5*CELL_SZ]    ; a1
    sub     rbp, 6*CELL_SZ
    syscall
    ret

p_OUTER:
    sPop    rdi
    jmp     outer               ; tail call

primEnd:

; ******************************************************************************
; Outer interpreter
; ******************************************************************************

; isColon set the carry flag if WD is a colon
isColon:
    cmp     byte [WD], 1        ; length == 1?
    jne     retFalse
    cmp     byte [WD+1], ':'    ; first char == ':'?
    je      retTrue
    jne     retFalse

; isSemi set the carry flag if WD is a semicolon
isSemi:
    cmp     byte [WD], 1        ; length == 1?
    jne     retFalse
    cmp     byte [WD+1], ';'    ; first char == ';'?
    jne     retFalse

retTrue:
    stc
    ret
retFalse:
    clc
    ret

; outer(rdi = source string)
; Saves/restores TOIN.  Loop: call next-word; if WD empty, done; else print it.
outer:
    push    qword [TOIN]        ; save current TOIN
    mov     [TOIN], rdi         ; point TOIN at input string

.loop:
    call    p_NEXTWORD
    cmp     byte [WD], 0        ; empty → end of input
    je      .done

    call    isColon             ; CF is set if WD is a colon
    jc      .colon
    call    isSemi              ; CF is set if WD is a semicolon
    jc      .semi

    ; Is it a number?
    sPush   WD                  ; ( -- cs )  push WD counted-string address
    call    p_ISNUM             ; ( cs -- n true|false )  leaves n or 0 on TOS
    sPop    rbx                 ; (n true|false -- )  get the flag into rbx
    test    rbx, rbx            ; non-zero => is number
    jz      .notnum
    cmp     qword [STATE], 0    ; interpreting?
    je      .loop
    call    p_LITCOMMA          ; compile number into code stream
    jmp     .loop

.notnum: ; It is not a number, is it a WORD in the dictionary?
    sPush   WD                  ; ( -- cs )  push WD counted-string address
    call    p_FIND              ; ( cs -- entry|0 )
    sPop    rbx                 ; zero if not found
    test    rbx, rbx
    jz      .notfound

    mov     rdx, [rbx + DE_XT_OFFSET] ; get entry's XT
    cmp     [STATE], 0          ; interpreting?
    je      .interp
    test    byte [rbx + DE_FLAGS_OFFSET], 0x80 ; check immediate flag
    jnz     .interp
    sPush   rdx                 ; ( -- xt )  push XT of found word
    call    p_COMMA             ; compile XT into code stream
    jmp     .loop

.colon:
    call    p_NEXTWORD          ; get next word, which should be the new word's name
    lea     rsi, [WD+1]         ; rsi = name char ptr
    call    addDictEntry        ; add new word to dictionary
    mov     [STATE], 1          ; switch to compiling
    jmp     .loop

.semi:
    sPush   p_EXIT
    call    p_COMMA             ; compile XT into code stream
    mov     [STATE], 0          ; switch to interpreting
    jmp     .loop

.interp:
    cmp     rdx, primEnd        ; primitive?
    jb      .prim
    xor     rbx, rbx
    rPush   rbx                 ; NULL sentinel so interpret exits cleanly
    push    r15                 ; save outer IP
    mov     r15, rdx
    call    interpret
    pop     r15                 ; restore outer IP
    jmp     .loop
.prim:
    call    rdx
    jmp     .loop

.notfound:
    ; Not found: print it with a "?" prefix
    mov     [STATE], 0          ; switch to interpreting
    sPush   '?'
    call    p_EMIT
    sPush   WD
    call    p_COUNT
    call    p_TYPE
    call    p_CR
    jmp     .loop

.done:
    pop     qword [TOIN]        ; restore TOIN
    ret

; ******************************************************************************
; Dictionary initialization
; ******************************************************************************

; addDictEntry(rsi=name) - adds one dictionary entry; XT = current HERE
; Returns rbx = new entry pointer
; Uses registers only; does not touch the Forth data stack
addDictEntry:
    ; Allocate entry growing downward
    mov     rbx, [LAST]
    sub     rbx, DE_SIZE
    mov     [LAST], rbx

    ; Zero the entry
    xor     rcx, rcx
    mov     qword [rbx],    rcx
    mov     qword [rbx+8],  rcx
    mov     qword [rbx+16], rcx
    mov     qword [rbx+24], rcx

    ; XT = HERE
    mov     rcx, [HERE]
    mov     [rbx + DE_XT_OFFSET], rcx

    ; strlen(rsi) -> rdx, capped at DE_MAX_NAME
    mov     rdx, rsi
.lenloop:
    cmp     byte [rdx], 0
    je      .lendone
    inc     rdx
    jmp     .lenloop
.lendone:
    sub     rdx, rsi
    cmp     rdx, DE_MAX_NAME
    jle     .lenok
    mov     rdx, DE_MAX_NAME
.lenok:
    mov     [rbx + DE_LEN_OFFSET], dl

    ; copy name into entry
    lea     rdi, [rbx + DE_NAME_OFFSET]
    mov     rcx, rdx
    rep movsb
    ret

; initDict - walk primTable, call addDictEntry for each {name,xt} pair
initDict:
    mov     r8, primTable
.loop:
    mov     rsi, [r8]           ; name ptr (0 = end of table)
    test    rsi, rsi
    jz      .done
    mov     r9, [r8+8]          ; primitive address - save before call (addDictEntry clobbers rcx/rdi)
    push    r8
    call    addDictEntry        ; rbx = new entry, XT set to HERE
    pop     r8
    mov     [rbx + DE_XT_OFFSET], r9 ; overwrite XT with real primitive address
    add     r8, 16
    jmp     .loop
.done:
    ret

primTable:
    dq nm_EXIT,      p_EXIT
    dq nm_DUP,       p_DUP
    dq nm_DROP,      p_DROP
    dq nm_SWAP,      p_SWAP
    dq nm_OVER,      p_OVER
    dq nm_PLUS,      p_PLUS
    dq nm_MINUS,     p_MINUS
    dq nm_MULT,      p_MULT
    dq nm_DIVMOD,    p_DIVMOD
    dq nm_INC,       p_INC
    dq nm_DEC,       p_DEC
    dq nm_NEG,       p_NEG
    dq nm_AND,       p_AND
    dq nm_OR,        p_OR
    dq nm_XOR,       p_XOR
    dq nm_INVERT,    p_INVERT
    dq nm_EQUAL,     p_EQUAL
    dq nm_LESS,      p_LESS
    dq nm_GREATER,   p_GREATER
    dq nm_FETCH,     p_FETCH
    dq nm_STORE,     p_STORE
    dq nm_CFETCH,    p_CFETCH
    dq nm_CSTORE,    p_CSTORE
    dq nm_TOR,       p_TOR
    dq nm_FROMR,     p_FROMR
    dq nm_RFETCH,    p_RFETCH
    dq nm_LIT,       p_LIT
    dq nm_EMIT,      p_EMIT
    dq nm_TYPE,      p_TYPE
    dq nm_KEY,       p_KEY
    dq nm_HERE,      p_HERE
    dq nm_MEM,       p_MEM
    dq nm_COMMA,     p_COMMA
    dq nm_LITCOMMA,  p_LITCOMMA
    dq nm_LAST,      p_LAST
    dq nm_BASE,      p_BASE
    dq nm_BRANCH,    p_BRANCH
    dq nm_ZBRANCH,   p_ZBRANCH
    dq nm_BYE,       p_BYE
    dq nm_CR,        p_CR
    dq nm_TSPI,      p_TSPI
    dq nm_TSPD,      p_TSPD
    dq nm_XFET,      p_XFET
    dq nm_XSTO,      p_XSTO
    dq nm_XFETI,     p_XFETI
    dq nm_YFET,      p_YFET
    dq nm_YSTO,      p_YSTO
    dq nm_YFETI,     p_YFETI
    dq nm_ZFET,      p_ZFET
    dq nm_ZSTO,      p_ZSTO
    dq nm_ZFETI,     p_ZFETI
    dq nm_SLEN,      p_SLEN
    dq nm_LCASE,     p_LCASE
    dq nm_SEQI,      p_SEQI
    dq nm_FIND,      p_FIND
    dq nm_ADDDICT,   p_ADDDICT
    dq nm_TOIN,      p_TOIN
    dq nm_WD,        p_WD
    dq nm_NEXTWORD,  p_NEXTWORD
    dq nm_ISNUM,     p_ISNUM
    dq nm_IMMEDIATE, p_IMMEDIATE
    dq nm_COUNT,     p_COUNT
    dq nm_FOPEN,     p_FOPEN
    dq nm_FCLOSE,    p_FCLOSE
    dq nm_FREAD,     p_FREAD
    dq nm_FWRITE,    p_FWRITE
    dq nm_OUTER,     p_OUTER
    dq nm_SYSCALL0,  p_SYSCALL0
    dq nm_SYSCALL1,  p_SYSCALL1
    dq nm_SYSCALL2,  p_SYSCALL2
    dq nm_SYSCALL3,  p_SYSCALL3
    dq nm_SYSCALL4,  p_SYSCALL4
    dq nm_SYSCALL5,  p_SYSCALL5
    dq nm_SYSCALL6,  p_SYSCALL6
    dq 0, 0  ; end of table

; ******************************************************************************
; Boot loader
; ******************************************************************************

; boot - open wc64-boot.fth, read into THE_CODE+100000, call outer
; Runs as threaded code via THE_ROM
boot:
    dq p_TSPI                           ; +L  (x=fd)
    dq p_LIT, bootFile, p_LIT, 0       ; ( name O_RDONLY )
    dq p_FOPEN                          ; ( fd )
    dq p_DUP, p_LIT, 0, p_LESS         ; ( fd fd<0 )
    dq p_ZBRANCH, boot_ok
    dq p_DROP
    dq p_LIT, bootErrStr, p_LIT, bootErrLen, p_TYPE
    dq p_TSPD, p_BYE
boot_ok:
    dq p_XSTO                           ; x = fd
    dq p_LIT, THE_CODE+100000
    dq p_LIT, bootBufSz
    dq p_XFET, p_FREAD                  ; ( n )  bytes read
    dq p_DUP, p_LIT, 0, p_LESS         ; ( n n<0 )
    dq p_ZBRANCH, boot_read_ok
    dq p_DROP
    dq p_LIT, bootErrStr, p_LIT, bootErrLen, p_TYPE
    dq p_TSPD, p_BYE
boot_read_ok:
    dq p_DROP
    dq p_XFET, p_FCLOSE                 ; close fd
    ; null-terminate the buffer
    dq p_LIT, THE_CODE+100000
    dq p_SLEN
    dq p_LIT, THE_CODE+100000
    dq p_PLUS                           ; ( end_addr )
    dq p_LIT, 0, p_SWAP, p_CSTORE      ; [end] = 0
    ; call outer with the buffer
    dq p_LIT, THE_CODE+100000
    dq p_OUTER
    dq p_TSPD, p_BYE

bootFile    db 'wc64-boot.fth', 0
bootErrStr  db 'Error: cannot open wc64-boot.fth', 10
bootErrLen  = $ - bootErrStr
bootBufSz   = CODE_SZ - 100000

; ******************************************************************************
; Data segment
; ******************************************************************************
segment readable writable

InitialRSP  dq 0
HERE        dq THE_CODE
LAST        dq THE_DICT + DICT_SZ
BASE        dq 10
STATE       dq 0
TOIN        dq 0

WD          rb 32
charBuf     db 0
spaceStr    db ' '
crStr       db 10
numBuf      rb 32
; Primitive names
nm_EXIT     db 'exit',    0
nm_DUP      db 'dup',     0
nm_DROP     db 'drop',    0
nm_SWAP     db 'swap',    0
nm_OVER     db 'over',    0
nm_PLUS     db '+',       0
nm_MINUS    db '-',       0
nm_MULT     db '*',       0
nm_DIVMOD   db '/mod',    0
nm_INC      db '1+',      0
nm_DEC      db '1-',      0
nm_NEG      db 'negate',  0
nm_AND      db 'and',     0
nm_OR       db 'or',      0
nm_XOR      db 'xor',     0
nm_INVERT   db 'invert',  0
nm_EQUAL    db '=',       0
nm_LESS     db '<',       0
nm_GREATER  db '>',       0
nm_FETCH    db '@',       0
nm_STORE    db '!',       0
nm_CFETCH   db 'c@',      0
nm_CSTORE   db 'c!',      0
nm_TOR      db '>r',      0
nm_FROMR    db 'r>',      0
nm_RFETCH   db 'r@',      0
nm_LIT      db 'lit',     0
nm_EMIT     db 'emit',    0
nm_TYPE     db 'type',    0
nm_KEY      db 'key',     0
nm_HERE     db 'here',    0
nm_MEM      db 'mem',     0
nm_COMMA    db ',',       0
nm_LITCOMMA db 'lit,',    0
nm_LAST     db 'last',    0
nm_BASE     db 'base',    0
nm_BRANCH   db 'branch',  0
nm_ZBRANCH  db '0branch', 0
nm_BYE      db 'bye',     0
nm_CR       db 'cr',      0
nm_TSPI     db '+L',      0
nm_TSPD     db '-L',      0
nm_XFET     db 'x@',      0
nm_XSTO     db 'x!',      0
nm_XFETI    db 'x@+',     0
nm_YFET     db 'y@',      0
nm_YSTO     db 'y!',      0
nm_YFETI    db 'y@+',     0
nm_ZFET     db 'z@',      0
nm_ZSTO     db 'z!',      0
nm_ZFETI    db 'z@+',     0
nm_SLEN     db 's-len',   0
nm_LCASE    db 'lcase',   0
nm_SEQI     db 's-eqi',   0
nm_FIND     db 'find',    0
nm_ADDDICT  db 'add-word',0
nm_TOIN     db '>in',     0
nm_WD       db 'wd',      0
nm_NEXTWORD db 'next-word',0
nm_ISNUM    db 'is-num',   0
nm_IMMEDIATE db 'immediate',0
nm_COUNT    db 'count',    0
nm_FOPEN    db 'fopen',    0
nm_FCLOSE   db 'fclose',   0
nm_FREAD    db 'fread',    0
nm_FWRITE   db 'fwrite',   0
nm_OUTER    db 'outer',    0
nm_SYSCALL0 db 'syscall0', 0
nm_SYSCALL1 db 'syscall1', 0
nm_SYSCALL2 db 'syscall2', 0
nm_SYSCALL3 db 'syscall3', 0
nm_SYSCALL4 db 'syscall4', 0
nm_SYSCALL5 db 'syscall5', 0
nm_SYSCALL6 db 'syscall6', 0

align 8
dStack      rq 256
rStack      rq 256
tStack      rq 64

THE_CODE:   rb CODE_SZ
THE_DICT:   rb DICT_SZ
THE_ROM = boot
