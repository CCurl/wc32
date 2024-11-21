; IO routines for Linux

; **********************************************************************
getHandles: ret

; **********************************************************************
doBye:  mov     eax,1           ; system call number (sys_exit)
        mov     ebx,0           ; return status
        int     0x80
        ret

; **********************************************************************
doTimer: ; invoke LinuxTimer
        sPush   eax
        ret

; **********************************************************************
EMIT:   push    eax
        sPop    eax             ; ( ch-- )
        mov     [buf1], al      ; put char in message
        mov     eax,4           ; system call number (sys_write)
        mov     ebx,1           ; file descriptor (stdout)
        mov     ecx,buf1        ; message to write
        mov     edx,1           ; message length
        int     0x80            ; call kernel
        pop     eax
        ret

; **********************************************************************
TYPE:   ; ( string len-- )
        sPop    edx             ; Len
        sPop    ecx             ; String
        mov     eax,4           ; system call number (sys_write)
        mov     ebx,1           ; file descriptor (stdout)
        int     0x80
        ret

; **********************************************************************
doReadL: ; ( addr sz--num )
        sPop   edx               ; buffer size ( buf sz--num )
        sPop   ecx              ; buffer
        mov    ebx, 0           ; stdin
        mov    eax, 3           ; sys_read
        push   ecx
        int    0x80
        pop    ecx
        dec    eax              ; Remove the <LF>
        sPush  eax
        mov    [ecx+eax], BYTE 0
        ret

; **********************************************************************
doQKey: ; ( --n ) - TODO
        sPush  0
        ret

; **********************************************************************
doKey:  ; ( --fl ) - TODO
        sPush  0
        ret

; **********************************************************************
