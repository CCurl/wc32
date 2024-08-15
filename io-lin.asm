; IO routines for Linux

; **********************************************************************
getHandles: ret

; **********************************************************************
doBye:  ; invoke  LinuxExit, 0
        ret

; **********************************************************************
doTimer: ; invoke LinuxTimer
        m_push  eax
        ret

; **********************************************************************
doEmit: push    eax
        m_pop   eax             ; ( ch-- )
        mov     [buf1], al      ; put char in message
        mov     eax,4           ; system call number (sys_write)
        mov     ebx,1           ; file descriptor (stdout)
        mov     ecx,buf1        ; message to write
        mov     edx,1           ; message length
        int     0x80            ; call kernel
        pop     eax
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
            push   ecx
            int    0x80
            pop    ecx
            dec    eax             ; Remove the <LF>
            m_push eax
            mov    [ecx+eax], BYTE 0
        ret

; **********************************************************************
doQKey: ; invoke LinuxKey
        m_push  DWORD 0
        ret

; **********************************************************************
doKey: ; invoke LinuxQKey
        m_push  DWORD 0
        ret

; **********************************************************************
