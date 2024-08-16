; IO routines for Windows

; **********************************************************************
getHandles:
        invoke GetStdHandle, STD_INPUT_HANDLE
        mov    [hStdIn], eax
        
        invoke GetStdHandle, STD_OUTPUT_HANDLE
        mov    [hStdOut], eax
        ret

doBye:  invoke  ExitProcess, 0
        ret

; **********************************************************************
doTimer: ; ( --N )
        invoke GetTickCount
        m_push  eax
        ret

; **********************************************************************
EMIT: ; ( ch-- )
        m_pop   eax
        mov     [buf1], al
        invoke  WriteConsole, [hStdOut], buf1, 1, NULL, NULL
        ret

; **********************************************************************
doType: m_pop   eax              ; Len ( addr len-- )
        m_pop   ebx              ; Addr
        invoke  WriteConsole, [hStdOut], ebx, eax, NULL, NULL
        ret

; **********************************************************************
doReadL: ; ( addr sz--num )
        m_pop  edx              ; buffer size
        getTOS ecx             ; buffer
        invoke ReadConsole, [hStdIn], ecx, edx, bytesRead, 0
        mov    eax, [bytesRead]
        dec    eax             ; Remove the <LF>
        dec    eax             ; Remove the <CR>
        getTOS ecx
        mov    [ecx+eax], BYTE 0
        setTOS eax
        ret

; **********************************************************************
doQKey: ; ( --flg )
        invoke _kbhit
        m_push  eax
        ret

; **********************************************************************
doKey: ; ( --ch )
        invoke  _getch
        m_push  eax
        ret

