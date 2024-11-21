; IO routines for Windows

; **********************************************************************
getHandles:
        invoke GetStdHandle, STD_INPUT_HANDLE
        mov    [hStdIn], eax
        
        invoke GetStdHandle, STD_OUTPUT_HANDLE
        mov    [hStdOut], eax
        ret

; **********************************************************************
doBye:  invoke  ExitProcess, 0
        ret

; **********************************************************************
doTimer: ; ( --N )
        invoke  GetTickCount
        sPush   eax
        ret

; **********************************************************************
EMIT: ; ( ch-- )
        sPop    eax
        mov     [buf1], al
        invoke  WriteConsole, [hStdOut], buf1, 1, NULL, NULL
        ret

; **********************************************************************
TYPE: ;  ( addr len-- )
        sPop    eax             ; Len
        sPop    ebx             ; Addr
        invoke  WriteConsole, [hStdOut], ebx, eax, NULL, NULL
        ret

; **********************************************************************
doReadL: ; ( addr sz--num )
        sPop    edx              ; buffer size
        getTOS  ecx              ; buffer
        invoke  ReadConsole, [hStdIn], ecx, edx, bytesRead, 0
        mov     eax, [bytesRead]
        dec     eax              ; Remove the <LF>
        dec     eax              ; Remove the <CR>
        getTOS  ecx
        mov     [ecx+eax], BYTE 0
        setTOS  eax
        ret

; **********************************************************************
doQKey: ; ( --flg )
        invoke  _kbhit
        sPush   eax
        ret

; **********************************************************************
doKey: ; ( --ch )
        invoke  _getch
        sPush   eax
        ret

