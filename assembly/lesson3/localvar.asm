extern	__imp__ExitProcess@4
%assign MB_ICONINFORMATION 40h
global _main

section .text

_main:
	mov eax, 42
    mov ecx, 10

	; ascii:
	;	getstdhandle
	;	writefile
	; unicode
	;	writeConsole
	push 0
	call [__imp__ExitProcess@4]

end

   ; assume number is in eax
