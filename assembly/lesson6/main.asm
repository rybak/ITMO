extern	__imp__SetConsoleTextAttribute@8
extern	__imp__GetStdHandle@4
extern	__imp__WriteConsoleA@20
extern	__imp__ExitProcess@4
extern	__imp__Sleep@4

extern	__imp__wsprintfA
extern 	__imp__printf
extern 	__imp__system
extern	__imp___getch
%assign MB_ICONINFORMATION 40h
global _main

section .rdata

pausestr db "pause",0
intformat db " %d ",0

section .bss
stdin: resd 1
stdout: resd 1
written: resd 1
doublebuf: resq 1
buffer: resb 2000 ; = 80 * 25

section .data

section .text

_main:
	call init_stdin_stdout

	push dword 42
	call print_int
	call println

exit:
	push 0
	call [__imp__ExitProcess@4]

section .rdata
	doubleformat db "%.19f",0

section .text
print_doublebuf:
	push dword[doublebuf + 4]
	push dword[doublebuf]
	push doubleformat
	call [__imp__printf]
	add esp, 0Ch
	ret

init_stdin_stdout:
	push dword -10
	call [__imp__GetStdHandle@4]
	mov [stdin], eax

	push dword -11
	call [__imp__GetStdHandle@4]
	mov [stdout], eax

	ret

section .rdata
	strdot db ".", 0
	strendl db 0Dh, 0Ah, 0

section .text

print_dot:
	pushad
		push dword 0
		push written
		push dword 1
		push strdot
		push dword[stdout]
		call [__imp__WriteConsoleA@20]
	popad
	ret

println:
	push dword 0
	push written
	push dword 2
	push strendl
	push dword[stdout]
	call [__imp__WriteConsoleA@20]
	ret

print_int:
	mov eax, [esp + 4]
	push eax
	push intformat
	call [__imp__printf]
	add esp, 8
	ret 4

end
