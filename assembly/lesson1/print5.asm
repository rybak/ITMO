extern	__imp__wsprintfA
extern	__imp__MessageBoxA@16
extern	__imp__ExitProcess@4
%assign MB_ICONINFORMATION 40h
global _main

section .text

factorial:
	mov ecx, [esp+4]
	mov eax, 1
	factorial_loop:
		mul ecx;
		dec ecx;
		jnz factorial_loop
	ret 4

print_int_to_buffer:
	mov eax, [esp+4]
	push eax
	push format
	push buffer
	call [__imp__wsprintfA]
	pop	eax; printf push [esp+4]
	pop eax; printf push format
	pop eax; printf push buffer
	ret 4

_main:
	; 
	push 5
	call factorial

	push eax
	call print_int_to_buffer

	push MB_ICONINFORMATION
	push hello_title
	push buffer
	push 0
	call [__imp__MessageBoxA@16]
	push 0
	call [__imp__ExitProcess@4]

section .data
	hello_title	db	"Factorial",0
	buffer	db	"                   ",0
	format	db	"%d",0
end
