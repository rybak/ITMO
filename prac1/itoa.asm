extern	__imp__wsprintfA
extern	__imp__MessageBoxA@16
extern	__imp__ExitProcess@4
%assign MB_ICONINFORMATION 40h
global _main

section .text

factorial:
	mov ecx, [esp+4]
	mov eax, 1
	factorial_for:
		mul ecx;
		dec ecx;
		jnz factorial_for
	ret 4

itoa: ; void itoa(char *buffer = [esp+4], int number = [esp+8])   // REVERSED!!!
	push edx
	push ebp
	push ebx
	xor edx, edx
	mov eax, [esp + 12 + 8] ; number
	mov ebp, [esp + 12 + 4] ; buffer
	mov ecx, 10 ; counter
	mov ebx, 10 ; base
	itoa_while:
		div ebx ; edx = eax % 10 ; eax = eax / 10
		add edx, '0'; char
		mov [ebp + ecx], dl; one digit
		dec ecx ; 
		xor edx, edx ; clean
		cmp eax, 0
		jnz itoa_while
	; used registers
	pop ebx
	pop ebp
	pop edx
	ret 8 ; sizeof(char *) + sizeof(int)

_main:
	push 5
	call factorial ; result in eax

	push eax
	push buffer
	call itoa

	push MB_ICONINFORMATION
	push hello_title
	push buffer
	push 0
	call [__imp__MessageBoxA@16]
	push 0
	call [__imp__ExitProcess@4]

section .data
	hello_title	db	"Factorial",0
	buffer	db	"               ",0

end
