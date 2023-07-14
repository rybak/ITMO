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

itoa: ; void itoa(char *buffer = [esp+4], int number = [esp+8])   // REVERSED!!!
	pushad
		mov edi, [esp + 32 + 4] ; buffer - destination
		mov esi, [esp + 32 + 8]	; number
		mov ebp, 10 ; for %10
		mov ebx, 429496730 ; for /10
	    mov ecx, 9; counter init : 10 digits in 4 bytes integer
		itoa_loop: ;			edx		eax			esi
			mov eax, esi ;		-		n			n
			mul ebx ;			n/10	-
			mov eax, edx ;		n/10	n/10
			push edx									; stack [n/10]
    			mul ebp ;		0		n/10*10		n
				mov edx, esi ;	n		n/10*10		n
				sub edx, eax ;	c-'0'	=			n
				add edx, '0' ;	c		=			n
				mov [edi + ecx], dl ; add to buffer
			dec ecx
			pop	esi			;	=		=			n/10
			cmp esi, 0
		jnz itoa_loop
	popad
	ret 8; sizeof(char *) + sizeof(int) == sizeof(buffer) + sizeof(number)

_main:
	mov edi, 9
	mov esi, 1 ; i = 1
	main_loop:
		push esi
		call factorial ; result in eax ; trash in ecx

		push esi
		push buffer
		call itoa

		push buffer
		pop ebx
		mov dl, '!'
		mov [ebx + 11], dl
		mov dl, '='
		mov [ebx + 16], dl

		push eax
		push buffer + 15
		call itoa

		push MB_ICONINFORMATION
		push hello_title
		push buffer
		push 0
		call [__imp__MessageBoxA@16]
	inc esi
	cmp esi, edi
	jnz main_loop
	push 0
	call [__imp__ExitProcess@4]

section .data

	buffer	db	"                                                  ",0
	debug_buffer	db	"               ",0

section .rdata
	hello_title	db	"Factorial",0
	debug_title db	"debug    ",0
	debug_eax	db	"debug eax",0
	debug_ecx	db	"debug ecx",0
	format	db	"%d",0
end
