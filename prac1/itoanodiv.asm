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

debug_print_eax:
	pushad		
		push eax
		push format
		push debug_buffer
		call [__imp__wsprintfA]
		add esp, 12 ; __cdecl
		push MB_ICONINFORMATION
		push debug_eax
		push debug_buffer
		push 0
		call [__imp__MessageBoxA@16] ; WINAPI == stdcall
	popad
	ret

debug_print_ecx:
	pushad		
		push ecx
		push format
		push debug_buffer
		call [__imp__wsprintfA]
		add esp, 12 ; __cdecl
		push MB_ICONINFORMATION
		push debug_ecx
		push debug_buffer
		push 0
		call [__imp__MessageBoxA@16] ; WINAPI == stdcall
	popad
	ret


itoa: ; void itoa(char *buffer = [esp+4], int number = [esp+8])
	push ebp ; for %10
	push ebx ; for /10
	push edi ; buffer - destination
	push esi ; number - source
		mov edi, [esp + 16 + 4] ; buffer - destination
		mov esi, [esp + 16 + 8]	; number
		mov ebp, 10 ; for %10
		mov ebx, 429496730 ; for /10
	    mov ecx, 10 ; counter init : 10 digits in 4 bytes integer
		itoa_while:;			edx		eax			esi
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
			pop	esi			;	=		=			n/10 ; stack []
			cmp esi, 0
		jnz itoa_while
	; Used registers
	pop esi
	pop edi
	pop ebx
	pop ebp
	ret 8 ; sizeof(char *) + sizeof(int) == sizeof(buffer) + sizeof(number)

_main:
	push 8
	call factorial ; result in eax
	
	push eax
	push buffer
	call itoa

	push MB_ICONINFORMATION
	push factorial_title
	push buffer
	push 0
	call [__imp__MessageBoxA@16]
	push 0
	call [__imp__ExitProcess@4]

section .data

	buffer	db	"               ",0
	debug_buffer	db	"               ",0

section .rdata
	factorial_title	db	"Factorial",0
	debug_title db	"debug    ",0
	debug_eax	db	"debug eax",0
	debug_ecx	db	"debug ecx",0
	format	db	"%d",0
end
