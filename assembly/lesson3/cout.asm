extern	__imp__wsprintfA

extern	__imp__GetStdHandle@4
extern	__imp__WriteConsoleA@20

extern	__imp__ExitProcess@4

global _main

section .data

buffer db "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ",0
strtitle	db "printer:", 0
char_format	db "%c", 0
str_format	db "%s", 0

section .bss
stdin: resb 4
stdout: resb 4
winapi: resb 4
written: resb 4

section .text

itoa: ; void itoa(char *buffer = [esp+4], int number = [esp+8])   // REVERSED!!!
	push edx
	push ebp
	push ebx
	mov eax, [esp + 0Ch + 8] ; number
	mov ebp, [esp + 0Ch + 4] ; buffer
	mov ecx, 9 ; counter
	mov ebx, 10 ; base
	itoa_loop:
		xor edx, edx
		div ebx ; edx = eax % 10 ; eax = eax / 10
		add edx, '0'; char
		mov [ebp + ecx], dl; one digit
		dec ecx ;
		cmp eax, 0
		jnz itoa_loop
	; used registers
	pop ebx
	pop ebp
	pop edx
	ret 8 ; sizeof(char *) + sizeof(int)

print_buffer_to_console:
	push ecx
	mov ecx, [esp + 8h]
	push dword 0
	push written
	push ecx
	push buffer
	push dword[stdout]
	call [__imp__WriteConsoleA@20]
	pop ecx
	ret 4

print_int:
	pushad
		mov eax, [esp + 20h + 4];
		push eax
		push buffer
		call itoa
		push dword 10
		call print_buffer_to_console
	popad
	ret 4

clear_buffer:
	push ebx
	mov eax, 80
	mov ebx, 25
	mul ebx
	mov ecx, eax
	dec ecx
	mov edx, buffer
	mov eax, ' '
	clear_buffer_loop:
		mov byte[edx + ecx], ' ' 
		dec ecx
		jnz clear_buffer_loop
	pop ebx
	ret

clear_console:
	push ebx
	call clear_buffer
	mov eax, 80
	mov ebx, 25
	mul ebx
	push eax
	call print_buffer_to_console
	pop ebx
	ret

init_stdin_stdout:
	push dword -10
	call [__imp__GetStdHandle@4]
	mov [stdin], eax

	push dword -11
	call [__imp__GetStdHandle@4]
	mov [stdout], eax
	ret

_main:
	call init_stdin_stdout

	call clear_console
	
	push dword 42
	call print_int
	push 0
	call [__imp__ExitProcess@4]

end

   ; assume number is in eax
