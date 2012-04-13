extern	__imp__GetStdHandle@4
extern	__imp__WriteConsoleA@20
extern	__imp__ExitProcess@4
extern	__imp__GetCommandLineA@0

%assign MB_ICONINFORMATION 40h
global _main

section .data
	buffer	db	"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ",0
	argvstr	db	"argv = ",0
section .bss
stdin: resb 4
stdout: resb 4
written: resb 4

cmdline: resb 4
cmdlinelen: resb 4

section .text

_main:
	call init_stdin_stdout
	call set_cmdline
	call calc_cmdlinelen
	mov [cmdlinelen], eax
	mov ebx, eax
	push ebx
	call print_int
	call println

	push ebx
	push dword[cmdline]
	call print_str
	call println
	
	mov esi, [cmdline] ; source
	mov ebx, [cmdlinelen] ; length
	xor ecx, ecx ; counter
	xor edi, edi

	calc_argv_loop:
		movzx eax, byte[esi + ecx]
		cmp eax, ' '
		jz calc_argv_loop_continue
		cmp eax, '"'
		jnz calc_argv_in_spaces

		calc_argv_in_quotes:
		inc edi
		calc_argv_in_quotes_loop:
			inc ecx
			cmp ecx, ebx
			jz calc_argv_loop_break
			movzx eax, byte[esi + ecx]
			cmp eax, '"'
			jnz calc_argv_in_quotes_loop
		jmp calc_argv_put_zero

		calc_argv_in_spaces:
		inc edi
		calc_argv_in_spaces_loop:
			inc ecx
			cmp ecx, ebx
			jz calc_argv_loop_break
			movzx eax, byte[esi + ecx]
			cmp eax, ' '
			jnz calc_argv_in_spaces_loop
		jmp calc_argv_put_zero

		calc_argv_put_zero:
			mov [esi + ecx], byte 0
		calc_argv_loop_continue:
		inc ecx
		cmp ecx, ebx
		jnz calc_argv_loop
	calc_argv_loop_break:

	push 7
	push argvstr
	call print_str

	push edi ; argv
	call print_int

exit_main:
	push 0
	call [__imp__ExitProcess@4]

set_cmdline:
	call [__imp__GetCommandLineA@0]
	mov [cmdline], eax;	
	ret

calc_cmdlinelen:
	mov edx, [cmdline]
	xor eax, eax
	calc_length_loop:
		mov cl, byte[edx + eax]
		inc eax;
		cmp cl, 0
		jnz calc_length_loop
	ret

init_stdin_stdout:
	push dword -10
	call [__imp__GetStdHandle@4]
	mov [stdin], eax

	push dword -11
	call [__imp__GetStdHandle@4]
	mov [stdout], eax
	ret

itoa: ; void itoa(char *buffer = [esp+4], int number = [esp+8])
	; returns position in buffer from which number was written
	push edi
	push ebx
	
	mov eax, [esp + 8h + 8h] ; number
	mov edi, [esp + 8h + 4h] ; buffer
	mov ecx, 10 ; counter at zero position
	mov ebx, 10 ; base
	itoa_loop:
		xor edx, edx ; clean for div
		div ebx ; edx = eax % 10 ; eax = eax / 10
		add edx, '0'; digit to char
		dec ecx ; to next position
		mov [edi + ecx], dl; put one digit to buffer
		test eax, eax
		jnz itoa_loop
	mov eax, ecx ; return position in buffer

	pop edi
	pop ebx
	ret 8 ; sizeof(char *) + sizeof(int)

section .bss
itoabuffer: resb 10

section .text

print_int:
	pushad
		mov ebx, [esp + 20h + 4] ; int
		; itoa args
		push ebx ; int number
		push itoabuffer ; char* buffer
		call itoa
		; calc args for WriteConsole
		mov esi, itoabuffer
		add esi, eax
		mov ebp, 10
		sub ebp, eax
		; WriteConsole args
		push dword 0
		push written
		push ebp
		push esi
		push dword[stdout]
		call [__imp__WriteConsoleA@20]
	popad
	ret 4

print_str:
	pushad
		mov ecx, [esp + 20h + 8]
		mov eax, [esp + 20h + 4]
		push dword 0
		push written
		push ecx
		push eax
		push dword[stdout]
		call [__imp__WriteConsoleA@20]
	popad
	ret 8

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
end
