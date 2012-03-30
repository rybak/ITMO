extern	__imp__wsprintfA
extern	__imp__MessageBoxA@16
extern	__imp__ExitProcess@4
extern	__imp__GetCommandLineA@0
%assign MB_ICONINFORMATION 40h
global _main

section .text

print_int:
	pushad
		mov eax, [esp + 20h + 4];
		push eax
		push int_format
		push buffer
		call [__imp__wsprintfA]
		add esp, 12 ; __cdecl
		push MB_ICONINFORMATION
		push strtitle
		push buffer
		push 0
		call [__imp__MessageBoxA@16] ; WINAPI == stdcall
	popad
	ret 4

print_char:
	pushad
		movzx eax, byte[esp + 20h + 4]
		push eax
		push char_format
		push buffer
		call [__imp__wsprintfA]
		add esp, 8 + 4 ; __cdecl
		push MB_ICONINFORMATION
		push strtitle
		push buffer
		push 0
		call [__imp__MessageBoxA@16] ; WINAPI == stdcall
	popad
	ret 4

print_str:
	pushad
		mov eax, [esp + 20h + 4]
		push eax
		push str_format
		push buffer
		call [__imp__wsprintfA]
		add esp, 12 ; __cdecl
		push MB_ICONINFORMATION
		push strtitle
		push buffer
		push 0
		call [__imp__MessageBoxA@16] ; WINAPI == stdcall
	popad
	ret 4

_main:

	call [__imp__GetCommandLineA@0]

	push eax
	call print_str
	mov esi, eax ; source

	xor ecx, ecx ;
	calc_length_loop:
		mov al, byte[esi + ecx]
		inc ecx;
		cmp al, 0
		jnz calc_length_loop

	push ecx
	call print_int

	mov ebx, ecx ; length
	xor ecx, ecx ; counter
	xor edi, edi
	calc_args_loop:
		movzx eax, byte[esi + ecx]
		inc ecx
		xor eax, ' '
		jnz calc_args_not_space
			inc edi
			calc_args_skip_spaces:
				cmp ecx, ebx
				jz calc_args_loop_brake;
				movzx eax, byte[esi + ecx]
				inc ecx
				xor eax, ' '
				jz calc_args_skip_spaces
		calc_args_not_space:
		cmp ecx, ebx
		jnz calc_args_loop;
	calc_args_loop_brake:

	push edi ; argc
	call print_int

	xor ebp, ebp;
	xor ecx, ecx
	print_args_loop:
		movzx eax, byte[esi + ecx]
		cmp eax, ' '
		jnz print_args_print_arg
		jz print_args_skip_spaces
		print_args_skip_spaces:
			cmp ecx, ebx ; length
			jz print_args_loop_brake
			movzx eax, byte[esi + ecx]
			inc ecx
			cmp eax, ' '
			jz print_args_skip_spaces
		print_args_print_arg:
			push ebp
			call print_int ; print arg number
			inc ebp
			push esi
				add esi, ecx
				push esi
				call print_str
			pop esi
		print_args_print_arg_loop:
			cmp ecx, ebx ; length
			jz print_args_loop_brake
			movzx eax, byte[esi + ecx]
			inc ecx
			cmp eax, ' '
			jnz print_args_print_arg_loop
		;inc ecx
		cmp ecx, ebx
		jnz print_args_loop;
	print_args_loop_brake:

	push 0
	call [__imp__ExitProcess@4]


section .data
	buffer	db	"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ",0
	strtitle	db	"printer:",0
	int_format	db	"%d",0
	char_format	db	"%c",0
	str_format	db "%s", 0
end
