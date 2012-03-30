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
		;push word[esp + 20h + 4];
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
		add esp, 12 ; __cdecl
		push MB_ICONINFORMATION
		push strtitle
		push buffer
		push 0
		call [__imp__MessageBoxA@16] ; WINAPI == stdcall
	popad
	ret 1

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

	;mov ebx, 1
	;push ebx
	;call print_int

	push eax
	call print_str

	mov esi, eax ; source

	xor ecx, ecx ;
	main_loop:
		mov al, byte[esi + ecx]
		inc ecx;
		cmp al, 0
		jnz main_loop
	push ecx
	call print_int
	
	mov 
	xor ecx, ecx ;
	calc_spaces_loop:
		mov al, byte[esi + ecx]
	;mov ebx, 2
	;push ebx
	;call print_int

	push 0
	call [__imp__ExitProcess@4]


section .data
	buffer	db	"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ",0
	strtitle	db	"printer:",0
	int_format	db	"%d",0
	char_format	db	"%c",0
	str_format	db "%s", 0
end
