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
	xor edi, edi ; spaces
	calc_spaces_loop:
		movzx eax, byte[esi + ecx]
		inc ecx
		cmp eax, ' '
		jnz calc_spaces_not_space
			inc edi
		calc_spaces_not_space:
		cmp ecx, ebx
		jnz calc_spaces_loop;

	push edi
	call print_int

	push 0
	call [__imp__ExitProcess@4]


section .data
	buffer	db	"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ",0
	strtitle	db	"printer:",0
	int_format	db	"%d",0
	char_format	db	"%c",0
	str_format	db "%s", 0
end
