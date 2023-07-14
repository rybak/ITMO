extern	__imp__wsprintfA
extern	__imp__MessageBoxA@16
extern	__imp__ExitProcess@4
%assign MB_ICONINFORMATION 40h
global _main

section .text

print_int:
	pushad
		mov eax, [esp + 20h + 4];
		push eax
		;push word[esp + 20h + 4];
		push format
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

print_stack:
	pushad
		mov ecx, [esp + 20h + 4]
		xor eax, eax
		mov al, byte[esp + ecx];
		push eax
		call print_int	
	popad
	ret 4

_main:
	mov ecx, 0;
	main_loop:
		push ecx
		call print_stack
		inc ecx
		cmp ecx, 10
		jnz main_loop


	push 0
	call [__imp__ExitProcess@4]


section .data
	buffer	db	"               ",0
	strtitle	db	"printer:",0
	format	db	"%d",0
end
