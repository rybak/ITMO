extern	__imp__SetConsoleTextAttribute@8
extern	__imp__GetStdHandle@4
extern	__imp__WriteConsoleA@20
extern	__imp__ExitProcess@4
extern	__imp__wsprintfA
extern 	__imp__printf
extern 	__imp__system
%assign MB_ICONINFORMATION 40h
global _main

section .rdata
;foreground:                  Irgb
;background:              Irgb
attr dd           0000000011001001b

cwidth dd 79
cheight dd 25

pausestr db "pause",0
intformat db " %d ",0

section .bss
stdin: resb 4
stdout: resb 4
written: resb 4
doublebuf: resb 8
buffer: resb 2000 ; = 80 * 25

section .data
padcoord dd 39
padhwidth dd 5
ballx dd 10
bally dd 10

section .text

_main:
	call init_stdin_stdout

	push dword [cwidth]
	call print_int
	push dword [cheight]
	call print_int
	call println

	call clean_buffer
	call draw_pad
	call draw_ball
	call print_buffer
	
exit:
	push 0
	call [__imp__ExitProcess@4]

draw_pad:
	pushad
		mov edi, [cheight] ; pad y
		dec edi
		mov ebx, [cwidth]
		mov edx, [padhwidth]

		mov ecx, [padcoord]
		sub ecx, edx
		cmp ecx, 0
		jg skip_zeroing_left
			xor ecx, ecx
		skip_zeroing_left:

		mov ebp, [padcoord]
		add ebp, edx
		cmp ebp, ebx 
		jl skip_cutting_right
			mov ebp, ebx
		skip_cutting_right:
		draw_pad_loop:
			push dword '='
			push edi
			push ecx
			call draw_charxy
			inc ecx
			cmp ecx, ebp
			jnz draw_pad_loop
	popad
	ret

draw_ball:
	pushad
		mov ecx, [ballx]
		mov edi, [bally]
		push dword 'O'
		push edi
		push ecx
		call draw_charxy
	popad
	ret

draw_charxy: ; (int x, int y, char c)
	pushad
		mov ecx, [esp + 20h + 4]
		mov eax, [esp + 20h + 8]
		mov ebx, [esp + 20h + 0Ch]
		mov ebp, [cwidth]
		mul ebp
		add eax, ecx
		mov byte[buffer + eax], bl
	popad
	ret 0Ch

print_buffer:
	pushad
		xor ecx, ecx
		mov ebp, [cheight]
		print_buffer_loop:
			push ecx
			call print_buffer_line
			inc ecx
			cmp ecx, ebp
			jnz print_buffer_loop
	popad
	ret

print_buffer_line:
	pushad ; esp OFFSET
		mov eax, [esp + 20h + 4]
		mov ebx, [cwidth]
		mul ebx
		add eax, buffer
		push dword 0
		push written
		push dword [cwidth]
		push eax
		push dword[stdout]
		call [__imp__WriteConsoleA@20]
		call println
	popad
	ret 4

clean_buffer:
	pushad
	    xor ecx, ecx
		mov ebx, [cwidth]
		mov ebp, [cheight]
		clean_loop_lines:
			xor esi, esi
			clean_loop_col:
				mov eax, ecx
				mul ebx
				add eax, esi
				mov byte[buffer + eax], '.'
				inc esi
				cmp esi, ebx
				jnz clean_loop_col
			inc ecx
			cmp ecx, ebp
			jnz clean_loop_lines
	popad
	ret
section .bss
	eaxint: resb 4

section .rdata
	doubleformat db "%.19f",0
section .text
print_doublebuf:
	push dword[doublebuf + 4]
	push dword[doublebuf]
	push doubleformat
	call [__imp__printf]
	add esp, 0Ch
	ret

init_stdin_stdout:
	push dword -10
	call [__imp__GetStdHandle@4]
	mov [stdin], eax

	push dword -11
	call [__imp__GetStdHandle@4]
	mov [stdout], eax

	ret

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

print_int:
	mov eax, [esp + 4]
	push eax
	push intformat
	call [__imp__printf]
	add esp, 8
	ret 4

end
