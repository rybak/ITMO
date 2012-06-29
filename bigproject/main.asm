extern	__imp__SetConsoleTextAttribute@8
extern	__imp__GetStdHandle@4
extern	__imp__WriteConsoleA@20
extern	__imp__WriteConsoleOutputA@20
extern	__imp__ExitProcess@4
extern	__imp__Sleep@4
extern	__imp__GetMessageA@16

extern	__imp__wsprintfA
extern 	__imp__printf
extern 	__imp__system
%assign MB_ICONINFORMATION 40h
global _main

section .rdata
;foreground:                  Irgb
;background:              Irgb
attr dw           0000000011001001b
space_attr dw     0000000000001111b

cwidth dd 70 ; 70
cheight dd 30 ; 0014h ; 20
char_info_size dd 04h

cleft	dw 0004h
ctop	dw 000Eh

section .bss

padcoord: resd 1

stdin: resd 1
stdout: resd 1
written: resd 1
doublebuf: resq 1
buffer: resb 10000
COORD_buffer_size: resd 1

section .data

padhwidth dd 5

ballx dd 40.0
bally dd 10.0
; speed
vx dd +1.0
vy dd -1.0

section .bss
ballxint: resd 1
ballyint: resd 1
msg: resb 10000

section .text

_main:
	call init_stdin_stdout

	push dword [cwidth]
	call print_int
	push dword [cheight]
	call print_int
	call println

	call init_vars

;	jmp exit
	call clean_buffer

	call draw_pad
	call draw_ball
	
	main_loop:
		
		call print_buffer
		;call sleep
		;cmp eax, 'h'
;		jnz main_loop

exit:
	push 0
	call [__imp__ExitProcess@4]

sleep:
	pushad	
		push dword 30
		call [__imp__Sleep@4]
	popad
	ret

draw_pad: ; old
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
			push word 0009h;
			push word '='
			push edi
			push ecx
			call draw_char_xy
			inc ecx
			cmp ecx, ebp
			jnz draw_pad_loop
	popad
	ret

draw_ball: ; old
	pushad
		fld dword[ballx]
		frndint
		fistp dword[ballxint]
		fld dword[bally]
		frndint
		fistp dword[ballyint]
		mov ecx, [ballxint]
		mov edi, [ballyint]

		push word 0004h; color of ball
		push word 'O'; char
		push edi
		push ecx
		call draw_char_xy
	popad
	ret

draw_char_xy: ; (int x, int y, char c, color col)
	pushad
		mov ecx, [esp + 20h + 4] ; x
		mov eax, [esp + 20h + 8] ; y
		mov bx, word[esp + 20h + 0Ch] ; char
		mov ebp, [cwidth]
		mul ebp
		add eax, ecx
		mov word[buffer + eax * 4], bx
		mov bx, word[esp + 20h + 0Eh] ; color
		mov word[buffer + eax * 4 + 2], bx
	popad
	ret 0Ch

section .rdata
tbuffer db "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",0
section .bss
write_region: resq 1

section .text

print_buffer: ; ~~~
	pushad
		push 5
		call print_int
		push write_region ; PSMALL_RECT lpWriteRegion // pointer to small rect
		push dword 00000000h; COORD dwBufferCoord upper left cell in the buffer
		push dword[COORD_buffer_size]; COORD buffersize 
		push buffer ; CHAR_INFO *lpBuffer  ;
		push dword[stdout]; HANDLE
		call [__imp__WriteConsoleOutputA@20]
		call print_dot
	popad
	ret
print_eax:
	push eax
	push eax
	call print_int
	pop eax
ret

clean_buffer:
	pushad
		push dword 7
		call print_int
		xor ecx, ecx
		mov eax, 5600 ; 4 * 70 * 20
		mov dx, word 65
		clean_buffer_loop1:
			mov word[buffer + ecx], dx
			inc ecx
			cmp ecx, eax
			jnz clean_buffer_loop1
		xor ecx, ecx
		mov eax, 1400
		mov dx, word 0CEh
		clean_buffer_loop2:
			mov word[buffer + ecx * 4], '.'; char
			mov word[buffer + ecx * 4 + 2], dx ; color
			inc ecx
			cmp ecx, eax
			jnz clean_buffer_loop2

		call print_dot
	popad
	ret

init_vars:
	pushad
		mov ax, word[cheight];
		shl eax, 10h
		mov ax, word[cwidth]
		mov [COORD_buffer_size], eax
		;   bottom right top left
		; 00 18 00 49 00 04 00 04h		
		mov ax, word[ctop]
		mov word[write_region + 2], ax	
		mov bx, word[cheight]
		dec bx
		add	ax, bx
		mov word[write_region + 6], ax
		mov ax, word[cleft]
		mov word[write_region + 0], ax
		mov bx, word[cwidth]
		dec bx
		add	ax, bx
		mov word[write_region + 4], ax
		mov eax, dword[write_region]
		call print_eax
		mov eax, dword[write_region + 4]
		call print_eax
		;padcoord
		mov eax, dword[cwidth]
		shr ax, 1
		mov dword[padcoord], eax
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
	pausestr db "pause",0
	intformat db " %X ",0

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
