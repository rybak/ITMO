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

float_zero dd 0.0
cwidth dd 70 ; 70
cheight dd 20 ; 0014h ; 20
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
vx dd +0.5
vy dd -0.7

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
;	mov ecx, 200
	xor ecx, ecx
	main_loop:
		push ecx
		call print_int
		;call check_keyboard
		call move_pad
		call move_ball
		call clean_buffer
    	call draw_pad
		call draw_ball
		call print_buffer
		call sleep
		inc ecx
		jnz main_loop

exit:
	push 0
	call [__imp__ExitProcess@4]

check_keyboard:
	ret
move_pad:
	ret

move_ball:
	pushad
		call recalc_ballint
		mov eax, dword[ballxint]
		mov ebx, dword[ballyint]
		xor edx, edx ; y for top
		cmp edx, ebx
		jz change_y_direction

		mov ecx, [cheight] ; y for bottom row
		dec ecx

		cmp ecx, ebx
		jz change_y_direction
		jmp skip_change_y_direction
		change_y_direction: 
			fld dword[vy] ; ST1
			fld dword[float_zero]; ST0
				fsub ST0, ST1 ; now ST0 stores negative of vy
			fstp dword[vy]
			fstp dword[doublebuf]
		skip_change_y_direction:
		fld dword[vy]
		fld dword[bally]
			fadd ST0, ST1
		fstp dword[bally]
		fstp dword[vy]

		xor edx, edx ; x for left
		cmp edx, eax
		jz change_x_direction

		mov ecx, [cwidth] ; x for right column
		dec ecx

		cmp ecx, eax
		jz change_x_direction
		jmp skip_change_x_direction
		change_x_direction: 
			fld dword[vx] ; ST1
			fld dword[float_zero]; ST0
				fsub ST0, ST1 ; now ST0 stores negative of vy
			fstp dword[vx]
			fstp dword[doublebuf]
		skip_change_x_direction:
		fld dword[vx]
		fld dword[ballx]
			fadd ST0, ST1
		fstp dword[ballx]
		fstp dword[vx]

		; TODO
	popad
	ret


sleep:
	pushad	
		push dword 70 ; milliseconds
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

recalc_ballint:
	pushad
		fld dword[ballx]
		frndint
		fistp dword[ballxint]
		fld dword[bally]
		frndint
		fistp dword[ballyint]
	popad
	ret

draw_ball:
	pushad
		call recalc_ballint
		mov ecx, dword[ballxint]
		mov edi, dword[ballyint]
			push edi
			push ecx
			call print_2_ints
		push word 000Fh; color of ball
		push word 'O'; char
		push edi ; y
		push ecx ; x
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
test_buffer db "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",0
section .bss
write_region: resq 1

section .text

print_buffer: ; ~~~
	pushad
		push write_region ; PSMALL_RECT lpWriteRegion // pointer to small rect
		push dword 00000000h; COORD dwBufferCoord upper left cell in the buffer
		push dword[COORD_buffer_size]; COORD buffersize 
		push buffer ; CHAR_INFO *lpBuffer  ;
		push dword[stdout]; HANDLE
		call [__imp__WriteConsoleOutputA@20]
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
		xor ecx, ecx
		mov eax, 1400
		mov dx, word 0CEh
		clean_buffer_loop2:
			mov word[buffer + ecx * 4], '.'; char
			mov word[buffer + ecx * 4 + 2], dx ; color
			inc ecx
			cmp ecx, eax
			jnz clean_buffer_loop2
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
	intformat db " %d                        ",0
	coordformat db 0Dh, "(%d, %d) ",0
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

print_2_ints:
	pushad
		mov eax, [esp + 20h + 4]
		mov ecx, [esp + 20h + 8]	
		push ecx
		push eax
		push coordformat
		call [__imp__printf]
		add esp, 12
	popad
	ret 8

print_int:
	pushad
		mov eax, [esp + 20h + 4]
		push eax
		push intformat
		call [__imp__printf]
		add esp, 8
	popad
	ret 4

end
