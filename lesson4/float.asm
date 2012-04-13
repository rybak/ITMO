extern	__imp__GetStdHandle@4
extern	__imp__WriteConsoleA@20
extern	__imp__ExitProcess@4
extern	__imp__wsprintfA
extern 	__imp__printf
%assign MB_ICONINFORMATION 40h
global _main

section .rdata
	funcarg dq 0.4
	
section .bss
stdin: resb 4
stdout: resb 4
written: resb 4
doublebuf: resb 8

section .text

_main:
	call init_stdin_stdout
	call print_funcarg
	call println

	;mov eax, [funcarg + 4]
	;push eax
	;mov eax, [funcarg]
	;push eax
	push dword[funcarg + 4]
	push dword[funcarg]
	call calc_func
	call print_double
	call println

exit:
	push 0
	call [__imp__ExitProcess@4]

section .bss
	eaxint: resb 4

section .text

calc_func:
	; ST0 many operations can work only with ST0, so we don't use it to store something important
	; ST1 : accumulator - we store result sum in here
	; ST2 : temporary for (x^k / k)
	; ST3 : (x^k)
	; ST4 : x^2 ; because $ k_i == k_{i-1} + 2 $, so we need to multiply by $ x^2 $
	; eax : k
	;          V CHECK esp offset
	fld qword[esp + 4]
	fld qword[esp + 4]
	fld qword[esp + 4]
	fld qword[esp + 4]
	fld qword[esp + 4]
	
	; calculating x^2
	fxch ST4
		fmul ST0, ST3
	fxch ST4

	mov ecx, 1000 ; iterations
	; x - x^3 / 3 + x^5 / 5 - ... +- x^k / k
	mov eax, 1 ; k
	xor edx, edx
	calc_func_loop:
		; x^{k_i} = x^{k_{i-1}} * x^2
		fxch ST3
			fmul ST0, ST4
	    fxch ST3
		; x^{k_i} / k_i
		fxch ST2
			; This code moves ST3 to ST0.
				;  | look carefully. `fstp` pops one number from stack
				;  V It causes _shift_ in ST(i) 
				fstp ST2
				; now old ST3 is stored in ST2 because of _shift_
				fld ST2; "push ST2 to fp stack"
			add eax, 2 ; k_i = k_{i-1} + 2 ; eax += 2
			mov dword[eaxint], eax ; FPU can't work with general registers, so we need to use memory
			fidiv dword[eaxint] ; ST0 /= eax
		fxch ST2
		; This code adds new summand (ST2) to accumulator (ST1) with sign, caused by boolean in edx.
		fxch ST1
			test edx, edx
			jnz calc_func_add
			calc_func_sub:
				fsub ST0, ST2
				jmp calc_func_after_add
			calc_func_add:
				fadd ST0, ST2
	    	calc_func_after_add:
    	fxch ST1
		
		xor edx, 1 ; edx = !edx
		dec ecx
		test ecx, ecx
		jnz calc_func_loop

	; return result in doublebuf
	fxch ST1
		fst qword[doublebuf] 
	fxch ST1
	ret 8

section .rdata
	doubleformat db "%.19f",0

section .text
print_double:
	push dword[doublebuf + 4]
	push dword[doublebuf]
	push doubleformat
	call [__imp__printf]
	add esp, 12
	ret

print_funcarg:
	push dword[funcarg + 4]
	push dword[funcarg]
	push doubleformat
	call [__imp__printf]
	add esp, 12
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

end
