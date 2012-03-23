extern	__imp__wsprintfA
extern	__imp__MessageBoxA@16
extern	__imp__ExitProcess@4
%assign MB_ICONINFORMATION 40h
global _main

section .text


_main:

	mov eax, 10 ; @before
	while0:
		cmp eax, 3
		jbe after0
    		dec eax; loop
		jmp while0
	after0:

	mov eax, 10 ; @before
	do_while:
		dec eax
		cmp eax, 3
		ja do_while

	mov eax, 10 ; @before
	; modified while v 1.0
	cmp eax, 3
	jbe after1
    while1:
		dec eax
		cmp eax, 3
		ja while1
	after1:

	; modified while v 2.0
	mov eax, 10 ; @before
	jmp while2cmp
	while2loop:
		dec eax
	while2cmp:
	cmp eax, 3
	ja while2loop

	; for(uint eax = 0; eax < 5; eax++)
	xor eax, eax
	for0:
		; code
		inc eax
		cmp eax, 5
		jb for0

	; for(uint eax = 5; eax > 0; eax--)
	; ver 1.0
	mov eax, 5
	for1_1:
		; code
		dec eax
		test eax, eax
		jnz for1_1

	; ver 2.0
	mov eax, 5
	for1_2:
		; code
		dec eax
		jnz for1_2

	; for(uint eax = 4; eax >= 0; eax--) // uint is good there for asm
    mov eax, 4
	for2:
		; code
		sub eax, 1; "dec eax" doesn't work! because `dec` doesn't setup CF
		jnc for2 ; check for carry-flag
	
	; for(uint eax = -5; eax != 0; eax++)
	mov eax, -5
	for3:
		
		
		inc eax ; `inc` setups ZF
		jnz for3

	push 0
	call [__imp__ExitProcess@4]

section .data
	hello_title	db	"Factorial",0
	buffer	db	"               ",0

end
