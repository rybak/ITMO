extern	__imp__ExitProcess@4
%assign MB_ICONINFORMATION 40h
global _main

section .text

_main:
	mov eax, 42
    mov ecx, 10

	cout_int_loop:
	    mov edx, 0
	    div ecx
		push eax
			add dl, '0'
			mov ah, 2  ; 2 is the function number of output char in the DOS Services.
		    int 21h    ; calls DOS Services
		pop eax
		cmp eax, 0
	    jnz cout_int_loop
	push 0
	call [__imp__ExitProcess@4]

end

   ; assume number is in eax
