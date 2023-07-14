extern	__imp__MessageBoxA@16
extern	__imp__ExitProcess@4
%assign MB_ICONINFORMATION 40h
global _main

section .text

_main:
	push	MB_ICONINFORMATION
	push	hello_title
	push	hello_message
	push	0
	call	[__imp__MessageBoxA@16]
	push	0
	call	[__imp__ExitProcess@4]

section .rdata
	hello_title	db	"First win32 GUI program",0
	hello_message	db	"Hello world!",0

end
