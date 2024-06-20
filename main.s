
.section .data





.section .text



_start:
	call main
	movl %ebx, %eax
	movl $1, %eax
	int $0x80

.type main, @function
main:
	pushl %ebp
	movl %esp, %ebp
	subl $4, %esp
	movl $10, %ecx

	movl %ecx, -4(%ebp)
	movl $0, %ecx

	movl %ecx, %eax
	jmp .lmain
	.lmain:
	movl %ebp, %esp
	popl %ebp
	ret

	.globl _start
