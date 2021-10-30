	.file	"main.c"
	.text
	.p2align 4,,15
	.globl	test
	.type	test, @function
test:
.LFB0:
	.cfi_startproc
	xorl	%eax, %eax
	cmpl	%esi, %edi
	movl	$-1, %edx
	seta	%al
	cmovb	%edx, %eax
	ret
	.cfi_endproc
.LFE0:
	.size	test, .-test
	.section	.text.startup,"ax",@progbits
	.p2align 4,,15
	.globl	main
	.type	main, @function
main:
.LFB1:
	.cfi_startproc
	xorl	%eax, %eax
	ret
	.cfi_endproc
.LFE1:
	.size	main, .-main
	.ident	"GCC: (Ubuntu 7.5.0-3ubuntu1~18.04) 7.5.0"
	.section	.note.GNU-stack,"",@progbits
