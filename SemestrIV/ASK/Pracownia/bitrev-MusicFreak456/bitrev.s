	.text
	.globl	bitrev
	.type	bitrev, @function

bitrev:
	# ABCDEFHG - bity w przykładowym bajcie
	# 12345678 - bajty w 64 bitowej liczbie
	movq %rdi, %rax                  # ABCDEFGH
	movq $0xAAAAAAAAAAAAAAAA, %r8
	andq %r8, %rax                   # A_C_E_G_
	shlq %rdi                        # BCDEFGH_
	andq %r8, %rdi                   # B_D_F_H_
	shrq %rax                        # _A_C_E_G
	orq  %rdi, %rax                  # BADCFEHG
	movq %rax, %rdi                  # BADCFEHG
	movq $0xCCCCCCCCCCCCCCCC, %r8
	andq %r8, %rax                   # BA__FE__
	shlq $0x02, %rdi                 # DCFEHG__
	andq %r8, %rdi                   # DC__HG__
	shrq $0x02, %rax                 # __BA__FE
	orq  %rdi, %rax                  # DCBAHGFE
	movq %rax, %rdi                  # DCBAHGFE
	movq $0xF0F0F0F0F0F0F0F0, %r8    
	andq %r8, %rax                   # DCBA____
	shlq $0x04, %rdi                 # HGFE____
	andq %r8, %rdi                   # HGFE____
	shrq $0x04, %rax                 # ____DCBA
	orq  %rdi, %rax                  # HGFEDCBA
	movq %rax, %rdi                  # 87654321
	shrq $0x20, %rax                 # 8765
	rorw $0x08, %ax                  # 8756
	rorl $0x10, %eax                 # 5687
	rorw $0x08, %ax                  # 5678
	rorw $0x08, %di                  # 4312
	rorl $0x10, %edi                 # 1243
	rorw $0x08, %di                  # 1234
	shlq $0x20, %rdi                 # 12340000
	orq  %rdi,  %rax                 # 12345678
	ret

	.size	bitrev, .-bitrev
