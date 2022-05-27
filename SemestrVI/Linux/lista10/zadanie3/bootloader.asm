%define prompt_string `1) License\r\n2) Reboot\r\n`
%strlen prompt_len prompt_string

bits 16        ; tell nasm to generate 16-bit code

org 0x7c00     ; expecting to be run from 0x7c00
jmp main
nop

times 87 db 0x0 ; bios parameter block

main:
  mov sp, 0x8000 ; init stack pointer
main_loop:
  call print_prompt
  mov ah, 0x00
  int 0x16
  cmp al, 0x31 ; user entered '1'
  je print_license
  cmp al, 0x32 ; user entered '2'
  je reboot
  jmp main_loop

print_license:
  mov ax, 0x0
  mov es, ax
  mov ds, ax
  mov al, 0x1 ; read one sector
  mov ch, 0x0 ; cylinder 0
  mov cl, 0x2 ; sector 2
  mov dh, 0x0 ; head 0
  mov bx, 0x9000 ; buffer at es:bx
  
  mov ah, 0x02 ; funtion: read sectors from drive
  int 0x13
  mov cx, 0x1C8
  mov bx, 0x9000
  call print_string
  jmp main_loop

print_prompt:
  push dx ; dl contains boot disk nr
  mov cx, prompt_len
  mov bx, prompt
  call print_string
  pop dx
  ret
 
print_string:  ; cx: length, bx: start of the string
  push cx
  push bx

  ; get cursor position
  mov bh, 0x0
  mov ah, 0x03 
  int 0x10

  pop bx
  pop cx 

  mov ax, 0x0  ;
  mov es, ax   ; es:bp -- pointer to string
  mov bp, bx   ;
  mov bh, 0    ; page number
  mov bl, 0x07 ; color -- light grey on black
  mov al, 0x1  ; write mode
  mov ah, 0x13 ; funtion: write string
  int 0x10     ; 10h int
  ret

reboot:
  jmp 0xffff:0x0000

prompt db prompt_string

times 510 - ($-$$) db 0

dw 0xaa55
