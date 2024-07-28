#
# Check if rsp, at the point just prior to the call of this
# function, is 16 byte aligned
#

#
# aliases with prototypes that take multiple arguments.
#
    .global     is_rsp_16_byte_aligned_7_args
    .global     is_rsp_16_byte_aligned_8_args
is_rsp_16_byte_aligned_7_args:
is_rsp_16_byte_aligned_8_args:

    .global     is_rsp_16_byte_aligned
is_rsp_16_byte_aligned:
    mov         %rsp, %rdi              # points to return address
    add         $8, %rdi                # points to previous frame
    xor         %rax, %rax              # clear for return
    and         $0xf, %rdi              # should be 16 byte aligned
    sete        %al                     # return 1 if so
    ret
