#pragma once

int is_rsp_16_byte_aligned(void);

//
// These are just aliases for is_rsp_16_byte_aligned. The arguments are ignored and
// are just there so we can test in the case of even and odd numbers of 
// stack-based arguments.
//
int is_rsp_16_byte_aligned_7_args(int rdi, int rsi, int rdx, int rcx, int r8, int r9, int stack0);
int is_rsp_16_byte_aligned_8_args(int rdi, int rsi, int rdx, int rcx, int r8, int r9, int stack0, int stack1);


