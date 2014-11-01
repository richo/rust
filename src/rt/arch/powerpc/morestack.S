#if defined(__linux__) || defined(__FreeBSD__)
#define MORESTACK               __morestack
#define EXHAUSTED               rust_stack_exhausted
#else
#define MORESTACK               ___morestack
#define EXHAUSTED               _rust_stack_exhausted
#endif

#if defined(__linux__) || defined(__FreeBSD__)
	.hidden MORESTACK
#endif

#ifdef __ELF__
	.type MORESTACK,@function
#endif

.globl MORESTACK
.globl EXHAUSTED

MORESTACK:
	.cfi_startproc
	.cfi_endproc
