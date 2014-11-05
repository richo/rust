// Mark stack as non-executable
#if defined(__linux__) && defined(__ELF__)
.section        .note.GNU-stack, "", @progbits
#endif

        .text

#define SWAP_REGISTERS rust_swap_registers

// swap_registers(registers_t *oregs, registers_t *regs)
.globl SWAP_REGISTERS
SWAP_REGISTERS:

