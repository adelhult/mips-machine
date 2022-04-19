# MIPS Virtual Machine

## Memory layout
The memory is 4096 bytes large by default.

| Address | Description                     |
|---------|---------------------------------|
| 0x7fff  | Stack base address              |
| 0x3000  | Heap base address               |
| 0x2000  | Data base address               |
| 0x0000  | Text (source code) base address |

When creating a new machine `$sp` will point to the stack base address and `$gp` will point to the data base address. Use the `.data` and `.text` assembly directives to place your program data in the right place. The program will start at the label "main" if there is one, otherwise it will start at the text base address.

## Assembly language
The following directives are supported (Note that all of them only support a single operand (no lists are allowed):
* ascii
* asciiz
* byte
* data
* globl (however, it does nothing)
* half
* space
* text
* word
* TODO: align
