# MIPS Virtual Machine
A small experiment to learn more about MIPS. This repo includes a
a virtual machine/mips simulator, an assembler and a REPL-style tool.

<a href="https://asciinema.org/a/lxhkcMpUJJndMzb6HQaPgvd4K" target="_blank"><img width="50%" src="https://asciinema.org/a/lxhkcMpUJJndMzb6HQaPgvd4K.svg" /></a>

You can load and assemble a file by running
```
$ machine program.asm
```

...or directly load a memory file
```
$ machine memory.mem
```

The implementation is loosely based on the [MIPS Architecture for Programmers
Volume II-A: The MIPS32 Instruction
Set Manual](https://s3-eu-west-1.amazonaws.com/downloads-mips/documents/MD00086-2B-MIPS32BIS-AFP-6.06.pdf). However, there is close to no 
test coverage so expect a lot of things to be broken.

## Memory layout
The memory is 4096 bytes large by default.

| Address | Description                     |
|---------|---------------------------------|
| 0x7fff  | Stack base address              |
| 0x4000  | Heap base address               |
| 0x3000  | Data base address               |
| 0x2FFC  | Terminal output address         |
| 0x0000  | Text (source code) base address |

When creating a new machine `$sp` will point to the stack base address and `$gp` will point to the data base address. Use the `.data` and `.text` assembly directives to place your program data in the right place. The program will start at the label "main" if there is one, otherwise it will start at the text base address.

## Output
If you wish to print something to the terminal. You can put an address to a zero terminated string at `0x244c..0x2fff` in the 
memory and it will be outputed to your stdout.

A simple print procedure could be implemented like this:
```s
        .data
msg:    .asciiz "Hello world!"

        .text 
main:
        la $t0, msg     # Load a pointer to the string in $t0 
        li $t1, 0x2FFC  # Load the address of the output in $t1
        sw $t0, 0($t1)  # Store the pointer to the string at 
                        # the addr of the output. 
```

# Assembly language
## Directives
The following directives are supported:
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

## Instruction
The following instruction are all implemented (although, definitely not tested).
* **add** rd, rs, rt
* **addi** rt, rs, imm
* **addiu** rt, rs, imm
* **addu** rd, rs, rt
* **and** rd, rs, rt
* **andi** rt, rs, imm
* **beq** rs, rt, offset
* **bne** rs, rt, offset
* **bgtz** rs, label
* **div** rd, rs, rt
* **mod** rd, rs, rt
* **divu** rd, rs, rt
* **modu** rd, rs, rt
* **j** label
* **jal** label
* **jalr** rd, rs
* **jr** rs
* **lb** rt, offset(base)
* **lbu** rt, offset(base)
* **lhu** rt, offset(base)
* **lui** rt, imm
* **lw** rt, offset(base)
* **nor** rd, rs, rt
* **xor** rd, rs, rt
* **or** rd, rs, rt
* **ori** rt, rs, imm
* **sb** rt offset(base)
* **sh** rt offset(base)
* **slt** rd, rs, rt
* **slti** rt, rs, imm
* **sltiu** rt, rs, imm
* **sltu** rd, rs, rt
* **sll** rd, rt, sa
* **srl** rd, rt, sa
* **sra** rd, rt, sa
* **sub** rd, rs, rt
* **subu** rd, rs, rt
* **sw** rt, offset(base)
* **li** li rs, imm (pseudo instruction)
* **la** la rs, label (pseudo instruction)
