# EasyAsm

EasyAsm is an on-device assembly language programming tool for the MEGA65 personal computer. It uses the MEGA65's operating system as part of a complete assembly language development workflow, and maintains a minimal memory footprint when not in use.

Features:
* Supports all 45GS02 CPU instruction types and addressing modes.
* Supports a subset of the [Acme cross-assembler](https://sourceforge.net/projects/acme-crossass/) syntax and features.
* Maintains a minimal memory footprint when running your program or during editing, so you can use the full power of your computer.
* Uses the MEGA65 screen editor's Edit mode for editing assembly language source code.
* Assembles to memory for testing, or to disk files for distribution.
* Can produce a single-file bootstrap loader as part of your program.
* Can store multiple memory segments compactly, with bootstrap code that positions segments automatically.
* Preserves source code in memory while running your program, and exits cleanly from your program back to the screen editor with source code restored. Can restore source code manually after an abnormal exit.

EasyAsm is released under the GNU Public License v3. See [LICENSE](LICENSE).

## Project status and roadmap

This is EasyAsm version 0.1.

**All 0.x versions are public beta test releases. Syntax and features may change before the 1.0 release.** Please [file issues](https://github.com/dansanderson/easyasm65/issues) to report bugs, request features, or provide feedback. Thank you for trying EasyAsm!

Release 0.1:
* Single segment programs only: one `!to` and up to one `* = ...` per source file

Roadmap:
* `!binary`
* `!source`
* `runnable` with custom PC (single relocatable segment)
* Multi-segment programs with `!to "...", cbm` and `raw`; multiple `* = ...` allowed; gaps filled on disk
* Multi-segment programs with `!to "...", runnable`; segments relocated by bootstrap, no gaps on disk
* Multi-file output, multiple `!to` allowed
* Freezable mode that uses bank 5 if it's already there
* List symbol definitions
* List disassembly alongside source code
* Automatic 16-bit branch instructions

## An important note

**Save your work to disk, early and often.**

Writing a program for a microcomputer using the microcomputer itself comes with the inherent risk that a bug in your program will interfere with your programming environment. EasyAsm preserves your source code in memory while you are testing your program, but this cannot be guaranteed to work if the program does something unexpected.

By design, EasyAsm does *not* force you to save your work to disk before testing your program. Please remember to do this yourself.

## Quick reference

* `RUN "EASYASM"` : install EasyAsm; erases program memory

* `SYS $1E00` : assemble source code to memory, then run it
* `SYS $1E03` : assemble source code to disk, per `!to` directive
* `SYS $1E06` : restore source code after abnormal program exit

* `EDIT ON` / `EDIT OFF` : enable/disable Edit mode; prompt is `OK.` when enabled
* `DSAVE "..."` : save source file to disk; do this with Edit mode *enabled!*
* `DSAVE "@..."` : save source file to disk, overwriting an existing file
* `MONITOR` : start the Monitor from the `READY.`/`OK.` prompt

Press the **Help** key to display a brief help message.

## Using EasyAsm

To activate EasyAsm, load and run the `EASYASM` program:

```
RUN "EASYASM"
```

EasyAsm installs itself in the MEGA65's upper memory, clears program memory, and returns to the `READY.` prompt.

To create your assembly language source code, switch the MEGA65 to Edit mode:

```
EDIT ON
```

The prompt changes from `READY.` to `OK.` to indicate that you are in Edit mode. This mode lets you edit a file of text data as if it were a BASIC program, including line numbers. All BASIC commands are still available for direct entry, but now when you type a line with a line number, the MEGA65 understands it as text instead of a line of a BASIC program.

Enter a simple assembly language program:

```
10 !TO "SIMPLE", RUNNABLE
20
30   INC $D020
40   RTS
```

> **Tip:** To insert a blank line in Edit mode (such as line 20 in this example), type the line number, press **Shift+Space**, then press **Return**.

Save the source file to disk:

```
DSAVE "SIMPLE.S"
```

> **Tip:** Use `.S` at the end of the source filename to indicate that it is assembly language source. In this case, I saved the source file as `SIMPLE.S`, so I can use the name `SIMPLE` for my program name.

Assemble and run the program:

```
SYS $1E00
```

EasyAsm assembles the program, finds no errors, installs it into memory, then runs the program. The program changes the border color, then exits. EasyAsm notices the program has exited using `rts`, and restores the source code into the editor's memory. View the listing:

```
LIST
```

To assemble the program to disk:

```
SYS $1E03
```

EasyAsm assembles the program again, finds no errors, then creates a new PRG file on disk. The `!TO` directive in the program tells EasyAsm that the PRG's filename is `SIMPLE`, and that EasyAsm should make the program "runnable."

Exit out of Edit mode, then load and run the runnable program from disk:

```
EDIT OFF

RUN "SIMPLE"
```

> **Note:** Source code and programs share the same memory. Be sure to save your source code file before loading a program from disk.

To load your source file back into memory, return to Edit mode, then `DLOAD` the file `SIMPLE.S`:

```
EDIT ON

DLOAD "SIMPLE.S"
```

Assuming your program hasn't erased EasyAsm from memory, it is still present, and you can continue working. See below for a description of EasyAsm's memory requirements.

## How to stop a running program

Because source code and programs often share the same memory, EasyAsm copies your source file to another memory location before it assembles and runs the program. When you assemble and run your program with `SYS $1E00`, EasyAsm watches for the program to exit using the `rts` instruction, then copies the source file back into program memory so you can continue to edit it.

That's nice, but it's not always possible—or even desired. A typical machine code program never exits. A broken machine code program might get stuck before it can exit.

Try entering a slightly different program:

```
10 !TO "FOREVER", RUNNABLE
20
30 LOOP:
40   INC $D020
50   JMP LOOP
```

Assemble and run the program. This program changes the border color repeatedly, in an infinite loop. It does not exit.

To interrupt this program and return to BASIC, hold **Run/Stop** and press **Restore**. The program stops, the screen clears, and the `OK.` prompt of Edit mode reappears.

Now re-enable Edit mode, then type `LIST`:

```
EDIT ON
LIST
```

Uh oh, that's not the program source file! EasyAsm did not see the program exit with the `RTS` instruction, so it didn't get a chance to restore the source file.

When this happens—and it will happen often—use this command to tell EasyAsm to bring back the source file:

```
SYS $1E06
```

## Breaking to the Monitor

EasyAsm is designed so that you can use all of the tools available in the MEGA65 operating system as part of your development workflow. This includes the Monitor, a powerful tool for inspecting and experimenting with the state of a machine code program.

Type the `MONITOR` command to start the Monitor at the `OK.` prompt. (Naturally, this also works at the `READY.` prompt when not in Edit mode.)

```
MONITOR
```

The Monitor accepts commands through the screen editor. A typical command is a single letter followed by some arguments. You can use the Monitor to inspect CPU registers (`R`) and memory locations (`M1600`), examine code (`D2014`), and call subroutines (`J2032`). Addresses and values are specified in hexadecimal.

To exit the Monitor, enter the `X` command. This returns you to the `OK.` (or `READY.`) prompt.

The Monitor can be especially useful for debugging an assembly language program thanks to a special feature of the MEGA65 operating system. When a program executes a `BRK` instruction, the operating system halts the program, and starts the Monitor. The Monitor displays the status of the CPU registers as they were at that exact point in the program. You can continue to enter Monitor commands to inspect the state of the system. When you're finished, use the `G` command without an argument to continue the program where it left off.

```
10 !TO "BRKDANCE", RUNNABLE
20
30   INC $D020
40   BRK
50   DEC $D020
60   RTS
```

The Monitor is powerful, but sometimes tricky to use. For example, it is not always possible to resume a paused program, and you may have to exit the Monitor with the `X` command. In this case, you must ask EasyAsm to restore the program source file, as we did after interrupting the program with Run/Stop-Restore.

```
SYS $1E06
```

## Edit mode tips and tricks

* Edit mode uses numbered line editing just like BASIC:
  * To add a line, type a line number not already in use, followed by the line's text.
  * To delete a line, type the line number, then press Return.
  * To insert a line between two other lines, choose a line number between those line numbers.
  * To display all lines in order, type `LIST`. To display ranges of lines: `LIST 100-200` To display lines, pausing for each page: `LIST P`
  * Use the F9 and F11 keys to display the listing as a scrolling display.
  * Use the built-in tools like `AUTO` and `RENUMBER` to manage line numbers.

* Unlike a BASIC program, Edit mode does *not* preserve line numbers when saving files to disk. When you load your source file again later, it may have different line numbers assigned automatically. EasyAsm does not use line numbers for anything—it relies on labels to identify points in the program code—so this typically does not matter as it does in BASIC.

* Press **Mega+Shift** to toggle lowercase text mode, if you prefer lowercase for assembly language source files. In EasyAsm, labels and strings are case sensitive. Instructions, directives, and hexadecimal literals are not case sensitive.

* Edit mode supports entering PETSCII codes into strings similarly to BASIC. When you enter a double-quote (`"`), the editor goes into "quote mode," and PETSCII control codes can be typed as special characters. This works well for string literals in EasyAsm assembly language programs.

* To enter a blank line, type the line number, press Shift+Space, then press Return. (This is not normally possible in BASIC.)

A few ways the MEGA65 behaves differently in BASIC mode vs. Edit mode:

| | BASIC mode | Edit mode |
|-|-|-|
| Prompt | `READY.` | `OK.` |
| A line contains... | BASIC commands | Text |
| File type for DLOAD and DSAVE | PRG | SEQ |
| Saves line numbers to disk | Yes | No |
| Allows blank lines (with Shift+Space) | No | Yes |
| To display a file without loading it: | `LIST "FNAME"` | `TYPE "FNAME"` |


## How EasyAsm uses memory

EasyAsm tries to maintain a minimal memory footprint while you are editing your source code, and while your program is running. This allows you to use all the tools at your disposal for editing, and allows your program to use most of the computer, while still retaining a useful on-device workflow.

Of course, EasyAsm has to live somewhere. This is what EasyAsm needs:

* EasyAsm reserves the memory ranges **$1E00-$1EFF** (256 bytes of bank 0) and **$8700000-$87FFFFF** (1 megabyte of Attic RAM). If your program overwrites any of this memory, you will need to reload EasyAsm, and any stashed source code data may not be recoverable.
* EasyAsm reserves the right to overwrite **$50000-$5FFFF** (all 64KB of bank 5) when you invoke EasyAsm. Your program can use this memory while it is running, but the state of this memory may change when EasyAsm is running.

As a safety precaution, EasyAsm will not assemble to addresses $1E00-$1EFF when assembling to memory. This restriction does not apply when assembling to disk.

EasyAsm uses program memory ($2001-$F6FF) in three ways:

1. When you load and run `EASYASM` at the start of a session, it overwrites program memory with its installer code. After it is installed, it clears program memory.
2. While you are editing source code in Edit mode, your source code occupies program memory. EasyAsm expects to find it there when you invoke the assembler.
3. To test your program, EasyAsm stashes the source code into Attic RAM, assembles the program to machine code, and installs your machine code in program memory. It runs from this location, as it would when a user loads and runs your program. If the program exits with `RTS`, EasyAsm copies the source code back into program memory (overwriting the assembled program) and restores the editing environment.

> **Note:** EasyAsm keeps its own code in Attic RAM while not in use, and copies itself to bank 5 when performing operations. Attic RAM is not included in MEGA65 Freezer states. It is safe to use the Freezer during an EasyAsm session, but if you start a new session restored from a freeze state, you must run the EasyAsm installer again.


## Assembly language syntax

Wherever possible, EasyAsm uses syntax compatible with the [Acme cross-assembler](https://sourceforge.net/projects/acme-crossass/), so you can enter example code from books and articles for that assembler without changes. Only a subset of Acme features are supported. Source code features exclusive to EasyAsm extend the Acme syntax.

### Comments

A semicolon starts a line comment. EasyAsm ignores everything from the semicolon to the end of the line.

```
; This is a comment.

loop:  ; beginning of the loop
  inc $d020  ; increment the background color
```

(Naturally, a semicolon that appears inside a character literal or string is not considered a comment.)

### Instructions

Instruction names (opcode mnemonics) are canonical for the 45GS02.

```
adc   bbs1  bvs   eor   lbvs  pla   rts   stz
adcq  bbs2  clc   eorq  lda   plp   sbc   tab
and   bbs3  cld   inc   ldq   plx   sbcq  tax
andq  bbs4  cle   inq   ldx   ply   sec   tay
asl   bbs5  cli   inw   ldy   plz   sed   taz
aslq  bbs6  clv   inx   ldz   rmb0  see   tba
asr   bbs7  cmp   iny   lsr   rmb1  sei   trb
asrq  bcc   cmpq  inz   lsrq  rmb2  smb0  tsb
asw   bcs   cpq   jmp   map   rmb3  smb1  tsx
aug   beq   cpx   jsr   neg   rmb4  smb2  tsy
bbr0  bit   cpy   lbcc  nop   rmb5  smb3  txa
bbr1  bitq  cpz   lbcs  ora   rmb6  smb4  txs
bbr2  bmi   dec   lbeq  orq   rmb7  smb5  tya
bbr3  bne   deq   lbmi  pha   rol   smb6  tys
bbr4  bpl   dew   lbne  php   rolq  smb7  tza
bbr5  bra   dex   lbpl  phw   ror   sta
bbr6  brk   dey   lbra  phx   rorq  stq
bbr7  bsr   dez   lbsr  phy   row   stx
bbs0  bvc   eom   lbvc  phz   rti   sty
```

EasyAsm supports explicit 16-bit branch instructions, using Acme syntax: `lbcc`, `lbcs`, `lbeq`, `lbmi`, `lbne`, `lbpl`, `lbra`, `lbsr`, `lbvc`, `lbvs` Neither EasyAsm nor Acme support automatic promotion of 8-bit branch instructions to 16-bit. The assembler will report an error if an 8-bit branch is too short.

EasyAsm supports both `cpq` (Acme) and `cmpq` (Monitor, MEGA65 manual) and spellings of that instruction.

Instructions that operate on the accumulator or quad register as an alternative to a memory location are sometimes spelled with an `A` or `Q` in the place of an argument. Omit these for EasyAsm, as you would for Acme or the Monitor. For example, to logical-shift right the accumulator, use `lsr`, not `lsr a`.

The following are examples of syntax for the addressing modes. See the [MEGA65 Compendium](https://files.mega65.org/?id=d668168c-1fef-4560-a530-77e9e237536d) for a complete description of which instructions support each addressing mode.

* `lda #$07` : immediate
* `lda $fe` : zero page direct
* `lda $1600` : address direct
* `lda $fe,x` : zero page direct, X-indexed
* `lda $1600,x` : address direct, X-indexed
* `lda ($fe,x)` : zero page indirect X-indexed
* `lda ($fe),y` : zero page indirect, Y-indexed
* `lda ($05,sp),y` : stack indirect, Y-indexed
* `lda [$fe],z` : 32-bit zero page indirect, Z-indexed
* `lsr` : "accumulator addressing" (implied "A" argument)
* `lsrq` : "quad register addressing" (implied "Q" argument)
* `jmp ($fffe)` : indirect jump

Multiple instructions can appear on a single line, separated by colons. `lda #0 : sta $d020`

### Values

EasyAsm uses 32-bit signed integer values in expressions for arguments. When a value is assembled into an instruction or used by an assembler directive, it must be in the range expected by how it is used:

* Address argument
  * Must be a value in the range 0 ($0000) to 65,535 ($FFFF).
  * Assembled to two bytes in little-endian order: $FFFC assembles to $FC $FF.
* Immediate mode argument
  * Expected to be either a signed value in the range -128 to 127, or an unsigned value in the range 0 to 255.
  * It is assembled to one byte, using two's complement for negative values: 255 and -1 both assemble to $FF.
* Assembler directives that take numeric arguments have their own range requirements.
  * The `!32` directive will render a complete 32-bit value as two's complement little-endian: $FFFFFFFC and -4 both assemble to $FC $FF $FF $FF.

Number literals:

* Decimal: `12345`, `-27`
* Hexadecimal: `$FFFC`
  * Letter digits are case insensitive: `$fffc` and `$FFFC` are equivalent.
* Binary: `%0010110`
  * Binary literals can also use `.` for `0` and `#` for `1`, for more easily representing bitmap graphics: `%..#.##.`
* PETSCII character: `'p'`
  * When used with the `!scr` directive, this is translated into a screen code if applicable. Otherwise it is interpreted as the PETSCII code of the character that appears in the source file.
  * EasyAsm does not support backslash-escape sequences. To represent the PETSCII code for the single-quote character `'`, put it in single quotes: `'''`. (As an alternative, the PETSCII code for a single-quote character is 39 ($27).)

To specify a negative number literal, use negation syntax: `-27` If a literal specifies all 32 bits and bit 31 is high, this will be treated as a two's complement negative number: `$FFFFFFFC` Negating such a literal will negate the number: `-$FFFFFFFC` and `4` are equivalent.

Some assembler directives accept text strings as arguments, surrounded by double-quotes: `"string"` These are not values and cannot be used in expressions. The `!pet` and `!scr` directives render strings into sequences of character bytes (PETSCII or screen codes, respectively).

### Entering PETSCII control codes

Similar to editing a BASIC program, you can enter PETSCII control codes inside double-quoted strings by typing the key that would normally perform that code, such as cursor movement, color changes, or clearing the screen.

When you type a double-quote character (`"`), the editor switches to *quote mode,* and keys that would type PETSCII control codes instead enter inverse symbols that represent those codes in the string. When you type the closing double-quote, the editor switches off quote mode.

Double-quoted string arguments to `!pet` can contain PETSCII control codes entered in this way. In the following example, type `{CLR}` as Shift + Clr, and `{WHT}` as Ctrl + 2.

```asm
  jsr primm
  !pet "{CLR}{WHT}Hello world!",0
```

Quote mode is also active temporarily for spaces inserted with the Shift + Inst key. This is convenient for inserting characters within quoted strings without having to type double-quotes just to activate quote mode.

When you enter a line by pressing the Return key, only PETSCII codes inside of double-quotes are interpreted as PETSCII codes. (This is true when editing BASIC programs as well as in Edit mode.) A PETSCII code outside of double-quotes is interpreted by the editor as a symbol character.

For EasyAsm, this means that it is not practical to enter PETSCII codes as single-quoted character literals. Instead, use number literals or named constants to refer to PETSCII control codes.

```asm
chr_clr = 147
chr_esc = 27

  lda #chr_clr
  jsr bsout

  lda #chr_esc
  jsr bsout
  lda #'5'
  jsr bsout
```

### Labels

A label is a name associated with a value. Unlike a "variable" in some programming languages, a label's value does not change: the assembler determines its value by examining the source code, then uses that value wherever the label appears.

A label can be assigned a value using the `=` sign, like so:

```
bgcolor = $d020

  inc bgcolor
```

A label can also be assigned the address of an instruction in the program:

```
loop:
  inc bgcolor
  jmp loop
```

The colon `:` is optional.

A global label name must start with a letter, either lowercase or uppercase. Subsequent characters can be either a letter (lowercase or uppercase), a number, back-arrow (ASCII underscore), a dot (`.`), or Mega + `@` (an underscore-like character in PETSCII).

> **Tip:** If you choose to use uppercase + graphics text mode for assembly programming, I recommend limiting the use of shifted letters in label names. They're allowed because they are uppercase letters in lowercase text mode, but they are difficult to remember how to type, and some are difficult to distinguish. For example, Shift + G and Shift + T both appear as vertical lines in uppercase text mode.

### "Cheap" local labels

EasyAsm supports global labels and Acme-style "cheap" local labels. A global label must be unique across all labels in the source file. A "cheap" local label starts with an `@` sign, and is only valid up to the next global label. This allows subroutines with global names to have local labels with useful names that can be reused by other subroutines.

```
flash_border:
  lda #15
@loop:
  sta $d020
  dec
  bne @loop
  rts

flash_background:
  lda #15
@loop:
  sta $d021
  dec
  bne @loop
  rts
```

A "cheap" local label name starts with `@` followed by a letter. Subsequent characters follow the rules for labels.

### Relative labels

EasyAsm supports relative labels for code. A relative label is a sequence of one or more minus `-` signs, or one or more plus `+` signs: `-` `--` `---` `+` `++` `+++` When an instruction uses a relative label as an address argument, it refers to the closest code label with that name in the source code. A relative label of minus signs scans upward to the closest assignment, and a relative label of plus signs scans downward.

```asm
flash_border:
  lda $d020
  cmp #3
  beq +
  lda #15
- sta $d020
  dec
  bne -
+ rts

flash_background:
  lda #15
- sta $d021
  dec
  bne -
  rts
```

Relative labels can only be used directly as instruction address arguments, and cannot participate in expressions.

### Expressions

An argument's value can be calculated using a mathematical expression. An expression can be a number literal, a label, or one or two expressions combined with a mathematical operator. EasyAsm calculates the value of the expression when assembling the program.

EasyAsm supports the following operators, listed in precedence order:

| Syntax | Definition | Notes |
|-|-|-|
| `!v` | Bitwise complement | |
| `v ^ w` | To the power of | Up-arrow |
| `-v` | Negate | |
| `v * w` | Multiply | |
| `v DIV w` | Integer divide | |
| `v % w` | Remainder | |
| `v + w` | Add | |
| `v - w` | Subtract | |
| `v << w` | Shift left | |
| `v >> w` | Arithmetic shift right | |
| `v >>> w` | Logical shift right | |
| `<v` | Low byte of | |
| `>v` | High byte of | |
| `^v` | Bank byte of | Up-arrow |
| `^^v` | Megabyte byte of | Up-arrow |
| `v & w` | Bitwise And | |
| `v XOR w` | Bitwise Exclusive Or | |
| `v \| w` | Bitwise Or | Mega + period |

EasyAsm does not support fractional number values, and so does not have a fractional division operator (`/`).

EasyAsm does not support Boolean values, and so does not have conditional operators. (This would primarily be used with conditional assembly, which EasyAsm also does not support.)

The "megabyte" operator `^^` is exclusive to EasyAsm, as a companion to low (`<`), high (`>`), and bank (`^`) byte selectors. The megabyte operator selects the highest byte of a 32-bit value.

The power operator is right-associative: `x^y^z` = `x^(y^z)`

To type the power operator, type the up-arrow character (next to the Restore key). To type the bitwise-or operator, type Mega + period.

> **Tip:** Enter the command `FONT A` and switch to lowercase mode (Mega + Shift) to display certain characters as their ASCII equivalents. To type the ASCII-specific characters:
> * Back-arrow: underscore (`_`)
> * Up-arrow: caret (`^`)
> * Mega + back-arrow: backtick
> * Mega + period: vertical bar (`|`)
> * Mega + comma: tilde (`~`)
> * Mega + forward-slash (or £): backslash (`\\`)
> Remember that you must be in lowercase mode with this font setting to see the ASCII characters.

### Parentheses and brackets

Both parentheses `(` `)` and square brackets `[` `]` can be used to group terms in expressions to influence the order of operations. The two bracket types can be used interchangeably, but must be used in matching pairs.

The indirect addressing modes also use brackets in their syntax. In this case, the type of bracket selects the addressing mode (as above). Addressing mode brackets are unambiguous from mathematical expression brackets by the following rule:

*If the entire address argument expression is surrounded by brackets, the outermost brackets are part of the addressing mode.*

Examples:

```
lda ($fe),y          ; indirect ZP, Y-indexed
lda ($ff-1),y        ; indirect ZP, Y-indexed
lda (($ff-1)-foo),y  ; indirect ZP, Y-indexed
lda ([$ff-1]-foo),y  ; indirect ZP, Y-indexed
lda ($ff-1)-foo,x    ; direct ZP, X-indexed
lda [$ff-1],y        ; 32-bit indirect ZP, Y-indexed
lda ([$ff-1]),y      ; indirect ZP, Y-indexed
```

### Zero-page address arguments

The absolute addressing mode and the base page addressing mode have the same assembler syntax, despite being separate instructions with separate behaviors. For example:

```
lda $fe            ; load index $fe off the base page
lda $16fe          ; load absolute address $16fe
```

If the address expression evaluates to a value larger than 255, then EasyAsm uses absolute addressing.

If the address expression evaluates to a value between 0 and 255, EasyAsm disambiguates using the following procedure:

1. If the address expression is a value literal *or* a symbol defined using a value literal, and the value literal has one or more leading zeroes, EasyAsm uses absolute mode: `$00fe` Otherwise it uses base page addressing: `$fe`
2. If the value can be calculated in the first pass of the assembler, i.e. all symbols in the expression are defined earlier in the source text, EasyAsm uses base page addressing: `$fc+earlierlabel`
3. Otherwise EasyAsm assumes absolute addressing, to be calculated in a subsequent pass: `$fc+laterlabel`

To clarify potential confusion in your source code, you can suffix the instruction mnemonic with `+1` to force base page addressing, or `+2` to for absolute addressing. If base page addressing is requested and the value is larger than 255, EasyAsm reports the error.

```
lda+1 $00fe        ; force base page addressing: $fe
lda+2 $fe          ; force absolute addressing: $00fe

varname = $fe
lda+2 varname      ; force absolute addressing: $00fe
```


## The program counter

EasyAsm maintains a *program counter* (PC), the address where the next assembled instruction will appear in memory. As the assembler encounters each CPU instruction, it generates the machine code for the instruction, places it at the address in the program counter, then increments the program counter by the size of the instruction.

A program can refer to the current value of the PC in code using an asterisk `*`. A program can use the value in an expression. It can also assign a new value to the PC, similar to a label, to influence the assembly process.

```
* = $c000    ; Set the PC to $C000

change_background:   ; change_background = $c000
  inc $d020   ; Assembles to three bytes
  rts         ; Assembles to one byte

blah = *     ; Label "blah" is now set to $c004

blah:        ; (This does the same thing.)
  ; ...
```

A typical program does not have to change the PC. It can use the `!to "...", runnable` directive to start a runnable program to be saved to the given filename. This sets the PC automatically to an address appropriate to the bootstrap code that EasyAsm puts at the beginning of the program data.

### Segments

A set of instructions and data assembled to a contiguous region of memory is known as a *segment*. A typical program consists of one segment.

When program code assigns a new value to the program counter and this is followed by an instruction, EasyAsm starts a new segment at that address. It keeps track of all the contiguous segments formed by assembled instructions and data.

### "Pseudo-PC"

EasyAsm currently does not support Acme's "pseudo-PC" feature, which allows for a section of code to be assembled as if the PC were a particular value, but the segment is stored in the PRG file consecutively with the surrounding code. (It would be the program's responsibility to copy the code to the correct location in order to run it.)

As an alternative, EasyAsm has a way to assemble multiple segments to disk as a single compact file, with a generated bootstrap routine that relocates all segments to their final memory locations. See the explanation of `!to "...", runnable`, below.


## Assembling to disk

When assembling to memory, EasyAsm writes each segment to the requested memory locations.

When assembling to disk, EasyAsm offers several options: writing a contiguous file, writing segments to separate files, or bundling segments into a runnable program.

### Writing a contiguous file

The `!to` directive sets a filename and writing mode for assembling subsequent code to disk. Using the `cbm` mode, this creates a PRG file, with the address of the first assembled instruction that follows as the load address. This requires that the source code set the PC before the first instruction, so the `!to` directive knows the starting address.

```
!to "routines", cbm
* = $a000

change_background:
  inc $d020
  rts
```

If a single `!to` directive is followed by more than one segment, EasyAsm creates a PRG file that starts at the segment with the lowest address. Each segment is followed by a region of empty data, such that each segment is written into its starting address when the file is loaded. This can be useful if the separator regions are expected to be small, such as to align code to specific addresses.

This example generates one file with the first segment, a region of empty data to align the next segment, followed by the second segment.

```
!to "routines", cbm

* = $7400
  jsr change_border
  ; ...
  rts

; (...empty data generated here...)

* = $7f00
change_border:
  inc $d020
  rts
```

If the gap between segments is large, this could result in excess use of disk space and loading time. EasyAsm offers other options to avoid this scenario.

### Writing segments to separate files

EasyAsm source code can request that segments be written to different files by providing the `!to "...", cbm` directive more than once. It is the program's responsibility to load each segment file from disk into the appropriate memory location.

This example generates two files, one for each segment.

```
!to "screen", cbm
* = $7400

  ; (Some disk code to load the "routines" file...)

  jsr change_border
  ; ...
  rts

!to "routines", cbm
* = $a000

change_border:
  inc $d020
  rts
```

### Generating a runnable program

In EasyAsm, the `!to "...", runnable` directive creates a program file that starts with a bootstrap routine. A user can load and `RUN` this program from the `READY.` prompt. The program starts with the first instruction after the `!to "...", runnable` directive.

`!to "...", runnable` sets the PC automatically. It is not necessary to set the PC explicitly in this case.

```asm
!to "myprog", runnable

  inc $d020
  rts
```

If the program redefines the PC before the first instruction, the segment will be located as requested, and the bootstrap routine will start at that segment's start address.

```asm
!to "myprog", runnable

* = $7000

  inc $d020
  rts
```

If `!to "...", runnable` is followed by more than one segment, EasyAsm generates additional code that relocates all of the segments to their starting addresses, as part of the bootstrap process. The first segment that follows the `!to` statement is assumed to be the start of the program. The generated program behaves similarly to assembling to memory, and only requires loading and running one file, with no disk space or load time spent on empty data regions between segments.

```
!to "program", runnable

  jsr change_border
  ; ...
  rts

* = $7000

change_border:
  inc $d020
  rts
```

## Using disk drives

Assembler directives that refer to files on disk (`!to`, `!source`, `!binary`) always use the current "default disk" unit. BASIC 65 initially sets this to unit 8. You can change the default disk with the `SET DEF` command.

```basic
SET DEF 9
```

BASIC disk commands use this default, and allow overriding the default with the `U` argument. EasyAsm does not currently have a way to overriding the default selectively. Take care to set the default disk unit when managing files across multiple disks.

```basic
SET DEF 9
EDIT ON
DLOAD "MYPROG.S",U8  : rem: Loads from unit 8
SYS $1E00            : rem: Assembles to unit 9
```


## Assembler directives (pseudo-operands)

EasyAsm supports the following assembler directives.

### `!to`

```
!to "...", <mode>
```

Sets the output file and mode when assembling to disk. `<mode>` can be `cbm` (PRG with address), `raw` (PRG without address), or `runnable` (bootstrap routine).

EasyAsm directives that refer to files on disk use the current "default disk" unit. Use the `SET DEF` command to change the default disk.

### `!byte` or `!8`, `!word` or `!16`, `!32`

```
!byte <val> [,<val> ...]
```

Assembles value expressions to data. `!byte` (or synonym `!8`) accepts 8-bit values. `!word` (or synonym `!16`) accepts 16-bit values. `!32` accepts 32-bit values. Multi-byte values are rendered as little-endian.

### `!fill`

```
!fill <amt> [, <val>]
```

Assembles a given number of bytes of data. The default value is $00. If a byte value is provided, that value is used for every byte.

### `!pet`, `!scr`

```
!pet "..." [, "..."]
!scr "..." [, "..."]
```

Assembles arguments that can include character strings as a series of bytes. `!pet` renders character strings and character literals as PETSCII codes, as written directly in the PETSCII source file. `!scr` converts PETSCII characters in double-quoted strings and single-quoted character literals to VIC screen codes. Number expressions are byte values and are rendered verbatim in either case.

String data does not automatically add a null terminator. If a null terminator is desired, end the directive with a 0 byte.

```
!pet "hello world!", 0
```

EasyAsm does not support backslash-escape character sequences. To include a double-quote character in a `!pet` or `!scr` directive, end the string, then use a byte value of 34 ($22).

```
!pet "i said, ", 34, "hello world!", 34, 0
```

If a PETSCII control code appears in a string or character literal passed to `!scr`, it is converted to the screen code for a space ($20).

### `!source`

```
!source "..."
```

Loads a source file from disk to be assembled as code at the given location.

EasyAsm directives that refer to files on disk use the current "default disk" unit. Use the `SET DEF` command to change the default disk.

### `!binary`

`!binary "..." [, <size> [, <skip>]]`

Loads a binary file from disk to be assembled as data at the given location. Without arguments, the entire file is included. `<size>` limits the number of bytes to include. `<skip>` starts assembling data that many bytes into the file.

EasyAsm directives that refer to files on disk use the current "default disk" unit. Use the `SET DEF` command to change the default disk.

### `!warn`

`!warn "..." [, "..."]`

Prints a given PETSCII message during the assembly process. Arguments are string literals or number expressions. A number expression argument is printed as both decimal and hexadecimal, useful for debugging, such as: `123 ($7B)`

No other Acme-style message directives are supported. They're only useful for conditional assembly, which is not supported.


## Acme assembler compatibility

EasyAsm tries to provide a subset of the features of the Acme assembler, using compatible syntax. Not all features of Acme are supported, and a few features are exclusive to EasyAsm.

### Acme features that are not supported

EasyAsm has the following limitations compared to the Acme assembler.

* Per-file source size limit of 44 KB
* No macros
* No conditional or looping assembly
* No symbol list output
* No fractional number values
* No fractional division operator
* No Boolean values
* No conditional expressions
* No value lists (used as arguments to macros)
* No zones (but "cheap" local symbols are supported)
* No CPUs other than the 45GS02
* No assembler directives (pseudo-ops) or directive aliases other than those listed
* No `0x` and `0b` syntax for hex and binary literals (use `$...` and `%...`)
* No octal literals
* No way to set a "pseudo-PC"

### Features exclusive to EasyAsm

Here is a quick summary of features available in EasyAsm that are not available in Acme:

* PETSCII character encoding of source files
* Single-quote character literal: `'''`
* Double quote characters not allowed in double-quoted strings
* `!to "...", runnable`
* Assemble to multiple files from one source file, with multiple `!to` directives
* The "megabyte" (`^^`) selector operator


## Building EasyAsm

Building EasyAsm from source requires the following tools:

* [Acme assembler](https://sourceforge.net/projects/acme-crossass/)
* [GNU Make](https://www.gnu.org/software/make/)
* [Python 3](https://www.python.org/)
* [The d64 Python package](https://pypi.org/project/d64/)
    * To install: `python3 -m pip install d64`
* `petcat`, [from VICE](https://vice-emu.sourceforge.io/) or [direct download](https://files.mega65.org?id=9561505c-a36d-4d3e-b158-d52a718e818e)
    * This requires a recent version capable of producing MEGA65 programs.

To build `easyasm.d81`:

```
make easyasm.d81
```

This project uses `makedisk.py`, my own tool for producing D81 disk files. The contents of the disk are described in `files.json`. It has built-in support for executing `petcat` to convert `.bas` files to PRG files, and also converts `.txt` files to `TYPE`-compatible SEQ files with nice formatting (lowercase text). See [makedisk.py](makedisk.py) for a complete description.
