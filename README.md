# EasyAsm 65

EasyAsm 65 (or just "EasyAsm") is an on-device assembly language programming tool for the MEGA65 personal computer. It uses the MEGA65's operating system as part of a complete assembly language development workflow, and maintains a minimal memory footprint when not in use.

Features:
* Supports all 45GS02 CPU instruction types and addressing modes.
* Supports a subset of the [Acme cross-assembler](https://sourceforge.net/projects/acme-crossass/) syntax and features.
* Maintains a minimal memory footprint when running your program or during editing, so you can use the full power of your computer.
* Uses the built-in screen editor's Edit mode for editing assembly language source code.
* Assembles to memory for testing, or to disk files for distribution.
* Can produce a single-file bootstrap loader.
* Can store multiple memory segments compactly, with bootstrap code that positions segments automatically.
* Preserves source code in memory while running your program, and exits cleanly from your program back to the screen editor with source code restored. Can restore source code manually after an abnormal exit.

EasyAsm 65 is copyright © 2024 Dan Sanderson, released under the GNU Public License v3. See [LICENSE](LICENSE).

## Project status and roadmap

PROJECT STATUS: **IN PROGRESS**

- [ ] Unit test framework
- [ ] Parser, with syntax errors
  - [ ] Skip empty line
  - [ ] Ignore comment
  - [ ] Instructions, one addressing mode, literals
  - [ ] All literal syntaxes
  - [ ] All addressing modes
  - [ ] Label / PC assignment
  - [ ] Labelled instruction, colon optional
  - [ ] Label argument
  - [ ] Expressions
- [ ] Assemble: single pass, literals only, no branching, no ZP
- [ ] ZP addressing modes
  - [ ] By argument value
  - [ ] By literal syntax
  - [ ] By +1/+2 syntax
- [ ] 8-bit branching; error on oversize branch
- [ ] Label assignment
- [ ] Labels as arguments; two-pass
  - [ ] Track 16-bit address syntax in assignment, for ZP addressing
- [ ] Evaluate expressions
- [ ] Read PC `*` in expressions
- [ ] Assign PC `*` once at start
- [ ] Assemble to memory workflow
- [ ] Assemble to disk, single-segment: `!to "...", <mode>`
  - [ ] `cbm`
  - [ ] `raw`
  - [ ] `runnable`, no PC assignment
- [ ] `!byte`, `!8`
- [ ] `!word`, `!16`
- [ ] `!32`
- [ ] `!fill`
- [ ] `!pet`
- [ ] `!scr`
- [ ] `!warn`

Release 0.1:
* Single segment programs only: one `!to` and up to one `* = ...` per source file

Roadmap:
* `!binary`
* `!source`
* Automatic 16-bit branching; multi-pass
* `runnable` with custom PC (single relocatable segment)
* Multi-segment programs with `!to "...", cbm` and `raw`; multiple `* = ...` allowed; gaps filled on disk
* Multi-segment programs with `!to "...", runnable`; segments relocated by bootstrap, no gaps on disk
* Multi-file output, multiple `!to` allowed
* Freezable mode that uses bank 5 if it's already there

## An important note

**Save your work to disk, early and often.**

Writing a program for a microcomputer using the microcomputer itself comes with the inherent risk that a bug in your program will interfere with your programming environment. EasyAsm includes a feature to preserve your source code in memory while you are testing your program, but this cannot be guaranteed to work if the program does something unexpected.

By design, EasyAsm does *not* force you to save your work to disk before testing your program. Please remember to do this yourself.

## Quick reference

* `RUN "EASYASM"` : install EasyAsm; erases program memory

* `SYS $1E00` : assemble source code to memory, then run it
* `SYS $1EXX` : assemble source code to disk, per `!to` directive
* `SYS $1EXX` : restore source code after abnormal program exit

* `EDIT ON` / `EDIT OFF` : enable/disable Edit mode; prompt is `OK.` when enabled
* `DSAVE "..."` : save source file to disk; do this with Edit mode *enabled!*
* `DSAVE "@..."` : save source file to disk, overwriting an existing file
* `MONITOR` : start the Monitor from the `READY.`/`OK.` prompt

## Using EasyAsm

To activate EasyAsm, load and run the `EASYASM` program:

```
RUN "EASYASM"
```

EasyAsm installs itself in the MEGA65's memory, and returns to the `READY.` prompt.

> TODO: Determine if EasyAsm can clear program memory automatically, or if the user needs to type `NEW`.

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

> TODO: Update this with the actual SYS address.

```
SYS $1EXX
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

Now type `LIST`:

```
LIST
```

Uh oh, that's not the program source file! EasyAsm did not see the program exit with the `RTS` instruction, so it didn't get a chance to restore the source file.

When this happens—and it will happen often—use this command to tell EasyAsm to bring back the source file:

> TODO: Update this with the actual SYS address.

```
SYS $1EXX
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

> TODO: Update this with the actual SYS address.

```
SYS $1EXX
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
| Allows blank lines | No | Yes |
| To display a file without loading it: | `LIST "FNAME"` | `TYPE "FNAME"` |


## How EasyAsm uses memory

EasyAsm tries to maintain a minimal memory footprint while you are editing your source code, and while your program is running. This allows you to use all the tools at your disposal for editing, and allows your program to use most of the computer, while still retaining a useful on-device workflow.

Of course, EasyAsm has to live somewhere. This is what EasyAsm needs:

* EasyAsm reserves the memory ranges **$1E00-$1EFF** (256 bytes of bank 0) and **$8700000-$87FFFFF** (1 megabyte of Attic RAM). If your program overwrites any of this memory, you will need to reload EasyAsm, and any stashed source code data may not be recoverable.
* EasyAsm reserves the right to overwrite **$50000-$5FFFF** (all 64KB of bank 5) when you call an EasyAsm function with `SYS`. Your program can use this memory while it is running, but the state of this memory may change when EasyAsm is running.

EasyAsm uses program memory ($2001-$F6FF) in three ways:

1. When you load and run `EASYASM` at the start of a session, it overwrites program memory with its installer code. After it is installed, it clears program memory.
2. While you are editing source code in Edit mode, your source code occupies program memory. EasyAsm expects to find it there when you invoke the assembler.
3. To test your program, EasyAsm stashes the source code into Attic RAM, assembles the program, and installs it in program memory. It runs from this location as it would when a user loads and runs your program. If the program exits with `RTS`, EasyAsm copies the source code back into program memory (overwriting the assembled program) and restores the editing environment.

Everything else is up to you.

> **Note:** EasyAsm keeps its own code in Attic RAM while not in use, and copies itself to bank 5 when performing operations. Attic RAM is not included in MEGA65 Freezer states. If EasyAsm finds its code in bank 5 but not in Attic RAM, it will attempt to re-stash itself in Attic. This may cause undesired results if a program modifies bank 5 in a way that EasyAsm doesn't notice!

## Assembly language syntax

Wherever possible, EasyAsm uses syntax compatible with the [Acme cross-assembler](https://sourceforge.net/projects/acme-crossass/), so you can enter example code from books and articles for that assembler without changes. Only a subset of Acme features are supported. Source code features exclusive to EasyAsm attempt to extend the Acme syntax in intuitive ways.

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

| | | | | | | | |
|-----|------|-----|------|-----|------|-----|------|
| adc | adcq | and | andq | asl | aslq | asr | asrq |
| asw | aug | bbr# | bbs# | bcc | bcs | beq | bit |
| bitq | bmi | bne | bpl | bra | brk | bsr | bvc |
| bvs | clc | cld | cle | cli | clv | cmp | cmpq |
| cpx | cpy | cpz | dec | deq | dew | dex | dey |
| dez | eom | eor | eorq | inc | inq | inw | inx |
| iny | inz | jmp | jsr | lda | ldq | ldx | ldy |
| ldz | lsr | lsrq | map | neg | nop | ora | orq |
| pha | php | phw | phx | phy | phz | pla | plp |
| plx | ply | plz | rmb# | rol | rolq | ror | rorq |
| row | rti | rts | sbc | sbcq | sec | sed | see |
| sei | smb# | sta | stq | stx | sty | stz | tab |
| tax | tay | taz | tba | trb | tsb | tsx | tsy |
| txa | txs | tya | tys | tza |

(For `bbr#`, `bbs#`, `rmb#`, and `smb#`, the `#` is a bit index 0 through 7, e.g. `smb3`.)

Instructions that operate on the accumulator or quad register as an alternative to a memory location are sometimes spelled with an `A` or `Q` in the place of an argument. Omit these for EasyAsm (as you would for Acme): to logical-shift right the accumulator, use `lsr`, not `lsr a`.

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
  * When used with the `!scr` directive, this is translated into a screen code.
  * EasyAsm does not support backslash-escape sequences. The PETSCII code for a single-quote character is 39 ($27).

To specify a negative number literal, use negation syntax: `-27` If a literal specifies all 32 bits and bit 31 is high, this will be treated as a two's complement negative number: `$FFFFFFFC` Negating such a literal will negate the number: `-$FFFFFFFC` and `4` are equivalent.

Some assembler directives accept text strings as arguments, surrounded by double-quotes: `"string"` These are not values and cannot be used in expressions. The `!pet` and `!scr` directives render strings into sequences of character bytes (PETSCII or screen codes, respectively).

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

A label name must start with a letter, either lowercase or uppercase. Subsequent characters can be either a letter (lowercase or uppercase), a number, back-arrow (ASCII underscore), or Mega + `@` (an underscore-like character in PETSCII).

> **Tip:** If you choose to use uppercase + graphics text mode for assembly programming, I recommend limiting the use of shifted letters in label names. They're allowed because they are uppercase letters in lowercase text mode, but they are difficult to remember how to type, and some are difficult to distinguish (e.g. Shift + G and Shift + T appear only slightly differently in uppercase text mode).

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
| `v & w` | Bitwise And | |
| `v XOR w` | Bitwise Exclusive Or | |
| `v \| w` | Bitwise Or | Mega + period |

EasyAsm does not support fractional number values, and so does not have a fractional division operator (`/`).

EasyAsm does not support Boolean values, and so does not have conditional operators. (This would primarily be used with conditional assembly, which EasyAsm also does not support.)

The power operator is right-associative: `x^y^z` = `x^(y^z)`

To type the power operator, type the up-arrow character (next to the Restore key). To type the bitwise-or operator, type Mega + period.

> **Tip:** Enter the command `FONT A` and switch to lowercase mode to display certain characters as their ASCII equivalents. To type the ASCII-specific characters:
> * Back-arrow: underscore (`_`)
> * Up-arrow: caret (`^`)
> * Mega + back-arrow: backtick
> * Mega + period: vertical bar (`|`)
> * Mega + comma: tilde (`~`)
> * Mega + forward-slash (or £): backslash (`\\`)
> Remember that you must be in lowercase mode with this font setting to see the ASCII characters.

### Parentheses

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

When assembling to memory, it writes each segment to the requested memory locations.

When assembling to disk, EasyAsm offers several options: writing a contiguous file, writing segments to separate files, or bundling segments into a runnable program.

### Writing a contiguous file

The `!to` directive sets a filename and writing mode for assembling subsequent code to disk. Using the `cbm` mode, this creates a PRG file, with the address of the first assembled instruction that follows as the load address. This requires that the source code set the PC at the beginning, so the `!to` directive knows the starting address.

```
!to "routines", cbm
* = $c000

change_background:
  inc $d020
  rts
```

If a single `!to` directive is followed by more than one segment, EasyAsm creates a PRG file that starts at the segment with the lowest address. Each segment is followed by a region of empty data, such that each segment is written into its starting address when the file is loaded. This can be useful if the separator regions are expected to be small, such as to align code to specific addresses.

This example generates one file with the first segment, a region of empty data to align the next segment, followed by the second segment.

```
!to "routines", cbm
* = $7400
  jsr change_background
  ; ...
  rts

; (...empty data generated here...)

* = $7f00
change_background:
  inc $d020
  rts
```

If the gap between segments is large, this could result in excess use of disk space and loading time. EasyAsm offers other options to avoid this scenario.

### Writing segments to separate files

EasyAsm source code can request that segments be written to different files by providing the `!to "...", cbm` directive more than once. It is the program's responsibility to load each segment file into the appropriate memory location.

This example generates two files, one for each segment.

```
!to "screen", cbm
* = $7400

  ; (Some disk code to load the "routines" file...)

  jsr change_background
  ; ...
  rts

!to "routines", cbm
* = $c000

change_background:
  inc $d020
  rts
```

### Generating a runnable program

In EasyAsm, the `!to "...", runnable` directive creates a program file that starts with a bootstrap routine. A user can load and `RUN` this program from the `READY.` prompt. The program starts with the first instruction after the `!to "...", runnable` directive.

`!to "...", runnable` sets the PC automatically. It is not necessary to set the PC explicitly in this case. If the program redefines the PC before the first instruction, the segment will be located as requested, and the bootstrap routine will start at that segment's start address.

If `!to "...", runnable` is followed by more than one segment, EasyAsm generates some additional code that automatically relocates all of the segments to their starting addresses, as part of the bootstrap process. The first segment is assumed to be the start of the program. The generated program behaves similarly to assembling to memory, and only requires loading and running one file, with no disk space or load time spent on empty data regions between segments.

```
!to "program", runnable

  jsr change_background
  ; ...
  rts

* = $c000

change_background:
  inc $d020
  rts
change_background_end = *
```

## Zero-page address arguments

The absolute addressing mode and the base page addressing mode have the same assembler syntax, despite being separate instructions with separate behaviors. For example:

```
lda $fe            ; load index $fe off the base page
lda $16fe          ; load address $16fe
```

If the address expression evaluates to a value larger than 255, then EasyAsm uses absolute addressing.

If the address expression evaluates to a value between 0 and 255, EasyAsm disambiguates using the following algorithm:

1. If the address expression is a value literal *or* a symbol defined using a value literal, one or more leading zeroes signify absolute mode: `$00fe` Otherwise base page addressing is used: `$fe`
2. If the value can be calculated in the first pass of the assembler, i.e. all symbols in the expression are defined earlier in the source text, base page addressing is used: `$fc+earlierlabel`
3. Otherwise absolute addressing is assumed, to be calculated in a subsequent pass: `$fc+laterlabel`

To clarify potential confusion in your source code, you can suffix the instruction mnemonic with `+1` to force base page addressing, or `+2` to for absolute addressing.

```
lda+1 $00fe        ; force base page addressing: $fe
lda+2 $fe          ; force absolute addressing: $00fe

varname = $fe
lda+2 varname      ; force absolute addressing: $00fe
```

## Assembler directives (pseudo-operands)

EasyAsm supports the following assembler directives.

### `!to`

```
!to "...", <mode>
```

Sets the output file and mode when assembling to disk. `<mode>` can be `cbm` (PRG with address), `raw` (PRG without address), or `runnable` (bootstrap routine).

> TODO: unit/drive number?

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

Assembles arguments that can include character strings as a series of bytes. `!pet` renders character strings and character literals as PETSCII codes, as written directly in the PETSCII source file. `!scr` attempts to convert PETSCII codes to VIC screen codes. Number expressions are byte values and are rendered verbatim in either case.

String data does not automatically add a null terminator. If a null terminator is desired, end the string with a 0 byte.

```
!pet "hello world!", 0
```

EasyAsm does not support backslash-escape character sequences. To include a double-quote character in a `!pet` or `!scr` directive, end the string, use a 34 ($22) byte.

```
!pet "i said, ", 34, "hello world!", 34, 0
```

### `!source`

`!source "..."`

Loads a source file from disk to be assembled as code at the given location.

> TODO: unit/drive number?

### `!binary`

`!binary "..." [, <size> [, <skip>]]`

Loads a binary file from disk to be assembled as data at the given location. Without arguments, the entire file is included. `<size>` limits the number of bytes to include. `<skip>` starts assembling data that many bytes into the file.

> TODO: unit/drive number?

### `!warn`

`!warn "..."`

Prints a given PETSCII message during the assembly process.

No other Acme-style message directives are supported. They're only useful for conditional assembly, which is not supported.


## Acme assembler compatibility

EasyAsm tries to provide a subset of the features of the Acme assembler, using compatible syntax. Not all features of Acme are supported, and a few features are exclusive to EasyAsm.

### Acme features that are not supported

EasyAsm does not currently have the following features that are present in the Acme assembler.

* No macros
* No conditional assembly
* No loops
* No symbol list output
* No fractional number values
* No fractional division operator
* No Boolean values
* No conditional expressions
* No value lists (used as arguments to macros)
* No zones (but "cheap" local symbols are supported)
* No CPUs other than the 45GS02
* No features not supported by the 45GS02 CPU
* No assembler directives (pseudo-ops) other than those listed
* No directive aliases other than those listed
* No `0x` and `0b` syntax for hex and binary literals (use `$...` and `%...`)
* No octal literals
* No way to set a "pseudo-PC"
* Only one statement per line

### Features exclusive to EasyAsm

Here is a quick summary of features available in EasyAsm that are not available in Acme:

* PETSCII character encoding of source files
* `!to "...", runnable`
* Assemble to multiple files from one source file, with multiple `!to` directives

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
