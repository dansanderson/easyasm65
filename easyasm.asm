; EasyAsm, an assembler for the MEGA65
; Copyright © 2024  Dan Sanderson
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;
; ---------------------------------------------------------
; easyasm : The main program
; ---------------------------------------------------------

!cpu m65

kernal_base_page = $00
easyasm_base_page = $1e

; BP map (B = $1E)
; 00 - ?? : EasyAsm dispatch; see easyasm-e.prg

* = $100 - 59

pass            *=*+1  ; $FF=final pass
program_counter *=*+2

asm_flags       *=*+1
; - The PC is defined
F_ASM_PC_DEFINED = %00000001
; - expect_addressing_expr is forcing 16-bit addressing
F_ASM_FORCE_MASK = %00000110
F_ASM_FORCE8     = %00000010
F_ASM_FORCE16    = %00000100
F_ASM_F16IMM     = %00000110
; - expect_addressing_expr subtracts address arg from PC for branching
F_ASM_AREL_MASK  = %00011000
F_ASM_AREL8      = %00001000
F_ASM_AREL16     = %00010000
F_ASM_BITBRANCH  = %00100000
; - assembly generated at least one warning
F_ASM_WARN       = %01000000

current_segment *=*+4
next_segment_pc *=*+2
next_segment_byte_addr *=*+4
stmt_tokpos     *=*+1
label_pos       *=*+1
label_length    *=*+1

instr_line_pos        *=*+1
instr_mnemonic_id     *=*+1
instr_buf_pos         *=*+1
instr_addr_mode       *=*+2
instr_mode_rec_addr   *=*+2
instr_supported_modes *=*+2

symtbl_next_name *=*+3
last_pc_defined_global_label *=*+2

expr_a          *=*+4
expr_b          *=*+4
expr_result     *=*+4
expr_flags      *=*+1
F_EXPR_BRACKET_MASK   = %00000011
F_EXPR_BRACKET_NONE   = %00000000
; - Entire expression surrounded by parentheses
F_EXPR_BRACKET_PAREN  = %00000001
; - Entire expression surrounded by square brackets
F_EXPR_BRACKET_SQUARE = %00000010
; - Hex/dec number literal with leading zero, or symbol assigned such a literal with =
F_EXPR_FORCE16   = %00000100
; - Expr contains undefined symbol
F_EXPR_UNDEFINED      = %00001000

tok_pos         *=*+1   ; Offset of tokbuf
line_pos        *=*+1   ; Offset of line_addr
err_code        *=*+1   ; Error code; 0=no error
line_addr       *=*+2   ; Addr of current BASIC line
code_ptr        *=*+2   ; 16-bit pointer, CPU
attic_ptr       *=*+4   ; 32-bit pointer, Attic
bas_ptr         *=*+4   ; 32-bit pointer, bank 0

!if * > $100 {
    !error "Exceeded BP map; move start to earlier, if possible : ", *
}


; Attic map
attic_easyasm_stash = $08700000                    ; 0.0000-0.5FFF
attic_source_stash  = attic_easyasm_stash + $6000  ; 0.6000-1.36FF
attic_symbol_table  = attic_source_stash + $d700   ; 1.3700-1.56FF (8 KB)
   ; If symbol table has to cross a bank boundary, check code for 16-bit addresses. :|
attic_symbol_names  = attic_symbol_table + $2000   ; 1.5700-1.B6FF (24 KB)
attic_symbol_names_end = attic_symbol_names + $6000
attic_segments      = attic_symbol_names + $6000   ; 1.B700-2.6700 (44 KB)
attic_segments_end  = attic_segments + $b000
attic_forced16s     = attic_segments + $b000       ; 2.6700-2.7D00 (5.5 KB)
attic_forced16s_end = attic_forced16s + $1600

; Symbol table constraints
;
; - Symbol table entries are 8 bytes: (name_ptr_24, flags_8, value_32)
; - 1023 symbols maximum (8KB of entries + list terminator)
; - Average name length of 23 for all 1023 symbols (24KB of names)
;
; For comparison, the BASIC 65 source file has 3301 symbols with an average
; name length of 15. The source file is 521,778 bytes long, which is >11x the
; maximum size of an EasyAsm source file. So 32KB of symbol data for EasyAsm
; is probably overkill.
SYMTBL_ENTRY_SIZE = 8
SYMTBL_MAX_ENTRIES = (attic_symbol_names-attic_symbol_table) / SYMTBL_ENTRY_SIZE
; - Symbol is defined in the current pass; value is valid
F_SYMTBL_DEFINED  = %00000001
; - Symbol was assigned a number literal with leading zeroes
F_SYMTBL_LEADZERO = %00000010


; Other memory
source_start = $2000  ; bank 0
tokbuf = $7e00        ; bank 5
strbuf = $7f00        ; bank 5
max_end_of_program = tokbuf

; KERNAL routines
bsout = $ffd2
primm = $ff7d

; MEGA65 registers
dmaimm = $d707
mathbusy = $d70f
divrema  = $d768
divquot  = $d76c
multina  = $d770
multinb  = $d774
product  = $d778

; Character constants
chr_cr = 13
chr_spc = 32
chr_shiftspc = 160
chr_tab = 9
chr_uparrow = 94
chr_backarrow = 95
chr_megaat = 164
chr_doublequote = 34


; Call a given KERNAL routine
!macro kcall .kaddr {
    pha
    lda #kernal_base_page
    tab
    pla
    jsr .kaddr
    pha
    lda #easyasm_base_page
    tab
    pla
}

; Call KERNAL primm
; Wrap string to print in +kprimm_start and +kprimm_end
!macro kprimm_start {
    pha
    lda #kernal_base_page
    tab
    pla
    jsr primm
}
!macro kprimm_end {
    pha
    lda #easyasm_base_page
    tab
    pla
}

!macro debug_print .msg {
    +kprimm_start
    !pet .msg
    !byte 0
    +kprimm_end
}

!macro push32 .addr {
    lda .addr
    pha
    lda .addr+1
    pha
    lda .addr+2
    pha
    lda .addr+3
    pha
}

!macro pull32 .addr {
    pla
    sta .addr+3
    pla
    sta .addr+2
    pla
    sta .addr+1
    pla
    sta .addr
}

; ------------------------------------------------------------
; Dispatch
; ------------------------------------------------------------

* = $2000    ; Actually $52000

    jmp dispatch
id_string:
    !pet "easyasm v0.1",0

; Initialize
; - Assume entry conditions (B, bank 5, MAP)
init:
    ; Init pointer banks
    lda #attic_easyasm_stash >> 24
    sta attic_ptr+3
    lda #(attic_easyasm_stash >> 16) & $ff
    sta attic_ptr+2
    lda #$00
    sta bas_ptr+3
    sta bas_ptr+2

    jsr init_symbol_table
    jsr init_segment_table
    jsr init_forced16

    rts


; All entry into EasyAsm comes through here.
; MAPL = (E)500  MAPH = (8)300  B = $1Exx
; A = dispatch index (1-indexed)
dispatch:
    pha
    jsr init
    txa
    tay  ; Y = argument
    pla

; A = menu option or 0 for menu, Y = argument (reserved)
invoke_menu_option:
    asl
    tax   ; X = A*2
    jmp (dispatch_jump,x)
dispatch_jump:
    !word do_menu
    !word assemble_to_memory_cmd
    !word assemble_to_disk_cmd
    !word restore_source_cmd
    !word run_test_suite_cmd

do_banner:
    +kprimm_start
    !pet "                                           ",13
    !pet 172,172,172," ",0
    +kprimm_end
    ldx #<id_string
    ldy #>id_string
    jsr print_cstr
    +kprimm_start
    !pet ", by dddaaannn ",187,187,187,"         ",13,13,0
    +kprimm_end
    rts

asciikey = $d610
do_menu:
    lda #147
    +kcall bsout
    jsr do_banner

    ; Flush keyboard buffer
-   sta asciikey
    lda asciikey
    bne -

    +kprimm_start
    !pet "https://github.com/dansanderson/easyasm65",13,13
    !pet " 1. assemble and test",13
    !pet " 2. assemble to disk",13
    !pet " 3. restore source",13,0
    +kprimm_end
    +kprimm_start
    !pet " run/stop: close menu",13,13
    !pet " your choice? ",15,166,143,13
    !pet 145,29,29,29,29,29,29,29,29,29,29,29,29,29,29,0
    +kprimm_end

-   lda asciikey
    beq -
    sta asciikey
    cmp #3  ; Stop key
    beq @exit_menu
    cmp #'1'
    bcc -
    cmp #'3'+1
    bcs -

    +kcall bsout
    pha
    lda #chr_cr
    +kcall bsout
    +kcall bsout
    pla
    sec
    sbc #'1'-1
    ldy #0
    jmp invoke_menu_option

@exit_menu
    +kprimm_start
    !pet "stop",13,0
    +kprimm_end
    lda #0
    sta $0091  ; Suppress Break Error (naughty!)
    rts


assemble_to_memory_cmd:
    +kprimm_start
    !pet "# assembling...",13,13,0
    +kprimm_end

    jsr stash_source
    jsr assemble_source
    ; TODO: the rest of the owl

    jsr restore_source
    +kprimm_start
    !pet 13,"# program returned, source restored",13,0
    +kprimm_end

    rts


assemble_to_disk_cmd:
    +kprimm_start
    !pet "# assembling to disk...",13,13,0
    +kprimm_end

    jsr stash_source
    jsr assemble_source
    ; TODO: the rest of the owl

    +kprimm_start
    !pet 13,"# assemble to disk complete",13,0
    +kprimm_end
    rts


restore_source_cmd:
    ; Safety check: probably never stashed source before
    ldz #0
    lda #<attic_source_stash
    sta attic_ptr
    lda #>attic_source_stash
    sta attic_ptr+1
    lda [attic_ptr],z
    bne +++

    jsr restore_source

    +kprimm_start
    !pet 13,"# source restored",13,0
    +kprimm_end

+++ rts


stash_source:
    sta dmaimm
    !byte $80, $00
    !byte $81, attic_source_stash >> 20
    !byte $0b, $00
    !byte $00, $00, $d7
    !byte <source_start, >source_start, $00
    !byte <attic_source_stash, >attic_source_stash, (attic_source_stash >> 16) & $0f
    !byte $00, $00, $00

    rts


restore_source:
    sta dmaimm
    !byte $80, attic_source_stash >> 20
    !byte $81, $00
    !byte $0b, $00
    !byte $00, $00, $d7
    !byte <attic_source_stash, >attic_source_stash, (attic_source_stash >> 16) & $0f
    !byte <source_start, >source_start, $00
    !byte $00, $00, $00

    rts


; ------------------------------------------------------------
; Utilities
; ------------------------------------------------------------

; Print a C-style string
; Input: X/Y address (bank 5)
print_cstr:
    ; Manage B manually, for speed
    lda #kernal_base_page
    tab

    stx $fc   ; B=0
    sty $fd
    lda #$05
    sta $fe
    lda #$00
    sta $ff

-   ldz #0
    lda [$fc],z
    beq +
    jsr bsout
    inw $fc
    bra -
+
    ; Restore B
    lda #easyasm_base_page
    tab
    rts


; Print a string from a source line
; Input: line_addr, X=line pos, Y=length
print_bas_str:
    lda line_addr
    sta $00fc
    lda line_addr+1
    sta $00fd
    lda #0
    sta $00fe
    sta $00ff

    ; Manage B manually, for speed
    lda #kernal_base_page
    tab

    txa
    taz
-   lda [$fc],z
    jsr bsout
    inz
    dey
    bne -

    ; Restore B
    lda #easyasm_base_page
    tab
    rts


; Input:
;   Q = 32-bit value (ZYXA)
;   C: 0=unsigned, 1=signed
print_dec32:
    ; Use strbuf like so:
    ; $00: negative sign or null
    ; $01-$0B: 10 final characters, null terminated
    ; $0C-$10: 5 BCD bytes
    ; $11-$14: 4 binary bytes
    sta strbuf+$11
    stx strbuf+$12
    sty strbuf+$13
    stz strbuf+$14
    ldx #$10
    lda #0
-   sta strbuf,x
    dex
    bpl -

    bcc @unsigned_continue
    tza
    bpl @unsigned_continue
    lda #$ff       ; Negate value
    tax
    tay
    taz
    eorq strbuf+$11
    inq
    stq strbuf+$11
    lda #'-'       ; Put negative sign in string buffer
    sta strbuf
@unsigned_continue

    ; Using BCD mode, double-with-carry each binary digit of $11-$14 into
    ; $0C-$10. Do 16 bits at a time.
    sed
    ldx #16
-   row strbuf+$13
    ldy #5
--  lda strbuf+$0c-1,y
    adc strbuf+$0c-1,y
    sta strbuf+$0c-1,y
    dey
    bne --
    dex
    bne -
    ldx #16
-   row strbuf+$11
    ldy #5
--  lda strbuf+$0c-1,y
    adc strbuf+$0c-1,y
    sta strbuf+$0c-1,y
    dey
    bne --
    dex
    bne -
    cld

    ; Convert BCD in $0C-$10 to PETSCII digits in $01-$0B.
    ldx #4
-   txa
    asl
    tay   ; Y = 2*x
    lda strbuf+$0c,x
    lsr
    lsr
    lsr
    lsr
    clc
    adc #'0'
    sta strbuf+$01,y
    lda strbuf+$0c,x
    and #$0f
    clc
    adc #'0'
    sta strbuf+$02,y
    dex
    bpl -

    ; Slide PETSCII digits left to eliminate leading zeroes.
-   lda strbuf+$01
    cmp #'0'
    bne @written_continue
    ldx #$02
--  lda strbuf,x
    sta strbuf-1,x
    inx
    cpx #$0b
    bne --
    lda #0
    sta strbuf-1,x
    bra -

@written_continue
    lda #0
    sta strbuf+$0c
    lda strbuf+$01  ; Edge case: 0
    bne +
    lda #'0'
    sta strbuf+$01
+
    ; Test for negative sign, and either print from sign or from first digit.
    ldx #<strbuf
    ldy #>strbuf
    lda strbuf
    bne +
    inx      ; (assume doesn't cross a page boundary)
+   jsr print_cstr
    rts


; Input: A = 8-bit value
print_hex8:
    jsr hex_az
    pha
    tza
    +kcall bsout
    pla
    +kcall bsout
    rts

; Input: A = byte
; Output: A/Z = hex digits
hex_az:
    pha
    lsr
    lsr
    lsr
    lsr
    jsr hex_nyb
    taz
    pla

hex_nyb:
    and #15
    cmp #10
    bcc +
    adc #6
+   adc #'0'
    rts

; Input: err_code, line_pos, line_addr
; - If line_pos = $FF, don't print line
print_error:
    lda err_code
    bne +        ; zero = no error
    rts
+

    dec
    asl
    tax
    lda err_message_tbl+1,x
    tay
    lda err_message_tbl,x
    tax
    jsr print_cstr
    +kprimm_start
    !pet " in line ",0
    +kprimm_end

    lda line_addr
    sta bas_ptr
    lda line_addr+1
    sta bas_ptr+1
    ldz #3
    lda [bas_ptr],z        ; line number high
    tax
    dez
    lda [bas_ptr],z        ; line number low
    ldy #0
    ldz #0
    clc                    ; request unsigned
    pha
    phx
    jsr print_dec32
    lda #chr_cr
    +kcall bsout

    ; Skip printing line if line_pos = $ff
    lda line_pos
    cmp #$ff
    lbeq +++

    ; Print line number again.
    plx
    pla
    ldy #0
    ldz #0
    clc                    ; request unsigned
    jsr print_dec32
    ; Sneak a peek at strbuf to get the line number length + 1
    ldx #0
-   inx
    lda strbuf,x
    bne -
    phx

    ; Print the source code line
    lda #chr_spc
    +kcall bsout
    inw bas_ptr
    inw bas_ptr
    inw bas_ptr
    inw bas_ptr
    ldz #0
    ldx #0
-   lda [bas_ptr],z
    sta strbuf,x
    beq +
    inz
    inx
    bra -
+   ldx #<strbuf
    ldy #>strbuf
    jsr print_cstr
    lda #chr_cr
    +kcall bsout

    ; Print an error position marker
    plx           ; Indent by width of line number + 1
-   lda #chr_spc
    +kcall bsout
    dex
    bne -
    ldx line_pos   ; Indent by line_pos - 4
    dex
    dex
    dex
    dex
    beq +
-   lda #chr_spc
    +kcall bsout
    dex
    bne -
+   lda #chr_uparrow
    +kcall bsout
    lda #chr_cr
    +kcall bsout

+++ rts


; (Used by print_warning and do_warn)
; Inits bas_ptr=line_addr for caller to use
print_warning_line_number:
    +kprimm_start
    !pet "line ",0
    +kprimm_end
    lda line_addr
    sta bas_ptr
    lda line_addr+1
    sta bas_ptr+1
    ldz #3
    lda [bas_ptr],z        ; line number high
    tax
    dez
    lda [bas_ptr],z        ; line number low
    ldy #0
    ldz #0
    clc                    ; request unsigned
    jsr print_dec32
    +kprimm_start
    !pet ": ",0
    +kprimm_end
    rts


; Input: A=warning ID
; Output: prints message with line number, sets F_ASM_WARN
print_warning:
    pha
    jsr print_warning_line_number
    pla
    dec
    asl
    tax
    lda warn_message_tbl+1,x
    tay
    lda warn_message_tbl,x
    tax
    jsr print_cstr

    lda #chr_cr
    +kcall bsout

    lda asm_flags
    ora #F_ASM_WARN
    sta asm_flags
    rts


; Test whether A is a letter
; Input: A=char
; Output: C: 0=no 1=yes
is_letter:
    ; I'm leaving Acme's converstion table set to "raw" so I'm forced to
    ; understand this explicitly. PETSCII has two sets of uppercase letters.
    cmp #'A'     ; $41 = PETSCII lower A
    bcc ++
    cmp #'Z'+1   ; $5A = PETSCII lower Z
    bcc +
    cmp #'a'     ; $61 = PETSCII upper A
    bcc ++
    cmp #'z'+1   ; $7A = PETSCII upper A
    bcc +
    cmp #193     ; $C1 = PETSCII Shift-A
    bcc ++
    cmp #218+1   ; $DA = PETSCII Shift-Z
    bcc +
    clc
    rts
+   sec
++  rts


; Test whether A is a secondary identifier character
; Input: A=char
; Output: C: 0=no 1=yes
is_secondary_ident_char:
    cmp #'0'
    bcc +
    cmp #'9'+1
    bcc ++
+   cmp #chr_backarrow
    beq ++
    cmp #chr_megaat
    beq ++
    cmp #'.'
    bne +++
++  sec
    rts
+++ jmp is_letter

; Test whether A is whitespace on a line
; (Does not include CR.)
; Input: A=char
; Output: C: 0=no 1=yes
is_space:
    cmp #chr_spc
    beq +
    cmp #chr_shiftspc
    beq +
    cmp #chr_tab
    beq +
    clc
    bra ++
+   sec
++  rts

; Input: A=char
; Output: A=lowercase letter, or original char if not letter
to_lowercase:
    jsr is_letter
    bcc +
    cmp #'Z'+1
    bcc +           ; already lower
    sec
    sbc #'a'-'A'
    cmp #193-('a'-'A')
    bcc +           ; lowered from first upper bank
    sec
    sbc #193-'a'    ; lowered from second upper bank
+   rts


; Input: strbuf contains null-terminated string
; Output: letters of strbuf changed to lowercase
strbuf_to_lowercase:
    ldx #0
-   lda strbuf,x
    beq +
    jsr to_lowercase
    sta strbuf,x
    inx
    bra -
+   rts


; Input: strbuf, code_ptr; X=strbuf start pos, Z=max length
; Output:
;   strbuf < code_ptr: A=$ff
;   strbuf = code_ptr: A=$00
;   strbuf > code_ptr; A=$01
;   X=strbuf last pos
strbuf_cmp_code_ptr:
    ldy #0
-   cpz #0
    beq @is_equal
    lda strbuf,x
    cmp (code_ptr),y
    bcc @is_less_than
    bne @is_greater_than
    lda strbuf,x
    beq @is_equal  ; null term before max length
    inx
    iny
    dez
    bra -

@is_less_than:
    lda #$ff
    rts
@is_equal:
    lda #$00
    rts
@is_greater_than:
    lda #$01
    rts


; ------------------------------------------------------------
; Tokenizer
; ------------------------------------------------------------

; Skip over whitespace, and also a line comment if found after whitespace
; Input: bas_ptr = line_addr, line_pos
; Output: line_pos advanced maybe; A=last read, Zero flag if zero
accept_whitespace_and_comment:
    lda line_pos
    taz
-   lda [bas_ptr],z
    tax
    jsr is_space
    bcc +
    inz
    bra -
+   cmp #';'
    bne +
-   inz            ; Ignore comment to end of line
    lda [bas_ptr],z
    tax
    bne -
+   stz line_pos
    txa   ; Set flags
    rts


; Consume identifier
; Input:
;   bas_ptr = line_addr
;   Z = line_pos
;   C: 0=must start with letter, 1=allow non-letter start
; Output:
;   If found, C=1, Z advanced (line_pos not)
;   If not found, C=0, Z = unchanged
; TODO: should line_pos be the input for consistency, or am I really using Z
; distinct from line_pos?
accept_ident:
    bcs +
    ; Must start with letter
    lda [bas_ptr],z
    jsr is_letter
    bcs +
    clc
    rts
+
    ; Can be followed by letter, number, back-arrow, Mega+@
-   inz
    lda [bas_ptr],z
    jsr is_secondary_ident_char
    bcs -
    sec
    rts


; Input: expr_result
; Output: expr_result = expr_result * 10
;   Overwrites expr_a
;   Preserves Z
expr_times_ten:
    phz
    ldq expr_result
    rolq
    stq expr_a
    rolq
    rolq
    adcq expr_a
    stq expr_result
    plz
    rts

; Accept a number/char literal
; Input: bas_ptr=line_addr, line_pos
; Output:
;  C: 0=not found, line_pos unchanged
;  C: 1=found; expr_result=value; line_pos advanced
;  expr_flags F_EXPR_FORCE16 bit set if hex or dec literal has a leading zero
accept_literal:
    ; Init expr zero flag to 0.
    lda expr_flags
    and #!F_EXPR_FORCE16
    sta expr_flags

    lda line_pos
    taz

    lda #0
    sta expr_result
    sta expr_result+1
    sta expr_result+2
    sta expr_result+3

    lda [bas_ptr],z
    cmp #'\''
    bne ++
    ; Char literal
    inz
    lda [bas_ptr],z
    tax
    inz
    lda [bas_ptr],z
    cmp #'\''
    lbne @not_found
    stx expr_result
    inz
    lbra @found

++  ldx #0
    stx expr_b+1
    stx expr_b+2
    stx expr_b+3

    cmp #'$'
    lbeq @do_hex_literal
    cmp #'%'
    lbeq @do_binary_literal
    cmp #'0'
    lbcc @not_found
    cmp #'9'+1
    lbcc @do_decimal_literal

@not_found
    lda line_pos
    taz
    clc
    rts

@do_decimal_literal
    cmp #'0'
    bne +
    pha
    lda expr_flags
    ora #F_EXPR_FORCE16
    sta expr_flags
    pla
+
@do_decimal_literal_loop
    cmp #'0'
    lbcc @found
    cmp #'9'+1
    lbcs @found
    jsr expr_times_ten
    lda [bas_ptr],z
    sec
    sbc #'0'
    sta expr_b
    phz
    ldq expr_b
    clc
    adcq expr_result
    stq expr_result
    plz
    inz
    lda [bas_ptr],z
    lbra @do_decimal_literal_loop

@do_hex_literal
    ; Set up first digit, confirm it's a hex digit
    inz
    lda [bas_ptr],z
    cmp #'0'
    lbcc @not_found
    bne +
    pha
    lda expr_flags
    ora #F_EXPR_FORCE16
    sta expr_flags
    pla
+   cmp #'9'+1
    bcc +
    jsr to_lowercase
    cmp #'A'
    lbcc @not_found
    cmp #'F'+1
    lbcs @not_found
+

@do_hex_literal_loop
    cmp #'0'
    lbcc @found
    cmp #'9'+1
    bcs +
    ; 0-9
    sec
    sbc #'0'
    bra +++

+   jsr to_lowercase
    cmp #'A'
    lbcc @found
    cmp #'F'+1
    lbcs @found
    ; A-F
    sec
    sbc #'A'-10

+++ sta expr_b
    phz
    clc
    ldq expr_result
    rolq
    rolq
    rolq
    rolq
    adcq expr_b
    stq expr_result
    plz
    inz
    lda [bas_ptr],z
    bra @do_hex_literal_loop

@do_binary_literal
    ; Set up first digit, confirm it's a binary digit
    inz
    lda [bas_ptr],z
    cmp #'0'
    beq +
    cmp #'.'
    beq +
    cmp #'1'
    beq +
    cmp #'#'
    lbne @not_found

+
--- cmp #'1'
    beq ++
    cmp #'#'
    beq ++
    cmp #'0'
    beq +
    cmp #'.'
    lbne @found
+   ; 0
    clc
    bra +++
++  ; 1
    sec
+++ rolq expr_result
    inz
    lda [bas_ptr],z
    bra ---

@found
    stz line_pos
    sec
    rts


; Locate a substring of strbuf in a null-terminated list of null-terminated lowercase strings
; Input:
;   strbuf
;   X=strbuf start pos
;   code_ptr = first char of first item in match list
;   C: 0=no restrictions; 1=next cannot be ident char
; Output:
;   If found, C=1, Y=entry number counted from zero, X=strbuf pos of next char
;   If not found, C=0
find_item_count = expr_a
find_start_pos = expr_a+1
find_item_length = expr_a+2
find_word_boundary = expr_a+3
find_in_token_list:
    lda #0
    sta find_item_count
    stx find_start_pos
    lda #0
    bcc +
    inc
+   sta find_word_boundary

@next_item
    ldz #0
    lda (code_ptr),z
    beq @find_fail
    ; Z = item length
-   inz
    lda (code_ptr),z
    bne -
    stz find_item_length

    ldx find_start_pos
    jsr strbuf_cmp_code_ptr
    bne +
    ; Item of length N has matched N characters in strbuf.
    ; Word boundary not requested? Accept prefix.
    lda find_word_boundary
    beq @find_success
    ; Word boundary requested, next strbuf char must be non-word char.
    lda strbuf,x
    jsr is_secondary_ident_char
    bcc @find_success
    ; strbuf has more word chars, so this is not a match.
+

    clc
    lda find_item_length
    inc  ; null terminator
    adc code_ptr
    sta code_ptr
    lda #0
    adc code_ptr+1
    sta code_ptr+1
    inc find_item_count
    bra @next_item

@find_success
    sec
    ldy find_item_count
    rts

@find_fail
    clc
    rts


; Tokenize mnemonic.
; Input: strbuf = lowercase line, line_pos at first char
; Output:
;   If found, C=1, X=token number, Y=flags, line_pos advanced
;   If not found, C=0, line_pos unchanged
tokenize_mnemonic:
    ldx line_pos
    lda #<mnemonics
    sta code_ptr
    lda #>mnemonics
    sta code_ptr+1
    sec  ; Must not immediately precede an identifier character.
    jsr find_in_token_list
    bcc @end
    stx line_pos  ; new line_pos
    ; X = line_pos, Y = mnemonic ID, Z = flags
    ; (Final: X = mnemonic ID, Y = flags)

    ; Check for +1/+2 suffix
    ldz #0
    lda strbuf,x
    cmp #'+'
    bne @end_ok
    inx
    lda strbuf,x
    cmp #'1'
    bne +
    ldz #F_ASM_FORCE8
    bra @end_forcewidth
+   cmp #'2'
    bne @end_ok
    ldz #F_ASM_FORCE16
@end_forcewidth
    inx
    lda strbuf,x
    jsr is_secondary_ident_char
    bcs @end_ok  ; Roll back to previous line_pos
    stx line_pos
@end_ok
    sec  ; C = 1
    tya
    tax  ; X = mnemonic ID
    tza
    tay  ; Y = flags
@end
    rts


; Tokenize pseudoop.
; Input: strbuf = lowercase line, line_pos at first char
; Output:
;   If found, C=1, X=token number, line_pos advanced
;   If not found, C=0, line_pos unchanged
tokenize_pseudoop:
    ldx line_pos
    lda strbuf,x
    cmp #'!'
    bne @not_found
    inx
    lda #<pseudoops
    sta code_ptr
    lda #>pseudoops
    sta code_ptr+1
    sec  ; Must not immediately precede an identifier character.
    jsr find_in_token_list
    bcc @not_found
    stx line_pos  ; new line pos
    tya
    clc
    adc #mnemonic_count  ; Y+mnemonic_count = pseudoop token ID
    tax
    sec
    rts
@not_found
    clc
    rts


; Tokenize punctuation tokens.
; Input: strbuf = lowercase line, line_pos at first char
; Output:
;   If found, C=1, X=token number, line_pos advanced
;   If not found, C=0, line_pos unchanged
;
; Note: Tokens spelled with letters that can also be labels are lexed as
; labels (xor, div, runnable, cbm, raw, x, y, z, sp).
tokenize_other:
    ldx line_pos
    lda #<other_tokens
    sta code_ptr
    lda #>other_tokens
    sta code_ptr+1
    clc  ; Allow an identifier character immediately after.
    jsr find_in_token_list
    bcc @end
    stx line_pos  ; new line pos
    tya
    clc
    adc #last_po  ; Y+last_po = non-keyword token ID
    tax
    sec
@end
    rts


; Load a full source line into strbuf, lowercased.
;
; This leaves the first four bytes of strbuf untouched to maintain an index
; correspondence with line_addr, so line_pos can index into both of them.
; Tokens are stored with line locations based on line_pos.
;
; Input: line_addr
; Output: line_addr copied to strbuf, lowercased
load_line_to_strbuf:
    lda line_addr
    sta bas_ptr
    lda line_addr+1
    sta bas_ptr+1
    ldy #4
    ldz #4
-   lda [bas_ptr],z
    beq +
    jsr to_lowercase
    sta strbuf,y
    iny
    inz
    bra -
+   sta strbuf,y  ; store null terminator in strbuf too
    rts


; Tokenize a full line.
;
; This populates tokbuf with tokens, null-terminated. Tokens are variable width.
; * String literal: tk_string_literal, line_pos, length
; * Number literal: tk_number_literal, line_pos, expr_result (4 bytes)
; * Label or register: tk_label_or_reg, line_pos, length
; * Mnemonic, pseudoop, keyword, non-keyword token: token ID, line_pos
;
; Input: line_addr
; Output:
;   On success, err_code=0, tokbuf populated.
;   On failure, err_code=syntax error, line_pos set to error.
tokenize:
    jsr load_line_to_strbuf
    ldy #0
    sty err_code
    sty tok_pos
    ldz #4
    stz line_pos

@tokenize_loop
    jsr accept_whitespace_and_comment
    cmp #0
    lbeq @success

    ; String literal
    cmp #chr_doublequote
    bne +
    lda line_pos
    taz
-   inz
    lda [bas_ptr],z
    cmp #chr_doublequote
    bne -
    ; Push tk_string_literal, line_pos, length (z-line_pos)
    ldx tok_pos
    lda #tk_string_literal
    sta tokbuf,x
    inx
    lda line_pos
    sta tokbuf,x
    inx
    tza
    sec
    sbc line_pos
    dec
    sta tokbuf,x
    inx
    stx tok_pos
    inz
    stz line_pos
    bra @tokenize_loop

+   ; Numeric literal
    phz
    jsr accept_literal
    plz
    bcc +++
    ; Push tk_number_literal, line_pos, expr_result (4 bytes)
    lda expr_flags
    and #F_EXPR_FORCE16
    beq +
    lda #tk_number_literal_leading_zero
    bra ++
+   lda #tk_number_literal
++  ldx tok_pos
    sta tokbuf,x
    inx
    stz tokbuf,x
    inx
    lda expr_result
    sta tokbuf,x
    inx
    lda expr_result+1
    sta tokbuf,x
    inx
    lda expr_result+2
    sta tokbuf,x
    inx
    lda expr_result+3
    sta tokbuf,x
    inx
    stx tok_pos
    bra @tokenize_loop

+++ ; Mnemonic
    phz  ; TODO: tokenize routines should put Z back to start?
    jsr tokenize_mnemonic
    plz
    bcc +++
    ; Push mnemonic ID (X), line_pos, flags (Y)
    txa
    ldx tok_pos
    sta tokbuf,x
    inx
    stz tokbuf,x
    inx
    sty tokbuf,x
    lda line_pos
    taz
    inx
    stx tok_pos
    lbra @tokenize_loop

    ; Pseudoop
+++ phz
    jsr tokenize_pseudoop
    plz
    bcs @push_tok_pos_then_continue

    ; TODO: tokenize relative labels

    ; Punctuation token
    phz
    jsr tokenize_other
    plz
    bcs @push_tok_pos_then_continue

    ; Label
    lda [bas_ptr],z
    cmp #'@'
    bne +
    inz
    sec
    bra ++
+   clc
++  jsr accept_ident
    bcc @syntax_error
    ; Push tk_label_or_reg, line_pos, length (z-line_pos)
    ldx tok_pos
    lda #tk_label_or_reg
    sta tokbuf,x
    inx
    lda line_pos
    sta tokbuf,x
    inx
    tza
    sec
    sbc line_pos
    sta tokbuf,x
    inx
    stx tok_pos
    stz line_pos
    lbra @tokenize_loop

@push_tok_pos_then_continue
    ; Push X, line_pos
    txa
    ldx tok_pos
    sta tokbuf,x
    inx
    stz tokbuf,x
    lda line_pos
    taz
    inx
    stx tok_pos
    lbra @tokenize_loop

@syntax_error
    lda #err_syntax
    sta err_code
    stz line_pos
    rts

@success
    ; Null terminate tokbuf: 0, $ff (line_pos=$ff -> don't print error location)
    lda #0
    ldx tok_pos
    sta tokbuf,x
    inx
    lda #$ff
    sta tokbuf,x
    rts


; ------------------------------------------------------------
; Symbol table
; ------------------------------------------------------------

init_symbol_table:
    ; Set first symbol table entry to null terminator
    lda #<attic_symbol_table
    ldx #>attic_symbol_table
    ldy #^attic_symbol_table
    ldz #$08
    stq attic_ptr
    dez
-   lda #0
    sta [attic_ptr],z
    dez
    bpl -

    ; Set name pointer to beginning of names region
    lda #<attic_symbol_names
    sta symtbl_next_name
    lda #>attic_symbol_names
    sta symtbl_next_name+1
    lda #^attic_symbol_names
    sta symtbl_next_name+2

    lda #0
    sta last_pc_defined_global_label
    sta last_pc_defined_global_label+1

    rts


; Find a symbol table entry for a name
; Input: bas_ptr=name, X=length (< 254)
; Output:
; - C=0 found, attic_ptr=entry address
; - C=1 not found, attic_ptr=next available table entry
; - bas_ptr and X preserved
find_symbol:
    phx
    lda #<attic_symbol_table
    ldx #>attic_symbol_table
    ldy #^attic_symbol_table
    ldz #$08
    stq attic_ptr
    plx

@symbol_find_loop
    ; attic_ptr = current entry
    ; Byte 2 is always $7x if value, $00 if terminator
    ldz #2
    lda [attic_ptr],z
    beq @not_found

    ; expr_a = current name ptr
    sta expr_a+2
    dez
    lda [attic_ptr],z
    sta expr_a+1
    dez
    lda [attic_ptr],z
    sta expr_a
    lda #$08
    sta expr_a+3

    ; Compare (expr_a) == (bas_ptr) up to length X
    txa
    taz
    dez
-   lda [expr_a],z
    cmp [bas_ptr],z
    bne @next_symbol
    dez
    bpl -
    ; (expr_a+length) == 0
    txa
    taz
    lda [expr_a],z
    bne @next_symbol
    ; Found.
    clc
    rts

@next_symbol
    phx
    lda #8
    ldx #0
    ldy #0
    ldz #0
    clc
    adcq attic_ptr
    stq attic_ptr
    plx
    bra @symbol_find_loop

@not_found
    sec
    rts


; Find or add a symbol table entry for a name
; Input: bas_ptr=name, X=length (< 254)
; Output:
; - C=0 found or added, attic_ptr=entry address
; - C=1 out of memory error
; - Uses expr_a
find_or_add_symbol:
    jsr find_symbol
    bcs +
    rts
+   ; attic_ptr is the null terminator in the symbol list
    ; Is there room for another symbol table entry here?
    phx
    lda #<(attic_symbol_names-SYMTBL_ENTRY_SIZE)
    ldx #>(attic_symbol_names-SYMTBL_ENTRY_SIZE)
    ldy #^(attic_symbol_names-SYMTBL_ENTRY_SIZE)
    ldz #$08
    cpq attic_ptr
    bne +
    ; Out of memory: no more symbol table entries.
    plx
    sec
    rts
+   plx
    phx
    ; Test for attic_symbol_names_end >= (symtbl_next_name + X + 1)
    lda symtbl_next_name
    sta expr_a
    lda symtbl_next_name+1
    sta expr_a+1
    lda symtbl_next_name+2
    sta expr_a+2
    lda #$08
    sta expr_a+3
    lda #0
    tay
    taz
    txa
    ldx #0
    inc
    adcq expr_a
    stq expr_a
    lda #<attic_symbol_names_end
    ldx #>attic_symbol_names_end
    ldy #^attic_symbol_names_end
    ldz #$08
    cpq expr_a
    bcs +
    ; Out of memory: not enough room for symbol name.
    plx
    sec
    rts
+
    ; (Name length is on the stack.)

    ; Create new table entry, and null terminator.
    ldz #0
    lda symtbl_next_name
    sta [attic_ptr],z
    inz
    lda symtbl_next_name+1
    sta [attic_ptr],z
    inz
    lda symtbl_next_name+2
    sta [attic_ptr],z
    inz
    ldx #13  ; Zero flags, value, and all of next entry (null terminator).
    lda #0
-   sta [attic_ptr],z
    inz
    dex
    bne -

    ; Copy name from bas_ptr, length X, to symtbl_next_name.
    plx
    lda symtbl_next_name
    sta expr_a
    lda symtbl_next_name+1
    sta expr_a+1
    lda symtbl_next_name+2
    sta expr_a+2
    txa
    taz
    dez
-   lda [bas_ptr],z
    sta [expr_a],z
    dez
    bpl -
    txa
    taz
    lda #0
    sta [expr_a],z

    ; Store new symtbl_next_name.
    tay
    taz
    txa
    ldx #0
    inc    ; Q = name length + 1
    clc
    adcq expr_a
    sta symtbl_next_name
    stx symtbl_next_name+1
    sty symtbl_next_name+2
    ; (symtbl_next_name is 24 bits.)

    ; Success.
    clc
    rts


; Gets a symbol's 32-bit value
; Input: attic_ptr=symbol table entry
; Output:
;   C=0 defined, Q=value
;   C=1 undefined
get_symbol_value:
    ldz #3
    lda [attic_ptr],z
    and #F_SYMTBL_DEFINED
    bne +
    ; Undefined.
    sec
    rts
+   lda #0
    tax
    tay
    taz
    lda #4
    clc
    adcq attic_ptr
    stq expr_a      ; expr_a = attic_ptr + 4
    ldz #0
    ldq [expr_a]
    clc
    rts


; Gets a symbol's 32-bit value
; Input: attic_ptr=symbol table entry, Q=value
; Output: entry value (attic_ptr+4)=Q, entry DEFINED flag set
; This does not validate inputs.
set_symbol_value:
    pha
    phx
    phy
    phz
    lda #0
    tax
    tay
    taz
    lda #4
    adcq attic_ptr
    stq expr_a
    plz
    ply
    plx
    pla
    stq [expr_a]
    ldz #3
    lda #F_SYMTBL_DEFINED
    sta [attic_ptr],z
    rts


; ------------------------------------------------------------
; Segment table
; ------------------------------------------------------------

; Initializes the segment table.
init_segment_table:
    ; (next_segment_byte_addr inits to top of table, for creation of first
    ; segment in assemble_bytes.)
    lda #<attic_segments
    sta current_segment
    sta next_segment_byte_addr
    lda #>attic_segments
    sta current_segment+1
    sta next_segment_byte_addr+1
    lda #^attic_segments
    sta current_segment+2
    sta next_segment_byte_addr+2
    lda #<(attic_segments >>> 24)
    sta current_segment+3
    sta next_segment_byte_addr+3

    lda #0
    sta next_segment_pc
    sta next_segment_pc+1
    ldz #0
    sta [current_segment],z
    inz
    sta [current_segment],z
    inz
    sta [current_segment],z
    inz
    sta [current_segment],z

    rts

; Initializes an assembly pass.
init_pass:
    lda #0
    sta program_counter
    sta program_counter+1
    sta asm_flags
    rts


; Gets the program counter, or fails if not defined.
; Input: program_counter, asm_flags
; Output:
;   C=0 ok, X/Y=PC
;   C=1 not defined
get_pc:
    lda asm_flags
    and #F_ASM_PC_DEFINED
    bne +
    sec
    rts
+   ldx program_counter
    ldy program_counter+1
    clc
    rts


; Sets the program counter.
; Input: X/Y=PC
; Output: program_counter, asm_flags
set_pc:
    stx program_counter
    sty program_counter+1
    lda asm_flags
    ora #F_ASM_PC_DEFINED
    sta asm_flags
    rts


; Assembles bytes to a segment.
; Input: Bytes in beginning of strbuf, X=length; init'd segment table
; Output:
;   C=0 ok; table state updated
;   C=1 fail:
;     err_code set; caller can react appropriately (err_pc_undef on pass 0 is ok)
;   Uses expr_a
assemble_bytes:
    cpx #0  ; Edge case: X = 0
    bne +
    clc
    rts
+
    ; expr_a = length
    stx expr_a
    lda #0
    sta expr_a+1
    sta expr_a+2
    sta expr_a+3

    ; If PC not defined, error.
    lda asm_flags
    and #F_ASM_PC_DEFINED
    bne +
    lda #err_pc_undef
    sta err_code
    sec
    rts
+
    ; If this isn't the final pass, simply increment the PC and don't do
    ; anything else.
    lda pass
    cmp #$ff
    lbne @increment_pc

    ; If next_segment_byte_addr+len is beyond maximum segment table address, out
    ; of memory error.
    lda #<attic_segments_end
    ldx #>attic_segments_end
    ldy #^attic_segments_end
    ldz #<(attic_segments_end >>> 24)
    sec
    sbcq expr_a
    sec
    sbcq next_segment_byte_addr
    bpl +
    lda #err_out_of_memory
    sta err_code
    sec
    rts
+
    ; If current segment has empty header or program_counter != next_segment_pc,
    ; create a new segment header.
    ; (segment_pc_16, length_16)
    ldz #0
    lda [current_segment],z
    inz
    ora [current_segment],z
    beq +
    lda program_counter
    cmp next_segment_pc
    bne +
    lda program_counter+1
    cmp next_segment_pc+1
    bne +
    bra ++
+   ldz #0
    lda program_counter
    sta [next_segment_byte_addr],z
    sta next_segment_pc
    inz
    lda program_counter+1
    sta [next_segment_byte_addr],z
    sta next_segment_pc+1
    inz
    lda #0
    sta [next_segment_byte_addr],z
    inz
    sta [next_segment_byte_addr],z
    ldq next_segment_byte_addr
    stq current_segment

    lda #0
    tax
    tay
    taz
    lda #4
    clc
    adcq next_segment_byte_addr
    stq next_segment_byte_addr

++  ; Write bytes to segment.
    lda expr_a
    dec
    tax
    taz
-   lda strbuf,x
    sta [next_segment_byte_addr],z
    dex
    dez
    bpl -

    ; Add length to segment length.
    ldz #2
    lda [current_segment],z
    clc
    adc expr_a
    sta [current_segment],z
    inz
    lda [current_segment],z
    adc expr_a+1
    sta [current_segment],z

    ; Add length to next_segment_byte_addr.
    ldq expr_a
    clc
    adcq next_segment_byte_addr
    stq next_segment_byte_addr

    ; Null terminate the segment list
    ldz #0
    lda #0
    sta [next_segment_byte_addr],z
    inz
    sta [next_segment_byte_addr],z
    inz
    sta [next_segment_byte_addr],z
    inz
    sta [next_segment_byte_addr],z

@increment_pc:
    ; Add length program_counter and next_segment_pc.
    lda expr_a
    clc
    adc program_counter
    sta program_counter
    sta next_segment_pc
    lda expr_a+1
    adc program_counter+1
    sta program_counter+1
    sta next_segment_pc+1
    bcc +
    lda #err_pc_overflow
    sta err_code
+   rts


; ------------------------------------------------------------
; Forced 16's list
; ------------------------------------------------------------
; This is a null-terminated list of 16-bit program counter values
; whose addresses are forced to 16-bit widths by the first pass.
; Specifically, an undefined operand expression forces 16 bits
; in the first pass, even when defined to a value < 256 in a later
; pass.

set_attic_ptr_to_forced16:
    lda #<attic_forced16s
    sta attic_ptr
    lda #>attic_forced16s
    sta attic_ptr+1
    lda #^attic_forced16s
    sta attic_ptr+2
    lda #<(attic_forced16s >>> 24)
    sta attic_ptr+3
    rts

init_forced16:
    jsr set_attic_ptr_to_forced16
    lda #0
    ldz #0
    sta [attic_ptr],z
    inz
    sta [attic_ptr],z
    rts

; Input: program_counter
; Output: C=1 if PC is in the list
;   attic_ptr at PC entry or end of list
find_forced16:
    jsr set_attic_ptr_to_forced16

    ; Report an undefined PC as "not found"
    lda asm_flags
    ora F_ASM_PC_DEFINED
    beq @not_found

    ldx program_counter
    ldy program_counter+1
-   ldz #0
    txa
    cmp [attic_ptr],z
    bne +
    tya
    inz
    cmp [attic_ptr],z
    beq @found
    dez
+   lda [attic_ptr],z
    inz
    ora [attic_ptr],z
    bne +
    bra @not_found

+   ; next
    lda #2
    clc
    adc attic_ptr
    sta attic_ptr
    lda #0
    adc attic_ptr+1
    sta attic_ptr+1
    lda #0
    adc attic_ptr+2
    sta attic_ptr+2
    lda #0
    adc attic_ptr+3
    sta attic_ptr+3
    bra -

@found
    sec
    rts
@not_found
    clc
    rts


; Add the program counter to the forced-16's list
; Input: program_counter
add_forced16:
    jsr find_forced16
    bcc +
    rts   ; already in the list
+
    ; Don't add an undefined PC
    lda asm_flags
    ora F_ASM_PC_DEFINED
    beq +

    ldz #0
    lda program_counter
    sta [attic_ptr],z
    inz
    lda program_counter+1
    sta [attic_ptr],z
    inz
    lda #0
    sta [attic_ptr],z
    inz
    sta [attic_ptr],z
+   rts


; ------------------------------------------------------------
; Parser primitives
; ------------------------------------------------------------

; Input: A=token; tokbuf, tok_pos
; Output:
;   C=0 ok, tok_pos advanced
;   C=1 not found, tok_pos preserved
expect_token:
    ldx tok_pos
    cmp tokbuf,x
    bne @fail
    inx
    inx
    stx tok_pos
    clc
    bra @end
@fail
    sec
@end
    rts


; Input: tokbuf, tok_pos
; Output:
;   C=0 ok, tok_pos advanced; X=line_pos, Y=length
;   C=1 not found, tok_pos preserved
expect_label:
    ldx tok_pos
    lda tokbuf,x
    cmp #tk_label_or_reg
    bne @fail
    inx
    lda tokbuf,x
    inx
    ldy tokbuf,x  ; Y = label token length
    inx
    stx tok_pos
    tax   ; X = label token line pos
    clc
    bra @end
@fail
    sec
@end
    rts

; Input: X/Y=code address of expected keyword; line_addr, tokbuf, tok_pos
; Output:
;   C=0 matched, tok_pos advanced
;   C=1 not found, tok_pos preserved
expect_keyword:
    stx code_ptr
    sty code_ptr+1
    ldx tok_pos
    phx
    jsr expect_label
    bcs @fail
    txa
    taz
    ldx #0
    lda line_addr
    sta bas_ptr
    lda line_addr+1
    sta bas_ptr+1
-   lda [bas_ptr],z
    sta strbuf,x
    inz
    inx
    dey
    bne -
    lda #0
    sta strbuf,x
    jsr strbuf_to_lowercase
    ldx #0
    jsr strbuf_cmp_code_ptr
    beq @succeed
@fail
    plx
    stx tok_pos
    sec
    bra @end
@succeed
    plx
    clc
@end
    rts


; Input: tokbuf, tok_pos
; Output:
;   C=0 ok, A=token ID, Y=flags, tok_pos advanced
;   C=1 not found, tok_pos preserved
expect_opcode:
    ldx tok_pos
    lda tokbuf,x
    beq @fail
    cmp #mnemonic_count
    bcs @fail
    inx
    inx
    ldy tokbuf,x
    inx
    stx tok_pos
    clc
    bra @end
@fail
    sec
@end
    rts


; Input: tokbuf, tok_pos
; Output:
;   C=0 ok, A=token ID, tok_pos advanced
;   C=1 not found, tok_pos preserved
expect_pseudoop:
    ldx tok_pos
    lda tokbuf,x
    cmp #po_to
    bcc @fail
    cmp #last_po+1
    bcs @fail
    inx
    inx
    stx tok_pos
    clc
    bra @end
@fail
    sec
@end
    rts


; Input: tokbuf, tok_pos
; Output:
;   C=0 ok, tok_pos advanced; expr_result, expr_flags
;   C=1 not found, tok_pos preserved
expect_literal:
    ldx tok_pos
    lda tokbuf,x
    cmp #tk_number_literal_leading_zero
    bne +
    lda #F_EXPR_FORCE16
    bra ++
+   cmp #tk_number_literal
    bne @fail
    lda #0
++  sta expr_flags
    lda tokbuf+2,x
    sta expr_result
    lda tokbuf+3,x
    sta expr_result+1
    lda tokbuf+4,x
    sta expr_result+2
    lda tokbuf+5,x
    sta expr_result+3
    lda #6
    clc
    adc tok_pos
    sta tok_pos
    lda #0
    adc tok_pos+1
    sta tok_pos+1
    clc
    bra @end
@fail
    sec
@end
    rts


; ------------------------------------------------------------
; Expressions
; ------------------------------------------------------------

; Input: expr_result
;   C=1 if signed, C=0 unsigned
; Output:
;   C=1 if:
;      signed:
;        high word = ffff and low word >= 8000, or
;        high word = 0000 and low word < 8000
;      unsigned:
;        high word = 0000 or ffff
;   Else, C=0
is_expr_word:
    lda expr_result+3
    and expr_result+2
    cmp #$ff
    bne +
    bcc @yes
    lda expr_result+1
    and #$80
    bne @yes
    bra @no
+   lda expr_result+3
    ora expr_result+2
    bne @no
    bcc @yes
    lda expr_result+1
    cmp #$80
    bcc @yes
@no
    clc
    rts
@yes
    sec
    rts

; Input: expr_result
;   C=1 if signed, C=0 unsigned
; Output:
;   C=1 if:
;     signed:
;       high word+byte = ffffff and LSB >= 80, or
;       high word+byte = 000000 and LSB < 80
;     unsigned:
;       high word+byte = ffffff or 000000
;   Else, C=0
is_expr_byte:
    lda expr_result+3
    and expr_result+2
    and expr_result+1
    cmp #$ff
    bne +
    bcc @yes
    lda expr_result
    and #$80
    bne @yes
    bra @no
+   lda expr_result+3
    ora expr_result+2
    ora expr_result+1
    bne @no
    bcc @yes
    lda expr_result
    cmp #$80
    bcc @yes
@no
    clc
    rts
@yes
    sec
    rts


; Expression grammar:
;   primary   ::= * | <label> | <literal> | "(" expr ")" | "[" expr "]"
;   inversion ::= (!)? primary
;   power     ::= inversion (^ inversion)*
;   negate    ::= (-)? power
;   factor    ::= negate ((* DIV / %) negate)*
;   term      ::= factor ((+ -) factor)*
;   shift     ::= term ((<< >> >>>) term)*
;   bytesel   ::= (< > ^ ^^)? shift
;   expr      ::= bytesel ((& XOR |) bytesel)*
;
; All "expect_" routines rely on global tokbuf and line_addr, and manipulate
; global tok_pos. Each returns a result as follows:
;   C=0 ok, tok_pos advanced; result in expr_result, expr_flags
;   C=1 not found, tok_pos preserved
;     err_code>0, line_pos: report error in expression
;
; Any C=0 return with expr_flags & F_EXPR_UNDEFINED should propagate
; F_EXPR_UNDEFINED and not bother to set expr_result.
;
; Bracket flags propagate if the rule matches the higher precedence rule
; without applying an operation.
;
; Intermediate results are kept on the stack, and unwound within each
; subroutine.

; primary   ::= * | <label> | <literal> | "(" expr ")" | "[" expr "]"
expect_primary:
    ldx tok_pos
    phx

    lda tokbuf,x
    lbeq @fail

    ; "(" expr ")"
    lda #tk_lparen
    jsr expect_token
    bcs +++
    jsr expect_expr
    lbcs @fail
    lda #tk_rparen
    jsr expect_token
    lbcs @fail
    lda expr_flags
    and #!F_EXPR_BRACKET_MASK
    ora #F_EXPR_BRACKET_PAREN
    sta expr_flags
    lbra @succeed

    ; "[" <expr> "]"
+++ lda #tk_lbracket
    jsr expect_token
    bcs +++
    jsr expect_expr
    lbcs @fail
    lda #tk_rbracket
    jsr expect_token
    lbcs @fail
    lda expr_flags
    and #!F_EXPR_BRACKET_MASK
    ora #F_EXPR_BRACKET_SQUARE
    sta expr_flags
    lbra @succeed

    ; Program counter (*)
+++ lda #tk_multiply
    jsr expect_token
    bcs +++
    lda program_counter
    sta expr_result
    lda program_counter+1
    sta expr_result+1
    lda #0
    sta expr_result+2
    sta expr_result+3
    lda asm_flags
    and #F_ASM_PC_DEFINED
    bne +
    lda expr_flags
    ora #F_EXPR_UNDEFINED
    sta expr_flags
+   lbra @succeed

    ; <label>
+++ jsr expect_label
    lbcs +++
    jsr find_or_add_label
    jsr get_symbol_value
    bcs ++
    stq expr_result
    ldz #3
    lda #0
    sta expr_flags
    lda [attic_ptr],z  ; flags
    and #F_SYMTBL_LEADZERO
    beq +
    lda #F_EXPR_FORCE16
    sta expr_flags
+   bra @succeed

++  lda #F_EXPR_UNDEFINED
    sta expr_flags
    lda pass           ; undefined label is an error in final pass
    cmp #$ff
    bne @succeed
    lda #err_undefined
    sta err_code
    ldx tok_pos
    lda tokbuf-2,x     ; back up to the label's line_pos
    sta line_pos
    bra @fail

    ; <literal>
+++ jsr expect_literal
    bcs @fail
    bra @succeed

@fail
    plx
    stx tok_pos
    sec
    rts
@succeed
    plx
    clc
    rts


; inversion ::= (!)* primary
expect_inversion:
    ldz #0
-   lda #tk_complement
    jsr expect_token
    bcs +
    inz
    bra -
+   phz
    jsr expect_primary
    plz
    bcs @end
    cpz #0
    beq @ok  ; 0 inversions, preserve flags
    lda expr_flags  ; at least one inversion, reset flags
    and #!F_EXPR_BRACKET_MASK
    sta expr_flags
    tza
    and #1
    beq @ok  ; even number of inversions, no value change
    lda expr_result
    eor #$ff
    sta expr_result
    lda expr_result+1
    eor #$ff
    sta expr_result+1
    lda expr_result+2
    eor #$ff
    sta expr_result+2
    lda expr_result+3
    eor #$ff
    sta expr_result+3
@ok
    clc
@end
    rts

; power     ::= inversion (^ inversion)*
expect_power:
    jsr expect_inversion
    lbcs @end  ; Missing first operand.
    lda #tk_power
    jsr expect_token
    lbcs @ok   ; Passthru

    ; For right associative, push all expressions parsed, then evaluate while
    ; unwinding.
    +push32 expr_result
    ldz #1
-   phz
    jsr expect_inversion
    plz
    lbcs @drop_stack_and_err  ; Power operator missing operand after operator.
    +push32 expr_result
    inz
    lda #tk_power
    phz
    jsr expect_token
    plz
    bcc -

    ; Clear bracket flags.
    lda expr_flags
    and #!F_EXPR_BRACKET_MASK
    sta expr_flags

    ; expr_b = 1, used twice later
    lda #1
    sta expr_b
    lda #0
    sta expr_b+1
    sta expr_b+2
    sta expr_b+3

    ; There are Z > 1 values on the stack. Last operand is first exponent.
    +pull32 expr_result
    dez  ; Z = Z - 1

    ; Z times, pull an operand, and take it to the previous result's power.
@power_loop
    ; If a power is negative (bit 31 is set), abort with error.
    bit expr_result+3
    bpl +
    lda #err_exponent_negative
    sta err_code
    lbra @drop_stack_and_err
+
    ; Pull the base.
    +pull32 expr_a

    ; Stash operand count during the exp_loop.
    phz

    ; Take expr_a to the expr_result power. Put final answer in expr_result.
    ; Edge case: power = 0
    ldq expr_result
    bne +
    lda #1
    sta expr_result
    bra @continue_power_loop
+

    ldq expr_a
    stq multina
    ldq expr_b  ; Start with A * 1
@exp_loop
    stq multinb
    ldq expr_result
    sec
    sbcq expr_b
    stq expr_result  ; expr_result = expr_result - 1
    beq +
    ldq product
    bra @exp_loop

+   ldq product
    stq expr_result

@continue_power_loop
    ; Restore Z = operand count. Proceed to next operand.
    plz
    dez
    bne @power_loop
    bra @ok

@drop_stack_and_err
-   cpz #0
    beq +
    pla
    pla
    pla
    pla
    dez
    bra -
+   sec
    rts
@ok
    clc
@end
    rts


; negate    ::= (-)? power
expect_negate:
    lda #tk_minus
    jsr expect_token
    bcc +
    jsr expect_power  ; Passthru
    lbra @end

+   jsr expect_power
    lbcs @end

    ; Negate expr_result (XOR $FFFFFFFF + 1)
    clc
    lda expr_result
    eor #$ff
    adc #1
    sta expr_result
    lda expr_result+1
    eor #$ff
    adc #0
    sta expr_result+1
    lda expr_result+2
    eor #$ff
    adc #0
    sta expr_result+2
    lda expr_result+3
    eor #$ff
    adc #0
    sta expr_result+3

    lda expr_flags
    and #!F_EXPR_BRACKET_MASK
    sta expr_flags

@ok
    clc
@end
    rts


; factor    ::= negate ((* DIV / %) negate)*
expect_factor:
    jsr expect_negate
    lbcs @end

    ; Special error message for unsupported fraction operator
    lda #tk_fraction
    jsr expect_token
    bcs +
    lda #err_fraction_not_supported
    sta err_code
    sec
    lbra @end
+

@factor_loop
    lda #tk_multiply
    jsr expect_token
    bcc +
    lda #tk_remainder
    jsr expect_token
    bcc +
    lda #0
    ldx #<kw_div
    ldy #>kw_div
    jsr expect_keyword
    lbcs @ok

+   pha
    ldq expr_result
    stq multina
    jsr expect_negate
    pla
    sta expr_a
    lbcs @end  ; Operator but no term, fail
    ldq expr_result
    stq multinb
    lda expr_a

    ; A=tk_multiply, tk_remainder, or 0 for DIV
    cmp #tk_multiply
    bne +
    ldq product
    bra ++
+   ; DIV and remainder need to wait for DIVBUSY bit
-   ldx mathbusy
    bmi -
    cmp #tk_remainder
    bne +
    ldq divrema  ; fractional part
    stq multina
    ldq product+4  ; frac * divisor
    bra ++
+   ldq divquot
++  stq expr_result
    lbra @factor_loop

@ok
    clc
@end
    rts


; term      ::= factor ((+ -) factor)*
expect_term:
    jsr expect_factor
    lbcs @end

@term_loop
    lda #tk_plus
    jsr expect_token
    bcc +
    lda #tk_minus
    jsr expect_token
    lbcs @ok

+   pha
    +push32 expr_result
    jsr expect_factor
    +pull32 expr_b
    pla
    lbcs @end  ; Operator but no term, fail
    ; A=tk_plus or tk_minus
    cmp #tk_plus
    bne +
    ; expr_result = expr_b + expr_result
    ldq expr_b
    clc
    adcq expr_result
    bra ++
+   ; expr_result = expr_b - expr_result
    ldq expr_b
    sec
    sbcq expr_result
++  stq expr_result
    lbra @term_loop

@ok
    clc
@end
    rts


; shift     ::= term ((<< >> >>>) term)*
expect_shift:
    jsr expect_term
    lbcs @end

@shift_loop
    lda #tk_asl
    jsr expect_token
    bcc +
    lda #tk_asr
    jsr expect_token
    bcc +
    lda #tk_lsr
    jsr expect_token
    lbcs @ok

+   pha
    +push32 expr_result
    jsr expect_term
    +pull32 expr_b
    pla
    lbcs @end  ; Operator but no term, fail

    ; expr_a = tk_asl, tk_asr, or tk_lsr
    ; Perform operation on expr_b, expr_result times. Store result in expr_result.
@count_loop
    lda expr_result+3
    ora expr_result+2
    ora expr_result+1
    ora expr_result
    lbeq @finish_count_loop

    clc
    lda expr_a
    cmp #tk_asl
    bne +
    ; asl
    asl expr_b
    rol expr_b+1
    rol expr_b+2
    rol expr_b+3
    bra +++
+   cmp #tk_asr
    bne +
    ; asr
    asr expr_b+3
    bra ++
+   ; lsr
    lsr expr_b+3
++  ror expr_b+2
    ror expr_b+1
    ror expr_b
+++

    sec
    lda expr_result
    sbc #1
    sta expr_result
    lda expr_result+1
    sbc #0
    sta expr_result+1
    lda expr_result+2
    sbc #0
    sta expr_result+2
    lda expr_result+3
    sbc #0
    sta expr_result+3
    lbra @count_loop

@finish_count_loop
    ldq expr_b
    stq expr_result
    lbra @shift_loop

@ok
    clc
@end
    rts


; bytesel   ::= (< > ^ ^^)? shift
expect_bytesel:
    lda #tk_lt
    jsr expect_token
    bcc +
    lda #tk_gt
    jsr expect_token
    bcc +
    lda #tk_power
    jsr expect_token
    bcc +
    lda #tk_megabyte
    jsr expect_token
    bcc +
    jsr expect_shift  ; Passthru
    lbra @end

+   pha
    jsr expect_shift
    pla
    lbcs @end  ; Operator but no term, fail
    ldx #0
    cmp #tk_lt
    beq +++
    cmp #tk_gt
    beq ++
    cmp #tk_power
    beq +
    inx  ; tk_megabyte
+   inx
++  inx
+++ lda expr_result,x
    sta expr_result
    lda #0
    sta expr_result+1
    sta expr_result+2
    sta expr_result+3

@ok
    clc
@end
    rts


; expr      ::= bytesel ((& XOR |) bytesel)*
expect_expr:
    jsr expect_bytesel
    lbcs @end

@bitop_loop
    lda #tk_ampersand
    jsr expect_token
    bcc +
    lda #tk_pipe
    jsr expect_token
    bcc +
    lda #0
    ldx #<kw_xor
    ldy #>kw_xor
    jsr expect_keyword
    bcs @ok

+   pha
    +push32 expr_result
    jsr expect_bytesel
    +pull32 expr_b
    pla
    lbcs @end  ; Operator but no term, fail

    ; expr_a = tk_ampersand, tk_pipe, or 0 for XOR
    ; expr_result = expr_b <op> expr_result
    lda expr_a
    cmp #tk_ampersand
    bne +
    ; expr_b & expr_result
    lda expr_b
    and expr_result
    sta expr_result
    lda expr_b+1
    and expr_result+1
    sta expr_result+1
    lda expr_b+2
    and expr_result+2
    sta expr_result+2
    lda expr_b+3
    and expr_result+3
    sta expr_result+3
    lbra @bitop_loop

+   cmp #tk_pipe
    bne +
    ; expr_b | expr_result
    lda expr_b
    ora expr_result
    sta expr_result
    lda expr_b+1
    ora expr_result+1
    sta expr_result+1
    lda expr_b+2
    ora expr_result+2
    sta expr_result+2
    lda expr_b+3
    ora expr_result+3
    sta expr_result+3
    lbra @bitop_loop

+   ; expr_b XOR expr_result
    lda expr_b
    eor expr_result
    sta expr_result
    lda expr_b+1
    eor expr_result+1
    sta expr_result+1
    lda expr_b+2
    eor expr_result+2
    sta expr_result+2
    lda expr_b+3
    eor expr_result+3
    sta expr_result+3
    lbra @bitop_loop

@ok
    clc
@end
    rts


; ------------------------------------------------------------
; PC assignment
; ------------------------------------------------------------

; Input: tokbuf, tok_pos
; Output:
;   C=0 success, PC updated, tok_pos advanced
;   C=1 fail
;     err_code=0 not a PC assign statement, tok_pos not advanced
;     err_code>0 fatal error with line_pos set
assemble_pc_assign:
    ; "*" "=" expr
    lda #tk_multiply
    jsr expect_token
    lbcs statement_err_exit
    lda #tk_equal
    jsr expect_token
    lbcs statement_err_exit
    ldx tok_pos
    lda tokbuf+1,x  ; expr line_pos
    pha
    jsr expect_expr
    plz             ; Z = expr line_pos
    lbcs statement_err_exit

    lda expr_flags
    and #F_EXPR_UNDEFINED
    beq +
    ; PC expression must be defined in first pass
    stz line_pos
    lda #err_pc_undef
    sta err_code
    lbra statement_err_exit

+   ; Value must be in address range
    lda expr_result+2
    ora expr_result+3
    beq +
    stz line_pos
    lda #err_value_out_of_range
    sta err_code
    lbra statement_err_exit

+   ; Set PC
    ldx expr_result
    ldy expr_result+1
    jsr set_pc
    lbra statement_ok_exit


; ------------------------------------------------------------
; Label assignment
; ------------------------------------------------------------

; Input: line_addr, label_pos
; Output:
;   X=0 global
;   X=1 cheap local
;   X=2 relative +
;   X=3 relative -
lbl_global = 0
lbl_cheaplocal = 1
lbl_relplus = 2
lbl_relminus = 3
determine_label_type:
    lda line_addr
    sta bas_ptr
    lda line_addr+1
    sta bas_ptr+1
    lda label_pos
    taz
    lda [bas_ptr],z
    cmp #'@'
    bne +
    ldx #lbl_cheaplocal
    rts
+   cmp #'+'
    bne +
    ldx #lbl_relplus
    rts
+   cmp #'-'
    bne +
    ldx #lbl_relminus
    rts
+   ldx #lbl_global
    rts


; Input: line_addr, X=label line pos, Y=label length
; Output:
; - C=0 found or added, attic_ptr=entry address
; - C=1 out of memory error
; Uses strbuf, expr_a.
find_or_add_label:
    phx
    phy

    ; TODO: relative labels

    ; Detect cheap local, rewrite name
    jsr determine_label_type
    cpx #lbl_cheaplocal
    bne ++
    ldx #0  ; strbuf position
    ; Copy last-seen global name to strbuf
    lda last_pc_defined_global_label
    ora last_pc_defined_global_label+1
    beq +++  ; Never seen a global, no cheap local prefix
    lda last_pc_defined_global_label
    sta attic_ptr
    lda last_pc_defined_global_label+1
    sta attic_ptr+1
    lda #^attic_symbol_table
    sta attic_ptr+2
    lda #$08
    sta attic_ptr+3
    sta expr_a+3
    ldz #0
    lda [attic_ptr],z
    sta expr_a
    inz
    lda [attic_ptr],z
    sta expr_a+1
    inz
    lda [attic_ptr],z
    sta expr_a+2       ; expr_a = name of last seen global
    ldx #0
    ldz #0             ; Copy global name to strbuf
-   lda [expr_a],z
    beq +++
    sta strbuf,x
    inx
    inz
    bra -

++  ldx #0  ; strbuf position
+++

    ; Copy label text to strbuf.
    ply  ; Y = length
    plz  ; Z = line pos
    lda line_addr
    sta bas_ptr
    lda line_addr+1
    sta bas_ptr+1
    ; X is current strbuf position, from above
-   lda [bas_ptr],z
    sta strbuf,x
    inx
    inz
    dey
    bne -

    lda #<strbuf
    sta bas_ptr
    lda #>strbuf
    sta bas_ptr+1
    lda bas_ptr+2
    pha
    lda #5
    sta bas_ptr+2
    ; X = new length (text added to strbuf)
    jsr find_or_add_symbol
    pla
    sta bas_ptr+2  ; Important! reset bas_ptr bank to 0
    rts


; Input: tokbuf, tok_pos
; Output:
;   C=0 success; label assigned, tok_pos advanced
;     Z = 0: was equal-assign, end statement
;     Z = 1: was PC assign, accept instruction or directive
;   C=1 fail
;     err_code=0 not a pc assign statement, tok_pos not advanced
;     err_code>0 fatal error with line_pos set
assemble_label:

    ; TODO: <rel-label> (no "=")

    ; <label> ["=" expr]
    jsr expect_label
    lbcs statement_err_exit
    stx label_pos      ; line_pos
    sty label_length   ; length
    lda #tk_equal
    jsr expect_token
    lbcs @label_without_equal
    jsr expect_expr
    lbcc @label_with_equal
    lda err_code
    lbne statement_err_exit
    lda label_pos
    sta line_pos
    lda #err_syntax  ; expr required after "="
    sta err_code
    lbra statement_err_exit

; (This is jsr'd.)
@find_or_add_symbol_to_define:
    ldx label_pos
    ldy label_length
    jsr find_or_add_label
    bcc +
    lda #err_out_of_memory
    sta err_code
    pla  ; pop caller address, go straight to exit
    pla
    lbra statement_err_exit
+
    ldz #3           ; Error if symbol is already defined in first pass
    lda [attic_ptr],z
    and #F_SYMTBL_DEFINED
    beq +
    lda pass
    bne +
    lda label_pos
    sta line_pos
    lda #err_already_defined
    sta err_code
    pla  ; pop caller address, go straight to exit
    pla
    lbra statement_err_exit
+   rts

@label_with_equal:
    ; Only global-type labels can be assigned with =
    jsr determine_label_type
    cpx #lbl_global
    beq +
    lda #err_label_assign_global_only
    sta err_code
    lbra statement_err_exit
+

    ; Set label to expr, possibly undefined
    jsr @find_or_add_symbol_to_define
    lda expr_flags
    and #F_EXPR_UNDEFINED
    beq +
    ldz #0  ; Return value
    lbra statement_ok_exit  ; label expr is undefined; leave symbol undefined
+
    ; label expr is defined, use value; propagate zero flag
    ldq expr_result
    jsr set_symbol_value
    lda expr_flags
    and #F_EXPR_FORCE16
    beq +
    ldz #3
    lda [attic_ptr],z
    ora #F_SYMTBL_LEADZERO
    sta [attic_ptr],z
+   ldz #0  ; Return value
    lbra statement_ok_exit

@label_without_equal
    ; Label without "=", set label to PC
    jsr @find_or_add_symbol_to_define
    ; PC undefined is an error
    jsr get_pc
    bcc +
    lda label_pos
    sta line_pos
    lda #err_pc_undef
    sta err_code
    lbra statement_err_exit
+   ; Move X/Y (from get_pc) to Q
    stx expr_result
    sty expr_result+1
    lda #0
    sta expr_result+2
    sta expr_result+3
    ldq expr_result
    jsr set_symbol_value

    ; Remember last seen PC-assigned global label
    jsr determine_label_type
    cpx #lbl_global
    bne +
    lda attic_ptr
    sta last_pc_defined_global_label
    lda attic_ptr+1
    sta last_pc_defined_global_label+1
+

    ldz #1  ; Return value
    lbra statement_ok_exit


; ------------------------------------------------------------
; Instructions
; ------------------------------------------------------------

; Input: expr_result, expr_flags
; Output: C=1 if expr is > 256 or is forced to 16-bit with leading zero
;   Also honors F_ASM_FORCE8/16 assembly flags.
is_expr_16bit:
    lda asm_flags
    and #F_ASM_FORCE_MASK
    cmp #F_ASM_FORCE16
    beq @yes_16
    cmp #F_ASM_FORCE8
    beq @no_16
    lda expr_flags
    and #F_EXPR_FORCE16
    bne @yes_16
    lda expr_result+1
    ora expr_result+2
    ora expr_result+3
    bne @yes_16
@no_16
    clc
    rts
@yes_16
    sec
    rts


!macro expect_addresing_expr_rts .mode {
    ldx #<.mode
    ldy #>.mode
    clc
    rts
}


force16_if_expr_undefined:
    lda asm_flags
    and #F_ASM_FORCE_MASK
    bne +  ; Undefined doesn't override other forces
    lda expr_flags
    and #F_EXPR_UNDEFINED
    beq +
    jsr add_forced16
    lda asm_flags
    and #!F_ASM_FORCE_MASK
    ora #F_ASM_FORCE16
    sta asm_flags
+   rts


; Input: expr_result, program_counter
; Output:
;   expr_result = expr_result - (program_counter + 2)
make_operand_rel:
    lda expr_flags
    and #F_EXPR_UNDEFINED
    beq +
    rts
+
    lda program_counter
    clc
    adc #2
    tay
    lda program_counter+1
    adc #0
    tax
    tya
    ldy #0
    ldz #0
    stq expr_a  ; expr_a = program_counter + 2
    ldq expr_result
    sec
    sbcq expr_a
    stq expr_result  ; expr_result = expr_result - expr_a
    rts


; Input: tokbuf, tok_pos, asm_flags
; Output:
;   C=0 ok, X/Y=addr mode flags (LSB/MSB), expr_result, expr_flags, tok_pos advanced
;   C=1 fail
;     err_code>0 fatal error with line_pos set
expect_addressing_expr:
    lda #0
    sta expr_result
    sta expr_result+1
    sta expr_result+2
    sta expr_result+3
    sta expr_flags
    sta err_code

    ; ":" or end of line: Implicit
    ldx tok_pos
    lda tokbuf,x
    beq +
    cmp #tk_colon
    beq +
    bra @try_immediate
+   +expect_addresing_expr_rts MODE_IMPLIED

@try_immediate
    ; "#" <expr>: Immediate
    lda #tk_hash
    jsr expect_token
    bcs @try_modes_with_leading_expr
    jsr expect_expr
    lbcs @addr_error
    lda asm_flags
    and #F_ASM_F16IMM
    beq +
    +expect_addresing_expr_rts MODE_IMMEDIATE_WORD
+   ; Value must be in byte range, signed or unsigned
    lda expr_result+1
    ora expr_result+2
    ora expr_result+3
    beq +  ; Positive 0 - 255
    lda expr_result+1
    and expr_result+2
    and expr_result+3
    cmp #$ff
    bne ++
    lda expr_result+0
    cmp #$80
    bcs ++
    ; Negative -128 - +127
+   +expect_addresing_expr_rts MODE_IMMEDIATE
++  lda #err_value_out_of_range
    sta err_code
    lbra @addr_error

@try_modes_with_leading_expr
    ; Check if the current PC is on the "forced16" list.
    lda asm_flags
    and #F_ASM_FORCE16
    bne +  ; skip if already forced
    jsr find_forced16
    bcc +
    lda asm_flags
    ora #F_ASM_FORCE16
    sta asm_flags
+

    ; Make the operand a relative address for branch instructions.
    ; (Leave it to assemble_instruction to range check.)
    jsr expect_expr
    lbcs @try_modes_without_leading_expr
    lda asm_flags
    and #F_ASM_AREL8
    beq +
    lda asm_flags   ; force16 for long branches
    ora #F_ASM_FORCE8
    sta asm_flags
    jsr make_operand_rel
    bra ++
+   lda asm_flags
    and #F_ASM_AREL16
    beq ++
    lda asm_flags   ; force16 for long branches
    ora #F_ASM_FORCE16
    sta asm_flags
    jsr make_operand_rel
++

    jsr force16_if_expr_undefined
    ; Addressing modes that start with expressions:
    lda expr_flags
    and #F_EXPR_BRACKET_MASK
    cmp #F_EXPR_BRACKET_NONE
    bne +++
    ; - Non-brackets:
    jsr is_expr_16bit
    bcs ++
    ;    <expr-8> ["," ("x" | "y")]
    lda #tk_comma
    jsr expect_token
    bcc +
    +expect_addresing_expr_rts MODE_BASE_PAGE
+   ldx #<kw_x
    ldy #>kw_x
    jsr expect_keyword
    bcs +
    +expect_addresing_expr_rts MODE_BASE_PAGE_X
+   ldx #<kw_y
    ldy #>kw_y
    jsr expect_keyword
    bcs +
    +expect_addresing_expr_rts MODE_BASE_PAGE_Y
+   lda asm_flags
    and #F_ASM_BITBRANCH
    beq +
    ldq expr_result
    stq expr_b
    jsr expect_expr
    bcs +
    ; <expr-8> "," <expr> (for bit branches)
    ; expr_b = ZP, expr_result = Rel
    jsr make_operand_rel
    +expect_addresing_expr_rts MODE_BASE_PAGE
+   ; Syntax error: <expr-8> "," non-x/y
    lbra @addr_error

++  ; <expr-16> ["," ("x" | "y")]
    lda #tk_comma
    jsr expect_token
    bcc +
    +expect_addresing_expr_rts MODE_ABSOLUTE
+   ldx #<kw_x
    ldy #>kw_x
    jsr expect_keyword
    bcs +
    +expect_addresing_expr_rts MODE_ABSOLUTE_X
+   ldx #<kw_y
    ldy #>kw_y
    jsr expect_keyword
    bcs +
    +expect_addresing_expr_rts MODE_ABSOLUTE_Y
+   ; Syntax error: <expr-16> "," non-x/y
    lbra @addr_error

+++ cmp #F_EXPR_BRACKET_PAREN
    bne +++
    ; - Parens:
    jsr is_expr_16bit
    bcs ++
    ; "(" <expr-8> ")" ["," ("y" | "z")]
    lda #tk_comma
    jsr expect_token
    bcc +
    ; (Base page indirect no register implies "Z".)
    +expect_addresing_expr_rts MODE_BASE_PAGE_IND_Z
+   ldx #<kw_y
    ldy #>kw_y
    jsr expect_keyword
    bcs +
    +expect_addresing_expr_rts MODE_BASE_PAGE_IND_Y
+   ldx #<kw_z
    ldy #>kw_z
    jsr expect_keyword
    bcs +
    +expect_addresing_expr_rts MODE_BASE_PAGE_IND_Z
+   ; Syntax error: "(" <expr-8> ")" "," non-y/z
    lbra @addr_error

++  ; "(" <expr-16> ")"
    +expect_addresing_expr_rts MODE_ABSOLUTE_IND

+++ ; - Brackets:
    ;    "[" <expr-8> "]" ["," "z"]
    jsr is_expr_16bit
    bcc +
    ; Error: Argument out of range
    lda tok_pos  ; position error at beginning of expression
    sec
    sbc #8
    sta tok_pos
    tax
    lda tokbuf+1,x
    sta line_pos
    lda #err_value_out_of_range
    sta err_code
    lbra @addr_error

+   lda #tk_comma
    jsr expect_token
    bcs +  ; ,z is optional
    ldx #<kw_z
    ldy #>kw_z
    jsr expect_keyword
    bcc +  ; z is required if , is provided
    ; Syntax error: "[" <expr-8> "]" "," non-z
    lbra @addr_error
+   +expect_addresing_expr_rts MODE_32BIT_IND

@try_modes_without_leading_expr
    ; Addressing modes that don't start with expressions:
    lda #tk_lparen
    jsr expect_token
    bcc +
    lbra @addr_error
+   jsr expect_expr
    bcc +
    lbra @addr_error
+   jsr force16_if_expr_undefined
    lda #tk_comma
    jsr expect_token
    bcc +
    lbra @addr_error
+   ldx #<kw_sp
    ldy #>kw_sp
    jsr expect_keyword
    bcc +++
    ; (<expr-8>,x)
    ; (<expr-16>,x)
    ldx #<kw_x
    ldy #>kw_x
    jsr expect_keyword
    bcc +
    lbra @addr_error
+   lda #tk_rparen
    jsr expect_token
    bcc +
    lbra @addr_error
+   jsr is_expr_16bit
    bcs +
    +expect_addresing_expr_rts MODE_BASE_PAGE_IND_X
+   +expect_addresing_expr_rts MODE_ABSOLUTE_IND_X

+++ ; (<expr>,sp),y
    lda #tk_rparen
    jsr expect_token
    bcc +
    lbra @addr_error
+   lda #tk_comma
    jsr expect_token
    bcc +
    lbra @addr_error
+   ldx #<kw_y
    ldy #>kw_y
    jsr expect_keyword
    bcc +
    lbra @addr_error
+   +expect_addresing_expr_rts MODE_STACK_REL

@addr_error
    lda err_code
    bne +
    lda #err_syntax
    sta err_code
    ldx tok_pos
    lda tokbuf+1,x
    sta line_pos
+   sec
    rts


; Input: A=mnemonic ID
; Output: C=1 yes is branch
is_bitbranch_mnemonic:
    ; Relies on bbr0 to bbs7 being alphabetically consecutive
    cmp #mnemonic_bbr0
    bcc ++
    cmp #mnemonic_bbs7+1
    bcs +
    sec
    bra ++
+   clc
++  rts


; Input: A=mnemonic ID
; Output: C=1 yes is branch
is_branch_mnemonic:
    ; Relies on bbr7 to bvs being alphabetically consecutive
    ; Omits "b" instructions that aren't branches
    cmp #mnemonic_bbs7+1
    bcc ++
    cmp #mnemonic_bvs+1
    bcs +
    cmp #mnemonic_bit
    beq +
    cmp #mnemonic_bitq
    beq +
    cmp #mnemonic_brk
    beq +
    sec
    bra ++
+   clc
++  rts


; Input: A=mnemonic ID
; Output: C=1 yes is branch
is_long_branch_mnemonic:
    ; Relies on lbcc to lbvs being alphabetically consecutive
    cmp #mnemonic_lbcc
    bcc ++
    cmp #mnemonic_lbvs+1
    bcs +
    sec
    bra ++
+   clc
++  rts


; Input: tokbuf, tok_pos
; Output:
;   C=0 ok, tok_pos advanced, instruction bytes assembled to segment
;   C=1 fail
;     err_code>0 fatal error with line_pos set
assemble_instruction:

    ; <opcode> <addr-expr>
    lda tokbuf+1,x
    sta instr_line_pos  ; stash line_pos for errors
    jsr expect_opcode
    lbcs statement_err_exit
    sta instr_mnemonic_id
    ; Reset instruction flags
    lda asm_flags
    and #!(F_ASM_FORCE_MASK | F_ASM_AREL_MASK)
    sta asm_flags
    ; Propagate flags from tokenizer: 8-bit if +1, 16-bit if +2
    tya
    ora asm_flags
    sta asm_flags

    ; Locate addressing mode record for mnemonic
    lda instr_mnemonic_id
    sta instr_mode_rec_addr
    lda #0
    sta instr_mode_rec_addr+1
    clc
    row (easyasm_base_page << 8) + instr_mode_rec_addr
    row (easyasm_base_page << 8) + instr_mode_rec_addr  ; instr_mode_rec_addr = ID*4
    lda #<addressing_modes
    clc
    adc instr_mode_rec_addr
    sta instr_mode_rec_addr
    lda #>addressing_modes
    adc instr_mode_rec_addr+1
    sta instr_mode_rec_addr+1  ; instr_mode_rec_addr = address of mode record for mnemonic

    ; Force 16-bit if instruction only accepts 16-bit arguments
    ; (Note that FORCE16 will not force Immediate Word mode, so LDZ is fine.)
    ;
    ; TODO: This is not implemented correctly. If there is no FOO ZP, FOO Addr
    ; should coerce FOO $fc to FOO $00fc, even if FOO has other byte-sized
    ; operand modes. This is only doing it if FOO *only* has word-sized
    ; operand modes.
    ldy #0
    lda (instr_mode_rec_addr),y
    and #<MODES_WORD_OPERAND
    bne +
    ldy #1
    lda (instr_mode_rec_addr),y
    and #>MODES_WORD_OPERAND
    beq +++  ; does not have word operand modes
+   ldy #0
    lda (instr_mode_rec_addr),y
    and #<MODES_BYTE_OPERAND
    bne +++  ; also has byte operand modes, no force
    ldy #1
    lda (instr_mode_rec_addr),y
    and #>MODES_BYTE_OPERAND
    bne +++
    ; Only has word operand modes. Coerce operands to 16-bit.
    ; (Overrides "+1", which is inappropriate in this case anyway.)
    lda asm_flags
    and #!F_ASM_FORCE_MASK
    ora #F_ASM_FORCE16
    sta asm_flags
+++

    ; Force 16-bit immediate just for PHW
    lda instr_mnemonic_id
    cmp #mnemonic_phw
    bne +++
    lda asm_flags
    ora #F_ASM_F16IMM
    sta asm_flags
+++

    ; Set AREL8/AREL16 for branch/long branch instructions
    lda instr_mnemonic_id
    jsr is_branch_mnemonic
    bcc +
    lda asm_flags
    ora #F_ASM_AREL8
    sta asm_flags
    bra ++
+   jsr is_long_branch_mnemonic
    bcc ++
    lda asm_flags
    ora #F_ASM_AREL16
    sta asm_flags
++

    ; Set BITBRANCH for bit branch instructions
    ; Allows two-operand syntax
    lda instr_mnemonic_id
    jsr is_bitbranch_mnemonic
    bcc +
    lda asm_flags
    ora #F_ASM_BITBRANCH
    sta asm_flags
+

    ; Process the addressing mode expression
    jsr expect_addressing_expr
    lbcs statement_err_exit
    stx instr_addr_mode
    sty instr_addr_mode+1

    ; Match addressing mode to opcode; error if not supported
    ldy #0
    lda instr_addr_mode
    and (instr_mode_rec_addr),y
    sta instr_addr_mode
    iny
    lda instr_addr_mode+1
    and (instr_mode_rec_addr),y
    sta instr_addr_mode+1  ; instr_addr_mode = selected addressing mode, or 0 if not supported
    ora instr_addr_mode
    bne +
    ; Mode not supported.
    lda instr_line_pos
    sta line_pos
    lda #err_unsupported_addr_mode
    sta err_code
    lbra statement_err_exit

+   ; Start at beginning of strbuf.
    lda #0
    sta instr_buf_pos

    ; Assemble Q prefix
    ;   q_mnemonics : 0-term'd list of mnemonic IDs
    ;   Emit $42 $42
    ldx #0
-   lda q_mnemonics,x
    beq ++
    cmp instr_mnemonic_id  ; mnemonic ID
    beq +
    inx
    bra -
+   ; Is a Q instruction, emit $42 $42
    ldx instr_buf_pos
    lda #$42
    sta strbuf,x
    inx
    sta strbuf,x
    inx
    stx instr_buf_pos

++  ; Assemble 32-bit indirect prefix
    ;   MODE_32BIT_IND
    ;   Emit $EA
    lda instr_addr_mode
    and #<MODE_32BIT_IND
    beq ++
    ; Is 32-bit indirect, emit $EA
    ldx instr_buf_pos
    lda #$ea
    sta strbuf,x
    inx
    stx instr_buf_pos

++  ; Assemble instruction (mnemonic + mode) encoding
    ;   addressing_modes: (mode_bits_16, enc_addr_16)
    ;   Rotate mode_bits left, count Carry; index into enc_addr
    ;   Emit encoding byte
    lda instr_addr_mode
    pha
    lda instr_addr_mode+1
    pha
    ldy #0
    lda (instr_mode_rec_addr),y
    sta instr_supported_modes
    iny
    lda (instr_mode_rec_addr),y
    sta instr_supported_modes+1
    ldx #0
-   row (easyasm_base_page << 8) + instr_supported_modes
    bcc +
    inx
+   row (easyasm_base_page << 8) + instr_addr_mode
    bcc -
    pla
    sta instr_addr_mode+1
    pla
    sta instr_addr_mode
    ; X = index into enc_addr + 1
    dex  ; X = X - 1
    ldy #2
    lda (instr_mode_rec_addr),y
    sta code_ptr
    iny
    lda (instr_mode_rec_addr),y
    sta code_ptr+1
    txa
    tay
    lda (code_ptr),y  ; A = the instruction encoding byte
    ldx instr_buf_pos
    sta strbuf,x
    inx
    stx instr_buf_pos

    ; Assemble operand
    ;   MODES_NO_OPERAND
    ;   MODES_BYTE_OPERAND
    ;   MODES_WORD_OPERAND
    lda instr_addr_mode
    and #<MODES_WORD_OPERAND
    sta instr_supported_modes ; (clobbers instr_supported_modes)
    lda instr_addr_mode+1
    and #>MODES_WORD_OPERAND
    ora instr_supported_modes
    beq @maybe_byte_operand
    ; Word operand: emit two expr_result bytes
    ; Range check
    lda instr_mnemonic_id
    jsr is_long_branch_mnemonic  ; C=1: signed operand
    jsr is_expr_word
    bcs +
    lda #err_value_out_of_range
    sta err_code
    lbra statement_err_exit
+   ldx instr_buf_pos
    lda expr_result
    sta strbuf,x
    inx
    lda expr_result+1
    sta strbuf,x
    inx
    stx instr_buf_pos
    bra @add_bytes

@maybe_byte_operand
    lda instr_addr_mode
    and #<MODES_BYTE_OPERAND
    sta instr_supported_modes
    lda instr_addr_mode+1
    and #>MODES_BYTE_OPERAND
    ora instr_supported_modes
    beq @add_bytes  ; No operand, emit no more bytes
    ; For bit branch instructions, range-check and emit ZP (expr_b).
    ; Rest will emit the Rel8 (expr_result).
    lda asm_flags
    and #F_ASM_BITBRANCH
    beq +++
    ldq expr_result
    stq expr_a
    ldq expr_b
    stq expr_result
    jsr is_expr_byte
    bcs +
    lda #err_value_out_of_range
    sta err_code
    lbra statement_err_exit
+   ldx instr_buf_pos
    lda expr_result
    sta strbuf,x
    inx
    stx instr_buf_pos
    ldq expr_a
    stq expr_result
    clc
    bra +
+++
    ; Byte operand: emit one expr_result byte
    jsr is_branch_mnemonic  ; C=1: signed operand
+   jsr is_expr_byte
    bcs +
    lda #err_value_out_of_range
    sta err_code
    lbra statement_err_exit
+   ldx instr_buf_pos
    lda expr_result
    sta strbuf,x
    inx
    stx instr_buf_pos

@add_bytes
    ; Add bytes to segment
    ldx instr_buf_pos
    jsr assemble_bytes
    bcc ++
    lda pass
    bne +
    lda err_code
    cmp #err_pc_undef
    bne +
    ; PC undef on pass 0 is ok
    lda #0
    sta err_code
    bra ++
+   lbra statement_err_exit
++  lbra statement_ok_exit


; ------------------------------------------------------------
; Directives
; ------------------------------------------------------------

; Input: tokbuf, tok_pos, line_addr
; Output:
;  C=0 ok, tok_pos advanced; X=line pos, Y=length
;  C=1 not found, tok_pos preserved
;    err_code>0, line_pos: report error in expression
expect_string_arg:
    ldx tok_pos
    lda tokbuf,x
    cmp #tk_string_literal
    beq +
    sec
    rts
+   inx
    lda tokbuf,x
    taz
    inx
    ldy tokbuf,x
    inx
    stx tok_pos
    tza
    tax
    inx  ; Starting quote not included
    clc
    rts

; Directives that process arg lists of arbitrary length use the
; process_arg_list macro and a handler routine with the following API:
;
; Input:
;   A==arg_type_string: string arg, X=line pos, Y=length
;   A==arg_type_expr: expression arg, expr_result
; Output:
;   err_code>0 : abort with error
arg_type_string = 1
arg_type_expr = 2
!macro process_arg_list .handler {
.loop
    jsr expect_string_arg
    bcs +
    lda #arg_type_string
    jsr .handler
    bra ++
+   jsr expect_expr
    bcs +
    lda #arg_type_expr
    jsr .handler
    bra ++
+   lda #err_syntax
    sta err_code
    lbra statement_err_exit
++  lda err_code
    lbne statement_err_exit
    lda #tk_comma
    jsr expect_token
    lbcc .loop
}


; General handler for byte/word/lword
; Input: A=arg type, etc.; Z=width (1, 2, 4)
do_assemble_bytes:
    cmp #arg_type_string
    bne +
    lda #err_invalid_arg
    sta err_code
    rts
+   ldx #0
    lda expr_result
    sta strbuf,x
    inx
    cpz #2
    bcc @end
    lda expr_result+1
    sta strbuf,x
    inx
    cpz #4
    bcc @end
    lda expr_result+2
    sta strbuf,x
    inx
    lda expr_result+3
    sta strbuf,x
    inx
@end
    jsr assemble_bytes
    rts

; !byte ...
; !8 ...
; Assembles byte values.
do_byte:
    ldz #1
    lbra do_assemble_bytes
assemble_dir_byte:
    +process_arg_list do_byte
    lbra statement_ok_exit

; !byte ...
; !8 ...
; Assembles word values, little-endian.
do_word:
    ldz #2
    lbra do_assemble_bytes
assemble_dir_word:
    +process_arg_list do_word
    lbra statement_ok_exit

; !32 ...
; Assembles long word values, little-endian.
do_lword:
    ldz #4
    lbra do_assemble_bytes
assemble_dir_lword:
    +process_arg_list do_lword
    lbra statement_ok_exit


; !warn ...
; Prints "Line #: " followed by one or more arguments.
; Value arguments print: "<dec> ($<hex>) "
; String arguments print their contents.
do_warn:
    taz

    lda pass
    cmp #$ff
    beq +
    rts
+

    cpz #arg_type_string
    bne +
    jsr print_bas_str
    rts

+   ldq expr_result
    jsr print_dec32
    +kprimm_start
    !pet " ($",0
    +kprimm_end

    ; Print expr_result as hex, adjusting for leading zeroes (32, 16, or 8 bit)
    lda expr_result+3
    ora expr_result+2
    ora expr_result+1
    beq ++
    lda expr_result+3
    ora expr_result+2
    beq +
    lda expr_result+3
    jsr print_hex8
    lda expr_result+2
    jsr print_hex8
+   lda expr_result+1
    jsr print_hex8
++  lda expr_result
    jsr print_hex8

    +kprimm_start
    !pet ") ",0
    +kprimm_end
    rts

assemble_dir_warn:
    ; Only print warnings on final pass.
    ; (Acme prints on every pass. Will I regret this?)
    lda pass
    cmp #$ff
    bne +
    jsr print_warning_line_number
+
    +process_arg_list do_warn

    lda pass
    cmp #$ff
    bne +
    lda #chr_cr
    +kcall bsout
+
    lda asm_flags
    ora #F_ASM_WARN
    sta asm_flags
    lbra statement_ok_exit


assemble_dir_to:
assemble_dir_fill:
assemble_dir_pet:
assemble_dir_scr:
assemble_dir_source:
assemble_dir_binary:
    lda #err_unimplemented
    sta err_code
    lbra statement_err_exit


directive_jump_table:
; Sorted by token ID
!word assemble_dir_to
!word assemble_dir_byte, assemble_dir_byte
!word assemble_dir_word, assemble_dir_word
!word assemble_dir_lword
!word assemble_dir_fill
!word assemble_dir_pet
!word assemble_dir_scr
!word assemble_dir_source
!word assemble_dir_binary
!word assemble_dir_warn

assemble_directive:
    ; <pseudoop> arglist
    jsr expect_pseudoop
    lbcs statement_err_exit
    ; A=token ID
    sec
    sbc #mnemonic_count
    asl
    tax
    jmp (directive_jump_table,x)


; ------------------------------------------------------------
; Assembler
; ------------------------------------------------------------


statement_err_exit
    ldx stmt_tokpos
    stx tok_pos
    sec
    rts

statement_ok_exit
    clc
    rts


; Input: line_addr
; Output: err_code, line_pos
;  C=0 success, continue
;  C=1 stop assembly (err_code=0 end of program, other on error)
assemble_line:
    lda #0
    sta err_code

    lda line_addr
    sta bas_ptr
    lda line_addr+1
    sta bas_ptr+1

    ldz #0
    lda [bas_ptr],z
    inz
    ora [bas_ptr],z
    bne +
    ; End of program
    sec
    rts
+
    jsr tokenize
    lda err_code
    beq +
    ; Error return
    sec
    rts
+

    ldx #0
    stx tok_pos

@tokbuf_loop
    ldx tok_pos
    lda tokbuf,x
    lbeq @end_tokbuf
    stx stmt_tokpos

    jsr assemble_pc_assign
    lbcc @next_statement
    lda err_code
    lbne @err_exit

    jsr assemble_label
    bcs +
    cpz #0
    lbeq @next_statement
    ldx tok_pos
    lda tokbuf,x
    lbeq @end_tokbuf  ; label at end of line
    cmp #tk_colon
    beq @next_statement  ; colon follows label
    bra ++  ; label without equal not end of line or statement,
            ; must precede an instruction or directive
+   lda err_code
    lbne @err_exit

++
    jsr assemble_instruction
    bcc @next_statement
    lda err_code
    lbne @err_exit

    jsr assemble_directive
    bcc @next_statement
    lda err_code
    lbne @err_exit

    ; No statement patterns match. Syntax error.
    ldx tok_pos
    lda tokbuf+1,x
    sta line_pos
    lda #err_syntax
    sta err_code
    bra @err_exit

@next_statement
    ; If colon, try another statement.
    ; (This covers <label> ":" also.)
    lda #tk_colon
    jsr expect_token
    lbcc @tokbuf_loop

@must_end
    ldx tok_pos
    lda tokbuf,x
    beq @end_tokbuf
    lda #err_syntax
    sta err_code
    lda tokbuf+1,x
    sta line_pos
@err_exit
    sec
    rts
@end_tokbuf
    clc
    rts


; Input: pass
; Output:
;   err_code = 0: one full assembly pass
;   err_code > 0: error result
do_assemble_pass:
    lda #<(source_start+1)
    sta line_addr
    lda #>(source_start+1)
    sta line_addr+1

@line_loop
    jsr assemble_line
    bcs @end
    lda line_addr
    sta bas_ptr
    lda line_addr+1
    sta bas_ptr+1
    ldz #0
    lda [bas_ptr],z
    sta line_addr
    inz
    lda [bas_ptr],z
    sta line_addr+1
    bra @line_loop
@end
    rts


; Input: source in BASIC region
; Output:
;   err_code = 0: segment table built successfully
;   err_code > 0: error result
assemble_source:
    ; Do two assembly passes ($00, $FF), aborting for errors.
    lda #0
-   sta pass
    jsr init_pass
    jsr do_assemble_pass
    lda err_code
    bne @done
    lda pass
    cmp #$ff
    beq @done
    lda #$ff
    bra -
@done
    jsr print_error
    rts


; ------------------------------------------------------------
; Data
; ------------------------------------------------------------

; ---------------------------------------------------------
; Error message strings

err_message_tbl:
!word e01,e02,e03,e04,e05,e06,e07,e08,e09,e10,e11,e12,e13

err_messages:
err_syntax = 1
e01: !pet "syntax error",0
err_pc_undef = 2
e02: !pet "program counter undefined",0
err_value_out_of_range = 3
e03: !pet "value out of range",0
err_unsupported_addr_mode = 4
e04: !pet "unsupported addressing mode for instruction",0
err_already_defined = 5
e05: !pet "symbol already defined",0
err_out_of_memory = 6
e06: !pet "out of memory",0
err_undefined = 7
e07: !pet "symbol undefined",0
err_pc_overflow = 8
e08: !pet "program counter overflowed $ffff",0
err_label_assign_global_only = 9
e09: !pet "only global labels can be assigned with =",0
err_unimplemented = 10
e10: !pet "unimplemented feature",0
err_exponent_negative = 11
e11: !pet "exponent cannot be negative",0
err_fraction_not_supported = 12
e12: !pet "fraction operator not supported",0
err_invalid_arg = 13
e13: !pet "argument not allowed",0

warn_message_tbl:
!word w01
warn_messages:
warn_ldz_range = 1
w01: !pet "ldz with address $00-$FF behaves as $0000-$00FF (ldz+2 to silence)"

; ---------------------------------------------------------
; Mnemonics token list
mnemonic_count = 145
mnemonics:
!pet "adc",0   ; $00
!pet "adcq",0  ; $01
!pet "and",0   ; $02
!pet "andq",0  ; $03
!pet "asl",0   ; $04
!pet "aslq",0  ; $05
!pet "asr",0   ; $06
!pet "asrq",0  ; $07
!pet "asw",0   ; $08
!pet "bbr0",0  ; $09
mnemonic_bbr0 = $09
!pet "bbr1",0  ; $0A
!pet "bbr2",0  ; $0B
!pet "bbr3",0  ; $0C
!pet "bbr4",0  ; $0D
!pet "bbr5",0  ; $0E
!pet "bbr6",0  ; $0F
!pet "bbr7",0  ; $10
!pet "bbs0",0  ; $11
!pet "bbs1",0  ; $12
!pet "bbs2",0  ; $13
!pet "bbs3",0  ; $14
!pet "bbs4",0  ; $15
!pet "bbs5",0  ; $16
!pet "bbs6",0  ; $17
!pet "bbs7",0  ; $18
mnemonic_bbs7 = $18
!pet "bcc",0   ; $19
!pet "bcs",0   ; $1A
!pet "beq",0   ; $1B
!pet "bit",0   ; $1C
mnemonic_bit = $1C
!pet "bitq",0  ; $1D
mnemonic_bitq = $1D
!pet "bmi",0   ; $1E
!pet "bne",0   ; $1F
!pet "bpl",0   ; $20
!pet "bra",0   ; $21
!pet "brk",0   ; $22
mnemonic_brk = $22
!pet "bsr",0   ; $23
!pet "bvc",0   ; $24
!pet "bvs",0   ; $25
mnemonic_bvs = $25
!pet "clc",0   ; $26
!pet "cld",0   ; $27
!pet "cle",0   ; $28
!pet "cli",0   ; $29
!pet "clv",0   ; $2A
!pet "cmp",0   ; $2B
!pet "cmpq",0  ; $2C
!pet "cpq",0   ; $2D
!pet "cpx",0   ; $2E
!pet "cpy",0   ; $2F
!pet "cpz",0   ; $30
!pet "dec",0   ; $31
!pet "deq",0   ; $32
!pet "dew",0   ; $33
!pet "dex",0   ; $34
!pet "dey",0   ; $35
!pet "dez",0   ; $36
!pet "eom",0   ; $37
!pet "eor",0   ; $38
!pet "eorq",0  ; $39
!pet "inc",0   ; $3A
!pet "inq",0   ; $3B
!pet "inw",0   ; $3C
!pet "inx",0   ; $3D
!pet "iny",0   ; $3E
!pet "inz",0   ; $3F
!pet "jmp",0   ; $40
!pet "jsr",0   ; $41
!pet "lbcc",0   ; $42
mnemonic_lbcc = $42
!pet "lbcs",0   ; $43
!pet "lbeq",0   ; $44
!pet "lbmi",0   ; $45
!pet "lbne",0   ; $46
!pet "lbpl",0   ; $47
!pet "lbra",0   ; $48
!pet "lbvc",0   ; $49
!pet "lbvs",0   ; $4A
mnemonic_lbvs = $4A
!pet "lda",0   ; $4B
!pet "ldq",0   ; $4C
!pet "ldx",0   ; $4D
!pet "ldy",0   ; $4E
!pet "ldz",0   ; $4F
mnemonic_ldz = $4F
!pet "lsr",0   ; $50
!pet "lsrq",0  ; $51
!pet "map",0   ; $52
!pet "neg",0   ; $53
!pet "ora",0   ; $54
!pet "orq",0   ; $55
!pet "pha",0   ; $56
!pet "php",0   ; $57
!pet "phw",0   ; $58
mnemonic_phw = $58
!pet "phx",0   ; $59
!pet "phy",0   ; $5A
!pet "phz",0   ; $5B
!pet "pla",0   ; $5C
!pet "plp",0   ; $5D
!pet "plx",0   ; $5E
!pet "ply",0   ; $5F
!pet "plz",0   ; $60
!pet "rmb0",0  ; $61
!pet "rmb1",0  ; $62
!pet "rmb2",0  ; $63
!pet "rmb3",0  ; $64
!pet "rmb4",0  ; $65
!pet "rmb5",0  ; $66
!pet "rmb6",0  ; $67
!pet "rmb7",0  ; $68
!pet "rol",0   ; $69
!pet "rolq",0  ; $6A
!pet "ror",0   ; $6B
!pet "rorq",0  ; $6C
!pet "row",0   ; $6D
!pet "rti",0   ; $6E
!pet "rts",0   ; $6F
!pet "sbc",0   ; $70
!pet "sbcq",0  ; $71
!pet "sec",0   ; $72
!pet "sed",0   ; $73
!pet "see",0   ; $74
!pet "sei",0   ; $75
!pet "smb0",0  ; $76
!pet "smb1",0  ; $77
!pet "smb2",0  ; $78
!pet "smb3",0  ; $79
!pet "smb4",0  ; $7A
!pet "smb5",0  ; $7B
!pet "smb6",0  ; $7C
!pet "smb7",0  ; $7D
!pet "sta",0   ; $7E
!pet "stq",0   ; $7F
!pet "stx",0   ; $80
!pet "sty",0   ; $81
!pet "stz",0   ; $82
!pet "tab",0   ; $83
!pet "tax",0   ; $84
!pet "tay",0   ; $85
!pet "taz",0   ; $86
!pet "tba",0   ; $87
!pet "trb",0   ; $88
!pet "tsb",0   ; $89
!pet "tsx",0   ; $8A
!pet "tsy",0   ; $8B
!pet "txa",0   ; $8C
!pet "txs",0   ; $8D
!pet "tya",0   ; $8E
!pet "tys",0   ; $8F
!pet "tza",0   ; $90
!byte 0

; Token IDs for the Q mnemonics, which all use a $42 $42 encoding prefix
q_mnemonics:
!byte $01, $03, $05, $07, $1D, $2C, $2D, $32, $39, $3B, $4C, $52, $46, $6A
!byte $6C, $71, $7F
!byte 0

; Pseudo-op table
; These tokens are preceded with a "!" character.
pseudoops:
po_to = mnemonic_count + 0
!pet "to",0
po_byte = mnemonic_count + 1
!pet "byte",0
po_8 = mnemonic_count + 2
!pet "8",0
po_word = mnemonic_count + 3
!pet "word",0
po_16 = mnemonic_count + 4
!pet "16",0
po_32 = mnemonic_count + 5
!pet "32",0
po_fill = mnemonic_count + 6
!pet "fill",0
po_pet = mnemonic_count + 7
!pet "pet",0
po_scr = mnemonic_count + 8
!pet "scr",0
po_source = mnemonic_count + 9
!pet "source",0
po_binary = mnemonic_count + 10
!pet "binary",0
po_warn = mnemonic_count + 11
!pet "warn",0
!byte 0
last_po = po_warn + 1

; Other tokens table
; These tokens are lexed up to their length, in order, with no delimiters.
other_tokens:
tk_complement = last_po + 0
!pet "!",0
tk_power = last_po + 1
!pet "^",0
tk_megabyte = last_po + 2
!pet "^^",0
tk_minus = last_po + 3
!pet "-",0
tk_multiply = last_po + 4
!pet "*",0
tk_remainder = last_po + 5
!pet "%",0
tk_plus = last_po + 6
!pet "+",0
tk_lsr = last_po + 7
!pet ">>>",0
tk_asr = last_po + 8
!pet ">>",0
tk_asl = last_po + 9
!pet "<<",0
tk_lt = last_po + 10
!pet "<",0
tk_gt = last_po + 11
!pet ">",0
tk_ampersand = last_po + 12
!pet "&",0
tk_pipe = last_po + 13
!pet "|",0
tk_comma = last_po + 14
!pet ",",0
tk_hash = last_po + 15
!pet "#",0
tk_colon = last_po + 16
!pet ":",0
tk_equal = last_po + 17
!pet "=",0
tk_lparen = last_po + 18
!pet "(",0
tk_rparen = last_po + 19
!pet ")",0
tk_lbracket = last_po + 20
!pet "[",0
tk_rbracket = last_po + 21
!pet "]",0
tk_fraction = last_po + 22  ; (Not supported, but special error message)
!pet "/",0
!byte 0
last_tk = tk_fraction + 1

; Other token IDs
tk_number_literal = last_tk + 0
tk_number_literal_leading_zero = last_tk + 1
tk_string_literal = last_tk + 2
tk_label_or_reg = last_tk + 3

; Keywords
; Tokenized as tk_label_or_reg. Case insensitive.
kw_x: !pet "x",0
kw_y: !pet "y",0
kw_z: !pet "z",0
kw_sp: !pet "sp",0
kw_div: !pet "div",0
kw_xor: !pet "xor",0
kw_cbm: !pet "cbm",0
kw_raw: !pet "raw",0
kw_runnable: !pet "runnable",0


; ------------------------------------------------------------
; Instruction encodings
;
; The addressing_modes table consists of one entry per instruction mnemonic,
; four bytes per entry, in token ID order.
;
; The first two bytes are an addressing mode bitmask, one bit set for each
; addressing mode supported by the instruction.
;
; The last two bytes are the code address for the encoding list. (See below,
; starting with enc_adc.)
;
; The bit branch instructions, which take two operands, are not uniquely
; represented by this data structure. expect_addressing_expr will return the
; "base page" mode and leave the token position on the comma for further
; processing.
;
;     %11111111,
;      ^ Implied (parameterless, or A/Q)
;       ^ Immediate
;        ^ Immedate word
;         ^ Base-Page, branch relative, bit-test branch relative
;          ^ Base-Page X-Indexed
;           ^ Base-Page Y-Indexed
;            ^ Absolute, 16-bit branch relative
;             ^ Absolute X-Indexed
;               %11111111
;                ^ Absolute Y-Indexed
;                 ^ Absolute Indirect
;                  ^ Absolute Indirect X-Indexed
;                   ^ Base-Page Indirect X-Indexed
;                    ^ Base-Page Indirect Y-Indexed
;                     ^ Base-Page Indirect Z-Indexed (or no index)
;                      ^ 32-bit Base-Page Indirect Z-Indexed (or no index)
;                       ^ Stack Relative Indirect, Y-Indexed
MODES_NO_OPERAND     = %1000000000000000
MODES_BYTE_OPERAND   = %0101110000011111
MODES_WORD_OPERAND   = %0010001111100000
MODE_IMPLIED         = %1000000000000000
MODE_IMMEDIATE       = %0100000000000000
MODE_IMMEDIATE_WORD  = %0010000000000000
MODE_BASE_PAGE       = %0001000000000000
MODE_BASE_PAGE_X     = %0000100000000000
MODE_BASE_PAGE_Y     = %0000010000000000
MODE_ABSOLUTE        = %0000001000000000
MODE_ABSOLUTE_X      = %0000000100000000
MODE_ABSOLUTE_Y      = %0000000010000000
MODE_ABSOLUTE_IND    = %0000000001000000
MODE_ABSOLUTE_IND_X  = %0000000000100000
MODE_BASE_PAGE_IND_X = %0000000000010000
MODE_BASE_PAGE_IND_Y = %0000000000001000
MODE_BASE_PAGE_IND_Z = %0000000000000100
MODE_32BIT_IND       = %0000000000000010
MODE_STACK_REL       = %0000000000000001
addressing_modes:
!word %0101101110011110  ; adc
!word enc_adc
!word %0001001000000110  ; adcq
!word enc_adcq
!word %0101101110011110  ; and
!word enc_and
!word %0001001000000110  ; andq
!word enc_andq
!word %1001101100000000  ; asl
!word enc_asl
!word %1001101100000000  ; aslq
!word enc_aslq
!word %1001100000000000  ; asr
!word enc_asr
!word %1001100000000000  ; asrq
!word enc_asrq
!word %0000001000000000  ; asw
!word enc_asw
!word %0001000000000000  ; bbr0
!word enc_bbr0
!word %0001000000000000  ; bbr1
!word enc_bbr1
!word %0001000000000000  ; bbr2
!word enc_bbr2
!word %0001000000000000  ; bbr3
!word enc_bbr3
!word %0001000000000000  ; bbr4
!word enc_bbr4
!word %0001000000000000  ; bbr5
!word enc_bbr5
!word %0001000000000000  ; bbr6
!word enc_bbr6
!word %0001000000000000  ; bbr7
!word enc_bbr7
!word %0001000000000000  ; bbs0
!word enc_bbs0
!word %0001000000000000  ; bbs1
!word enc_bbs1
!word %0001000000000000  ; bbs2
!word enc_bbs2
!word %0001000000000000  ; bbs3
!word enc_bbs3
!word %0001000000000000  ; bbs4
!word enc_bbs4
!word %0001000000000000  ; bbs5
!word enc_bbs5
!word %0001000000000000  ; bbs6
!word enc_bbs6
!word %0001000000000000  ; bbs7
!word enc_bbs7
!word %0001000000000000  ; bcc
!word enc_bcc
!word %0001000000000000  ; bcs
!word enc_bcs
!word %0001000000000000  ; beq
!word enc_beq
!word %0101101100000000  ; bit
!word enc_bit
!word %0001001000000000  ; bitq
!word enc_bitq
!word %0001000000000000  ; bmi
!word enc_bmi
!word %0001000000000000  ; bne
!word enc_bne
!word %0001000000000000  ; bpl
!word enc_bpl
!word %0001000000000000  ; bra
!word enc_bra
!word %1000000000000000  ; brk
!word enc_brk
!word %0000001000000000  ; bsr
!word enc_bsr
!word %0001000000000000  ; bvc
!word enc_bvc
!word %0001000000000000  ; bvs
!word enc_bvs
!word %1000000000000000  ; clc
!word enc_clc
!word %1000000000000000  ; cld
!word enc_cld
!word %1000000000000000  ; cle
!word enc_cle
!word %1000000000000000  ; cli
!word enc_cli
!word %1000000000000000  ; clv
!word enc_clv
!word %0101101110011110  ; cmp
!word enc_cmp
!word %0001001000000110  ; cmpq
!word enc_cmpq
!word %0001001000000110  ; cpq
!word enc_cmpq
!word %0101001000000000  ; cpx
!word enc_cpx
!word %0101001000000000  ; cpy
!word enc_cpy
!word %0101001000000000  ; cpz
!word enc_cpz
!word %1001101100000000  ; dec
!word enc_dec
!word %1001101100000000  ; deq
!word enc_deq
!word %0001000000000000  ; dew
!word enc_dew
!word %1000000000000000  ; dex
!word enc_dex
!word %1000000000000000  ; dey
!word enc_dey
!word %1000000000000000  ; dez
!word enc_dez
!word %1000000000000000  ; eom
!word enc_eom
!word %0101101110011110  ; eor
!word enc_eor
!word %0001001000000110  ; eorq
!word enc_eorq
!word %1001101100000000  ; inc
!word enc_inc
!word %1001101100000000  ; inq
!word enc_inq
!word %0001000000000000  ; inw
!word enc_inw
!word %1000000000000000  ; inx
!word enc_inx
!word %1000000000000000  ; iny
!word enc_iny
!word %1000000000000000  ; inz
!word enc_inz
!word %0000001001100000  ; jmp
!word enc_jmp
!word %0000001001100000  ; jsr
!word enc_jsr
!word %0000001000000000  ; lbcc
!word enc_lbcc
!word %0000001000000000  ; lbcs
!word enc_lbcs
!word %0000001000000000  ; lbeq
!word enc_lbeq
!word %0000001000000000  ; lbmi
!word enc_lbmi
!word %0000001000000000  ; lbne
!word enc_lbne
!word %0000001000000000  ; lbpl
!word enc_lbpl
!word %0000001000000000  ; lbra
!word enc_lbra
!word %0000001000000000  ; lbvc
!word enc_lbvc
!word %0000001000000000  ; lbvs
!word enc_lbvs
!word %0101101110011111  ; lda
!word enc_lda
!word %0001001000000110  ; ldq
!word enc_ldq
!word %0101011010000000  ; ldx
!word enc_ldx
!word %0101101100000000  ; ldy
!word enc_ldy
!word %0100001100000000  ; ldz
!word enc_ldz
!word %1001101100000000  ; lsr
!word enc_lsr
!word %1001101100000000  ; lsrq
!word enc_lsrq
!word %1000000000000000  ; map
!word enc_map
!word %1000000000000000  ; neg
!word enc_neg
!word %0101101110011110  ; ora
!word enc_ora
!word %0001001000000110  ; orq
!word enc_orq
!word %1000000000000000  ; pha
!word enc_pha
!word %1000000000000000  ; php
!word enc_php
!word %0010001000000000  ; phw
!word enc_phw
!word %1000000000000000  ; phx
!word enc_phx
!word %1000000000000000  ; phy
!word enc_phy
!word %1000000000000000  ; phz
!word enc_phz
!word %1000000000000000  ; pla
!word enc_pla
!word %1000000000000000  ; plp
!word enc_plp
!word %1000000000000000  ; plx
!word enc_plx
!word %1000000000000000  ; ply
!word enc_ply
!word %1000000000000000  ; plz
!word enc_plz
!word %0001000000000000  ; rmb0
!word enc_rmb0
!word %0001000000000000  ; rmb1
!word enc_rmb1
!word %0001000000000000  ; rmb2
!word enc_rmb2
!word %0001000000000000  ; rmb3
!word enc_rmb3
!word %0001000000000000  ; rmb4
!word enc_rmb4
!word %0001000000000000  ; rmb5
!word enc_rmb5
!word %0001000000000000  ; rmb6
!word enc_rmb6
!word %0001000000000000  ; rmb7
!word enc_rmb7
!word %1001101100000000  ; rol
!word enc_rol
!word %1001101100000000  ; rolq
!word enc_rolq
!word %1001101100000000  ; ror
!word enc_ror
!word %1001101100000000  ; rorq
!word enc_rorq
!word %0000001000000000  ; row
!word enc_row
!word %1000000000000000  ; rti
!word enc_rti
!word %1100000000000000  ; rts
!word enc_rts
!word %0101101110011110  ; sbc
!word enc_sbc
!word %0001001000000110  ; sbcq
!word enc_sbcq
!word %1000000000000000  ; sec
!word enc_sec
!word %1000000000000000  ; sed
!word enc_sed
!word %1000000000000000  ; see
!word enc_see
!word %1000000000000000  ; sei
!word enc_sei
!word %0001000000000000  ; smb0
!word enc_smb0
!word %0001000000000000  ; smb1
!word enc_smb1
!word %0001000000000000  ; smb2
!word enc_smb2
!word %0001000000000000  ; smb3
!word enc_smb3
!word %0001000000000000  ; smb4
!word enc_smb4
!word %0001000000000000  ; smb5
!word enc_smb5
!word %0001000000000000  ; smb6
!word enc_smb6
!word %0001000000000000  ; smb7
!word enc_smb7
!word %0001101110011111  ; sta
!word enc_sta
!word %0001001000000110  ; stq
!word enc_stq
!word %0001011010000000  ; stx
!word enc_stx
!word %0001101100000000  ; sty
!word enc_sty
!word %0001101100000000  ; stz
!word enc_stz
!word %1000000000000000  ; tab
!word enc_tab
!word %1000000000000000  ; tax
!word enc_tax
!word %1000000000000000  ; tay
!word enc_tay
!word %1000000000000000  ; taz
!word enc_taz
!word %1000000000000000  ; tba
!word enc_tba
!word %0001001000000000  ; trb
!word enc_trb
!word %0001001000000000  ; tsb
!word enc_tsb
!word %1000000000000000  ; tsx
!word enc_tsx
!word %1000000000000000  ; tsy
!word enc_tsy
!word %1000000000000000  ; txa
!word enc_txa
!word %1000000000000000  ; txs
!word enc_txs
!word %1000000000000000  ; tya
!word enc_tya
!word %1000000000000000  ; tys
!word enc_tys
!word %1000000000000000  ; tza
!word enc_tza

; ------------------------------------------------------------
; Encoding lists
; Single-byte encodings for each supported addressing mode, msb to lsb in the bitfield
; Quad prefix $42 $42 and 32-bit Indirect prefix $ea are added in code.
; Example:
;   "adc",0,%01011011,%10011110
;             ^ Immediate = $69
;               ^ Base page = $65
;                ^ Base page, X-indexed = $75
;                  ^ Absolute = $6d
;                   ^ Absolute, X-indexed = $7d
;                      ^ Absolute, Y-indexed = $79
;                         ^ Base-Page Indirect X-Indexed = $61
;                          ^ Base-Page Indirect Y-Indexed = $71
;                           ^ Base-Page Indirect Z-Indexed = $72
;                            ^ 32-bit Base-Page Indirect Z-Indexed = ($EA) $72
enc_adc : !byte $69, $65, $75, $6d, $7d, $79, $61, $71, $72, $72
enc_adcq: !byte $65, $6d, $72, $72
enc_and : !byte $29, $25, $35, $2d, $3d, $39, $21, $31, $32, $32
enc_andq: !byte $25, $2d, $32, $32
enc_asl : !byte $0a, $06, $16, $0e, $1e
enc_aslq: !byte $0a, $06, $16, $0e, $1e
enc_asr : !byte $43, $44, $54
enc_asrq: !byte $43, $44, $54
enc_asw : !byte $cb
enc_bbr0: !byte $0f
enc_bbr1: !byte $1f
enc_bbr2: !byte $2f
enc_bbr3: !byte $3f
enc_bbr4: !byte $4f
enc_bbr5: !byte $5f
enc_bbr6: !byte $6f
enc_bbr7: !byte $7f
enc_bbs0: !byte $8f
enc_bbs1: !byte $9f
enc_bbs2: !byte $af
enc_bbs3: !byte $bf
enc_bbs4: !byte $cf
enc_bbs5: !byte $df
enc_bbs6: !byte $ef
enc_bbs7: !byte $ff
enc_bcc : !byte $90
enc_bcs : !byte $b0
enc_beq : !byte $f0
enc_bit : !byte $89, $24, $34, $2c, $3c
enc_bitq: !byte $24, $2c
enc_bmi : !byte $30
enc_bne : !byte $d0
enc_bpl : !byte $10
enc_bra : !byte $80
enc_brk : !byte $00
enc_bsr : !byte $63
enc_bvc : !byte $50
enc_bvs : !byte $70
enc_clc : !byte $18
enc_cld : !byte $d8
enc_cle : !byte $02
enc_cli : !byte $58
enc_clv : !byte $b8
enc_cmp : !byte $c9, $c5, $d5, $cd, $dd, $d9, $c1, $d1, $d2, $d2
enc_cmpq: !byte $c5, $cd, $d2, $d2
enc_cpx : !byte $e0, $e4, $ec
enc_cpy : !byte $c0, $c4, $cc
enc_cpz : !byte $c2, $d4, $dc
enc_dec : !byte $3a, $c6, $d6, $ce, $de
enc_deq : !byte $3a, $c6, $d6, $ce, $de
enc_dew : !byte $c3
enc_dex : !byte $ca
enc_dey : !byte $88
enc_dez : !byte $3b
enc_eom : !byte $ea
enc_eor : !byte $49, $45, $55, $4d, $5d, $59, $41, $51, $52, $52
enc_eorq: !byte $45, $4d, $52, $52
enc_inc : !byte $1a, $e6, $f6, $ee, $fe
enc_inq : !byte $1a, $e6, $f6, $ee, $fe
enc_inw : !byte $e3
enc_inx : !byte $e8
enc_iny : !byte $c8
enc_inz : !byte $1b
enc_jmp : !byte $4c, $6c, $7c
enc_jsr : !byte $20, $22, $23
enc_lbcc : !byte $93
enc_lbcs : !byte $b3
enc_lbeq : !byte $f3
enc_lbmi : !byte $33
enc_lbne : !byte $d3
enc_lbpl : !byte $13
enc_lbra : !byte $83
enc_lbvc : !byte $53
enc_lbvs : !byte $73
enc_lda : !byte $a9, $a5, $b5, $ad, $bd, $b9, $a1, $b1, $b2, $b2, $e2
enc_ldq : !byte $a5, $ad, $b2, $b2
enc_ldx : !byte $a2, $a6, $b6, $ae, $be
enc_ldy : !byte $a0, $a4, $b4, $ac, $bc
enc_ldz : !byte $a3, $ab, $bb
enc_lsr : !byte $4a, $46, $56, $4e, $5e
enc_lsrq: !byte $4a, $46, $56, $4e, $5e
enc_map : !byte $5c
enc_neg : !byte $42
enc_ora : !byte $09, $05, $15, $0d, $1d, $19, $01, $11, $12, $12
enc_orq : !byte $05, $0d, $12, $12
enc_pha : !byte $48
enc_php : !byte $08
enc_phw : !byte $f4, $fc
enc_phx : !byte $da
enc_phy : !byte $5a
enc_phz : !byte $db
enc_pla : !byte $68
enc_plp : !byte $28
enc_plx : !byte $fa
enc_ply : !byte $7a
enc_plz : !byte $fb
enc_rmb0: !byte $07
enc_rmb1: !byte $17
enc_rmb2: !byte $27
enc_rmb3: !byte $37
enc_rmb4: !byte $47
enc_rmb5: !byte $57
enc_rmb6: !byte $67
enc_rmb7: !byte $77
enc_rol : !byte $2a, $26, $36, $2e, $3e
enc_rolq: !byte $2a, $26, $36, $2e, $3e
enc_ror : !byte $6a, $66, $76, $6e, $7e
enc_rorq: !byte $6a, $66, $76, $6e, $7e
enc_row : !byte $eb
enc_rti : !byte $40
enc_rts : !byte $60, $62
enc_sbc : !byte $e9, $e5, $f5, $ed, $fd, $f9, $e1, $f1, $f2, $f2
enc_sbcq: !byte $e5, $ed, $f2, $f2
enc_sec : !byte $38
enc_sed : !byte $f8
enc_see : !byte $03
enc_sei : !byte $78
enc_smb0: !byte $87
enc_smb1: !byte $97
enc_smb2: !byte $a7
enc_smb3: !byte $b7
enc_smb4: !byte $c7
enc_smb5: !byte $d7
enc_smb6: !byte $e7
enc_smb7: !byte $f7
enc_sta : !byte $85, $95, $8d, $9d, $99, $81, $91, $92, $92, $82
enc_stq : !byte $85, $8d, $92, $92
enc_stx : !byte $86, $96, $8e, $9b
enc_sty : !byte $84, $94, $8c, $8b
enc_stz : !byte $64, $74, $9c, $9e
enc_tab : !byte $5b
enc_tax : !byte $aa
enc_tay : !byte $a8
enc_taz : !byte $4b
enc_tba : !byte $7b
enc_trb : !byte $14, $1c
enc_tsb : !byte $04, $0c
enc_tsx : !byte $ba
enc_tsy : !byte $0b
enc_txa : !byte $8a
enc_txs : !byte $9a
enc_tya : !byte $98
enc_tys : !byte $2b
enc_tza : !byte $6b


; ------------------------------------------------------------
; Screen code translation table
; Index: PETSCII code, value: screen code
; Untranslatable characters become spaces.
scr_table:
!scr $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
!scr $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
!scr ' ', '!', $22, '#', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/'
!scr '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ':', ';', '<', '=', '>', '?'
!scr '@', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o'
!scr 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '[', $1c, ']', $1e, $1f
!scr $40, 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O'
!scr 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', $5b, $5c, $5d, $5e, $5f
!scr $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
!scr $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
!scr $60, $61, $62, $63, $64, $65, $66, $67, $68, $69, $6a, $6b, $6c, $6d, $6e, $6f
!scr $70, $71, $72, $73, $74, $75, $76, $77, $78, $79, $7a, $7b, $7c, $7d, $7e, $7f
!scr $40, 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O'
!scr 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', $5b, $5c, $5d, $5e, $5f
!scr $60, $61, $62, $63, $64, $65, $66, $67, $68, $69, $6a, $6b, $6c, $6d, $6e, $6f
!scr $70, $71, $72, $73, $74, $75, $76, $77, $78, $79, $7a, $7b, $7c, $7d, $7e, $7f


; ---------------------------------------------------------
; Tests
; ---------------------------------------------------------

; A test suite provides run_test_suite_cmd, run with: SYS $1E04,4

!source "test_common.asm"
; !source "test_suite_1.asm"
; !source "test_suite_2.asm"
; !source "test_suite_3.asm"
; !source "test_suite_4.asm"
!source "test_suite_5.asm"
; run_test_suite_cmd: rts

; ---------------------------------------------------------

; !warn "EasyAsm remaining code space: ", max_end_of_program - *
!if * >= max_end_of_program {
    !error "EasyAsm code is too large, * = ", *
}
