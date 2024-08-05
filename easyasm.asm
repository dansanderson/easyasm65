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

* = $100 - 46

pass            *=*+1  ; $FF=final pass
program_counter *=*+2
asm_flags       *=*+1
F_ASM_PC_DEFINED = %00000001
current_segment *=*+4
next_segment_pc *=*+2
next_segment_byte_addr *=*+4

symtbl_next_name *=*+3

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
F_EXPR_BRACKET_ZERO   = %00000011
; - Expr contains undefined symbol
F_EXPR_UNDEFINED      = %00000100

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
attic_symbol_names  = attic_symbol_table + $2000   ; 1.5700-1.B6FF (24 KB)
attic_symbol_names_end = attic_symbol_names + $6000
attic_segments      = attic_symbol_names + $6000   ; 1.B700-2.6700 (44 KB)
attic_segments_end  = attic_segments + $b000

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
    !pet "*** ",0
    +kprimm_end
    ldx #<id_string
    ldy #>id_string
    jsr print_cstr
    +kprimm_start
    !pet ", by dddaaannn ***         ",13,0
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
    !pet "https://github.com/dansanderson/easyasm65  ",13
    !pet "                                           ",13
    !pet " 1. assemble to memory                     ",13
    !pet " 2. assemble to disk                       ",13
    !pet " 3. restore source                         ",13,0
    +kprimm_end
    +kprimm_start
    !pet " run/stop: close menu                      ",13
    !pet "                                           ",13
    !pet " your choice? ",15,166,143,"                            ",13
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
    jsr do_banner
    +kprimm_start
    !pet "assemble to memory",13,13,0
    +kprimm_end

    jsr stash_source
    jsr assemble_source
    ; TODO: the rest of the owl

    jsr restore_source
    rts


assemble_to_disk_cmd:
    jsr do_banner
    +kprimm_start
    !pet "assemble to disk",13,13,0
    +kprimm_end

    jsr stash_source
    jsr assemble_source
    ; TODO: the rest of the owl

    rts


restore_source_cmd:
    jsr do_banner
    +kprimm_start
    !pet "restore source",13,13,0
    +kprimm_end

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
    ldx line_pos   ; Indent by line_pos
-   lda #chr_spc
    +kcall bsout
    dex
    bne -
    lda #chr_uparrow
    +kcall bsout
    lda #chr_cr
    +kcall bsout

+++ rts


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
;  expr_flags F_EXPR_BRACKET_ZERO bit set if hex or dec literal has a leading zero
accept_literal:
    ; Init expr zero flag to 0.
    lda expr_flags
    and #!F_EXPR_BRACKET_ZERO
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
    ora #F_EXPR_BRACKET_ZERO
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
    ora #F_EXPR_BRACKET_ZERO
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
;   If found, C=1, X=token number, line_pos advanced
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
    stx line_pos  ; new line pos
    tya
    tax  ; X = mnemonic token ID
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
    and #F_EXPR_BRACKET_ZERO
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
    bcs @push_tok_pos_then_continue

    ; Pseudoop
    phz
    jsr tokenize_pseudoop
    plz
    bcs @push_tok_pos_then_continue

    ; Punctuation token
    ; Note: This tokenizes relative labels (---, +++) as punctuation tokens.
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
    ; TODO: add to/look up in symbol table, store table index and not label name?
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
    ; Null terminate tokbuf
    lda #0
    ldx tok_pos
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
    stq symtbl_next_name

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
    adcq attic_ptr
    stq expr_a
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
    lda [attic_ptr],z
    ora F_SYMTBL_DEFINED
    sta [attic_ptr],z
    rts


; ------------------------------------------------------------
; Segment table
; ------------------------------------------------------------

; Initializes the segment table.
init_segment_table:
    lda #<attic_segments
    sta current_segment
    lda #>attic_segments
    sta current_segment+1
    lda #^attic_segments
    sta current_segment+2
    lda #<(attic_segments >>> 24)
    sta current_segment+3

    lda #<(attic_segments+4)
    sta next_segment_byte_addr
    lda #>(attic_segments+4)
    sta next_segment_byte_addr+1
    lda #^(attic_segments+4)
    sta next_segment_byte_addr+2
    lda #<((attic_segments + 4) >>> 24)
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
;     pass $00: PC not defined
;     pass $FF: out of memory error
;   Uses expr_a
assemble_bytes:
    cpx #0
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
    sec
    rts
+

    ; If program_counter != next_segment_pc, create a new segment header.
    ; (segment_pc_16, length_16)
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

    rts


; ------------------------------------------------------------
; Assembler
; ------------------------------------------------------------

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

    ; TODO: Parse and assemble tokbuf.
    ; DEBUG: print source line, with line number
    ldz #3
    lda [bas_ptr],z
    tax
    dez
    lda [bas_ptr],z
    ldy #0
    ldz #0
    clc                    ; request unsigned
    jsr print_dec32
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

    ; DEBUG: print tokbuf as hex values
    ldx #0
@print_tokbuf_loop
    lda tokbuf,x
    lbeq @end_tokbuf

    cmp #tk_string_literal
    bne +
    +kprimm_start
    !pet "[str l:",0
    +kprimm_end
    inx
    inx
    lda tokbuf,x
    jsr print_hex8
    +kprimm_start
    !pet "]",0
    +kprimm_end
    inx
    bra @print_tokbuf_loop

+   cmp #tk_number_literal
    bne +
    +kprimm_start
    !pet "[lit v:",0
    +kprimm_end
    inx
    inx
    lda tokbuf,x
    sta expr_b
    inx
    lda tokbuf,x
    sta expr_b+1
    inx
    lda tokbuf,x
    sta expr_b+2
    inx
    lda tokbuf,x
    sta expr_b+3
    phx
    ldq expr_b
    jsr print_dec32
    plx
    +kprimm_start
    !pet "]",0
    +kprimm_end
    inx
    lbra @print_tokbuf_loop

+   cmp #tk_label_or_reg
    bne +
    +kprimm_start
    !pet "[label l:",0
    +kprimm_end
    inx
    inx
    lda tokbuf,x
    jsr print_hex8
    +kprimm_start
    !pet "]",0
    +kprimm_end
    inx
    lbra @print_tokbuf_loop

+
    +kprimm_start
    !pet "[tok ",0
    +kprimm_end
    jsr print_hex8
    inx
    inx
    +kprimm_start
    !pet "]",0
    +kprimm_end
    lbra @print_tokbuf_loop

@end_tokbuf
    lda #chr_cr
    +kcall bsout

    clc
    rts


assemble_source:
    lda #<(source_start+1)
    sta line_addr
    lda #>(source_start+1)
    sta line_addr+1

    lda #0
    sta pass
    jsr init_pass

    ; DEBUG: single pass to print source tokenized, detect lexer errors
@line_loop
    jsr assemble_line
    bcs +
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
+   lda err_code
    beq @end_of_program
    jsr print_error
@end_of_program
    rts


; ---------------------------------------------------------
; Error message strings

err_message_tbl:
!word e01

err_messages:
err_syntax = 1
e01: !pet "syntax error",0


; ---------------------------------------------------------
; Mnemonics token list
mnemonic_count = 136
mnemonics:
!pet "adc",0
!pet "adcq",0  ; $01
!pet "and",0
!pet "andq",0  ; $03
!pet "asl",0
!pet "aslq",0  ; $05
!pet "asr",0
!pet "asrq",0  ; $07
!pet "asw",0
!pet "bbr0",0
!pet "bbr1",0
!pet "bbr2",0
!pet "bbr3",0
!pet "bbr4",0
!pet "bbr5",0
!pet "bbr6",0
!pet "bbr7",0
!pet "bbs0",0
!pet "bbs1",0
!pet "bbs2",0
!pet "bbs3",0
!pet "bbs4",0
!pet "bbs5",0
!pet "bbs6",0
!pet "bbs7",0
!pet "bcc",0
!pet "bcs",0
!pet "beq",0
!pet "bit",0
!pet "bitq",0  ; $1D
!pet "bmi",0
!pet "bne",0
!pet "bpl",0
!pet "bra",0
!pet "brk",0
!pet "bsr",0
!pet "bvc",0
!pet "bvs",0
!pet "clc",0
!pet "cld",0
!pet "cle",0
!pet "cli",0
!pet "clv",0
!pet "cmp",0
!pet "cmpq",0  ; $2D
!pet "cpq",0   ; $2E
!pet "cpx",0
!pet "cpy",0
!pet "cpz",0
!pet "dec",0
!pet "deq",0   ; $33
!pet "dew",0
!pet "dex",0
!pet "dey",0
!pet "dez",0
!pet "eom",0
!pet "eor",0
!pet "eorq",0  ; $3A
!pet "inc",0
!pet "inq",0   ; $3C
!pet "inw",0
!pet "inx",0
!pet "iny",0
!pet "inz",0
!pet "jmp",0
!pet "jsr",0
!pet "lda",0
!pet "ldq",0  ; $44
!pet "ldx",0
!pet "ldy",0
!pet "ldz",0
!pet "lsr",0
!pet "lsrq",0  ; $49
!pet "map",0
!pet "neg",0
!pet "ora",0
!pet "orq",0   ; $4D
!pet "pha",0
!pet "php",0
!pet "phw",0
!pet "phx",0
!pet "phy",0
!pet "phz",0
!pet "pla",0
!pet "plp",0
!pet "plx",0
!pet "ply",0
!pet "plz",0
!pet "rmb0",0
!pet "rmb1",0
!pet "rmb2",0
!pet "rmb3",0
!pet "rmb4",0
!pet "rmb5",0
!pet "rmb6",0
!pet "rmb7",0
!pet "rol",0
!pet "rolq",0  ; $62
!pet "ror",0
!pet "rorq",0  ; $64
!pet "row",0
!pet "rti",0
!pet "rts",0
!pet "sbc",0
!pet "sbcq",0  ; $69
!pet "sec",0
!pet "sed",0
!pet "see",0
!pet "sei",0
!pet "smb0",0
!pet "smb1",0
!pet "smb2",0
!pet "smb3",0
!pet "smb4",0
!pet "smb5",0
!pet "smb6",0
!pet "smb7",0
!pet "sta",0
!pet "stq",0  ; $77
!pet "stx",0
!pet "sty",0
!pet "stz",0
!pet "tab",0
!pet "tax",0
!pet "tay",0
!pet "taz",0
!pet "tba",0
!pet "trb",0
!pet "tsb",0
!pet "tsx",0
!pet "tsy",0
!pet "txa",0
!pet "txs",0
!pet "tya",0
!pet "tys",0
!pet "tza",0
!byte 0

; Token IDs for the Q mnemonics, which all use a $42 $42 encoding prefix
q_mnemonics:
!byte $01, $03, $05, $07, $1D, $2D, $2E, $33, $3A, $3C, $44, $49, $4D, $62,
!byte $64, $69, $77
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
!byte 0
last_tk = tk_rbracket + 1

; Other token IDs
tk_number_literal = last_tk + 0
tk_number_literal_leading_zero = last_tk + 1
tk_string_literal = last_tk + 2
tk_label_or_reg = last_tk + 3

; Keywords
; (tokenized as labels)
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
; The addressing_modes table consists of one entry per instruction
; mnemonic, four bytes per entry, in token ID order.
;
; The first two bytes are an addressing mode bitmask, one bit set for each
; addressing mode supported by the instruction.
;
; The last two bytes are the code address for the encoding list. (See below,
; starting with enc_adc.)
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
MODES_NO_OPERAND    = %1000000000000000
MODES_BYTE_OPERAND  = %0101110000011111
MODES_WORD_OPERAND  = %0010001111100000
MODE_32BIT_INDIRECT = %0000000000000010
addressing_modes:
!byte %01011011,%10011110  ; adc
!word enc_adc
!byte %00010010,%00000110  ; adcq
!word enc_adcq
!byte %01011011,%10011110  ; and
!word enc_and
!byte %00010010,%00000110  ; andq
!word enc_andq
!byte %10011011,%00000000  ; asl
!word enc_asl
!byte %10011011,%00000000  ; aslq
!word enc_aslq
!byte %10011000,%00000000  ; asr
!word enc_asr
!byte %10011000,%00000000  ; asrq
!word enc_asrq
!byte %00000010,%00000000  ; asw
!word enc_asw
!byte %00010000,%00000000  ; bbr0
!word enc_bbr0
!byte %00010000,%00000000  ; bbr1
!word enc_bbr1
!byte %00010000,%00000000  ; bbr2
!word enc_bbr2
!byte %00010000,%00000000  ; bbr3
!word enc_bbr3
!byte %00010000,%00000000  ; bbr4
!word enc_bbr4
!byte %00010000,%00000000  ; bbr5
!word enc_bbr5
!byte %00010000,%00000000  ; bbr6
!word enc_bbr6
!byte %00010000,%00000000  ; bbr7
!word enc_bbr7
!byte %00010000,%00000000  ; bbs0
!word enc_bbs0
!byte %00010000,%00000000  ; bbs1
!word enc_bbs1
!byte %00010000,%00000000  ; bbs2
!word enc_bbs2
!byte %00010000,%00000000  ; bbs3
!word enc_bbs3
!byte %00010000,%00000000  ; bbs4
!word enc_bbs4
!byte %00010000,%00000000  ; bbs5
!word enc_bbs5
!byte %00010000,%00000000  ; bbs6
!word enc_bbs6
!byte %00010000,%00000000  ; bbs7
!word enc_bbs7
!byte %00010010,%00000000  ; bcc
!word enc_bcc
!byte %00010010,%00000000  ; bcs
!word enc_bcs
!byte %00010010,%00000000  ; beq
!word enc_beq
!byte %01011011,%00000000  ; bit
!word enc_bit
!byte %00010010,%00000000  ; bitq
!word enc_bitq
!byte %00010010,%00000000  ; bmi
!word enc_bmi
!byte %00010010,%00000000  ; bne
!word enc_bne
!byte %00010010,%00000000  ; bpl
!word enc_bpl
!byte %00010010,%00000000  ; bra
!word enc_bra
!byte %10000000,%00000000  ; brk
!word enc_brk
!byte %00000010,%00000000  ; bsr
!word enc_bsr
!byte %00010010,%00000000  ; bvc
!word enc_bvc
!byte %00010010,%00000000  ; bvs
!word enc_bvs
!byte %10000000,%00000000  ; clc
!word enc_clc
!byte %10000000,%00000000  ; cld
!word enc_cld
!byte %10000000,%00000000  ; cle
!word enc_cle
!byte %10000000,%00000000  ; cli
!word enc_cli
!byte %10000000,%00000000  ; clv
!word enc_clv
!byte %01011011,%10011110  ; cmp
!word enc_cmp
!byte %00010010,%00000110  ; cmpq
!word enc_cmpq
!byte %00010010,%00000110  ; cpq
!word enc_cmpq
!byte %01010010,%00000000  ; cpx
!word enc_cpx
!byte %01010010,%00000000  ; cpy
!word enc_cpy
!byte %01010010,%00000000  ; cpz
!word enc_cpz
!byte %10011011,%00000000  ; dec
!word enc_dec
!byte %10011011,%00000000  ; deq
!word enc_deq
!byte %00010000,%00000000  ; dew
!word enc_dew
!byte %10000000,%00000000  ; dex
!word enc_dex
!byte %10000000,%00000000  ; dey
!word enc_dey
!byte %10000000,%00000000  ; dez
!word enc_dez
!byte %10000000,%00000000  ; eom
!word enc_eom
!byte %01011011,%10011110  ; eor
!word enc_eor
!byte %00010010,%00000110  ; eorq
!word enc_eorq
!byte %10011011,%00000000  ; inc
!word enc_inc
!byte %10011011,%00000000  ; inq
!word enc_inq
!byte %00010000,%00000000  ; inw
!word enc_inw
!byte %10000000,%00000000  ; inx
!word enc_inx
!byte %10000000,%00000000  ; iny
!word enc_iny
!byte %10000000,%00000000  ; inz
!word enc_inz
!byte %00000010,%01100000  ; jmp
!word enc_jmp
!byte %00000010,%01100000  ; jsr
!word enc_jsr
!byte %01011011,%10011111  ; lda
!word enc_lda
!byte %00010010,%00000110  ; ldq
!word enc_ldq
!byte %01010110,%10000000  ; ldx
!word enc_ldx
!byte %01011011,%00000000  ; ldy
!word enc_ldy
!byte %01000011,%00000000  ; ldz
!word enc_ldz
!byte %10011011,%00000000  ; lsr
!word enc_lsr
!byte %10011011,%00000000  ; lsrq
!word enc_lsrq
!byte %10000000,%00000000  ; map
!word enc_map
!byte %10000000,%00000000  ; neg
!word enc_neg
!byte %01011011,%10011110  ; ora
!word enc_ora
!byte %00010010,%00000110  ; orq
!word enc_orq
!byte %10000000,%00000000  ; pha
!word enc_pha
!byte %10000000,%00000000  ; php
!word enc_php
!byte %00100010,%00000000  ; phw
!word enc_phw
!byte %10000000,%00000000  ; phx
!word enc_phx
!byte %10000000,%00000000  ; phy
!word enc_phy
!byte %10000000,%00000000  ; phz
!word enc_phz
!byte %10000000,%00000000  ; pla
!word enc_pla
!byte %10000000,%00000000  ; plp
!word enc_plp
!byte %10000000,%00000000  ; plx
!word enc_plx
!byte %10000000,%00000000  ; ply
!word enc_ply
!byte %10000000,%00000000  ; plz
!word enc_plz
!byte %00010000,%00000000  ; rmb0
!word enc_rmb0
!byte %00010000,%00000000  ; rmb1
!word enc_rmb1
!byte %00010000,%00000000  ; rmb2
!word enc_rmb2
!byte %00010000,%00000000  ; rmb3
!word enc_rmb3
!byte %00010000,%00000000  ; rmb4
!word enc_rmb4
!byte %00010000,%00000000  ; rmb5
!word enc_rmb5
!byte %00010000,%00000000  ; rmb6
!word enc_rmb6
!byte %00010000,%00000000  ; rmb7
!word enc_rmb7
!byte %10011011,%00000000  ; rol
!word enc_rol
!byte %10011011,%00000000  ; rolq
!word enc_rolq
!byte %10011011,%00000000  ; ror
!word enc_ror
!byte %10011011,%00000000  ; rorq
!word enc_rorq
!byte %00000010,%00000000  ; row
!word enc_row
!byte %10000000,%00000000  ; rti
!word enc_rti
!byte %11000000,%00000000  ; rts
!word enc_rts
!byte %01011011,%10011110  ; sbc
!word enc_sbc
!byte %00010010,%00000110  ; sbcq
!word enc_sbcq
!byte %10000000,%00000000  ; sec
!word enc_sec
!byte %10000000,%00000000  ; sed
!word enc_sed
!byte %10000000,%00000000  ; see
!word enc_see
!byte %10000000,%00000000  ; sei
!word enc_sei
!byte %00010000,%00000000  ; smb0
!word enc_smb0
!byte %00010000,%00000000  ; smb1
!word enc_smb1
!byte %00010000,%00000000  ; smb2
!word enc_smb2
!byte %00010000,%00000000  ; smb3
!word enc_smb3
!byte %00010000,%00000000  ; smb4
!word enc_smb4
!byte %00010000,%00000000  ; smb5
!word enc_smb5
!byte %00010000,%00000000  ; smb6
!word enc_smb6
!byte %00010000,%00000000  ; smb7
!word enc_smb7
!byte %00011011,%10011111  ; sta
!word enc_sta
!byte %00010010,%00000110  ; stq
!word enc_stq
!byte %00010110,%10000000  ; stx
!word enc_stx
!byte %00011011,%00000000  ; sty
!word enc_sty
!byte %00011011,%00000000  ; stz
!word enc_stz
!byte %10000000,%00000000  ; tab
!word enc_tab
!byte %10000000,%00000000  ; tax
!word enc_tax
!byte %10000000,%00000000  ; tay
!word enc_tay
!byte %10000000,%00000000  ; taz
!word enc_taz
!byte %10000000,%00000000  ; tba
!word enc_tba
!byte %00010010,%00000000  ; trb
!word enc_trb
!byte %00010010,%00000000  ; tsb
!word enc_tsb
!byte %10000000,%00000000  ; tsx
!word enc_tsx
!byte %10000000,%00000000  ; tsy
!word enc_tsy
!byte %10000000,%00000000  ; txa
!word enc_txa
!byte %10000000,%00000000  ; txs
!word enc_txs
!byte %10000000,%00000000  ; tya
!word enc_tya
!byte %10000000,%00000000  ; tys
!word enc_tys
!byte %10000000,%00000000  ; tza
!word enc_tza

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
enc_bcc : !byte $90, $93
enc_bcs : !byte $b0, $b3
enc_beq : !byte $f0, $f3
enc_bit : !byte $89, $24, $34, $2c, $3c
enc_bitq: !byte $24, $2c
enc_bmi : !byte $30, $33
enc_bne : !byte $d0, $d3
enc_bpl : !byte $10, $13
enc_bra : !byte $80, $83
enc_brk : !byte $00
enc_bsr : !byte $63
enc_bvc : !byte $50, $53
enc_bvs : !byte $70, $73
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
; Each test routine should brk on failure

!macro print_chr .chr {
    lda #.chr
    +kcall bsout
}

!macro print_strlit .strlit {
    +kprimm_start
    !pet .strlit,0
    +kprimm_end
}

!macro print_strlit_line .strlit {
    +kprimm_start
    !pet .strlit,13,0
    +kprimm_end
}

!macro test_start .tnum {
    +print_strlit "  test "
    lda #.tnum
    jsr print_hex8
}
!macro test_end {
    +print_strlit_line "... ok"
}

; Input: code_ptr = first char of null-terminated string
; Output: strbuf = same string
; (This is similar to code_to_strbuf except it copies from code
; memory, i.e. test data.)
copy_ptr_to_strbuf:
    ldx #0
    ldy #0
-   lda (code_ptr),y
    sta strbuf,x
    beq +
    iny
    inx
    bra -
+   rts

; Input: code_ptr = first char of null-terminated string
; Output: Z=0 if matches strbuf exactly, otherwise Z=1
match_ptr_strbuf:
    ldx #0
    ldy #0
-   lda (code_ptr),y
    cmp strbuf,x
    bne +
    cmp #0
    beq +
    iny
    inx
    bra -
+   rts

!macro test_copy_to_strbuf .straddr {
    lda #<.straddr
    sta code_ptr
    lda #>.straddr
    sta code_ptr+1
    jsr copy_ptr_to_strbuf
}

!macro test_match_strbuf .straddr {
    lda #<.straddr
    sta code_ptr
    lda #>.straddr
    sta code_ptr+1
    jsr match_ptr_strbuf
}

!macro test_is_letter .tnum, .in_a, .ec {
    +test_start .tnum

    lda #.in_a
    jsr is_letter
!if .ec {
    bcs +
    brk
+
} else {
    bcc +
    brk
+
}

    +test_end
}

!macro test_is_secondary_ident_char .tnum, .in_a, .ec {
    +test_start .tnum

    lda #.in_a
    jsr is_secondary_ident_char
!if .ec {
    bcs +
    brk
+
} else {
    bcc +
    brk
+
}

    +test_end
}

!macro test_strbuf_to_lowercase .tnum, .instraddr, .outstraddr {
    +test_start .tnum
    +test_copy_to_strbuf .instraddr
    jsr strbuf_to_lowercase
    +test_match_strbuf .outstraddr
    beq +
    brk
+
    +test_end
}

test_strbuf_to_lowercase_2_in:   !pet "ABC",0
test_strbuf_to_lowercase_3_in:   !pet "aBc",0
test_strbuf_to_lowercase_1_out:  !pet "abc",0

!macro test_strbuf_cmp_code_ptr .tnum, .astraddr, .bstraddr, .maxlen, .ea, .ex {
    +test_start .tnum
    +test_copy_to_strbuf .astraddr
    lda #<.bstraddr
    sta code_ptr
    lda #>.bstraddr
    sta code_ptr+1
    ldz #.maxlen
    ldx #0
    jsr strbuf_cmp_code_ptr
    cmp #.ea
    beq +
    brk
+   cpx #.ex
    beq +
    brk
+
    +test_end
}

test_strbuf_cmp_code_ptr_abc:     !pet "abc",0
test_strbuf_cmp_code_ptr_acb:     !pet "acb",0
test_strbuf_cmp_code_ptr_abcd:     !pet "abcd",0

!macro test_accept_whitespace_and_comment .tnum, .basptr, .pos, .epos, .ezero {
    +test_start .tnum
    lda #<.basptr
    sta bas_ptr
    lda #>.basptr
    sta bas_ptr+1
    lda #$05
    sta bas_ptr+2   ; test data in bank 5
    ldz #.pos
    stz line_pos
    jsr accept_whitespace_and_comment
!if .ezero {
    beq +
    brk
+
}
    lda line_pos
    taz
    cpz #.epos
    beq +
    brk
+
    +test_end
}

test_accept_whitespace_and_comment_empty_line:
    !word +
    !word 12345
    !pet 0
+   !word 0

test_accept_whitespace_and_comment_comment:
    !word +
    !word 12345
    !pet "   ; comment ; with ; semicolons", 0
+   !word 0
test_accept_whitespace_and_comment_comment_length = len("   ; comment ; with ; semicolons")

test_accept_whitespace_and_comment_space_then_stuff:
    !word +
    !word 12345
    !pet "   stuff", 0
+   !word 0

test_accept_whitespace_and_comment_other_spaces:
    !word +
    !word 12345
    !pet "  ", chr_shiftspc, chr_tab, "  stuff", 0
+   !word 0


!macro test_accept_ident .tnum, .basptr, .c, .ec, .ez {
    +test_start .tnum
    lda #<.basptr
    sta bas_ptr
    lda #>.basptr
    sta bas_ptr+1
    lda #$05
    sta bas_ptr+2   ; test data in bank 5
    ldz #0
!if .c {
    sec
} else {
    clc
}
    jsr accept_ident
!if .ec {
    bcs +
    brk
+
} else {
    bcc +
    brk
+
}
    cpz #.ez
    beq +
    brk
+
    +test_end
}

test_accept_ident_1: !pet "label  ",0
test_accept_ident_2: !pet "label", chr_backarrow, "12", chr_megaat, "3  ",0
test_accept_ident_3: !pet "0label",0
test_accept_ident_4: !pet "!label",0

!macro test_accept_literal .tnum, .lineaddr, .ec, .eval, .epos, .ezeroflag {
    +test_start .tnum
    lda #<.lineaddr
    sta bas_ptr
    lda #>.lineaddr
    sta bas_ptr+1
    lda #$05
    sta bas_ptr+2   ; test data in bank 5
    ldz #0
    stz line_pos
    jsr accept_literal

!if .ec {
    bcs +
    brk
+   lda #<.eval
    ldx #>.eval
    ldy #^.eval
    ldz #<(.eval >>> 24)
    cpq expr_result
    beq +
    ldq expr_result
    brk
+   lda line_pos
    cmp #.epos
    beq +
    brk
+
} else {
    bcc +
    brk
+
}

    lda expr_flags
    and #F_EXPR_BRACKET_ZERO
!if .ezeroflag {
    bne +
    brk
+
} else {
    beq +
    brk
+
}

    +test_end
}

test_accept_literal_1: !pet "'x'",0
test_accept_literal_2: !pet "'''",0
test_accept_literal_3: !pet "$0",0
test_accept_literal_4: !pet "$1",0
test_accept_literal_5: !pet "$f",0
test_accept_literal_6: !pet "$F",0
test_accept_literal_7: !pet "$deadbeef",0
test_accept_literal_8: !pet "$DEadbeEF",0
test_accept_literal_9: !pet "$000a",0
test_accept_literal_10: !pet "$1000",0
test_accept_literal_11: !pet "%0",0
test_accept_literal_12: !pet "%1",0
test_accept_literal_13: !pet "%0101",0
test_accept_literal_14: !pet "%1010",0
test_accept_literal_15: !pet "%.#.#",0
test_accept_literal_16: !pet "%#.#.",0
test_accept_literal_17: !pet "%....##..####....##..####....####",0
test_accept_literal_18: !pet "0",0
test_accept_literal_19: !pet "9",0
test_accept_literal_20: !pet "1234567890",0
test_accept_literal_21: !pet "0123",0
test_accept_literal_22: !pet "$z",0
test_accept_literal_23: !pet "%z",0
test_accept_literal_24: !pet "a",0
test_accept_literal_25: !pet "$Fg",0
test_accept_literal_26: !pet "%12",0
test_accept_literal_27: !pet "56a",0
test_accept_literal_28: !pet 0

!macro test_find_in_token_list .tnum, .str, .pos, .word_boundary, .ec, .eentry, .epos {
    +test_start .tnum

    lda #<mnemonics
    sta code_ptr
    lda #>mnemonics
    sta code_ptr+1

    ; Copy .str to strbuf
    ldx #0
-   lda .str,x
    sta strbuf,x
    beq +
    inx
    bra -
+
    ldx #.pos
!if .word_boundary {
    sec
} else {
    clc
}
    jsr find_in_token_list
!if .ec {
    bcs +
    brk
+   cpy #.eentry
    beq +
    brk
+   cpx #.epos
    beq +
    brk
+
} else {
    bcc +
    brk
+
}

    +test_end
}

test_find_in_token_list_1: !pet "adc  ",0
test_find_in_token_list_2: !pet "adcq  ",0
test_find_in_token_list_3: !pet "tza  ",0
test_find_in_token_list_4: !pet "zzz  ",0
test_find_in_token_list_5: !pet "adcz  ",0
test_find_in_token_list_6: !pet "adc#  ",0
test_find_in_token_list_7: !pet "#adc#  ",0

!macro test_tokenize_mnemonic .tnum, .str, .pos, .ec, .etoken, .epos {
    +test_start .tnum

    ; Copy .str to strbuf
    ldx #0
-   lda .str,x
    sta strbuf,x
    beq +
    inx
    bra -
+
    ldx #.pos
    stx line_pos
    jsr tokenize_mnemonic
!if .ec {
    bcs +
    brk
+   cpx #.etoken
    beq +
    brk
+   lda line_pos
    cmp #.epos
    beq +
    brk
+
} else {
    bcc +
    brk
+   lda line_pos
    cmp #.pos
    beq +
    brk
+
}

    +test_end
}

!macro test_tokenize_pseudoop .tnum, .str, .pos, .ec, .etoken, .epos {
    +test_start .tnum

    ; Copy .str to strbuf
    ldx #0
-   lda .str,x
    sta strbuf,x
    beq +
    inx
    bra -
+
    ldx #.pos
    stx line_pos
    jsr tokenize_pseudoop
!if .ec {
    bcs +
    brk
+   cpx #.etoken
    beq +
    brk
+   lda line_pos
    cmp #.epos
    beq +
    brk
+
} else {
    bcc +
    brk
+   lda line_pos
    cmp #.pos
    beq +
    brk
+
}

    +test_end
}

test_tokenize_pseudoop_1: !pet "!to  ",0
test_tokenize_pseudoop_2: !pet "!byte  ",0
test_tokenize_pseudoop_3: !pet "!warn  ",0
test_tokenize_pseudoop_4: !pet "!zzz  ",0
test_tokenize_pseudoop_5: !pet "!toz  ",0
test_tokenize_pseudoop_6: !pet "#!to#  ",0
test_tokenize_pseudoop_7: !pet "to  ",0

!macro test_tokenize_other .tnum, .str, .pos, .ec, .etoken, .epos {
    +test_start .tnum

    ; Copy .str to strbuf
    ldx #0
-   lda .str,x
    sta strbuf,x
    beq +
    inx
    bra -
+
    ldx #.pos
    stx line_pos
    jsr tokenize_other
!if .ec {
    bcs +
    brk
+   cpx #.etoken
    beq +
    brk
+   lda line_pos
    cmp #.epos
    beq +
    brk
+
} else {
    bcc +
    brk
+   lda line_pos
    cmp #.pos
    beq +
    brk
+
}

    +test_end
}

test_tokenize_other_1: !pet "!ident", 0
test_tokenize_other_2: !pet "^ident", 0
test_tokenize_other_3: !pet ">>>ident", 0
test_tokenize_other_4: !pet ">> >ident", 0
test_tokenize_other_5: !pet "],z", 0
test_tokenize_other_6: !pet "ident", 0

!macro test_load_line_to_strbuf .tnum, .str, .estr {
    +test_start .tnum

    ; Fake assembly location in bank 5
    lda #5
    sta bas_ptr+2
    lda #0
    sta bas_ptr+3
    lda #<(.str - 4)
    sta line_addr
    lda #>(.str - 4)
    sta line_addr+1
    jsr load_line_to_strbuf

    ldx #4-1
-   inx
    lda .estr-4,x
    beq +
    cmp strbuf,x
    beq -
    brk
+

    +test_end
}

test_load_line_to_strbuf_1:  !pet "AbC",0
test_load_line_to_strbuf_1e: !pet "abc",0

!macro test_tokenize .tnum, .str, .etokbuf, .etokbuf_end, .eerror, .eerror_pos {
    +test_start .tnum

    ; Fake assembly location in bank 5
    lda #5
    sta bas_ptr+2
    lda #0
    sta bas_ptr+3
    lda #<(.str - 4)
    sta line_addr
    lda #>(.str - 4)
    sta line_addr+1
    jsr tokenize

!if .eerror {
    lda err_code
    cmp #.eerror
    beq +
    +print_strlit_line "...fail, wrong error code"
    brk
+   lda line_pos
    cmp #.eerror_pos
    beq +
    +print_strlit_line "...fail, wrong error pos"
    brk
+
} else {
    ldx #.etokbuf_end-.etokbuf
-   dex
    lda .etokbuf,x
    cmp tokbuf,x
    bne +
    cpx #0
    beq ++
    bra -
+
    +print_strlit_line "...fail, wrong tokbuf"
    brk
++
}

    +test_end
}

test_tokenize_1: !pet 0
test_tokenize_1e: !byte 0
test_tokenize_2: !pet "    ; comment only",0
test_tokenize_2e: !byte 0
test_tokenize_3: !pet "\"string literal\"",0
test_tokenize_3e: !byte tk_string_literal, 4, 14, 0
test_tokenize_4: !pet "12345",0
test_tokenize_4e:
    !byte tk_number_literal, 4
    !32 12345
    !byte 0
test_tokenize_5: !pet "$DeAdBeEf",0
test_tokenize_5e:
    !byte tk_number_literal, 4
    !32 $deadbeef
    !byte 0
test_tokenize_6: !pet "tZa",0
test_tokenize_6e: !byte 137, 4, 0
test_tokenize_7: !pet "!wOrD",0
test_tokenize_7e: !byte po_word, 4, 0
test_tokenize_8: !pet "xOr",0
test_tokenize_8e: !byte kw_xor, 4, 0
test_tokenize_9: !pet ">>>",0
test_tokenize_9e: !byte tk_lsr, 4, 0
test_tokenize_10: !pet "label",0
test_tokenize_10e: !byte tk_label_or_reg, 4, 5, 0
test_tokenize_11: !pet "@label",0
test_tokenize_11e: !byte tk_label_or_reg, 4, 6, 0
test_tokenize_12: !pet "label: lda (45, sp), y  ; comment",0
test_tokenize_12e:
    !byte tk_label_or_reg, 4, 5
    !byte tk_colon, 9
    !byte 66, 11
    !byte tk_lparen, 15
    !byte tk_number_literal, 16
    !32 45
    !byte tk_comma, 18
    !byte tk_label_or_reg, 20, 2
    !byte tk_rparen, 22
    !byte tk_comma, 23
    !byte tk_label_or_reg, 25, 1
    !byte 0
test_tokenize_13: !pet "label = *+4",0
test_tokenize_13e:
    !byte tk_label_or_reg, 4, 5
    !byte tk_equal, 10
    !byte tk_multiply, 12
    !byte tk_plus, 13
    !byte tk_number_literal, 14
    !32 4
    !byte 0
test_tokenize_14: !pet "* = $d000",0
test_tokenize_14e:
    !byte tk_multiply, 4
    !byte tk_equal, 6
    !byte tk_number_literal, 8
    !32 $d000
    !byte 0
test_tokenize_15: !pet "!to \"lda\", runnable",0
test_tokenize_15e:
    !byte po_to, 4
    !byte tk_string_literal, 8, 3
    !byte tk_comma, 13
    !byte tk_label_or_reg, 15, 8
    !byte 0
test_tokenize_16: !pet "lda $000a",0
test_tokenize_16e:
    !byte 66, 4
    !byte tk_number_literal_leading_zero, 8
    !32 $000a
    !byte 0
test_tokenize_last:

test_tokenize_error_1: !pet "$$$",0


test_symbol_table:
    ; label
    !byte <attic_symbol_names, >attic_symbol_names, ^attic_symbol_names
    !byte F_SYMTBL_DEFINED
    !32 12345
    ; Alpha
    !byte <(attic_symbol_names+6), >(attic_symbol_names+6), ^(attic_symbol_names+6)
    !byte F_SYMTBL_DEFINED
    !32 23456
    ; Alph
    !byte <(attic_symbol_names+12), >(attic_symbol_names+12), ^(attic_symbol_names+12)
    !byte F_SYMTBL_DEFINED
    !32 34567
    ; Beta
    !byte <(attic_symbol_names+17), >(attic_symbol_names+17), ^(attic_symbol_names+17)
    !byte F_SYMTBL_DEFINED
    !32 45678
    ; BetaZ
    !byte <(attic_symbol_names+22), >(attic_symbol_names+22), ^(attic_symbol_names+22)
    !byte F_SYMTBL_DEFINED
    !32 56789
    ; GAMMA
    !byte <(attic_symbol_names+28), >(attic_symbol_names+28), ^(attic_symbol_names+28)
    !byte F_SYMTBL_DEFINED
    !32 99999
    ; (END)
    !byte 0,0,0,0,0,0,0,0
test_symbol_table_end:
test_symbol_table_last_addr = test_symbol_table_end-test_symbol_table+attic_symbol_table-SYMTBL_ENTRY_SIZE
test_symbol_names:
    !pet "label",0
    !pet "Alpha",0
    !pet "Alph",0
    !pet "Beta",0
    !pet "BetaZ",0
    !pet "GAMMA",0
test_symbol_names_end:

test_set_up_symbol_data:
    jsr init_symbol_table

    lda #<test_symbol_table
    sta code_ptr
    lda #>test_symbol_table
    sta code_ptr+1
    lda #<attic_symbol_table
    ldx #>attic_symbol_table
    ldy #^attic_symbol_table
    ldz #$08
    stq attic_ptr
    ldy #0
    ldz #0
-   lda (code_ptr),y
    sta [attic_ptr],z
    inw code_ptr
    inw attic_ptr
    lda code_ptr+1
    cmp #>test_symbol_names
    bcc -
    lda code_ptr
    cmp #<test_symbol_names
    bcc -

    lda #<test_symbol_names
    sta code_ptr
    lda #>test_symbol_names
    sta code_ptr+1
    lda #<attic_symbol_names
    ldx #>attic_symbol_names
    ldy #^attic_symbol_names
    ldz #$08
    stq attic_ptr
    ldy #0
    ldz #0
-   lda (code_ptr),y
    sta [attic_ptr],z
    inw code_ptr
    inw attic_ptr
    lda code_ptr+1
    cmp #>test_symbol_names_end
    bcc -
    lda code_ptr
    cmp #<test_symbol_names_end
    bcc -
    lda attic_ptr
    sta symtbl_next_name
    lda attic_ptr+1
    sta symtbl_next_name+1
    lda attic_ptr+2
    sta symtbl_next_name+2
    rts

!macro test_find_symbol .tnum, .str, .length, .ec, .eatticptr {
    +test_start .tnum

    jsr test_set_up_symbol_data

    ; Fake assembly location in bank 5
    lda #<.str
    sta bas_ptr
    lda #>.str
    sta bas_ptr+1
    lda #5
    sta bas_ptr+2
    lda #0
    sta bas_ptr+3

    ldx #.length
    jsr find_symbol

    ; (C set = fail)
!if .ec {
    bcs +
    +print_strlit_line "...fail, expected carry set"
    brk
+
    lda #<test_symbol_table_last_addr
    ldx #>test_symbol_table_last_addr
    ldy #^test_symbol_table_last_addr
    ldz #$08
    cpq attic_ptr
    beq +
    +print_strlit_line "...fail, wrong attic ptr (not found case)"
    brk
+
} else {
    bcc +
    +print_strlit_line "...fail, expected carry clear"
    brk
+
    lda #<.eatticptr
    ldx #>.eatticptr
    ldy #^.eatticptr
    ldz #$08
    cpq attic_ptr
    beq +
    +print_strlit_line "...fail, wrong attic ptr (found case)"
    brk
+
}

    +test_end
}

test_find_symbol_1: !pet "label = 999",0
test_find_symbol_2: !pet "Alpha",0
test_find_symbol_3: !pet "Alph",0
test_find_symbol_4: !pet "Beta",0
test_find_symbol_5: !pet "BetaZ",0
test_find_symbol_6: !pet "GAMMA",0
test_find_symbol_7: !pet "GAMMB",0

!macro test_find_or_add_symbol .tnum, .str, .length, .ec, .eatticptr {
    +test_start .tnum

    jsr test_set_up_symbol_data

    ; Fake assembly location in bank 5
    lda #<.str
    sta bas_ptr
    lda #>.str
    sta bas_ptr+1
    lda #5
    sta bas_ptr+2
    lda #0
    sta bas_ptr+3

    ldx #.length
    jsr find_or_add_symbol

    ; (C set = fail)
!if .ec {
    bcs +
    +print_strlit_line "...fail, expected carry set"
    brk
+
    ; TODO: confirm out of memory conditions
} else {
    bcc +
    +print_strlit_line "...fail, expected carry clear"
    brk
+
    lda #<.eatticptr
    ldx #>.eatticptr
    ldy #^.eatticptr
    ldz #$08
    cpq attic_ptr
    beq +
    +print_strlit_line "...fail, wrong attic ptr (found case)"
    brk
+
    ; TODO: test symtbl_next_name has advanced
}

    +test_end
}

!macro test_get_symbol_value .tnum, .str, .length, .ec, .eq {
    +test_start .tnum

    jsr test_set_up_symbol_data

    ; Fake assembly location in bank 5
    lda #<.str
    sta bas_ptr
    lda #>.str
    sta bas_ptr+1
    lda #5
    sta bas_ptr+2
    lda #0
    sta bas_ptr+3

    ldx #.length
    jsr find_or_add_symbol
    jsr get_symbol_value

!if .ec {
    bcs +
    +print_strlit_line "... fail, expected carry set"
    brk
+
} else {
    bcc +
    +print_strlit_line "... fail, expected carry clear"
    brk
+   cmp #<.eq
    beq +
    +print_strlit_line "... fail, wrong q (a)"
    brk
+   cpx #>.eq
    beq +
    +print_strlit_line "... fail, wrong q (x)"
    brk
+   cpy #^.eq
    beq +
    +print_strlit_line "... fail, wrong q (y)"
    brk
+   cpz #<(.eq >>> 24)
    beq +
    +print_strlit_line "... fail, wrong q (z)"
    brk
+
}

    +test_end
}

!macro test_set_symbol_value .tnum, .str, .length, .q {
    +test_start .tnum

    jsr test_set_up_symbol_data

    ; Fake assembly location in bank 5
    lda #<.str
    sta bas_ptr
    lda #>.str
    sta bas_ptr+1
    lda #5
    sta bas_ptr+2
    lda #0
    sta bas_ptr+3

    ldx #.length
    jsr find_or_add_symbol
    lda #<.q
    ldx #>.q
    ldy #^.q
    ldz #<(.q >>> 24)
    jsr set_symbol_value

    ldz #3
    lda [attic_ptr],z
    and #F_SYMTBL_DEFINED
    bne +
    brk
+   inz
    lda [attic_ptr],z
    cmp #<.q
    beq +
    brk
+   inz
    lda [attic_ptr],z
    cmp #>.q
    beq +
    brk
+   inz
    lda [attic_ptr],z
    cmp #^.q
    beq +
    brk
+   inz
    lda [attic_ptr],z
    cmp #<(.q >>> 24)
    beq +
    brk
+
    +test_end
}

!macro test_do_assemble_bytes .x {
!if .x > 0 {
    ldx #.x-1
-   txa
    sta strbuf,x
    dex
    bpl -
}
    ldx #.x
    jsr assemble_bytes
}

!macro test_assemble_bytes .tnum, .pass, .pc, .x, .ec, .epc {
    +test_start .tnum
    jsr init_symbol_table
    lda #.pass
    sta pass
    jsr init_pass
!if .pc {
    ldx #<.pc
    ldy #>.pc
    jsr set_pc
}
    +test_do_assemble_bytes .x

!if .ec {
    bcs +
    +print_strlit_line "... fail, expected carry set"
    brk
+
} else {
    bcc +
    +print_strlit_line "... fail, expected carry clear"
    brk
+
}

!if .epc {
    lda program_counter
    cmp #<.epc
    bne +
    lda program_counter+1
    cmp #>.epc
    bne +
    bra ++
+   +print_strlit_line "... fail, wrong program-counter"
    brk
++
    lda next_segment_pc
    cmp #<.epc
    bne +
    lda next_segment_pc+1
    cmp #>.epc
    bne +
    bra ++
+   +print_strlit_line "... fail, wrong next-segment-pc"
    brk
++
}

!if .pass == $ff {
    ; validate segment memory: .pc (2), .x (2), 0, 1, 2, 3, ...
}

    +test_end
}

run_test_suite_cmd:
    +print_strlit_line "-- test suite --"

    +print_chr chr_cr
    +print_strlit_line "is-letter"
    +test_is_letter $01, 'A', 1
    +test_is_letter $02, 'B', 1
    +test_is_letter $03, 'Z', 1
    +test_is_letter $04, 'a', 1
    +test_is_letter $05, 'b', 1
    +test_is_letter $06, 'z', 1
    +test_is_letter $07, 193, 1
    +test_is_letter $08, 194, 1
    +test_is_letter $09, 218, 1
    +test_is_letter $0A, '@', 0
    +test_is_letter $0B, '0', 0
    +test_is_letter $0C, '9', 0
    +test_is_letter $0D, ']', 0
    +test_is_letter $0E, chr_backarrow, 0
    +test_is_letter $0F, chr_megaat, 0
    +test_is_letter $10, $e1, 0

    +print_chr chr_cr
    +print_strlit_line "is-secondary-ident-char"
    +test_is_secondary_ident_char $01, 'A', 1
    +test_is_secondary_ident_char $02, 'B', 1
    +test_is_secondary_ident_char $03, 'Z', 1
    +test_is_secondary_ident_char $04, 'a', 1
    +test_is_secondary_ident_char $05, 'b', 1
    +test_is_secondary_ident_char $06, 'z', 1
    +test_is_secondary_ident_char $07, '0', 1
    +test_is_secondary_ident_char $08, '9', 1
    +test_is_secondary_ident_char $09, 193, 1
    +test_is_secondary_ident_char $0A, 194, 1
    +test_is_secondary_ident_char $0B, 218, 1
    +test_is_secondary_ident_char $0C, chr_backarrow, 1
    +test_is_secondary_ident_char $0D, chr_megaat, 1
    +test_is_secondary_ident_char $0E, '@', 0
    +test_is_secondary_ident_char $0F, ']', 0
    +test_is_secondary_ident_char $10, $e1, 0
    +test_is_secondary_ident_char $11, '.', 1

    +print_chr chr_cr
    +print_strlit_line "strbuf-to-lowercase"
    +test_strbuf_to_lowercase $01, test_strbuf_to_lowercase_1_out, test_strbuf_to_lowercase_1_out
    +test_strbuf_to_lowercase $02, test_strbuf_to_lowercase_2_in, test_strbuf_to_lowercase_1_out
    +test_strbuf_to_lowercase $03, test_strbuf_to_lowercase_3_in, test_strbuf_to_lowercase_1_out

    +print_chr chr_cr
    +print_strlit_line "strbuf-cmp-code-ptr"
    +test_strbuf_cmp_code_ptr $01, test_strbuf_cmp_code_ptr_abc, test_strbuf_cmp_code_ptr_abc, 3, $00, 3
    +test_strbuf_cmp_code_ptr $02, test_strbuf_cmp_code_ptr_abc, test_strbuf_cmp_code_ptr_abc, 6, $00, 3
    +test_strbuf_cmp_code_ptr $03, test_strbuf_cmp_code_ptr_abc, test_strbuf_cmp_code_ptr_acb, 3, $ff, 1
    +test_strbuf_cmp_code_ptr $04, test_strbuf_cmp_code_ptr_acb, test_strbuf_cmp_code_ptr_abc, 3, $01, 1
    +test_strbuf_cmp_code_ptr $05, test_strbuf_cmp_code_ptr_abc, test_strbuf_cmp_code_ptr_abcd, 4, $ff, 3
    +test_strbuf_cmp_code_ptr $06, test_strbuf_cmp_code_ptr_abcd, test_strbuf_cmp_code_ptr_abc, 4, $01, 3

    +print_chr chr_cr
    +print_strlit_line "accept-whitespace-and-comment"
    +test_accept_whitespace_and_comment $01, test_accept_whitespace_and_comment_empty_line, 4, 4, 1
    +test_accept_whitespace_and_comment $02, test_accept_whitespace_and_comment_comment, 4, 4+test_accept_whitespace_and_comment_comment_length, 1
    +test_accept_whitespace_and_comment $03, test_accept_whitespace_and_comment_space_then_stuff, 4, 7, 0
    +test_accept_whitespace_and_comment $04, test_accept_whitespace_and_comment_other_spaces, 4, 10, 0

    +print_chr chr_cr
    +print_strlit_line "accept-ident"
    +test_accept_ident $01, test_accept_ident_1, 0, 1, 5
    +test_accept_ident $02, test_accept_ident_2, 0, 1, 10
    +test_accept_ident $03, test_accept_ident_3, 0, 0, 0
    +test_accept_ident $04, test_accept_ident_4, 0, 0, 0
    +test_accept_ident $05, test_accept_ident_4, 1, 1, 6

    +print_chr chr_cr
    +print_strlit_line "accept-literal"
    +test_accept_literal $01, test_accept_literal_1, 1, 88, 3, 0
    +test_accept_literal $02, test_accept_literal_2, 1, '\'', 3, 0
    +test_accept_literal $03, test_accept_literal_3, 1, $0, 2, 1
    +test_accept_literal $04, test_accept_literal_4, 1, $1, 2, 0
    +test_accept_literal $05, test_accept_literal_5, 1, $f, 2, 0
    +test_accept_literal $06, test_accept_literal_6, 1, $F, 2, 0
    +test_accept_literal $07, test_accept_literal_7, 1, $deadbeef, 9, 0
    +test_accept_literal $08, test_accept_literal_8, 1, $DEadbeEF, 9, 0
    +test_accept_literal $09, test_accept_literal_9, 1, $000a, 5, 1
    +test_accept_literal $0a, test_accept_literal_10, 1, $1000, 5, 0
    +test_accept_literal $0b, test_accept_literal_11, 1, %0000, 2, 0
    +test_accept_literal $0c, test_accept_literal_12, 1, %0001, 2, 0
    +test_accept_literal $0d, test_accept_literal_13, 1, %0101, 5, 0
    +test_accept_literal $0e, test_accept_literal_14, 1, %1010, 5, 0
    +test_accept_literal $0f, test_accept_literal_15, 1, %.#.#, 5, 0
    +test_accept_literal $10, test_accept_literal_16, 1, %#.#., 5, 0
    +test_accept_literal $11, test_accept_literal_17, 1, %....##..####....##..####....####, 33, 0
    +test_accept_literal $12, test_accept_literal_18, 1, 0, 1, 1
    +test_accept_literal $13, test_accept_literal_19, 1, 9, 1, 0
    +test_accept_literal $14, test_accept_literal_20, 1, 1234567890, 10, 0
    +test_accept_literal $15, test_accept_literal_21, 1, 0123, 4, 1
    +test_accept_literal $16, test_accept_literal_22, 0, 0, 0, 0
    +test_accept_literal $17, test_accept_literal_23, 0, 0, 0, 0
    +test_accept_literal $18, test_accept_literal_24, 0, 0, 0, 0
    +test_accept_literal $19, test_accept_literal_25, 1, $F, 2, 0
    +test_accept_literal $1a, test_accept_literal_26, 1, %0001, 2, 0
    +test_accept_literal $1b, test_accept_literal_27, 1, 56, 2, 0
    +test_accept_literal $1c, test_accept_literal_28, 0, 0, 0, 0

    +print_chr chr_cr
    +print_strlit_line "find-in-token-list"
    +test_find_in_token_list $01, test_find_in_token_list_1, 0, 1, 1, 0, 3
    +test_find_in_token_list $02, test_find_in_token_list_2, 0, 1, 1, 1, 4
    +test_find_in_token_list $03, test_find_in_token_list_3, 0, 1, 1, 137, 3
    +test_find_in_token_list $04, test_find_in_token_list_4, 0, 1, 0, 0, 0
    +test_find_in_token_list $05, test_find_in_token_list_5, 0, 0, 1, 0, 3
    +test_find_in_token_list $06, test_find_in_token_list_5, 0, 1, 0, 0, 0
    +test_find_in_token_list $07, test_find_in_token_list_6, 0, 0, 1, 0, 3
    +test_find_in_token_list $08, test_find_in_token_list_6, 0, 1, 1, 0, 3
    +test_find_in_token_list $09, test_find_in_token_list_7, 1, 1, 1, 0, 4

    +print_chr chr_cr
    +print_strlit_line "tokenize-mnemonic"
    +test_tokenize_mnemonic $01, test_find_in_token_list_1, 0, 1, 0, 3
    +test_tokenize_mnemonic $02, test_find_in_token_list_2, 0, 1, 1, 4
    +test_tokenize_mnemonic $03, test_find_in_token_list_3, 0, 1, 137, 3
    +test_tokenize_mnemonic $04, test_find_in_token_list_4, 0, 0, 0, 0
    +test_tokenize_mnemonic $05, test_find_in_token_list_5, 0, 0, 0, 0
    +test_tokenize_mnemonic $06, test_find_in_token_list_6, 0, 1, 0, 3
    +test_tokenize_mnemonic $07, test_find_in_token_list_7, 1, 1, 0, 4

    +print_chr chr_cr
    +print_strlit_line "tokenize-pseudoop"
    +test_tokenize_pseudoop $01, test_tokenize_pseudoop_1, 0, 1, po_to, 3
    +test_tokenize_pseudoop $02, test_tokenize_pseudoop_2, 0, 1, po_byte, 5
    +test_tokenize_pseudoop $03, test_tokenize_pseudoop_3, 0, 1, po_warn, 5
    +test_tokenize_pseudoop $04, test_tokenize_pseudoop_4, 0, 0, 0, 0
    +test_tokenize_pseudoop $05, test_tokenize_pseudoop_5, 0, 0, 0, 0
    +test_tokenize_pseudoop $06, test_tokenize_pseudoop_6, 1, 1, po_to, 4
    +test_tokenize_pseudoop $07, test_tokenize_pseudoop_7, 0, 0, 0, 0

    +print_chr chr_cr
    +print_strlit_line "tokenize-other"
    +test_tokenize_other $01, test_tokenize_other_1, 0, 1, tk_complement, 1
    +test_tokenize_other $02, test_tokenize_other_2, 0, 1, tk_power, 1
    +test_tokenize_other $03, test_tokenize_other_3, 0, 1, tk_lsr, 3
    +test_tokenize_other $04, test_tokenize_other_4, 0, 1, tk_asr, 2
    +test_tokenize_other $05, test_tokenize_other_5, 0, 1, tk_rbracket, 1
    +test_tokenize_other $06, test_tokenize_other_6, 0, 0, 0, 0

    +print_chr chr_cr
    +print_strlit_line "tokenize-load-line-to-strbuf"
    +test_load_line_to_strbuf $01, test_load_line_to_strbuf_1e, test_load_line_to_strbuf_1e
    +test_load_line_to_strbuf $02, test_load_line_to_strbuf_1, test_load_line_to_strbuf_1e

    +print_chr chr_cr
    +print_strlit_line "tokenize"
    +test_tokenize $01, test_tokenize_1, test_tokenize_1e, test_tokenize_2, 0, 0
    +test_tokenize $02, test_tokenize_2, test_tokenize_2e, test_tokenize_3, 0, 0
    +test_tokenize $03, test_tokenize_3, test_tokenize_3e, test_tokenize_4, 0, 0
    +test_tokenize $04, test_tokenize_4, test_tokenize_4e, test_tokenize_5, 0, 0
    +test_tokenize $05, test_tokenize_5, test_tokenize_5e, test_tokenize_6, 0, 0
    +test_tokenize $06, test_tokenize_6, test_tokenize_6e, test_tokenize_7, 0, 0
    +test_tokenize $07, test_tokenize_7, test_tokenize_7e, test_tokenize_8, 0, 0
    +test_tokenize $08, test_tokenize_8, test_tokenize_8e, test_tokenize_9, 0, 0
    +test_tokenize $09, test_tokenize_9, test_tokenize_9e, test_tokenize_10, 0, 0
    +test_tokenize $0A, test_tokenize_10, test_tokenize_10e, test_tokenize_11, 0, 0
    +test_tokenize $0B, test_tokenize_11, test_tokenize_11e, test_tokenize_12, 0, 0
    +test_tokenize $0C, test_tokenize_error_1, 0, 0, 1, 4
    +test_tokenize $0D, test_tokenize_12, test_tokenize_12e, test_tokenize_13, 0, 0
    +test_tokenize $0E, test_tokenize_13, test_tokenize_13e, test_tokenize_14, 0, 0
    +test_tokenize $0F, test_tokenize_14, test_tokenize_14e, test_tokenize_15, 0, 0
    +test_tokenize $10, test_tokenize_15, test_tokenize_15e, test_tokenize_16, 0, 0
    +test_tokenize $11, test_tokenize_16, test_tokenize_16e, test_tokenize_last, 0, 0

    +print_chr chr_cr
    +print_strlit_line "test-find-symbol"
    +test_find_symbol $01, test_find_symbol_1, 5, 0, attic_symbol_table+(8*0)
    +test_find_symbol $02, test_find_symbol_2, 5, 0, attic_symbol_table+(8*1)
    +test_find_symbol $03, test_find_symbol_3, 4, 0, attic_symbol_table+(8*2)
    +test_find_symbol $04, test_find_symbol_4, 4, 0, attic_symbol_table+(8*3)
    +test_find_symbol $05, test_find_symbol_5, 5, 0, attic_symbol_table+(8*4)
    +test_find_symbol $06, test_find_symbol_6, 5, 0, attic_symbol_table+(8*5)
    +test_find_symbol $07, test_find_symbol_7, 5, 1, 0

    +print_chr chr_cr
    +print_strlit_line "test-find-or-add-symbol"
    +test_find_or_add_symbol $01, test_find_symbol_3, 4, 0, attic_symbol_table+(8*2)
    +test_find_or_add_symbol $02, test_find_symbol_7, 5, 0, attic_symbol_table+(8*6)

    +print_chr chr_cr
    +print_strlit_line "test-get-symbol-value"
    +test_get_symbol_value $01, test_find_symbol_2, 5, 0, 23456
    +test_get_symbol_value $02, test_find_symbol_7, 5, 1, 0

    +print_chr chr_cr
    +print_strlit_line "test-set-symbol-value"
    +test_set_symbol_value $01, test_find_symbol_2, 5, 98765
    +test_set_symbol_value $02, test_find_symbol_7, 5, 87654

    +print_chr chr_cr
    +print_strlit_line "test-assemble-bytes"
    +test_assemble_bytes $01, 0, 0, 1, 1, 0  ; undefined PC is error
    +test_assemble_bytes $02, 0, $c000, 0, 0, 0  ; zero length is ok
    +test_assemble_bytes $03, 0, $c000, 5, 0, $c005
    +test_assemble_bytes $04, $ff, $c000, 5, 0, $c005
    ; TODO: assemble_bytes, two writes with no PC change between (single segment)
    ; TODO: assemble_btyes, two writes with PC change between (two segments)

    +print_chr chr_cr
    +print_strlit_line "-- all tests passed --"
    rts

; ---------------------------------------------------------

!if * >= max_end_of_program {
    !error "EasyAsm code is too large, * = ", *
}
