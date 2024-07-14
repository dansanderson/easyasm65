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

* = $f0

; Parser
err_code        *=*+1
err_pos         *=*+1
tok_start       *=*+2
cur_line_addr   *=*+2

; General purpose
code_ptr        *=*+2   ; 16-bit pointer, CPU
attic_ptr       *=*+4   ; 32-bit pointer, Attic
bas_ptr         *=*+4   ; 32-bit pointer, bank 0

!if * > $100 {
    !error "Exceeded BP map; move start to earlier, if possible : ", *
}


; Attic map
attic_easyasm_stash = $08700000
attic_source_stash  = attic_easyasm_stash + $6000
attic_data          = attic_source_stash + $d700


; Other memory
source_start = $2000  ; bank 0
strbuf = $7f00        ; bank 5

; KERNAL routines
bsout = $ffd2
primm = $ff7d

; MEGA65 registers
dmaimm = $d707

; Character constants
chr_cr = 13
chr_spc = 32
chr_uparrow = 94
chr_backarrow = 95
chr_megaat = 164


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


* = $2000    ; Actually $52000

    jmp dispatch
id_string:
    !pet "easyasm v0.1",0


; Initialize
; - Assume entry conditions (B, bank 5, MAP)
; - Preserve CPU registers
init:
    pha

    ; Init pointer banks
    lda #attic_easyasm_stash >> 24
    sta attic_ptr+3
    lda #(attic_easyasm_stash >> 16) & $ff
    sta attic_ptr+2
    lda #$00
    sta bas_ptr+3
    sta bas_ptr+2

    pla
    rts


; All entry into EasyAsm comes through here.
; MAPL = (E)500  MAPH = (8)300  B = $1Exx
; A = dispatch index (1-indexed)
dispatch:
    jsr init
    dec
    asl
    tax   ; X = (A-1)*2
    jmp (dispatch_jump,x)
dispatch_jump:
    !word assemble_to_memory_cmd
    !word assemble_to_disk_cmd
    !word restore_source_cmd


assemble_to_memory_cmd:
    +kprimm_start
    !pet "debug: assemble to memory",13,0
    +kprimm_end

    jsr stash_source

    jsr test_parser
    ;jsr parse_source
    ; TODO: the rest of the owl

    jsr restore_source
    rts


assemble_to_disk_cmd:
    +kprimm_start
    !pet "debug: assemble to disk",13,0
    +kprimm_end

    jsr stash_source
    jsr parse_source
    ; TODO: the rest of the owl

    rts


restore_source_cmd:
    ; Safety check: probably never stashed source before
    ldz #0
    lda #>attic_source_stash
    sta attic_ptr+2
    lda #<attic_source_stash
    sta attic_ptr+3
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

; Input: err_code, err_pos, cur_line_addr
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

    lda cur_line_addr
    sta bas_ptr
    lda cur_line_addr+1
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
    beq +
    sta strbuf,x
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
    ldx err_pos   ; Indent by err_pos
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
    bne +++
++  sec
    rts
+++ jmp is_letter


; Skip over whitespace, and also a line comment if found after whitespace
; Input: cur_line_addr, Y=pos
; Output: Y advanced maybe; C=0 (no fail)
accept_whitespace_and_comment:
    lda (cur_line_addr),y
    cmp #chr_spc
    bne +
    iny
    bra accept_whitespace_and_comment
+   cmp #';'
    bne +
-   iny            ; Ignore comment to end of line
    lda (cur_line_addr),y
    bne -
+   clc
    rts


; Consume identifier
; Input: cur_line_addr, Y=pos
; Output: C: 0=not found, Y unchanged; 1=found, Y advanced
accept_ident:
    phy
    lda (cur_line_addr),y

    ; Must start with letter
    jsr is_letter
    bcc accept_fail

    ; Can be followed by letter, number, back-arrow, Mega+@
-   iny
    lda (cur_line_addr),y
    jsr is_secondary_ident_char
    bcs -

    bra accept_success


; Consume label, mnemonic, or pseudo-op (if C=0)
; Input: cur_line_addr, Y=pos; C: 0=accept pseudo-op, 1=no pseudo-op
; Output: C: 0=not found, Y unchanged; 1=found, Y advanced
accept_label_mnemonic_pseudoop:
    phy
    lda (cur_line_addr),y

    bcc +
    cmp #'!'           ; Pseudoops not allowed here
    beq accept_fail
    bra ++

+   cmp #'!'
    bne ++
    iny
    jsr accept_ident
    bcs accept_success
    bra accept_fail

++  cmp #'@'
    bne +
    iny
    jsr accept_ident
    bcs accept_success
    bra accept_fail

    ; If start is + or -, must be a sequence of + or - up to space, colon, or EOL.
+   cmp #'+'
    bne +
-   iny
    lda (cur_line_addr),y
    cmp #'+'
    beq -
    bra @check_rel_end

+   cmp #'-'
    bne +
-   iny
    lda (cur_line_addr),y
    cmp #'-'
    beq -

@check_rel_end
    lda (cur_line_addr),y
    beq accept_success   ; end of line
    cmp #chr_spc
    beq accept_success   ; space
    cmp #':'
    beq accept_success   ; colon
    bra accept_fail

    ; Otherwise expect an identifier.
+   ply
    jmp accept_ident


; Input: Previous Y on stack
; Output: Stack popped, C=1
accept_success
    pla
    sec
    rts

; Input: Previous Y on stack
; Output: Y=previous Y, C=0
accept_fail
    ply
    clc
    rts


; Convert indicated source string to lowercase in strbuf
; Input: A=start pos, Y=end pos (+1), cur_line_addr
; Output: strbuf, position 0 to null terminated
ay_to_lower:
    pha
    sta tok_start   ; (Borrow tok_start as scratch)
    tya
    sec
    sbc tok_start   ;
    tax             ; X = length
    pla             ; A = start offset

    clc
    adc cur_line_addr
    sta tok_start
    lda #0
    adc cur_line_addr+1
    sta tok_start+1 ; tok_start = first char

    inx
    lda #0
    sta strbuf,x    ; null terminator
    dex
-   lda tok_start,x
    jsr is_letter
    bcc +
    sec
    sbc #'a'-'A'
    cmp #193-('a'-'A')
    bcc +
    sec
    sbc #193-'a'
+   sta strbuf,x
    dex
    bpl -

    rts


; Is a substring on the current line a mnemonic
; Input: A=start pos, Y=end pos (+1), cur_line_addr
; Output: C=1: is mnemonic, A=mnemonic number; strbuf is normalized mnemonic string
ay_to_mnemonic:
    jsr ay_to_lower
    ; TODO: detect and identify mnemonic
    ; - mnemonics is a table of eight-byte records; the first four bytes are the mnemonic
    sec
    rts


; Is a substring on the current line a pseudoop
; Input: A=start pos, Y=end pos (+1), cur_line_addr
; Output: C=1: is pseudoop, A=pseudoop number; strbuf is normalized pseudoop string
ay_to_pseudoop:
    jsr ay_to_lower
    ; TODO: detect and identify pseudoop
    sec
    rts


; Input: cur_line_addr
; Output: err_code, err_pos
parse_line:
    lda #0
    sta err_code

    ; Start line index Y at first character
    ; (0=next addr word, 2=line number word)
    ldy #4

    ; Handle empty or comment-only lines.
    jsr accept_whitespace_and_comment
    lda (cur_line_addr),y
    bne +
    rts
+
    ; Does line start with label, mnemonic, or pseudoop?
    phy
    jsr accept_label_mnemonic_pseudoop
    bcs +
    ; Something starts this line, but it's not a label, mnemonic, or pseudoop.
    ply
    lda #err_syntax
    sta err_code
    sty err_pos
    rts

+   pla   ; A = prev Y
    ; A to Y is label, mnemonic, or pseudoop. Disambiguate...
    jsr ay_to_mnemonic
    bcs @is_mnemonic
    jsr ay_to_pseudoop
    bcs @is_pseudoop

    ; This is a label.
    jsr accept_whitespace_and_comment
    lda (cur_line_addr),y
    bne +
    rts
+   cmp #'='
    bne +
    ; Label assignment.
    ; TODO: Expect expression.
    rts
+   cmp #':'
    bne +
    ; Ignore a single colon after a line-starting label.
    iny
+   jsr accept_whitespace_and_comment
    lda (cur_line_addr),y
    bne +
    ; Label on a line by itself.
    rts
+

    ; Accept a mnemonic or pseudoop, and handle it.
    phy
    jsr accept_label_mnemonic_pseudoop
    bcc +
    pla
    jsr ay_to_mnemonic
    bcs @is_mnemonic
    jsr ay_to_pseudoop
    bcs @is_pseudoop
+   ; Something follows the label, but it's not a mnemonic or pseudoop.
    ply
    lda #err_syntax
    sta err_code
    sty err_pos
    rts

@is_mnemonic
    ; A=start pos, Y=end pos (+1)
    ; TODO: handle mnemonic
    rts

@is_pseudoop
    ; A=start pos, Y=end pos (+1)
    ; TODO: handle pseudoop
    rts


parse_source:
    lda #<(source_start+1)
    sta cur_line_addr
    lda #>(source_start+1)
    sta cur_line_addr+1

@line_loop
    ldy #0
    lda (cur_line_addr),y
    iny
    ora (cur_line_addr),y
    bne +
    bra @end_of_program
+   jsr parse_line
    lda err_code
    bne @found_error
    ldy #0
    lda (cur_line_addr),y
    sta cur_line_addr
    iny
    lda (cur_line_addr),y
    sta cur_line_addr+1
    bra @line_loop

@found_error
    jsr print_error

@end_of_program
    rts


test_line:
    !word test_line_end
    !word 12345
    ; !pet "!Abc", chr_backarrow, "d0123456789e", chr_megaat, "f", 0
    !pet "label", 0
test_line_end:
    !word 0

test_parser:
    +kprimm_start
    !pet "hey there, i'm gonna test some stuff",13,0
    +kprimm_end

    lda #<test_line
    sta cur_line_addr
    lda #>test_line
    sta cur_line_addr+1
    ldy #4

    ldz #1
    clc
    jsr accept_label_mnemonic_pseudoop
    bcc @test_fail
    ldz #2
    cpy #test_line_end-test_line-1
    bne @test_fail

    +kprimm_start
    !pet "all tests passed",13,0
    +kprimm_end
    rts

@test_fail
    pha
    phx
    phy
    phz
    php
    +kprimm_start
    !pet "failed ",0
    +kprimm_end
    tza
    jsr print_hex8
    lda #chr_cr
    +kcall bsout
    plp
    plz
    ply
    plx
    pla
    brk


; ---------------------------------------------------------
; Error message strings

; Error code constants
err_syntax = 1

err_message_tbl:
!word e01, e02

err_messages:
e01: !pet "syntax error",0
e02: !pet "wassup",0


; ---------------------------------------------------------
; Mnemonics tables

; 00-03: lowercase text, left aligned, null padded
; 04-05: addressing mode bitmask
; 06-07: address of encoding list for supported addressing modes
;            %11111111,
;             ^ Implied
;              ^ Immediate
;               ^ Immedate word
;                ^ Base-page
;                 ^ Base-page X-Indexed
;                  ^ Base-page Y-Indexed
;                   ^ Absolute
;                    ^ Absolute X-Indexed
;                      %11111111
;                       ^ Absolute Y-Indexed
;                        ^ Absolute Indirect
;                         ^ Absolute Indirect X-Indexed
;                          ^ Base-Page Indirect X-Indexed
;                           ^ Base-Page Indirect Y-Indexed
;                            ^ Base-Page Indirect Z-Indexed
;                             ^ 32-bit Base-Page Indirect Z-Indexed
;                              ^ Stack Relative Indirect, Y-Indexed
mnemonics:
!pet "adc",0,%00000000,%00000000
!word enc_adc
!pet "adcq" ,%00000000,%00000000
!word enc_adcq
!pet "and",0,%00000000,%00000000
!word enc_and
!pet "andq" ,%00000000,%00000000
!word enc_andq
!pet "asl",0,%00000000,%00000000
!word enc_asl
!pet "aslq" ,%00000000,%00000000
!word enc_aslq
!pet "asr",0,%00000000,%00000000
!word enc_asr
!pet "asrq" ,%00000000,%00000000
!word enc_asrq
!pet "asw",0,%00000000,%00000000
!word enc_asw
!pet "aug",0,%00000000,%00000000
!word enc_aug
!pet "bbr0" ,%00000000,%00000000
!word enc_bbr0
!pet "bbr1" ,%00000000,%00000000
!word enc_bbr1
!pet "bbr2" ,%00000000,%00000000
!word enc_bbr2
!pet "bbr3" ,%00000000,%00000000
!word enc_bbr3
!pet "bbr4" ,%00000000,%00000000
!word enc_bbr4
!pet "bbr5" ,%00000000,%00000000
!word enc_bbr5
!pet "bbr6" ,%00000000,%00000000
!word enc_bbr6
!pet "bbr7" ,%00000000,%00000000
!word enc_bbr7
!pet "bbs0" ,%00000000,%00000000
!word enc_bbs0
!pet "bbs1" ,%00000000,%00000000
!word enc_bbs1
!pet "bbs2" ,%00000000,%00000000
!word enc_bbs2
!pet "bbs3" ,%00000000,%00000000
!word enc_bbs3
!pet "bbs4" ,%00000000,%00000000
!word enc_bbs4
!pet "bbs5" ,%00000000,%00000000
!word enc_bbs5
!pet "bbs6" ,%00000000,%00000000
!word enc_bbs6
!pet "bbs7" ,%00000000,%00000000
!word enc_bbs7
!pet "bcc",0,%00000000,%00000000
!word enc_bcc
!pet "bcs",0,%00000000,%00000000
!word enc_bcs
!pet "beq",0,%00000000,%00000000
!word enc_beq
!pet "bit",0,%00000000,%00000000
!word enc_bit
!pet "bitq" ,%00000000,%00000000
!word enc_bitq
!pet "bmi",0,%00000000,%00000000
!word enc_bmi
!pet "bne",0,%00000000,%00000000
!word enc_bne
!pet "bpl",0,%00000000,%00000000
!word enc_bpl
!pet "bra",0,%00000000,%00000000
!word enc_bra
!pet "brk",0,%00000000,%00000000
!word enc_brk
!pet "bsr",0,%00000000,%00000000
!word enc_bsr
!pet "bvc",0,%00000000,%00000000
!word enc_bvc
!pet "bvs",0,%00000000,%00000000
!word enc_bvs
!pet "clc",0,%00000000,%00000000
!word enc_clc
!pet "cld",0,%00000000,%00000000
!word enc_cld
!pet "cle",0,%00000000,%00000000
!word enc_cle
!pet "cli",0,%00000000,%00000000
!word enc_cli
!pet "clv",0,%00000000,%00000000
!word enc_clv
!pet "cmp",0,%00000000,%00000000
!word enc_cmp
!pet "cmpq" ,%00000000,%00000000
!word enc_cmpq
!pet "cpq",0,%00000000,%00000000
!word enc_cpq
!pet "cpx",0,%00000000,%00000000
!word enc_cpx
!pet "cpy",0,%00000000,%00000000
!word enc_cpy
!pet "cpz",0,%00000000,%00000000
!word enc_cpz
!pet "dec",0,%00000000,%00000000
!word enc_dec
!pet "deq",0,%00000000,%00000000
!word enc_deq
!pet "dew",0,%00000000,%00000000
!word enc_dew
!pet "dex",0,%00000000,%00000000
!word enc_dex
!pet "dey",0,%00000000,%00000000
!word enc_dey
!pet "dez",0,%00000000,%00000000
!word enc_dez
!pet "eom",0,%00000000,%00000000
!word enc_eom
!pet "eor",0,%00000000,%00000000
!word enc_eor
!pet "eorq" ,%00000000,%00000000
!word enc_eorq
!pet "inc",0,%00000000,%00000000
!word enc_inc
!pet "inq",0,%00000000,%00000000
!word enc_inq
!pet "inw",0,%00000000,%00000000
!word enc_inw
!pet "inx",0,%00000000,%00000000
!word enc_inx
!pet "iny",0,%00000000,%00000000
!word enc_iny
!pet "inz",0,%00000000,%00000000
!word enc_inz
!pet "jmp",0,%00000000,%00000000
!word enc_jmp
!pet "jsr",0,%00000000,%00000000
!word enc_jsr
!pet "lbcc" ,%00000000,%00000000
!word enc_lbcc
!pet "lbcs" ,%00000000,%00000000
!word enc_lbcs
!pet "lbeq" ,%00000000,%00000000
!word enc_lbeq
!pet "lbmi" ,%00000000,%00000000
!word enc_lbmi
!pet "lbne" ,%00000000,%00000000
!word enc_lbne
!pet "lbpl" ,%00000000,%00000000
!word enc_lbpl
!pet "lbra" ,%00000000,%00000000
!word enc_lbra
!pet "lbsr" ,%00000000,%00000000
!word enc_lbsr
!pet "lbvc" ,%00000000,%00000000
!word enc_lbvc
!pet "lbvs" ,%00000000,%00000000
!word enc_lbvs
!pet "lda",0,%00000000,%00000000
!word enc_lda
!pet "ldq",0,%00000000,%00000000
!word enc_ldq
!pet "ldx",0,%00000000,%00000000
!word enc_ldx
!pet "ldy",0,%00000000,%00000000
!word enc_ldy
!pet "ldz",0,%00000000,%00000000
!word enc_ldz
!pet "lsr",0,%00000000,%00000000
!word enc_lsr
!pet "lsrq" ,%00000000,%00000000
!word enc_lsrq
!pet "map",0,%00000000,%00000000
!word enc_map
!pet "neg",0,%00000000,%00000000
!word enc_neg
!pet "nop",0,%00000000,%00000000
!word enc_nop
!pet "ora",0,%00000000,%00000000
!word enc_ora
!pet "orq",0,%00000000,%00000000
!word enc_orq
!pet "pha",0,%00000000,%00000000
!word enc_pha
!pet "php",0,%00000000,%00000000
!word enc_php
!pet "phw",0,%00000000,%00000000
!word enc_phw
!pet "phx",0,%00000000,%00000000
!word enc_phx
!pet "phy",0,%00000000,%00000000
!word enc_phy
!pet "phz",0,%00000000,%00000000
!word enc_phz
!pet "pla",0,%00000000,%00000000
!word enc_pla
!pet "plp",0,%00000000,%00000000
!word enc_plp
!pet "plx",0,%00000000,%00000000
!word enc_plx
!pet "ply",0,%00000000,%00000000
!word enc_ply
!pet "plz",0,%00000000,%00000000
!word enc_plz
!pet "rmb0" ,%00000000,%00000000
!word enc_rmb0
!pet "rmb1" ,%00000000,%00000000
!word enc_rmb1
!pet "rmb2" ,%00000000,%00000000
!word enc_rmb2
!pet "rmb3" ,%00000000,%00000000
!word enc_rmb3
!pet "rmb4" ,%00000000,%00000000
!word enc_rmb4
!pet "rmb5" ,%00000000,%00000000
!word enc_rmb5
!pet "rmb6" ,%00000000,%00000000
!word enc_rmb6
!pet "rmb7" ,%00000000,%00000000
!word enc_rmb7
!pet "rol",0,%00000000,%00000000
!word enc_rol
!pet "rolq" ,%00000000,%00000000
!word enc_rolq
!pet "ror",0,%00000000,%00000000
!word enc_ror
!pet "rorq" ,%00000000,%00000000
!word enc_rorq
!pet "row",0,%00000000,%00000000
!word enc_row
!pet "rti",0,%00000000,%00000000
!word enc_rti
!pet "rts",0,%00000000,%00000000
!word enc_rts
!pet "sbc",0,%00000000,%00000000
!word enc_sbc
!pet "sbcq" ,%00000000,%00000000
!word enc_sbcq
!pet "sec",0,%00000000,%00000000
!word enc_sec
!pet "sed",0,%00000000,%00000000
!word enc_sed
!pet "see",0,%00000000,%00000000
!word enc_see
!pet "sei",0,%00000000,%00000000
!word enc_sei
!pet "smb0" ,%00000000,%00000000
!word enc_smb0
!pet "smb1" ,%00000000,%00000000
!word enc_smb1
!pet "smb2" ,%00000000,%00000000
!word enc_smb2
!pet "smb3" ,%00000000,%00000000
!word enc_smb3
!pet "smb4" ,%00000000,%00000000
!word enc_smb4
!pet "smb5" ,%00000000,%00000000
!word enc_smb5
!pet "smb6" ,%00000000,%00000000
!word enc_smb6
!pet "smb7" ,%00000000,%00000000
!word enc_smb7
!pet "sta",0,%00000000,%00000000
!word enc_sta
!pet "stq",0,%00000000,%00000000
!word enc_stq
!pet "stx",0,%00000000,%00000000
!word enc_stx
!pet "sty",0,%00000000,%00000000
!word enc_sty
!pet "stz",0,%00000000,%00000000
!word enc_stz
!pet "tab",0,%00000000,%00000000
!word enc_tab
!pet "tax",0,%00000000,%00000000
!word enc_tax
!pet "tay",0,%00000000,%00000000
!word enc_tay
!pet "taz",0,%00000000,%00000000
!word enc_taz
!pet "tba",0,%00000000,%00000000
!word enc_tba
!pet "trb",0,%00000000,%00000000
!word enc_trb
!pet "tsb",0,%00000000,%00000000
!word enc_tsb
!pet "tsx",0,%00000000,%00000000
!word enc_tsx
!pet "tsy",0,%00000000,%00000000
!word enc_tsy
!pet "txa",0,%00000000,%00000000
!word enc_txa
!pet "txs",0,%00000000,%00000000
!word enc_txs
!pet "tya",0,%00000000,%00000000
!word enc_tya
!pet "tys",0,%00000000,%00000000
!word enc_tys
!pet "tza",0,%00000000,%00000000
!word enc_tza
mnemonics_end:

; Encoding lists
; Encodings for each supported addressing mode, lsb to msb in the bitfield
; (Quad prefix NEG NEG and 32-bit Indirect prefix NOP is added in code.)
enc_adc : !byte $00
enc_adcq: !byte $00
enc_and : !byte $00
enc_andq: !byte $00
enc_asl : !byte $00
enc_aslq: !byte $00
enc_asr : !byte $00
enc_asrq: !byte $00
enc_asw : !byte $00
enc_aug : !byte $00
enc_bbr0: !byte $00
enc_bbr1: !byte $00
enc_bbr2: !byte $00
enc_bbr3: !byte $00
enc_bbr4: !byte $00
enc_bbr5: !byte $00
enc_bbr6: !byte $00
enc_bbr7: !byte $00
enc_bbs0: !byte $00
enc_bbs1: !byte $00
enc_bbs2: !byte $00
enc_bbs3: !byte $00
enc_bbs4: !byte $00
enc_bbs5: !byte $00
enc_bbs6: !byte $00
enc_bbs7: !byte $00
enc_bcc : !byte $00
enc_bcs : !byte $00
enc_beq : !byte $00
enc_bit : !byte $00
enc_bitq: !byte $00
enc_bmi : !byte $00
enc_bne : !byte $00
enc_bpl : !byte $00
enc_bra : !byte $00
enc_brk : !byte $00
enc_bsr : !byte $00
enc_bvc : !byte $00
enc_bvs : !byte $00
enc_clc : !byte $00
enc_cld : !byte $00
enc_cle : !byte $00
enc_cli : !byte $00
enc_clv : !byte $00
enc_cmp : !byte $00
enc_cmpq: !byte $00
enc_cpq : !byte $00
enc_cpx : !byte $00
enc_cpy : !byte $00
enc_cpz : !byte $00
enc_dec : !byte $00
enc_deq : !byte $00
enc_dew : !byte $00
enc_dex : !byte $00
enc_dey : !byte $00
enc_dez : !byte $00
enc_eom : !byte $00
enc_eor : !byte $00
enc_eorq: !byte $00
enc_inc : !byte $00
enc_inq : !byte $00
enc_inw : !byte $00
enc_inx : !byte $00
enc_iny : !byte $00
enc_inz : !byte $00
enc_jmp : !byte $00
enc_jsr : !byte $00
enc_lbcc: !byte $00
enc_lbcs: !byte $00
enc_lbeq: !byte $00
enc_lbmi: !byte $00
enc_lbne: !byte $00
enc_lbpl: !byte $00
enc_lbra: !byte $00
enc_lbsr: !byte $00
enc_lbvc: !byte $00
enc_lbvs: !byte $00
enc_lda : !byte $00
enc_ldq : !byte $00
enc_ldx : !byte $00
enc_ldy : !byte $00
enc_ldz : !byte $00
enc_lsr : !byte $00
enc_lsrq: !byte $00
enc_map : !byte $00
enc_neg : !byte $00
enc_nop : !byte $00
enc_ora : !byte $00
enc_orq : !byte $00
enc_pha : !byte $00
enc_php : !byte $00
enc_phw : !byte $00
enc_phx : !byte $00
enc_phy : !byte $00
enc_phz : !byte $00
enc_pla : !byte $00
enc_plp : !byte $00
enc_plx : !byte $00
enc_ply : !byte $00
enc_plz : !byte $00
enc_rmb0: !byte $00
enc_rmb1: !byte $00
enc_rmb2: !byte $00
enc_rmb3: !byte $00
enc_rmb4: !byte $00
enc_rmb5: !byte $00
enc_rmb6: !byte $00
enc_rmb7: !byte $00
enc_rol : !byte $00
enc_rolq: !byte $00
enc_ror : !byte $00
enc_rorq: !byte $00
enc_row : !byte $00
enc_rti : !byte $00
enc_rts : !byte $00
enc_sbc : !byte $00
enc_sbcq: !byte $00
enc_sec : !byte $00
enc_sed : !byte $00
enc_see : !byte $00
enc_sei : !byte $00
enc_smb0: !byte $00
enc_smb1: !byte $00
enc_smb2: !byte $00
enc_smb3: !byte $00
enc_smb4: !byte $00
enc_smb5: !byte $00
enc_smb6: !byte $00
enc_smb7: !byte $00
enc_sta : !byte $00
enc_stq : !byte $00
enc_stx : !byte $00
enc_sty : !byte $00
enc_stz : !byte $00
enc_tab : !byte $00
enc_tax : !byte $00
enc_tay : !byte $00
enc_taz : !byte $00
enc_tba : !byte $00
enc_trb : !byte $00
enc_tsb : !byte $00
enc_tsx : !byte $00
enc_tsy : !byte $00
enc_txa : !byte $00
enc_txs : !byte $00
enc_tya : !byte $00
enc_tys : !byte $00
enc_tza : !byte $00


; ---------------------------------------------------------
; Pseudo-op table

pseudoops:
po_to = 1
!pet "to",0
po_byte = 2
!pet "byte",0
po_8 = 3
!pet "8",0
po_word = 4
!pet "word",0
po_16 = 5
!pet "16",0
po_32 = 6
!pet "32",0
po_fill = 7
!pet "fill",0
po_pet = 8
!pet "pet",0
po_scr = 9
!pet "scr",0
po_source = 10
!pet "source",0
po_binary = 11
!pet "binary",0
po_warn = 12
!pet "warn",0
!byte 0


; ---------------------------------------------------------

!if * >= strbuf {
    !error "EasyAsm code is too large, * = ", *
}
