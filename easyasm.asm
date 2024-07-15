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
; Uses: code_ptr
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
    sta bas_ptr
    lda #0
    adc cur_line_addr+1
    sta bas_ptr+1

    lda #0
    sta strbuf,x    ; null terminator
    dex
    txa
    taz
-   lda [bas_ptr],z
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
+   sta strbuf,x
    dex
    dez
    bpl -

    rts


; Is a substring on the current line a mnemonic
; Input: A=start pos, Y=end pos (+1), cur_line_addr
; Output:
;   C=1: is mnemonic, code_ptr=mnemonic table row
;   C=0: is not mnemonic
ay_to_mnemonic:
    jsr ay_to_lower   ; move candidate to strbuf

strbuf_to_mnemonic:
    lda #<mnemonics
    sta code_ptr
    lda #>mnemonics
    sta code_ptr+1

    ; code_ptr = beginning of row
    ; Y = row index; (code_ptr),y = table char
    ; X = buffer index; strbuf,x = buffer char
@nextrow
    ldy #0
    lda (code_ptr),y
    bne +
    ; No match in table. Exit C=0.
    clc
    rts

+   ldx #0
-   lda (code_ptr),y
    cmp strbuf,x
    bne +
    inx
    iny
    cpx #4
    bne -
    ; Full match. Exit C=1, code_ptr=row.
    sec
    rts

+   lda code_ptr    ; advance row
    clc
    adc #8
    sta code_ptr
    lda code_ptr+1
    adc #0
    sta code_ptr+1
    bra @nextrow


; Is a substring on the current line a pseudoop
; Input: A=start pos, Y=end pos (+1), cur_line_addr
; Output: C=1: is pseudoop, A=pseudoop number; strbuf is normalized pseudoop string
ay_to_pseudoop:
    jsr ay_to_lower   ; move candidate to strbuf

strbuf_to_pseudoop:
    ; Y = current pseudoop number
    ; X = buffer index; strbuf,x = buffer char
    ; (Z=0. All table reads via code_ptr+0.)
    ldx #0
    lda strbuf,x
    cmp #'!'          ; confirm leading '!'
    beq +
    clc
    rts

+   lda #<pseudoops
    sta code_ptr
    lda #>pseudoops
    sta code_ptr+1
    ldy #1
    ldz #0

@try_token
    lda (code_ptr),z
    bne +
    ; No matches. Exit with C=0.
    clc
    rts

+   ldx #1
-   lda (code_ptr),z
    cmp strbuf,x
    bne @skip_token
    lda (code_ptr),z
    bne +
    ; Matched up to end. Exit with C=1, A=Y.
    sec
    tya
    rts

+   inx
    inw code_ptr
    bra -

@skip_token
    lda (code_ptr),z
    beq +
    inw code_ptr
    bra @skip_token
+   iny
    inw code_ptr
    bra @try_token


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
    !pet "!Warn more text", 0
test_line_end:
    !word 0

test_num:
    !byte 0

test_parser:
    +kprimm_start
    !pet "hey there, i'm gonna test some stuff",13,0
    +kprimm_end

    lda #<test_line
    sta code_ptr
    lda #>test_line
    sta code_ptr+1
    lda #<$3000
    sta bas_ptr
    lda #>$3000
    sta bas_ptr+1
    ldz #0
    ldy #0
-   lda (code_ptr),y
    sta [bas_ptr],z
    beq +
    inw code_ptr
    inw bas_ptr
    bra -
+   ; (Not quite right, doesn't copy last two zeros, next line ptr is wrong)

    lda #<$3000
    sta cur_line_addr
    lda #>$3000
    sta cur_line_addr+1

    lda #4
    ldy #9

    ldz #1
    stz test_num
    jsr ay_to_pseudoop
    bcc @test_fail
    ldz #2
    stz test_num
    cmp #12
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
    lda test_num
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
;             ^ Implied (parameterless, or A/Q)
;              ^ Immediate
;               ^ Immedate word
;                ^ Base-page, branch relative, bit-test branch relative
;                 ^ Base-page X-Indexed
;                  ^ Base-page Y-Indexed
;                   ^ Absolute, 16-bit branch relative
;                    ^ Absolute X-Indexed
;                      %11111111
;                       ^ Absolute Y-Indexed
;                        ^ Absolute Indirect
;                         ^ Absolute Indirect X-Indexed
;                          ^ Base-Page Indirect X-Indexed
;                           ^ Base-Page Indirect Y-Indexed
;                            ^ Base-Page Indirect Z-Indexed (or no index)
;                             ^ 32-bit Base-Page Indirect Z-Indexed (or no index)
;                              ^ Stack Relative Indirect, Y-Indexed
mnemonics:
!pet "adc",0,%01011011,%10011110
!word enc_adc
!pet "adcq" ,%00010010,%00000110
!word enc_adcq
!pet "and",0,%01011011,%10011110
!word enc_and
!pet "andq" ,%00010010,%00000110
!word enc_andq
!pet "asl",0,%10011011,%00000000
!word enc_asl
!pet "aslq" ,%10011011,%00000000
!word enc_aslq
!pet "asr",0,%10011000,%00000000
!word enc_asr
!pet "asrq" ,%10011000,%00000000
!word enc_asrq
!pet "asw",0,%00000010,%00000000
!word enc_asw
!pet "bbr0" ,%00010000,%00000000
!word enc_bbr0
!pet "bbr1" ,%00010000,%00000000
!word enc_bbr1
!pet "bbr2" ,%00010000,%00000000
!word enc_bbr2
!pet "bbr3" ,%00010000,%00000000
!word enc_bbr3
!pet "bbr4" ,%00010000,%00000000
!word enc_bbr4
!pet "bbr5" ,%00010000,%00000000
!word enc_bbr5
!pet "bbr6" ,%00010000,%00000000
!word enc_bbr6
!pet "bbr7" ,%00010000,%00000000
!word enc_bbr7
!pet "bbs0" ,%00010000,%00000000
!word enc_bbs0
!pet "bbs1" ,%00010000,%00000000
!word enc_bbs1
!pet "bbs2" ,%00010000,%00000000
!word enc_bbs2
!pet "bbs3" ,%00010000,%00000000
!word enc_bbs3
!pet "bbs4" ,%00010000,%00000000
!word enc_bbs4
!pet "bbs5" ,%00010000,%00000000
!word enc_bbs5
!pet "bbs6" ,%00010000,%00000000
!word enc_bbs6
!pet "bbs7" ,%00010000,%00000000
!word enc_bbs7
!pet "bcc",0,%00010010,%00000000
!word enc_bcc
!pet "bcs",0,%00010010,%00000000
!word enc_bcs
!pet "beq",0,%00010010,%00000000
!word enc_beq
!pet "bit",0,%01011011,%00000000
!word enc_bit
!pet "bitq" ,%00010010,%00000000
!word enc_bitq
!pet "bmi",0,%00010010,%00000000
!word enc_bmi
!pet "bne",0,%00010010,%00000000
!word enc_bne
!pet "bpl",0,%00010010,%00000000
!word enc_bpl
!pet "bra",0,%00010010,%00000000
!word enc_bra
!pet "brk",0,%10000000,%00000000
!word enc_brk
!pet "bsr",0,%00000010,%00000000
!word enc_bsr
!pet "bvc",0,%00010010,%00000000
!word enc_bvc
!pet "bvs",0,%00010010,%00000000
!word enc_bvs
!pet "clc",0,%10000000,%00000000
!word enc_clc
!pet "cld",0,%10000000,%00000000
!word enc_cld
!pet "cle",0,%10000000,%00000000
!word enc_cle
!pet "cli",0,%10000000,%00000000
!word enc_cli
!pet "clv",0,%10000000,%00000000
!word enc_clv
!pet "cmp",0,%01011011,%10011110
!word enc_cmp
!pet "cmpq" ,%00010010,%00000110
!word enc_cmpq
!pet "cpx",0,%01010010,%00000000
!word enc_cpx
!pet "cpy",0,%01010010,%00000000
!word enc_cpy
!pet "cpz",0,%01010010,%00000000
!word enc_cpz
!pet "dec",0,%10011011,%00000000
!word enc_dec
!pet "deq",0,%10011011,%00000000
!word enc_deq
!pet "dew",0,%00010000,%00000000
!word enc_dew
!pet "dex",0,%10000000,%00000000
!word enc_dex
!pet "dey",0,%10000000,%00000000
!word enc_dey
!pet "dez",0,%10000000,%00000000
!word enc_dez
!pet "eom",0,%10000000,%00000000
!word enc_eom
!pet "eor",0,%01011011,%10011110
!word enc_eor
!pet "eorq" ,%00010010,%00000110
!word enc_eorq
!pet "inc",0,%10011011,%00000000
!word enc_inc
!pet "inq",0,%10011011,%00000000
!word enc_inq
!pet "inw",0,%00010000,%00000000
!word enc_inw
!pet "inx",0,%10000000,%00000000
!word enc_inx
!pet "iny",0,%10000000,%00000000
!word enc_iny
!pet "inz",0,%10000000,%00000000
!word enc_inz
!pet "jmp",0,%00000010,%01100000
!word enc_jmp
!pet "jsr",0,%00000010,%01100000
!word enc_jsr
!pet "lda",0,%01011011,%10011111
!word enc_lda
!pet "ldq",0,%00010010,%00000110
!word enc_ldq
!pet "ldx",0,%01010110,%10000000
!word enc_ldx
!pet "ldy",0,%01011011,%00000000
!word enc_ldy
!pet "ldz",0,%01000011,%00000000
!word enc_ldz
!pet "lsr",0,%10011011,%00000000
!word enc_lsr
!pet "lsrq" ,%10011011,%00000000
!word enc_lsrq
!pet "map",0,%10000000,%00000000
!word enc_map
!pet "neg",0,%10000000,%00000000
!word enc_neg
!pet "ora",0,%01011011,%10011110
!word enc_ora
!pet "orq",0,%00010010,%00000110
!word enc_orq
!pet "pha",0,%10000000,%00000000
!word enc_pha
!pet "php",0,%10000000,%00000000
!word enc_php
!pet "phw",0,%00100010,%00000000
!word enc_phw
!pet "phx",0,%10000000,%00000000
!word enc_phx
!pet "phy",0,%10000000,%00000000
!word enc_phy
!pet "phz",0,%10000000,%00000000
!word enc_phz
!pet "pla",0,%10000000,%00000000
!word enc_pla
!pet "plp",0,%10000000,%00000000
!word enc_plp
!pet "plx",0,%10000000,%00000000
!word enc_plx
!pet "ply",0,%10000000,%00000000
!word enc_ply
!pet "plz",0,%10000000,%00000000
!word enc_plz
!pet "resq" ,%00001001,%10011000
!word enc_resq
!pet "rmb0" ,%00010000,%00000000
!word enc_rmb0
!pet "rmb1" ,%00010000,%00000000
!word enc_rmb1
!pet "rmb2" ,%00010000,%00000000
!word enc_rmb2
!pet "rmb3" ,%00010000,%00000000
!word enc_rmb3
!pet "rmb4" ,%00010000,%00000000
!word enc_rmb4
!pet "rmb5" ,%00010000,%00000000
!word enc_rmb5
!pet "rmb6" ,%00010000,%00000000
!word enc_rmb6
!pet "rmb7" ,%00010000,%00000000
!word enc_rmb7
!pet "rol",0,%10011011,%00000000
!word enc_rol
!pet "rolq" ,%10011011,%00000000
!word enc_rolq
!pet "ror",0,%10011011,%00000000
!word enc_ror
!pet "rorq" ,%10011011,%00000000
!word enc_rorq
!pet "row",0,%00000010,%00000000
!word enc_row
!pet "rsvq" ,%00001001,%10011001
!word enc_rsvq
!pet "rti",0,%10000000,%00000000
!word enc_rti
!pet "rts",0,%11000000,%00000000
!word enc_rts
!pet "sbc",0,%01011011,%10011110
!word enc_sbc
!pet "sbcq" ,%00010010,%00000110
!word enc_sbcq
!pet "sec",0,%10000000,%00000000
!word enc_sec
!pet "sed",0,%10000000,%00000000
!word enc_sed
!pet "see",0,%10000000,%00000000
!word enc_see
!pet "sei",0,%10000000,%00000000
!word enc_sei
!pet "smb0" ,%00010000,%00000000
!word enc_smb0
!pet "smb1" ,%00010000,%00000000
!word enc_smb1
!pet "smb2" ,%00010000,%00000000
!word enc_smb2
!pet "smb3" ,%00010000,%00000000
!word enc_smb3
!pet "smb4" ,%00010000,%00000000
!word enc_smb4
!pet "smb5" ,%00010000,%00000000
!word enc_smb5
!pet "smb6" ,%00010000,%00000000
!word enc_smb6
!pet "smb7" ,%00010000,%00000000
!word enc_smb7
!pet "sta",0,%00011011,%10011111
!word enc_sta
!pet "stq",0,%00010010,%00000110
!word enc_stq
!pet "stx",0,%00010110,%10000000
!word enc_stx
!pet "sty",0,%00011011,%00000000
!word enc_sty
!pet "stz",0,%00011011,%00000000
!word enc_stz
!pet "tab",0,%10000000,%00000000
!word enc_tab
!pet "tax",0,%10000000,%00000000
!word enc_tax
!pet "tay",0,%10000000,%00000000
!word enc_tay
!pet "taz",0,%10000000,%00000000
!word enc_taz
!pet "tba",0,%10000000,%00000000
!word enc_tba
!pet "trb",0,%00010010,%00000000
!word enc_trb
!pet "tsb",0,%00010010,%00000000
!word enc_tsb
!pet "tsx",0,%10000000,%00000000
!word enc_tsx
!pet "tsy",0,%10000000,%00000000
!word enc_tsy
!pet "txa",0,%10000000,%00000000
!word enc_txa
!pet "txs",0,%10000000,%00000000
!word enc_txs
!pet "tya",0,%10000000,%00000000
!word enc_tya
!pet "tys",0,%10000000,%00000000
!word enc_tys
!pet "tza",0,%10000000,%00000000
!word enc_tza
!byte 0

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
enc_resq: !byte $75, $7d, $79, $61, $71
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
enc_rsvq: !byte $f5, $fd, $f9, $e1, $f1, $82
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
