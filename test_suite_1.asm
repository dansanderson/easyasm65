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
    brk
+   lda line_pos
    cmp #.eerror_pos
    beq +
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
    brk
++
}

    +test_end
}

test_tokenize_1: !pet 0
test_tokenize_1e: !byte 0, $ff
test_tokenize_2: !pet "    ; comment only",0
test_tokenize_2e: !byte 0, $ff
test_tokenize_3: !pet "\"string literal\"",0
test_tokenize_3e: !byte tk_string_literal, 4, 14, 0, $ff
test_tokenize_4: !pet "12345",0
test_tokenize_4e:
    !byte tk_number_literal, 4
    !32 12345
    !byte 0, $ff
test_tokenize_5: !pet "$DeAdBeEf",0
test_tokenize_5e:
    !byte tk_number_literal, 4
    !32 $deadbeef
    !byte 0, $ff
test_tokenize_6: !pet "tZa",0
test_tokenize_6e: !byte 135, 4, 0, $ff
test_tokenize_7: !pet "!wOrD",0
test_tokenize_7e: !byte po_word, 4, 0, $ff
test_tokenize_8: !pet "xOr",0
test_tokenize_8e: !byte tk_label_or_reg, 4, 3, 0, $ff
test_tokenize_9: !pet ">>>",0
test_tokenize_9e: !byte tk_lsr, 4, 0, $ff
test_tokenize_10: !pet "label",0
test_tokenize_10e: !byte tk_label_or_reg, 4, 5, 0, $ff
test_tokenize_11: !pet "@label",0
test_tokenize_11e: !byte tk_label_or_reg, 4, 6, 0, $ff
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
    !byte 0, $ff
test_tokenize_13: !pet "label = *+4",0
test_tokenize_13e:
    !byte tk_label_or_reg, 4, 5
    !byte tk_equal, 10
    !byte tk_multiply, 12
    !byte tk_plus, 13
    !byte tk_number_literal, 14
    !32 4
    !byte 0, $ff
test_tokenize_14: !pet "* = $d000",0
test_tokenize_14e:
    !byte tk_multiply, 4
    !byte tk_equal, 6
    !byte tk_number_literal, 8
    !32 $d000
    !byte 0, $ff
test_tokenize_15: !pet "!to \"lda\", runnable",0
test_tokenize_15e:
    !byte po_to, 4
    !byte tk_string_literal, 8, 3
    !byte tk_comma, 13
    !byte tk_label_or_reg, 15, 8
    !byte 0, $ff
test_tokenize_16: !pet "lda $000a",0
test_tokenize_16e:
    !byte 66, 4
    !byte tk_number_literal_leading_zero, 8
    !32 $000a
    !byte 0, $ff
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
    brk
+
    lda #<test_symbol_table_last_addr
    ldx #>test_symbol_table_last_addr
    ldy #^test_symbol_table_last_addr
    ldz #$08
    cpq attic_ptr
    beq +
    brk
+
} else {
    bcc +
    brk
+
    lda #<.eatticptr
    ldx #>.eatticptr
    ldy #^.eatticptr
    ldz #$08
    cpq attic_ptr
    beq +
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
    brk
+
    ; TODO: confirm out of memory conditions
} else {
    bcc +
    brk
+
    lda #<.eatticptr
    ldx #>.eatticptr
    ldy #^.eatticptr
    ldz #$08
    cpq attic_ptr
    beq +
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
    brk
+
} else {
    bcc +
    brk
+   cmp #<.eq
    beq +
    brk
+   cpx #>.eq
    beq +
    brk
+   cpy #^.eq
    beq +
    brk
+   cpz #<(.eq >>> 24)
    beq +
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
    jsr init_segment_table
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
    brk
+
} else {
    bcc +
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
+   brk
++
    lda next_segment_pc
    cmp #<.epc
    bne +
    lda next_segment_pc+1
    cmp #>.epc
    bne +
    bra ++
+   brk
++
}

!if .pass = $ff {
    lda #<attic_segments
    ldx #>attic_segments
    ldy #^attic_segments
    ldz #<(attic_segments >>> 24)
    stq attic_ptr
    ldz #0
    lda [attic_ptr],z
    cmp #<.pc
    beq +
    brk
+   inz
    lda [attic_ptr],z
    cmp #>.pc
    beq +
    brk
+   inz
    lda [attic_ptr],z
    cmp #<.x
    beq +
    brk
+   inz
    lda [attic_ptr],z
    cmp #>.x
    beq +
    brk
+   inz

    ldy #.x
    ldx #0
-   txa
    cmp [attic_ptr],z
    beq +
    brk
+
    inz
    inx
    dey
    bne -
}

    +test_end
}

!macro test_assemble_bytes_twice .tnum, .pass, .pc1, .pc2, .x, .epc, .edata, .edataend {
    +test_start .tnum
    jsr init_segment_table
    lda #.pass
    sta pass
    jsr init_pass
    ldx #<.pc1
    ldy #>.pc1
    jsr set_pc
    +test_do_assemble_bytes .x
    bcc +
    brk
+
    ldx #<.pc2
    ldy #>.pc2
    jsr set_pc
    +test_do_assemble_bytes .x
    bcc +
    brk
+
    lda program_counter
    cmp #<.epc
    bne +
    lda program_counter+1
    cmp #>.epc
    bne +
    bra ++
+   brk
++
    lda next_segment_pc
    cmp #<.epc
    bne +
    lda next_segment_pc+1
    cmp #>.epc
    bne +
    bra ++
+   brk
++
!if .pass = $ff {
    lda #<attic_segments
    ldx #>attic_segments
    ldy #^attic_segments
    ldz #<(attic_segments >>> 24)
    stq attic_ptr

    ldx #.edataend-.edata-1
-   txa
    taz
    lda .edata,x
    cmp [attic_ptr],z
    beq +
    brk
+
    dex
    bpl -
}
    +test_end
}

test_assemble_bytes_twice_1:
    !word $c000
    !word 10
    !byte 0, 1, 2, 3, 4, 0, 1, 2, 3, 4
test_assemble_bytes_twice_2:
    !word $c000
    !word 5
    !byte 0, 1, 2, 3, 4
    !word $d000
    !word 5
    !byte 0, 1, 2, 3, 4
test_assemble_bytes_twice_2_end:


!macro test_expect_token .tnum, .a, .tokbuf, .tokbufend, .ec, .etokpos {
    +test_start .tnum
    ldx #.tokbufend-.tokbuf
    dex
-   lda .tokbuf,x
    sta tokbuf,x
    dex
    bpl -

    lda #.a
    ldx #0
    stx tok_pos
    jsr expect_token
!if .ec {
    bcs +
    brk
+   ldx tok_pos
    beq +
    brk
+
} else {
    bcc +
    brk
+   ldx tok_pos
    cpx #.etokpos
    beq +
    brk
+
}

    +test_end
}

test_expect_token_1: !byte 0, $ff
test_expect_token_2: !byte 1, 4, 0, $ff
test_expect_token_end:

!macro test_expect_label .tnum, .tokbuf, .tokbufend, .ec, .etokpos, .ex, .ey {
    +test_start .tnum
    ldx #.tokbufend-.tokbuf
    dex
-   lda .tokbuf,x
    sta tokbuf,x
    dex
    bpl -

    ldx #0
    stx tok_pos
    jsr expect_label
!if .ec {
    bcs +
    brk
+   ldx tok_pos
    beq +
    brk
+
} else {
    bcc +
    brk
+   cpx #.ex
    beq +
    brk
+   cpy #.ey
    beq +
    brk
+   ldx tok_pos
    cpx #.etokpos
    beq +
    brk
+
}

    +test_end
}

test_expect_label_1: !byte 0, $ff
test_expect_label_2: !byte tk_label_or_reg, 99, 5, 0, $ff
test_expect_label_end:

!macro test_expect_keyword .tnum, .tokbuf, .tokbufend, .lineaddr, .kw, .ec, .etokpos {
    +test_start .tnum
    ldx #.tokbufend-.tokbuf
    dex
-   lda .tokbuf,x
    sta tokbuf,x
    dex
    bpl -

    ; Fake assembly location in bank 5
    lda #5
    sta bas_ptr+2
    lda #0
    sta bas_ptr+3

    lda #<.lineaddr
    sta line_addr
    lda #>.lineaddr
    sta line_addr+1
    ldx #0
    stx tok_pos
    ldx #<.kw
    ldy #>.kw
    jsr expect_keyword
!if .ec {
    bcs +
    brk
+   ldx tok_pos
    beq +
    brk
+
} else {
    bcc +
    brk
+   ldx tok_pos
    cpx #.etokpos
    beq +
    brk
+
}
    +test_end
}

test_expect_keyword_1: !byte 0, $ff
test_expect_keyword_2: !byte tk_label_or_reg, 2, 3, 0, $ff
test_expect_keyword_end:
test_expect_keyword_line_1: !pet "5 xor 7",0
test_expect_keyword_line_2: !pet "5 XoR 7",0
test_expect_keyword_line_3: !pet "5 ror 7",0

!macro test_expect_oppop .tnum, .isop, .tokbuf, .tokbufend, .ec, .etokpos, .ea {
    +test_start .tnum
    ldx #.tokbufend-.tokbuf
    dex
-   lda .tokbuf,x
    sta tokbuf,x
    dex
    bpl -

    ldx #0
    stx tok_pos
!if .isop {
    jsr expect_opcode
} else {
    jsr expect_pseudoop
}
!if .ec {
    bcs +
    brk
+   ldx tok_pos
    beq +
    brk
+
} else {
    bcc +
    brk
+   cmp #.ea
    beq +
    brk
+   ldx tok_pos
    cpx #.etokpos
    beq +
    brk
+
}

    +test_end
}
!macro test_expect_opcode .tnum, .tokbuf, .tokbufend, .ec, .etokpos, .ea {
    +test_expect_oppop .tnum, 1, .tokbuf, .tokbufend, .ec, .etokpos, .ea
}
!macro test_expect_pseudoop .tnum, .tokbuf, .tokbufend, .ec, .etokpos, .ea {
    +test_expect_oppop .tnum, 0, .tokbuf, .tokbufend, .ec, .etokpos, .ea
}

test_expect_oppop_1: !byte 0, $ff
test_expect_oppop_2: !byte 1, 4, 0, $ff
test_expect_oppop_3: !byte po_to, 4, 0, $ff
test_expect_oppop_end:

!macro test_expect_literal .tnum, .tokbuf, .tokbufend, .ec, .etokpos, .eresult, .eflags {
    +test_start .tnum
    ldx #.tokbufend-.tokbuf
    dex
-   lda .tokbuf,x
    sta tokbuf,x
    dex
    bpl -

    ldx #0
    stx tok_pos
    jsr expect_literal

!if .ec {
    bcs +
    brk
+   ldx tok_pos
    beq +
    brk
+
} else {
    bcc +
    brk
+   lda expr_result
    cmp #<.eresult
    beq +
    brk
+   lda expr_result+1
    cmp #>.eresult
    beq +
    brk
+   lda expr_result+2
    cmp #^.eresult
    beq +
    brk
+   lda expr_result+3
    cmp #<(.eresult >>> 24)
    beq +
    brk
+   lda expr_flags
    cmp #.eflags
    beq +
    brk
+
}

    +test_end
}

test_expect_literal_1: !byte 0, $ff
test_expect_literal_2: !byte tk_number_literal, 6, $dd, $cc, $bb, $aa, 0, $ff
test_expect_literal_3: !byte tk_number_literal_leading_zero, 6, $dd, $cc, $bb, $aa, 0, $ff
test_expect_literal_end:


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
    +test_find_in_token_list $03, test_find_in_token_list_3, 0, 1, 1, 135, 3
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
    +test_tokenize_mnemonic $03, test_find_in_token_list_3, 0, 1, 135, 3
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
    +test_assemble_bytes_twice $05, 0, $c000, $c005, 5, $c00a, 0, 0
    +test_assemble_bytes_twice $06, 0, $c000, $d000, 5, $d005, 0, 0
    +test_assemble_bytes_twice $07, $ff, $c000, $c005, 5, $c00a, test_assemble_bytes_twice_1, test_assemble_bytes_twice_2
    +test_assemble_bytes_twice $08, $ff, $c000, $d000, 5, $d005, test_assemble_bytes_twice_2, test_assemble_bytes_twice_2_end

    +print_chr chr_cr
    +print_strlit_line "test-expect-token"
    +test_expect_token $01, 1, test_expect_token_1, test_expect_token_2, 1, 0
    +test_expect_token $02, 1, test_expect_token_2, test_expect_token_end, 0, 2
    +test_expect_token $03, 4, test_expect_token_2, test_expect_token_end, 1, 0

    +print_chr chr_cr
    +print_strlit_line "test-expect-label"
    +test_expect_label $01, test_expect_label_1, test_expect_label_2, 1, 0, 0, 0
    +test_expect_label $02, test_expect_label_2, test_expect_label_end, 0, 3, 99, 5

    +print_chr chr_cr
    +print_strlit_line "test-expect-keyword"
    +test_expect_keyword $01, test_expect_keyword_1, test_expect_keyword_2, test_expect_keyword_line_1, kw_xor, 1, 0
    +test_expect_keyword $02, test_expect_keyword_2, test_expect_keyword_end, test_expect_keyword_line_1, kw_xor, 0, 3
    +test_expect_keyword $03, test_expect_keyword_2, test_expect_keyword_end, test_expect_keyword_line_2, kw_xor, 0, 3
    +test_expect_keyword $04, test_expect_keyword_2, test_expect_keyword_end, test_expect_keyword_line_3, kw_xor, 1, 0

    +print_chr chr_cr
    +print_strlit_line "test-expect-opcode"
    +test_expect_opcode $01, test_expect_oppop_1, test_expect_oppop_2, 1, 0, 0
    +test_expect_opcode $02, test_expect_oppop_2, test_expect_oppop_3, 0, 2, 1
    +test_expect_opcode $03, test_expect_oppop_3, test_expect_oppop_end, 1, 0, 0

    +print_chr chr_cr
    +print_strlit_line "test-expect-pseudoop"
    +test_expect_pseudoop $01, test_expect_oppop_1, test_expect_oppop_2, 1, 0, 0
    +test_expect_pseudoop $02, test_expect_oppop_2, test_expect_oppop_3, 1, 0, 0
    +test_expect_pseudoop $03, test_expect_oppop_3, test_expect_oppop_end, 0, 2, po_to

    +print_chr chr_cr
    +print_strlit_line "test-expect-literal"
    +test_expect_literal $01, test_expect_literal_1, test_expect_literal_2, 1, 0, 0, 0
    +test_expect_literal $02, test_expect_literal_2, test_expect_literal_3, 0, 6, $aabbccdd, 0
    +test_expect_literal $03, test_expect_literal_3, test_expect_literal_end, 0, 6, $aabbccdd, F_EXPR_BRACKET_ZERO

    +print_chr chr_cr
    +print_strlit_line "-- all tests passed --"
    rts
