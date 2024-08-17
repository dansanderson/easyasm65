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
    and #F_EXPR_FORCE16
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
    lda #1  ; mnemonics start at 1
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
test_find_in_token_list_8: !pet "adc+1  ",0
test_find_in_token_list_9: !pet "adc+2  ",0

!macro test_tokenize_mnemonic .tnum, .str, .pos, .ec, .etoken, .epos, .eflags {
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
    +assert_cs test_msg_ecs
    cpx #.etoken
    +assert_eq test_msg_wrong_result
    +assert_mem_eq_byte line_pos, .epos, test_msg_wrong_result
} else {
    +assert_cc test_msg_ecc
    +assert_mem_eq_byte line_pos, .epos, test_msg_wrong_err_pos
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
    +test_find_in_token_list $01, test_find_in_token_list_1, 0, 1, 1, mnemonic_adc, 3
    +test_find_in_token_list $02, test_find_in_token_list_2, 0, 1, 1, mnemonic_adcq, 4
    +test_find_in_token_list $03, test_find_in_token_list_3, 0, 1, 1, mnemonic_tza, 3
    +test_find_in_token_list $04, test_find_in_token_list_4, 0, 1, 0, mnemonic_adc, 0
    +test_find_in_token_list $05, test_find_in_token_list_5, 0, 0, 1, mnemonic_adc, 3
    +test_find_in_token_list $06, test_find_in_token_list_5, 0, 1, 0, mnemonic_adc, 0
    +test_find_in_token_list $07, test_find_in_token_list_6, 0, 0, 1, mnemonic_adc, 3
    +test_find_in_token_list $08, test_find_in_token_list_6, 0, 1, 1, mnemonic_adc, 3
    +test_find_in_token_list $09, test_find_in_token_list_7, 1, 1, 1, mnemonic_adc, 4

    +print_chr chr_cr
    +print_strlit_line "tokenize-mnemonic"
    ; .tnum, .str, .pos, .ec, .etoken, .epos, .eflags
    +test_tokenize_mnemonic $01, test_find_in_token_list_1, 0, 1, mnemonic_adc, 3, 0
    +test_tokenize_mnemonic $02, test_find_in_token_list_2, 0, 1, mnemonic_adcq, 4, 0
    +test_tokenize_mnemonic $03, test_find_in_token_list_3, 0, 1, mnemonic_tza, 3, 0
    +test_tokenize_mnemonic $04, test_find_in_token_list_4, 0, 0, 0, 0, 0
    +test_tokenize_mnemonic $05, test_find_in_token_list_5, 0, 0, 0, 0, 0
    +test_tokenize_mnemonic $06, test_find_in_token_list_6, 0, 1, mnemonic_adc, 3, 0
    +test_tokenize_mnemonic $07, test_find_in_token_list_7, 1, 1, mnemonic_adc, 4, 0
    +test_tokenize_mnemonic $08, test_find_in_token_list_8, 0, 1, mnemonic_adc, 5, F_ASM_FORCE8
    +test_tokenize_mnemonic $09, test_find_in_token_list_9, 0, 1, mnemonic_adc, 5, F_ASM_FORCE16

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
    +print_strlit_line "-- all tests passed --"
    rts
