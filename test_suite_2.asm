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

!macro test_expect_oppop .tnum, .isop, .tokbuf, .tokbufend, .ec, .etokpos, .ea, .ey {
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
!if .isop {
    cpy #.ey
    beq +
    brk
+
}
}

    +test_end
}
!macro test_expect_opcode .tnum, .tokbuf, .tokbufend, .ec, .etokpos, .ea, .ey {
    +test_expect_oppop .tnum, 1, .tokbuf, .tokbufend, .ec, .etokpos, .ea, .ey
}
!macro test_expect_pseudoop .tnum, .tokbuf, .tokbufend, .ec, .etokpos, .ea {
    +test_expect_oppop .tnum, 0, .tokbuf, .tokbufend, .ec, .etokpos, .ea, 0
}

test_expect_oppop_1: !byte 0, $ff
test_expect_oppop_2: !byte 1, 4, 0, 0, $ff
test_expect_oppop_3: !byte po_to, 4, 0, $ff
test_expect_oppop_4: !byte 1, 4, F_ASM_FORCE16, 0, $ff
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
    +assert_cs test_msg_ecs
    ldx tok_pos
    +assert_eq test_msg_wrong_tokpos
} else {
    +assert_cc test_msg_ecc
    lda expr_result
    cmp #<.eresult
    +assert_eq test_msg_wrong_result
    lda expr_result+1
    cmp #>.eresult
    +assert_eq test_msg_wrong_result
    lda expr_result+2
    cmp #^.eresult
    +assert_eq test_msg_wrong_result
    lda expr_result+3
    cmp #<(.eresult >>> 24)
    +assert_eq test_msg_wrong_result
    lda expr_flags
    cmp #.eflags
    +assert_eq test_msg_wrong_flags
}

    +test_end
}

test_expect_literal_1: !byte 0, $ff
test_expect_literal_2: !byte tk_number_literal, 6, $dd, $cc, $bb, $aa, 0, $ff
test_expect_literal_3: !byte tk_number_literal_leading_zero, 6, $dd, $cc, $bb, $aa, 0, $ff
test_expect_literal_end:


!macro test_expect_pms .tnum, .tokbuf, .tokbufend, .ec, .etok, .elen {
    +test_start .tnum

    ldx #.tokbufend-.tokbuf
    dex
-   lda .tokbuf,x
    sta tokbuf,x
    dex
    bpl -

    ldx #0
    stx tok_pos
    jsr expect_pluses_or_minuses

!if .ec {
    +assert_cs test_msg_ecs
    ldx tok_pos
    +assert_eq test_msg_wrong_tokpos
} else {
    +assert_cc test_msg_ecc
    cmp #.etok
    +assert_eq test_msg_wrong_result
    cpy #.elen
    +assert_eq test_msg_wrong_value
}

    +test_end
}

!macro test_expect_p_or_m .tnum, .tokbuf, .tokbufend, .ec, .etok {
    +test_start .tnum

    ldx #.tokbufend-.tokbuf
    dex
-   lda .tokbuf,x
    sta tokbuf,x
    dex
    bpl -

    ldx #0
    stx tok_pos
    jsr expect_single_plus_or_minus

!if .ec {
    +assert_cs test_msg_ecs
    ldx tok_pos
    +assert_eq test_msg_wrong_tokpos
} else {
    +assert_cc test_msg_ecc
    cmp #.etok
    +assert_eq test_msg_wrong_result
}

    +test_end
}

!macro test_expect_m .tnum, .tokbuf, .tokbufend, .ec {
    +test_start .tnum

    ldx #.tokbufend-.tokbuf
    dex
-   lda .tokbuf,x
    sta tokbuf,x
    dex
    bpl -

    ldx #0
    stx tok_pos
    jsr expect_single_minus

!if .ec {
    +assert_cs test_msg_ecs
    ldx tok_pos
    +assert_eq test_msg_wrong_tokpos
} else {
    +assert_cc test_msg_ecc
}

    +test_end
}

test_expect_pms_1: !byte 0, $ff
test_expect_pms_2: !byte tk_pluses, 0, 1, 0, $ff
test_expect_pms_3: !byte tk_minuses, 0, 1, 0, $ff
test_expect_pms_4: !byte tk_pluses, 0, 3, 0, $ff
test_expect_pms_5: !byte tk_minuses, 0, 3, 0, $ff
test_expect_pms_end


!macro start_test_expect_expr .pass {
    jsr init_symbol_table

    ; Fake assembly location in bank 5
    lda #5
    sta bas_ptr+2
    lda #0
    sta bas_ptr+3

    sta err_code
    sta asm_flags

    lda #.pass
    sta pass
    jsr init_pass
}

!macro create_undefined_symbol_for_test .name, .namelength {
    lda #<.name
    sta bas_ptr
    lda #>.name
    sta bas_ptr+1
    ldx #.namelength
    jsr find_or_add_symbol
}

!macro set_symbol_for_test .name, .namelength, .val {
    +create_undefined_symbol_for_test .name, .namelength
    lda #<.val
    ldx #>.val
    ldy #^.val
    ldz #<(.val >>> 24)
    jsr set_symbol_value
}

!macro test_expect_expr .tnum, .tname, .tokbuf, .tokbufend, .lineaddr, .ec, .etokpos, .eresult, .eflags {
    +test_start .tnum

    +print_chr chr_spc
    +print_strlit .tname

    ldx #.tokbufend-.tokbuf
    dex
-   lda .tokbuf,x
    sta tokbuf,x
    dex
    bpl -

    lda #<.lineaddr
    sta line_addr
    lda #>.lineaddr
    sta line_addr+1

    ldx #0
    stx tok_pos
    jsr expect_expr

!if .ec {
    +assert_cs test_msg_ecs
    +assert_mem_eq_byte tok_pos, .etokpos, test_msg_wrong_tokpos
} else {
    +assert_cc test_msg_ecc
    +assert_mem_eq_byte tok_pos, .etokpos, test_msg_wrong_tokpos

    !if .eflags != F_EXPR_UNDEFINED {
        +assert_mem_eq_32 expr_result, .eresult, test_msg_wrong_result
    }

    +assert_mem_eq_byte expr_flags, .eflags, test_msg_wrong_flags
}
    +test_end
}

tee_tb_1: !byte 0, $ff
tee_tb_2: !byte tk_number_literal, 0, $dd, $cc, $bb, $aa, 0, $ff
tee_tb_3: !byte tk_number_literal_leading_zero, 0, $dd, $cc, $bb, $0a, 0, $ff
tee_tb_4: !byte tk_label_or_reg, 0, 5, 0, $ff
tee_tb_5: !byte tk_lparen, 0, tk_label_or_reg, 0, 5, tk_rparen, 6, 0, $ff
tee_tb_6: !byte tk_lbracket, 0, tk_label_or_reg, 0, 5, tk_rbracket, 6, 0, $ff
tee_tb_end:
tee_line_1: !pet "label",0
tee_line_2: !pet "8 div 2",0
tee_line_3: !pet "60 div 5 div 4",0

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

!macro test_tokenize_pluses_and_minuses .tnum, .str, .pos, .ec, .etoken, .epos, .elen {
    +test_start .tnum

    ; Copy .str to strbuf
    ldx #0
-   lda .str,x
    sta strbuf,x
    beq +
    inx
    bra -
+
    lda #0
    sta tok_pos
    ldx #.pos
    stx line_pos
    jsr tokenize_pluses_and_minuses
!if .ec {
    +assert_cs test_msg_ecs
    ldx #0
    lda tokbuf,x
    cmp #.etoken
    +assert_eq test_msg_wrong_result
    inx
    lda tokbuf,x
    cmp #.epos
    +assert_eq test_msg_wrong_value
    inx
    lda tokbuf,x
    cmp #.elen
    +assert_eq test_msg_wrong_value
} else {
    +assert_cc test_msg_ecc
    lda line_pos
    cmp #.pos
    +assert_eq test_msg_wrong_result
}

    +test_end
}

test_tokenize_pluses_and_minuses_1: !pet "+ +++",0
test_tokenize_pluses_and_minuses_2: !pet "- ---",0
test_tokenize_pluses_and_minuses_3: !pet "+++ +++",0
test_tokenize_pluses_and_minuses_4: !pet "--- ---",0
test_tokenize_pluses_and_minuses_5: !pet "-+- +-+",0


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
    +assert_cs test_msg_ecs
    cpx #.etoken
    +assert_eq test_msg_wrong_result
    lda line_pos
    cmp #.epos
    +assert_eq test_msg_wrong_value
} else {
    +assert_cc test_msg_ecc
    lda line_pos
    cmp #.pos
    +assert_eq test_msg_wrong_result
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
    +print_strlit_line "test-expect-keyword"
    +test_expect_keyword $01, test_expect_keyword_1, test_expect_keyword_2, test_expect_keyword_line_1, kw_xor, 1, 0
    +test_expect_keyword $02, test_expect_keyword_2, test_expect_keyword_end, test_expect_keyword_line_1, kw_xor, 0, 3
    +test_expect_keyword $03, test_expect_keyword_2, test_expect_keyword_end, test_expect_keyword_line_2, kw_xor, 0, 3
    +test_expect_keyword $04, test_expect_keyword_2, test_expect_keyword_end, test_expect_keyword_line_3, kw_xor, 1, 0

    +print_chr chr_cr
    +print_strlit_line "test-expect-opcode"
    ; .tnum, .tokbuf, .tokbufend, .ec, .etokpos, .ea
    +test_expect_opcode $01, test_expect_oppop_1, test_expect_oppop_2, 1, 0, 0, 0
    +test_expect_opcode $02, test_expect_oppop_2, test_expect_oppop_3, 0, 3, 1, 0
    +test_expect_opcode $03, test_expect_oppop_3, test_expect_oppop_4, 1, 0, 0, 0
    +test_expect_opcode $04, test_expect_oppop_4, test_expect_oppop_end, 0, 3, 1, F_ASM_FORCE16

    +print_chr chr_cr
    +print_strlit_line "test-expect-pseudoop"
    +test_expect_pseudoop $01, test_expect_oppop_1, test_expect_oppop_2, 1, 0, 0
    +test_expect_pseudoop $02, test_expect_oppop_2, test_expect_oppop_3, 1, 0, 0
    +test_expect_pseudoop $03, test_expect_oppop_3, test_expect_oppop_end, 0, 2, po_to

    +print_chr chr_cr
    +print_strlit_line "test-expect-literal"
    +test_expect_literal $01, test_expect_literal_1, test_expect_literal_2, 1, 0, 0, 0
    +test_expect_literal $02, test_expect_literal_2, test_expect_literal_3, 0, 6, $aabbccdd, 0
    +test_expect_literal $03, test_expect_literal_3, test_expect_literal_end, 0, 6, $aabbccdd, F_EXPR_FORCE16

    +print_chr chr_cr
    +print_strlit_line "test-expect-pluses-or-minuses"
    +test_expect_pms $01, test_expect_pms_1, test_expect_pms_2, 1, 0, 0
    +test_expect_pms $02, test_expect_pms_2, test_expect_pms_3, 0, tk_pluses, 1
    +test_expect_pms $03, test_expect_pms_3, test_expect_pms_4, 0, tk_minuses, 1
    +test_expect_p_or_m $04, test_expect_pms_2, test_expect_pms_3, 0, tk_pluses
    +test_expect_p_or_m $05, test_expect_pms_3, test_expect_pms_4, 0, tk_minuses
    +test_expect_p_or_m $06, test_expect_pms_4, test_expect_pms_5, 1, 0
    +test_expect_p_or_m $07, test_expect_pms_5, test_expect_pms_end, 1, 0
    +test_expect_m $08, test_expect_pms_1, test_expect_pms_2, 1
    +test_expect_m $09, test_expect_pms_2, test_expect_pms_3, 1
    +test_expect_m $0A, test_expect_pms_3, test_expect_pms_4, 0
    +test_expect_m $0B, test_expect_pms_4, test_expect_pms_5, 1
    +test_expect_m $0C, test_expect_pms_5, test_expect_pms_end, 1

    ; -----------------------------------

    +print_chr chr_cr
    +print_strlit_line "test-expr"

    +start_test_expect_expr 0
    +test_expect_expr $01, "empty", tee_tb_1, tee_tb_2, tee_line_1, 1, 0, 0, 0
    +test_expect_expr $02, "literal", tee_tb_2, tee_tb_3, tee_line_1, 0, 6, $aabbccdd, 0
    +test_expect_expr $03, "literal w zero", tee_tb_3, tee_tb_4, tee_line_1, 0, 6, $0abbccdd, F_EXPR_FORCE16

    +start_test_expect_expr 0
    +test_expect_expr $04, "label undef", tee_tb_4, tee_tb_5, tee_line_1, 0, 3, 0, F_EXPR_UNDEFINED

    +start_test_expect_expr 0
    +create_undefined_symbol_for_test tee_line_1, 5
    +test_expect_expr $05, "label undef in tbl", tee_tb_4, tee_tb_5, tee_line_1, 0, 3, 0, F_EXPR_UNDEFINED

    +start_test_expect_expr 0
    +set_symbol_for_test tee_line_1, 5, 98765
    +test_expect_expr $06, "label def", tee_tb_4, tee_tb_5, tee_line_1, 0, 3, 98765, 0

    +start_test_expect_expr 0
    +set_symbol_for_test tee_line_1, 5, 98765
    +test_expect_expr $07, "label def parens", tee_tb_5, tee_tb_6, tee_line_1, 0, 7, 98765, F_EXPR_BRACKET_PAREN

    +start_test_expect_expr 0
    +set_symbol_for_test tee_line_1, 5, 98765
    +test_expect_expr $08, "label def brackets", tee_tb_6, tee_tb_end, tee_line_1, 0, 7, 98765, F_EXPR_BRACKET_SQUARE

    +start_test_expect_expr $ff
    +test_expect_expr $09, "label undef last pass", tee_tb_4, tee_tb_5, tee_line_1, 1, 0, 0, F_EXPR_UNDEFINED
    lda err_code
    cmp #err_undefined
    beq +
    +print_strlit_line "... fail: did not return undefined error"
    brk
+

    +start_test_expect_expr $ff
    +set_symbol_for_test tee_line_1, 5, 98765
    +test_expect_expr $0A, "label def last pass", tee_tb_4, tee_tb_5, tee_line_1, 0, 3, 98765, 0


    ; Continued in test_suite_10

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
    +print_strlit_line "tokenize-pluses-and-minuses"
    ; .tnum, .str, .pos, .ec, .etoken, .epos, .elen
    +test_tokenize_pluses_and_minuses $01, test_tokenize_pluses_and_minuses_1, 0, 1, tk_pluses, 0, 1
    +test_tokenize_pluses_and_minuses $02, test_tokenize_pluses_and_minuses_2, 0, 1, tk_minuses, 0, 1
    +test_tokenize_pluses_and_minuses $03, test_tokenize_pluses_and_minuses_3, 0, 1, tk_pluses, 0, 3
    +test_tokenize_pluses_and_minuses $04, test_tokenize_pluses_and_minuses_4, 0, 1, tk_minuses, 0, 3
    +test_tokenize_pluses_and_minuses $05, test_tokenize_pluses_and_minuses_5, 0, 1, tk_minuses, 0, 1

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
    ;+test_load_line_to_strbuf $02, test_load_line_to_strbuf_1, test_load_line_to_strbuf_1e


    ; -----------------------------------

    +print_chr chr_cr
    +print_strlit_line "-- all tests passed --"
    rts
