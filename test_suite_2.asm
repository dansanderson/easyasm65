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
tee_tb_7: !byte tk_complement, 0, tk_number_literal, 2, $dd, $cc, $bb, $aa, 0, $ff
tee_tb_8: !byte tk_complement, 0, tk_lparen, 2, tk_number_literal, 4, $dd, $cc, $bb, $aa, tk_rparen, 8, 0, $ff
tee_tb_9: !byte tk_complement, 0, tk_complement, 1, tk_lparen, 2, tk_number_literal, 3, $dd, $cc, $bb, $aa, tk_rparen, 10, 0, $ff
tee_tb_10: !byte tk_number_literal, 0, $02, $00, $00, $00, tk_power, 1, tk_number_literal, 2, $03, $00, $00, $00, 0, $ff
tee_tb_11: !byte tk_number_literal, 0, $02, $00, $00, $00, tk_power, 1, tk_number_literal, 2, $01, $00, $00, $00, tk_power, 3, tk_number_literal, 4, $03, $00, $00, $00, 0, $ff
tee_tb_12: !byte tk_number_literal, 0, $02, $00, $00, $00, tk_power, 1, tk_number_literal, 2, $00, $00, $00, $00, 0, $ff
tee_tb_13: !byte tk_lparen, 0, tk_number_literal, 1, $02, $00, $00, $00, tk_power, 2, tk_number_literal, 3, $01, $00, $00, $00, tk_rparen, 4, tk_power, 5, tk_number_literal, 6, $03, $00, $00, $00, 0, $ff
tee_tb_14: !byte tk_number_literal, 0, $02, $00, $00, $00, tk_power, 1, tk_number_literal, 2, $fd, $ff, $ff, $ff, 0, $ff
tee_tb_15: !byte tk_minus, 0, tk_number_literal, 0, $02, $00, $00, $00, 0, $ff
tee_tb_end:
tee_line_1: !pet "label",0
tee_line_2: !pet "8 div 2",0
tee_line_3: !pet "60 div 5 div 4",0


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
    +test_expect_expr $08, "label def brackets", tee_tb_6, tee_tb_7, tee_line_1, 0, 7, 98765, F_EXPR_BRACKET_SQUARE

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

    +start_test_expect_expr 0
    +test_expect_expr $0B, "inversion", tee_tb_7, tee_tb_8, tee_line_1, 0, 8, !$aabbccdd, 0
    +test_expect_expr $0C, "inversion paren", tee_tb_8, tee_tb_9, tee_line_1, 0, 12, !$aabbccdd, 0
    +test_expect_expr $0D, "double inversion paren", tee_tb_9, tee_tb_10, tee_line_1, 0, 14, !!$aabbccdd, 0
    +test_expect_expr $0E, "one exponent", tee_tb_10, tee_tb_11, tee_line_1, 0, 14, 2^3, 0
    +test_expect_expr $0F, "two exponents", tee_tb_11, tee_tb_12, tee_line_1, 0, 22, 2^1^3, 0
    +test_expect_expr $10, "exponent of zero", tee_tb_12, tee_tb_13, tee_line_1, 0, 14, 2^0, 0
    +test_expect_expr $11, "grouping to a power", tee_tb_13, tee_tb_14, tee_line_1, 0, 26, (2^1)^3, 0
    +test_expect_expr $12, "negative exponent", tee_tb_14, tee_tb_15, tee_line_1, 1, 14, 0, 0
    +test_expect_expr $13, "negate", tee_tb_15, tee_tb_end, tee_line_1, 0, 8, -2, 0

    ; Continued in test_suite_10

    ; -----------------------------------

    +print_chr chr_cr
    +print_strlit_line "-- all tests passed --"
    rts
