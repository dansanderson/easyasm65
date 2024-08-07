!macro start_test_expect_expr {
    jsr init_symbol_table

    ; Fake assembly location in bank 5
    lda #5
    sta bas_ptr+2
    lda #0
    sta bas_ptr+3
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
    bcs +
    +print_strlit_line "... fail, expected carry set"
    brk
+   ldx tok_pos
    beq +
    +print_strlit_line "... fail, expected tok-pos zero"
    brk
+
} else {
    bcc +
    +print_strlit_line "... fail, expected carry clear"
    brk
+   lda tok_pos
    cmp #.etokpos
    beq +
    +print_strlit_line "... fail, wrong tokpoad (a)"
    brk
+
    !if .eflags != F_EXPR_UNDEFINED {
    lda expr_result
    cmp #<.eresult
    beq +
    +print_strlit_line "... fail, wrong result (0; a)"
    brk
+   lda expr_result+1
    cmp #>.eresult
    beq +
    +print_strlit_line "... fail, wrong result (1; a)"
    brk
+   lda expr_result+2
    cmp #^.eresult
    beq +
    +print_strlit_line "... fail, wrong result (2; a)"
    brk
+   lda expr_result+3
    cmp #<(.eresult >>> 24)
    beq +
    +print_strlit_line "... fail, wrong result (3; a)"
    brk
+
    }
    lda expr_flags
    cmp #.eflags
    beq +
    +print_strlit_line "... fail, wrong expr flags (a)"
    brk
+
}
    +test_end
}

test_expect_expr_tb_1: !byte 0, $ff
test_expect_expr_tb_2: !byte tk_number_literal, 0, $dd, $cc, $bb, $aa, 0, $ff
test_expect_expr_tb_3: !byte tk_number_literal_leading_zero, 0, $dd, $cc, $bb, $0a, 0, $ff
test_expect_expr_tb_4: !byte tk_label_or_reg, 0, 5, 0, $ff
test_expect_expr_tb_end:
test_expect_expr_line_1: !pet "label",0

run_test_suite_cmd:
    +print_strlit_line "-- test suite --"

    +print_chr chr_cr
    +print_strlit_line "test-expr"

    +start_test_expect_expr
    +test_expect_expr $01, "empty", test_expect_expr_tb_1, test_expect_expr_tb_2, test_expect_expr_line_1, 1, 0, 0, 0
    +test_expect_expr $02, "literal", test_expect_expr_tb_2, test_expect_expr_tb_3, test_expect_expr_line_1, 0, 6, $aabbccdd, 0
    +test_expect_expr $03, "literal w zero", test_expect_expr_tb_3, test_expect_expr_tb_4, test_expect_expr_line_1, 0, 6, $0abbccdd, F_EXPR_BRACKET_ZERO

    +start_test_expect_expr
    +test_expect_expr $04, "label undef", test_expect_expr_tb_4, test_expect_expr_tb_end, test_expect_expr_line_1, 0, 3, 0, F_EXPR_UNDEFINED

    +start_test_expect_expr
    +create_undefined_symbol_for_test test_expect_expr_line_1, 5
    +test_expect_expr $05, "label undef in tbl", test_expect_expr_tb_4, test_expect_expr_tb_end, test_expect_expr_line_1, 0, 3, 0, F_EXPR_UNDEFINED

    +start_test_expect_expr
    +set_symbol_for_test test_expect_expr_line_1, 5, 98765
    +test_expect_expr $06, "label def", test_expect_expr_tb_4, test_expect_expr_tb_end, test_expect_expr_line_1, 0, 3, 98765, 0

    +print_chr chr_cr
    +print_strlit_line "-- all tests passed --"
    rts
