!macro test_expect_addressing_expr .tnum, .str, .ec, .emode, .eresult, .eflags, .etokpos, .eerror, .eerror_pos {
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
    lda err_code
    beq +
    +print_strlit_line "...test error: tokenize failed"
    brk
+
    ldz #2
    stz tok_pos
    jsr expect_addressing_expr
!if .ec {
    bcs +
    +print_strlit_line "...fail: expected carry set"
    brk
+   lda err_code
    cmp #.eerror
    beq +
    +print_strlit_line "...fail: wrong error code"
    brk
+   lda line_pos
    cmp #.eerror_pos
    beq +
    +print_strlit_line "...fail: wrong error pos"
    brk
+
} else {
    bcc +
    +print_strlit_line "...fail: expected carry clear"
    brk
+   cpx #<.emode
    beq +
    +print_strlit_line "...fail: wrong mode (x)"
    brk
+   cpy #>.emode
    beq +
    +print_strlit_line "...fail: wrong mode (y)"
    brk
+   lda expr_result
    cmp #<.eresult
    beq +
    +print_strlit_line "...fail: wrong result (0)"
    brk
+   lda expr_result+1
    cmp #>.eresult
    beq +
    +print_strlit_line "...fail: wrong result (1)"
    brk
+   lda expr_result+2
    cmp #^.eresult
    beq +
    +print_strlit_line "...fail: wrong result (2)"
    brk
+   lda expr_result+3
    cmp #<(.eresult >>> 24)
    beq +
    +print_strlit_line "...fail: wrong result (3)"
    brk
+   lda expr_flags
    cmp #.eflags
    beq +
    +print_strlit_line "...fail: wrong flags"
    brk
+   lda tok_pos
    cmp #.etokpos
    beq +
    +print_strlit_line "...fail: wrong tokpos"
    brk
+
}
    +test_end
}

; Valid addressing mode expressions
test_expect_addressing_expr_1: !pet "inx",0
test_expect_addressing_expr_2: !pet "inx : iny",0
test_expect_addressing_expr_3: !pet "lda #13",0
test_expect_addressing_expr_4: !pet "phw #$aabb",0
test_expect_addressing_expr_5: !pet "lda $fe",0
test_expect_addressing_expr_6: !pet "lda $fe,x",0
test_expect_addressing_expr_7: !pet "lda $fe,y",0
test_expect_addressing_expr_8: !pet "lda $d020",0
test_expect_addressing_expr_9: !pet "lda $d020,x",0
test_expect_addressing_expr_10: !pet "lda $d020,y",0
test_expect_addressing_expr_11: !pet "lda $00fe,x",0
test_expect_addressing_expr_12: !pet "jmp ($fffe)",0
test_expect_addressing_expr_13: !pet "lda ($c000,x)",0
test_expect_addressing_expr_14: !pet "lda ($fe,x)",0
test_expect_addressing_expr_15: !pet "lda ($fe),y",0
test_expect_addressing_expr_16: !pet "lda ($fe),z",0
test_expect_addressing_expr_17: !pet "lda [$fe],z",0
test_expect_addressing_expr_18: !pet "lda [$fe]",0
test_expect_addressing_expr_19: !pet "lda (4,sp),y",0
test_expect_addressing_expr_20: !pet "lda (4,sP),Y",0
; Syntax errors
test_expect_addressing_expr_21: !pet "lda # : inx",0
test_expect_addressing_expr_22: !pet "lda $fe,w",0
test_expect_addressing_expr_23: !pet "lda $fffe,w",0
test_expect_addressing_expr_24: !pet "lda ($fe),w",0
test_expect_addressing_expr_25: !pet "lda [$fe],w",0
test_expect_addressing_expr_26: !pet "lda lda",0
test_expect_addressing_expr_27: !pet "lda (lda,x)",0
test_expect_addressing_expr_28: !pet "lda ($fe x)",0
test_expect_addressing_expr_29: !pet "lda ($fe,y)",0
test_expect_addressing_expr_30: !pet "lda ($fe,x",0
test_expect_addressing_expr_31: !pet "lda (4,sp",0
test_expect_addressing_expr_32: !pet "lda (4,sp)",0
test_expect_addressing_expr_33: !pet "lda (4,sp),",0
test_expect_addressing_expr_34: !pet "lda (4,sp),x",0
; Other errors
test_expect_addressing_expr_35: !pet "lda [$fffe],z",0  ; err_value_out_of_range


run_test_suite_cmd:
    +print_strlit_line "-- test suite --"

    +print_chr chr_cr
    +print_strlit_line "test-expect-addressing-expr"
    ; .tnum, .str, .ec, .emode, .eresult, .eflags, .etokpos, .eerror, .eerror_pos
    +test_expect_addressing_expr $01, test_expect_addressing_expr_1, 0, MODE_IMPLIED, 0, 0, 2, 0, 0
    +test_expect_addressing_expr $02, test_expect_addressing_expr_2, 0, MODE_IMPLIED, 0, 0, 2, 0, 0
    +test_expect_addressing_expr $03, test_expect_addressing_expr_3, 0, MODE_IMMEDIATE, 13, 0, 10, 0, 0
    +test_expect_addressing_expr $04, test_expect_addressing_expr_4, 0, MODE_IMMEDIATE_WORD, $aabb, 0, 10, 0, 0
    +test_expect_addressing_expr $05, test_expect_addressing_expr_5, 0, MODE_BASE_PAGE, $fe, 0, 8, 0, 0
    +test_expect_addressing_expr $06, test_expect_addressing_expr_6, 0, MODE_BASE_PAGE_X, $fe, 0, 13, 0, 0
    +test_expect_addressing_expr $07, test_expect_addressing_expr_7, 0, MODE_BASE_PAGE_Y, $fe, 0, 13, 0, 0
    +test_expect_addressing_expr $08, test_expect_addressing_expr_8, 0, MODE_ABSOLUTE, $d020, 0, 8, 0, 0
    +test_expect_addressing_expr $09, test_expect_addressing_expr_9, 0, MODE_ABSOLUTE_X, $d020, 0, 13, 0, 0
    +test_expect_addressing_expr $0a, test_expect_addressing_expr_10, 0, MODE_ABSOLUTE_Y, $d020, 0, 13, 0, 0
    +test_expect_addressing_expr $0b, test_expect_addressing_expr_11, 0, MODE_ABSOLUTE_X, $fe, F_EXPR_BRACKET_ZERO, 13, 0, 0
    +test_expect_addressing_expr $0c, test_expect_addressing_expr_12, 0, MODE_ABSOLUTE_IND, $fffe, F_EXPR_BRACKET_PAREN, 12, 0, 0
    +test_expect_addressing_expr $0d, test_expect_addressing_expr_13, 0, MODE_ABSOLUTE_IND_X, $c000, 0, 17, 0, 0
    +test_expect_addressing_expr $0e, test_expect_addressing_expr_14, 0, MODE_BASE_PAGE_IND_X, $fe, 0, 17, 0, 0
    +test_expect_addressing_expr $0f, test_expect_addressing_expr_15, 0, MODE_BASE_PAGE_IND_Y, $fe, F_EXPR_BRACKET_PAREN, 17, 0, 0
    +test_expect_addressing_expr $10, test_expect_addressing_expr_16, 0, MODE_BASE_PAGE_IND_Z, $fe, F_EXPR_BRACKET_PAREN, 17, 0, 0
    +test_expect_addressing_expr $11, test_expect_addressing_expr_17, 0, MODE_32BIT_IND, $fe, F_EXPR_BRACKET_SQUARE, 17, 0, 0
    +test_expect_addressing_expr $12, test_expect_addressing_expr_18, 0, MODE_32BIT_IND, $fe, F_EXPR_BRACKET_SQUARE, 12, 0, 0
    +test_expect_addressing_expr $13, test_expect_addressing_expr_19, 0, MODE_STACK_REL, 4, 0, 22, 0, 0
    +test_expect_addressing_expr $14, test_expect_addressing_expr_20, 0, MODE_STACK_REL, 4, 0, 22, 0, 0
    +test_expect_addressing_expr $15, test_expect_addressing_expr_21, 1, 0, 0, 0, 0, err_syntax, 6+4
    +test_expect_addressing_expr $16, test_expect_addressing_expr_22, 1, 0, 0, 0, 0, err_syntax, 8+4
    +test_expect_addressing_expr $17, test_expect_addressing_expr_23, 1, 0, 0, 0, 0, err_syntax, 10+4
    +test_expect_addressing_expr $18, test_expect_addressing_expr_24, 1, 0, 0, 0, 0, err_syntax, 10+4
    +test_expect_addressing_expr $19, test_expect_addressing_expr_25, 1, 0, 0, 0, 0, err_syntax, 10+4
    +test_expect_addressing_expr $1a, test_expect_addressing_expr_26, 1, 0, 0, 0, 0, err_syntax, 4+4
    +test_expect_addressing_expr $1b, test_expect_addressing_expr_27, 1, 0, 0, 0, 0, err_syntax, 5+4
    +test_expect_addressing_expr $1c, test_expect_addressing_expr_28, 1, 0, 0, 0, 0, err_syntax, 9+4
    +test_expect_addressing_expr $1d, test_expect_addressing_expr_29, 1, 0, 0, 0, 0, err_syntax, 9+4
    +test_expect_addressing_expr $1e, test_expect_addressing_expr_30, 1, 0, 0, 0, 0, err_syntax, $ff ; end of line
    +test_expect_addressing_expr $1f, test_expect_addressing_expr_31, 1, 0, 0, 0, 0, err_syntax, $ff
    +test_expect_addressing_expr $20, test_expect_addressing_expr_32, 1, 0, 0, 0, 0, err_syntax, $ff
    +test_expect_addressing_expr $21, test_expect_addressing_expr_33, 1, 0, 0, 0, 0, err_syntax, $ff
    +test_expect_addressing_expr $22, test_expect_addressing_expr_34, 1, 0, 0, 0, 0, err_syntax, 11+4
    +test_expect_addressing_expr $23, test_expect_addressing_expr_35, 1, 0, 0, 0, 0, err_value_out_of_range, 5+4

    +print_chr chr_cr
    +print_strlit_line "-- all tests passed --"
    rts
