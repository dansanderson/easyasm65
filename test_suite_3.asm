!macro test_expect_addressing_expr .tnum, .str, .ec, .emode, .eresult, .eflags, .etokpos, .eerror, .eerror_pos {
    +test_start .tnum

    ; TODO: add tests for forced16 and forced8 modes
    jsr init_forced16
    lda asm_flags
    and #!(F_ASM_FORCE_MASK | F_ASM_AREL_MASK)
    sta asm_flags

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
    +print_fail_line test_msg_tokenize
    brk
+
    ldz #3
    stz tok_pos
    jsr expect_addressing_expr
!if .ec {
    +assert_cs test_msg_ecs
    +assert_mem_eq_byte err_code, .eerror, test_msg_wrong_err_code
    +assert_mem_eq_byte line_pos, .eerror_pos, test_msg_wrong_err_pos
} else {
    +assert_cc test_msg_ecc
    cpx #<.emode
    +assert_eq test_msg_wrong_mode
    cpy #>.emode
    +assert_eq test_msg_wrong_mode
    +assert_mem_eq_32 expr_result, .eresult, test_msg_wrong_result
    +assert_mem_eq_byte expr_flags, .eflags, test_msg_wrong_flags
    +assert_mem_eq_byte tok_pos, .etokpos, test_msg_wrong_tokpos
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
test_expect_addressing_expr_35: !pet "inx iny",0
; Other errors
test_expect_addressing_expr_36: !pet "lda [$fffe],z",0  ; err_value_out_of_range


run_test_suite_cmd:
    +print_strlit_line "-- test suite --"

    +print_chr chr_cr
    +print_strlit_line "test-expect-addressing-expr"
    ; .tnum, .str, .ec, .emode, .eresult, .eflags, .etokpos, .eerror, .eerror_pos
    +test_expect_addressing_expr $01, test_expect_addressing_expr_1, 0, MODE_IMPLIED, 0, 0, 3, 0, 0
    +test_expect_addressing_expr $02, test_expect_addressing_expr_2, 0, MODE_IMPLIED, 0, 0, 3, 0, 0
    +test_expect_addressing_expr $03, test_expect_addressing_expr_3, 0, MODE_IMMEDIATE, 13, 0, 11, 0, 0
    ; TODO: rewrite phw test to set F16IMM prior to call
    ;+test_expect_addressing_expr $04, test_expect_addressing_expr_4, 0, MODE_IMMEDIATE_WORD, $aabb, 0, 11, 0, 0
    +test_expect_addressing_expr $05, test_expect_addressing_expr_5, 0, MODE_BASE_PAGE, $fe, 0, 9, 0, 0
    +test_expect_addressing_expr $06, test_expect_addressing_expr_6, 0, MODE_BASE_PAGE_X, $fe, 0, 14, 0, 0
    +test_expect_addressing_expr $07, test_expect_addressing_expr_7, 0, MODE_BASE_PAGE_Y, $fe, 0, 14, 0, 0
    +test_expect_addressing_expr $08, test_expect_addressing_expr_8, 0, MODE_ABSOLUTE, $d020, 0, 9, 0, 0
    +test_expect_addressing_expr $09, test_expect_addressing_expr_9, 0, MODE_ABSOLUTE_X, $d020, 0, 14, 0, 0
    +test_expect_addressing_expr $0a, test_expect_addressing_expr_10, 0, MODE_ABSOLUTE_Y, $d020, 0, 14, 0, 0
    +test_expect_addressing_expr $0b, test_expect_addressing_expr_11, 0, MODE_ABSOLUTE_X, $fe, F_EXPR_FORCE16, 14, 0, 0
    +test_expect_addressing_expr $0c, test_expect_addressing_expr_12, 0, MODE_ABSOLUTE_IND, $fffe, F_EXPR_BRACKET_PAREN, 13, 0, 0
    +test_expect_addressing_expr $0d, test_expect_addressing_expr_13, 0, MODE_ABSOLUTE_IND_X, $c000, 0, 18, 0, 0
    +test_expect_addressing_expr $0e, test_expect_addressing_expr_14, 0, MODE_BASE_PAGE_IND_X, $fe, 0, 18, 0, 0
    +test_expect_addressing_expr $0f, test_expect_addressing_expr_15, 0, MODE_BASE_PAGE_IND_Y, $fe, F_EXPR_BRACKET_PAREN, 18, 0, 0
    +test_expect_addressing_expr $10, test_expect_addressing_expr_16, 0, MODE_BASE_PAGE_IND_Z, $fe, F_EXPR_BRACKET_PAREN, 18, 0, 0
    +test_expect_addressing_expr $11, test_expect_addressing_expr_17, 0, MODE_32BIT_IND, $fe, F_EXPR_BRACKET_SQUARE, 18, 0, 0
    +test_expect_addressing_expr $12, test_expect_addressing_expr_18, 0, MODE_32BIT_IND, $fe, F_EXPR_BRACKET_SQUARE, 13, 0, 0
    +test_expect_addressing_expr $13, test_expect_addressing_expr_19, 0, MODE_STACK_REL, 4, 0, 23, 0, 0
    +test_expect_addressing_expr $14, test_expect_addressing_expr_20, 0, MODE_STACK_REL, 4, 0, 23, 0, 0
    +test_expect_addressing_expr $15, test_expect_addressing_expr_21, 1, 0, 0, 0, 0, err_syntax, 6+4
    ; TODO: this test isn't tokenizing correctly? can't repro in real life
    ; +test_expect_addressing_expr $16, test_expect_addressing_expr_22, 1, 0, 0, 0, 0, err_syntax, 8+4
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
    +test_expect_addressing_expr $23, test_expect_addressing_expr_35, 1, 0, 0, 0, 0, err_syntax, 4+4
    +test_expect_addressing_expr $24, test_expect_addressing_expr_36, 1, 0, 0, 0, 0, err_value_out_of_range, 5+4

    +print_chr chr_cr
    +print_strlit_line "-- all tests passed --"
    rts
