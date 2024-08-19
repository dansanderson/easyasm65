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

tee_tb_16: !byte tk_number_literal, 0, $02, $00, $00, $00, tk_multiply, 1, tk_number_literal, 2, $03, $00, $00, $00, 0, $ff
tee_tb_17: !byte tk_number_literal, 0, $08, $00, $00, $00, tk_label_or_reg, 2, 3, tk_number_literal, 6, $02, $00, $00, $00, 0, $ff
tee_tb_18: !byte tk_number_literal, 0, $09, $00, $00, $00, tk_label_or_reg, 2, 3, tk_number_literal, 6, $04, $00, $00, $00, 0, $ff
tee_tb_19: !byte tk_number_literal, 0, $09, $00, $00, $00, tk_remainder, 1, tk_number_literal, 2, $04, $00, $00, $00, 0, $ff
tee_tb_20: !byte tk_number_literal, 0, $09, $00, $00, $00, tk_fraction, 1, tk_number_literal, 2, $04, $00, $00, $00, 0, $ff
tee_tb_21: !byte tk_number_literal, 0, $3c, $00, $00, $00, tk_label_or_reg, 3, 3, tk_number_literal, 7, $05, $00, $00, $00, tk_label_or_reg, 9, 3, tk_number_literal, 13, $04, $00, $00, $00, 0, $ff
tee_tb_22: !byte tk_number_literal, 0, $02, $00, $00, $00, tk_plus, 1, tk_number_literal, 2, $03, $00, $00, $00, 0, $ff
tee_tb_23: !byte tk_number_literal, 0, $02, $00, $00, $00, tk_minus, 1, tk_number_literal, 2, $03, $00, $00, $00, 0, $ff
tee_tb_24: !byte tk_number_literal, 0, $02, $00, $00, $00, tk_plus, 1, tk_number_literal, 2, $03, $00, $00, $00, tk_minus, 1, tk_number_literal, 2, $01, $00, $00, $00, 0, $ff
tee_tb_25: !byte tk_number_literal, 0, $03, $00, $00, $00, tk_asl, 1, tk_number_literal, 2, $05, $00, $00, $00, 0, $ff
tee_tb_26: !byte tk_number_literal, 0, $0c, $00, $00, $00, tk_asr, 1, tk_number_literal, 2, $03, $00, $00, $00, 0, $ff
tee_tb_27: !byte tk_number_literal, 0, $f4, $ff, $ff, $ff, tk_asr, 1, tk_number_literal, 2, $03, $00, $00, $00, 0, $ff
tee_tb_28: !byte tk_number_literal, 0, $0c, $00, $00, $00, tk_lsr, 1, tk_number_literal, 2, $03, $00, $00, $00, 0, $ff
tee_tb_29: !byte tk_number_literal, 0, $f4, $ff, $ff, $ff, tk_lsr, 1, tk_number_literal, 2, $03, $00, $00, $00, 0, $ff
tee_tb_30: !byte tk_number_literal, 0, $01, $00, $00, $00, tk_asl, 1, tk_number_literal, 2, $02, $00, $00, $00, tk_asl, 1, tk_number_literal, 2, $03, $00, $00, $00, 0, $ff
tee_tb_end:
tee_line_1: !pet "label",0
tee_line_2: !pet "8 div 2",0
tee_line_3: !pet "60 div 5 div 4",0


!macro start_test_rellabel {
    jsr init_rellabel_table
}

!macro add_rellabel_for_test .plusminus, .length, .pc {
    !if .plusminus = 1 {
        sec
    } else {
        clc
    }
    lda #.length
    ldx #<.pc
    ldy #>.pc
    jsr add_rellabel
    +assert_cc test_msg_ecc
}

!macro test_add_rellabel .tnum, .ebytes_start, .ebytes_end {
    +test_start .tnum

    jsr start_rellabel_table
    ldx #.ebytes_end-.ebytes_start-1
-   txa
    taz
    lda [attic_ptr],z
    cmp .ebytes_start,x
    +assert_eq test_msg_wrong_result
    dex
    bpl -

    +test_end
}

!macro test_eval_rellabel .tnum, .plusminus, .length, .pc, .ec, .epc {
    +test_start .tnum

    !if .plusminus = 1 {
        sec
    } else {
        clc
    }
    lda #.length
    ldx #<.pc
    ldy #>.pc
    jsr eval_rellabel

    !if .ec {
        +assert_cs test_msg_ecs
        cpx #<.epc
        +assert_eq test_msg_wrong_pc
        cpy #>.epc
        +assert_eq test_msg_wrong_pc
    } else {
        +assert_cc test_msg_ecc
    }

    +test_end
}

test_add_rellabel_1: !byte $00, $00, $00, $00, $00, $00
test_add_rellabel_2: !byte $00, $00, $00, $83, $00, $16, $00, $00, $00
test_add_rellabel_3: !byte $00, $00, $00, $83, $00, $16, $02, $04, $16, $81, $08, $16, $00, $00, $00
test_add_rellabel_end:


run_test_suite_cmd:
    +print_strlit_line "-- test suite --"

    +print_chr chr_cr
    +print_strlit_line "test-expr"

    +start_test_expect_expr 0

    ;+test_expect_expr $14, "product", tee_tb_16, tee_tb_17, tee_line_1, 0, 14, 2 * 3, 0
    +test_expect_expr $15, "integer div", tee_tb_17, tee_tb_18, tee_line_2, 0, 15, 8 div 2, 0
    +test_expect_expr $16, "integer div 2", tee_tb_18, tee_tb_19, tee_line_2, 0, 15, 9 div 4, 0
    +test_expect_expr $17, "remainder", tee_tb_19, tee_tb_20, tee_line_1, 0, 14, 9 % 4, 0
    +test_expect_expr $18, "fraction error", tee_tb_20, tee_tb_21, tee_line_1, 1, 8, 0, 0
    +test_expect_expr $19, "multiple div", tee_tb_21, tee_tb_22, tee_line_3, 0, 24, 60 div 5 div 4, 0
    +test_expect_expr $1A, "plus", tee_tb_22, tee_tb_23, tee_line_3, 0, 14, 2 + 3, 0
    +test_expect_expr $1B, "minus", tee_tb_23, tee_tb_24, tee_line_3, 0, 14, 2 - 3, 0
    +test_expect_expr $1C, "multiple plus", tee_tb_24, tee_tb_25, tee_line_3, 0, 22, 2 + 3 - 1, 0

    +test_expect_expr $1D, "asl", tee_tb_25, tee_tb_26, tee_line_3, 0, 14, 3 << 5, 0
    +test_expect_expr $1E, "asr positive", tee_tb_26, tee_tb_27, tee_line_3, 0, 14, 12 >> 3, 0
    +test_expect_expr $1F, "asr negative", tee_tb_27, tee_tb_28, tee_line_3, 0, 14, -12 >> 3, 0
    +test_expect_expr $20, "lsr positive", tee_tb_28, tee_tb_29, tee_line_3, 0, 14, 12 >>> 3, 0
    ; (Acme's own integer width is platform dependent (C int) and likely
    ; 64-bit, so we can't ask Acme to calculate -12 <<< 3.)
    +test_expect_expr $21, "lsr negative", tee_tb_29, tee_tb_30, tee_line_3, 0, 14, $1ffffffe, 0
    +test_expect_expr $22, "multiple asl", tee_tb_30, tee_tb_end, tee_line_3, 0, 22, 1 << 2 << 3, 0

    +print_chr chr_cr
    +print_strlit_line "test rellabels"

    +start_test_rellabel
    +test_add_rellabel $01, test_add_rellabel_1, test_add_rellabel_2

    +start_test_rellabel
    +add_rellabel_for_test 1, 3, $1600
    +test_add_rellabel $02, test_add_rellabel_2, test_add_rellabel_3

    +start_test_rellabel
    +add_rellabel_for_test 1, 3, $1600
    +add_rellabel_for_test 0, 2, $1604
    +add_rellabel_for_test 1, 1, $1608
    +test_add_rellabel $03, test_add_rellabel_3, test_add_rellabel_end

    +start_test_rellabel
    ; test_eval_rellabel $04, .plusminus, .length, .pc, .ec, .epc
    +test_eval_rellabel $04, 0, 1, $1600, 0, 0
    +test_eval_rellabel $05, 1, 1, $1600, 0, 0

    ; one entry, minus, same pc (yes)
    +start_test_rellabel
    +add_rellabel_for_test 1, 1, $1600
    +test_eval_rellabel $06, 1, 1, $1600, 1, $1600

    ; one entry, plus,  same pc (no)
    +start_test_rellabel
    +add_rellabel_for_test 0, 1, $1600
    +test_eval_rellabel $07, 0, 1, $1600, 0, 0

    ; one entry, minus, pc+1    (no)
    +start_test_rellabel
    +add_rellabel_for_test 1, 1, $1601
    +test_eval_rellabel $08, 1, 1, $1600, 0, 0

    ; one entry, plus,  pc+1    (yes)
    +start_test_rellabel
    +add_rellabel_for_test 0, 1, $1601
    +test_eval_rellabel $09, 0, 1, $1600, 1, $1601

    ; one entry, minus, pc-1    (yes)
    +start_test_rellabel
    +add_rellabel_for_test 1, 1, $15ff
    +test_eval_rellabel $0A, 1, 1, $1600, 1, $15ff

    ; one entry, plus,  pc-1    (no)
    +start_test_rellabel
    +add_rellabel_for_test 0, 1, $15ff
    +test_eval_rellabel $0B, 0, 1, $1600, 0, 0

    ; three entries, two match, minus...
    +start_test_rellabel
    +add_rellabel_for_test 1, 1, $15fe
    +add_rellabel_for_test 1, 1, $15ff
    +add_rellabel_for_test 1, 1, $1601
    +test_eval_rellabel $0C, 1, 1, $1600, 1, $15ff

    ; three entries, two match, plus...
    +start_test_rellabel
    +add_rellabel_for_test 0, 1, $1600
    +add_rellabel_for_test 0, 1, $1601
    +add_rellabel_for_test 0, 1, $1602
    +test_eval_rellabel $0D, 0, 1, $1600, 1, $1601

    ; three entries, zero matches, minus...
    +start_test_rellabel
    +add_rellabel_for_test 1, 2, $15fe
    +add_rellabel_for_test 1, 3, $15ff
    +add_rellabel_for_test 1, 1, $1601
    +test_eval_rellabel $0E, 1, 1, $1600, 0, 0

    ; three entries, zero matches, plus...
    +start_test_rellabel
    +add_rellabel_for_test 0, 1, $1600
    +add_rellabel_for_test 0, 2, $1601
    +add_rellabel_for_test 0, 3, $1602
    +test_eval_rellabel $0F, 0, 1, $1600, 0, 0

    +print_chr chr_cr
    +print_strlit_line "-- all tests passed --"
    rts
