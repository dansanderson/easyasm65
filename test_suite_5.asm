!macro test_forced16 .tnum, .pc, .do_add, .efound {
    +test_start .tnum

    lda #<.pc
    sta program_counter
    lda #>.pc
    sta program_counter+1
    !if .do_add {
        jsr add_forced16
    }
    jsr find_forced16
    !if .efound {
        +assert_cs test_msg_ecs
    } else {
        +assert_cc test_msg_ecc
    }

    +test_end
}


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

tee_tb_31: !byte tk_lt, 0, tk_number_literal, 1, $aa, $bb, $cc, $dd, 0, $ff
tee_tb_32: !byte tk_gt, 0, tk_number_literal, 1, $aa, $bb, $cc, $dd, 0, $ff
tee_tb_33: !byte tk_power, 0, tk_number_literal, 1, $aa, $bb, $cc, $dd, 0, $ff
tee_tb_34: !byte tk_megabyte, 0, tk_number_literal, 1, $aa, $bb, $cc, $dd, 0, $ff

tee_tb_35: !byte tk_number_literal, 0, $f0, $f0, $f0, $f0, tk_ampersand, 1, tk_number_literal, 2, $cc, $aa, $cc, $aa, 0, $ff
tee_tb_36: !byte tk_number_literal, 0, $f0, $f0, $f0, $f0, tk_pipe, 1, tk_number_literal, 2, $cc, $aa, $cc, $aa, 0, $ff
tee_tb_37: !byte tk_number_literal, 0, $f0, $f0, $f0, $f0, tk_label_or_reg, 10, 3, tk_number_literal, 14, $cc, $aa, $cc, $aa, 0, $ff
tee_tb_38: !byte tk_number_literal, 0, $f0, $f0, $f0, $f0, tk_ampersand, 1, tk_number_literal, 2, $cc, $aa, $cc, $aa, tk_pipe, 2, tk_number_literal, 3, $01, $02, $03, $04, 0, $ff

tee_tb_39: !byte tk_number_literal, 0, $06, $00, $00, $00, tk_remainder, 1, tk_number_literal, 2, $07, $00, $00, $00, 0, $ff
tee_tb_40: !byte tk_number_literal, 0, $07, $00, $00, $00, tk_remainder, 1, tk_number_literal, 2, $07, $00, $00, $00, 0, $ff
tee_tb_41: !byte tk_number_literal, 0, $08, $00, $00, $00, tk_remainder, 1, tk_number_literal, 2, $07, $00, $00, $00, 0, $ff

tee_tb_end:
tee_line_1: !pet "label",0
tee_line_2: !pet "8 div 2",0
tee_line_3: !pet "60 div 5 div 4",0
tee_line_4: !pet "$f0f0f0f0 xor $ccaaccaa"


run_test_suite_cmd:
    +print_strlit_line "-- test suite --"

    +print_chr chr_cr
    +print_strlit_line "test-forced16"
    jsr init_forced16
    +test_forced16 $01, $aabb, 0, 0
    jsr init_forced16
    +test_forced16 $02, $aabb, 1, 1
    +test_forced16 $03, $bbcc, 1, 1
    +test_forced16 $04, $ccdd, 1, 1
    +test_forced16 $05, $aabb, 0, 1
    +test_forced16 $06, $bbcc, 0, 1
    +test_forced16 $07, $ccdd, 0, 1
    jsr init_forced16
    +test_forced16 $08, $aabb, 0, 0
    +test_forced16 $09, $bbcc, 0, 0
    +test_forced16 $0A, $ccdd, 0, 0

    ; -----------------------------------

    +print_chr chr_cr
    +print_strlit_line "test-expr continued"

    +start_test_expect_expr 0
    +test_expect_expr $23, "low byte", tee_tb_31, tee_tb_32, tee_line_3, 0, 8, <$ddccbbaa, 0
    +test_expect_expr $24, "high byte", tee_tb_32, tee_tb_33, tee_line_3, 0, 8, >$ddccbbaa, 0
    +test_expect_expr $25, "bank byte", tee_tb_33, tee_tb_34, tee_line_3, 0, 8, ^$ddccbbaa, 0
    +test_expect_expr $26, "mega byte", tee_tb_34, tee_tb_35, tee_line_3, 0, 8, <($ddccbbaa >> 24), 0

    +test_expect_expr $27, "and", tee_tb_35, tee_tb_36, tee_line_4, 0, 14, $a0c0a0c0, 0
    +test_expect_expr $28, "or", tee_tb_36, tee_tb_37, tee_line_4, 0, 14, $fafcfafc, 0
    +test_expect_expr $29, "xor", tee_tb_37, tee_tb_38, tee_line_4, 0, 15, $5a3c5a3c, 0
    +test_expect_expr $2A, "and or", tee_tb_38, tee_tb_39, tee_line_4, 0, 22, $a4c3a2c1, 0

    +test_expect_expr $2B, "remainder b", tee_tb_39, tee_tb_40, tee_line_1, 0, 14, 6 % 7, 0
    +test_expect_expr $2C, "remainder c", tee_tb_40, tee_tb_41, tee_line_1, 0, 14, 7 % 7, 0
    +test_expect_expr $2D, "remainder d", tee_tb_41, tee_tb_end, tee_line_1, 0, 14, 8 % 7, 0

    ; -----------------------------------

    +print_chr chr_cr
    +print_strlit_line "-- all tests passed --"
    rts
