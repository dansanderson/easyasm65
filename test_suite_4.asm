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

!macro test_assemble_pc_assign .tnum, .tokbuf, .tokbufend, .lineaddr, .ec, .etokpos, .eerr, .epc, .edefined {
    +test_start .tnum

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
    stx stmt_tokpos
    stx err_code
    stx asm_flags
    stx program_counter
    stx program_counter+1
    jsr assemble_pc_assign

!if .ec {
    +assert_cs test_msg_ecs
    +assert_mem_eq_byte tok_pos, 0, test_msg_tokpos_zero
    +assert_mem_eq_byte err_code, .eerr, test_msg_wrong_err_code
} else {
    +assert_cc test_msg_ecc
    +assert_mem_eq_byte tok_pos, .etokpos, test_msg_wrong_tokpos
    +assert_mem_eq_word program_counter, .epc, test_msg_wrong_pc

    !if .edefined {
        lda asm_flags
        and #F_ASM_PC_DEFINED
        +assert_ne test_msg_expected_defined_pc
    } else {
        lda asm_flags
        and #F_ASM_PC_DEFINED
        +assert_eq test_msg_expected_undefined_pc
    }
}

    +test_end
}

test_assemble_pc_assign_tb_1: !byte 0, $ff
test_assemble_pc_assign_tb_2: !byte tk_multiply, 0, tk_equal, 2, tk_number_literal, 0, $dd, $cc, $00, $00, 0, $ff
test_assemble_pc_assign_tb_3: !byte tk_multiply, 0, tk_equal, 2, tk_label_or_reg, 4, 5, 0, $ff
test_assemble_pc_assign_tb_4: !byte tk_multiply, 0, tk_equal, 2, tk_number_literal, 0, $dd, $cc, $bb, $aa, 0, $ff
test_assemble_pc_assign_tb_end:
test_assemble_pc_assign_line_1: !pet "* = label",0

!macro assemble_label_for_test .tokbuf, .tokbufend, .lineaddr {
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
    stx stmt_tokpos
    jsr assemble_label
}

!macro test_assemble_label .tnum, .tokbuf, .tokbufend, .lineaddr, .ec, .etokpos, .eerr, .ez, .edefined, .evalue, .ezero {
    +test_start .tnum

    +assemble_label_for_test .tokbuf, .tokbufend, .lineaddr

!if .ec {
    +assert_cs test_msg_ecs
    +assert_mem_eq_byte tok_pos, 0, test_msg_tokpos_zero
    +assert_mem_eq_byte err_code, .eerr, test_msg_wrong_err_code
} else {
    +assert_cc test_msg_ecc
    +assert_mem_eq_byte tok_pos, .etokpos, test_msg_wrong_tokpos
    cpz #.ez
    +assert_eq test_msg_wrong_z

    lda #<.lineaddr
    sta bas_ptr
    lda #>.lineaddr
    sta bas_ptr+1
    ldx .tokbuf + 1
    ldy .tokbuf + 2
    jsr find_or_add_label
    jsr get_symbol_value

    !if .edefined {
        +assert_q_eq_32 .evalue, test_msg_wrong_value

        ldz #3
        lda [attic_ptr],z
        and #F_SYMTBL_DEFINED
        +assert_ne test_msg_expected_defined_value

        !if .ezero {
            ldz #3
            lda [attic_ptr],z
            and #F_SYMTBL_LEADZERO
            +assert_ne test_msg_expected_leading_zero_value
        }
    } else {
        ldz #3
        lda [attic_ptr],z
        and #F_SYMTBL_DEFINED
        +assert_eq test_msg_expected_undefined_value
    }

}

    +test_end
}

test_assemble_label_tb_1: !byte 0, $ff
test_assemble_label_tb_2: !byte tk_label_or_reg, 0, 5, tk_equal, 6, tk_number_literal, 8, $dd, $cc, $00, $00, 0, $ff
test_assemble_label_tb_3: !byte tk_label_or_reg, 0, 5, tk_equal, 6, tk_number_literal_leading_zero, 8, $dd, $cc, $00, $00, 0, $ff
test_assemble_label_tb_4: !byte tk_label_or_reg, 0, 5, tk_equal, 6, tk_label_or_reg, 8, 6, 0, $ff
test_assemble_label_tb_5: !byte tk_label_or_reg, 0, 5, 0, $ff
test_assemble_label_tb_6: !byte tk_label_or_reg, 0, 5, 1, 6, 0, $ff
test_assemble_label_tb_7: !byte tk_label_or_reg, 0, 7, tk_equal, 8, tk_label_or_reg, 10, 6, 0, $ff
test_assemble_label_tb_8: !byte tk_label_or_reg, 0, 7, 0, $ff
test_assemble_label_tb_end:
test_assemble_label_line_1: !pet "label = label2",0
test_assemble_label_line_2: !pet "@cheapo = label2",0

tee_line_1: !pet "label",0


run_test_suite_cmd:
    +print_strlit_line "-- test suite --"

    ; -----------------------------------

    +print_chr chr_cr
    +print_strlit_line "test-assemble-pc"
    +start_test_expect_expr 0
    +test_assemble_pc_assign $01, test_assemble_pc_assign_tb_1, test_assemble_pc_assign_tb_2, test_assemble_pc_assign_line_1, 1, 0, 0, 0, 0
    +test_assemble_pc_assign $02, test_assemble_pc_assign_tb_2, test_assemble_pc_assign_tb_3, test_assemble_pc_assign_line_1, 0, 10, 0, $0000ccdd, 1

    +start_test_expect_expr 0
    +test_assemble_pc_assign $03, test_assemble_pc_assign_tb_3, test_assemble_pc_assign_tb_4, test_assemble_pc_assign_line_1, 1, 0, err_pc_undef, 0, 0

    +start_test_expect_expr 0
    +set_symbol_for_test test_assemble_pc_assign_line_1 + 4, 5, $0000aabb
    +test_assemble_pc_assign $04, test_assemble_pc_assign_tb_3, test_assemble_pc_assign_tb_4, test_assemble_pc_assign_line_1, 0, 7, 0, $0000aabb, 1

    +start_test_expect_expr 0
    +test_assemble_pc_assign $05, test_assemble_pc_assign_tb_4, test_assemble_pc_assign_tb_end, test_assemble_pc_assign_line_1, 1, 0, err_value_out_of_range, 0, 0

    ; -----------------------------------

    +print_chr chr_cr
    +print_strlit_line "test-assemble-label"
    +start_test_expect_expr 0

    ; .tnum, .tokbuf, .tokbufend, .lineaddr, .ec, .etokpos, .eerr, .ez, .edefined, .evalue, .ezero
    +test_assemble_label $01, test_assemble_label_tb_1, test_assemble_label_tb_2, test_assemble_label_line_1, 1, 0, 0, 0, 0, 0, 0
    +start_test_expect_expr 0
    +test_assemble_label $02, test_assemble_label_tb_2, test_assemble_label_tb_3, test_assemble_label_line_1, 0, 11, 0, 0, 1, $ccdd, 0

    +start_test_expect_expr 0
    +test_assemble_label $03, test_assemble_label_tb_3, test_assemble_label_tb_4, test_assemble_label_line_1, 0, 11, 0, 0, 1, $ccdd, 1

    +start_test_expect_expr 0
    +test_assemble_label $04, test_assemble_label_tb_4, test_assemble_label_tb_5, test_assemble_label_line_1, 0, 8, 0, 0, 0, 0, 0

    +start_test_expect_expr 0
    jsr init_pass
    +test_assemble_label $05, test_assemble_label_tb_5, test_assemble_label_tb_6, test_assemble_label_line_1, 1, 0, err_pc_undef, 0, 0, 0, 0

    +start_test_expect_expr 0
    ldx #$bb
    ldy #$aa
    jsr set_pc
    +test_assemble_label $06, test_assemble_label_tb_5, test_assemble_label_tb_6, test_assemble_label_line_1, 0, 3, 0, 1, 1, $aabb, 0

    +start_test_expect_expr 0
    ldx #$bb
    ldy #$aa
    jsr set_pc
    +test_assemble_label $07, test_assemble_label_tb_6, test_assemble_label_tb_7, test_assemble_label_line_1, 0, 3, 0, 1, 1, $aabb, 0

    +start_test_expect_expr 0
    +set_symbol_for_test tee_line_1, 5, 98765
    +test_assemble_label $08, test_assemble_label_tb_2, test_assemble_label_tb_3, test_assemble_label_line_1, 1, 0, err_already_defined, 0, 0, 0, 0

    +start_test_expect_expr 0
    +set_symbol_for_test test_assemble_label_line_2+10, 6, 98765
    +test_assemble_label $09, test_assemble_label_tb_7, test_assemble_label_tb_8, test_assemble_label_line_2, 1, 0, err_label_assign_global_only, 0, 0, 0, 0

    +start_test_expect_expr 0
    ldx #$bb
    ldy #$aa
    jsr set_pc
    +assemble_label_for_test test_assemble_label_tb_5, test_assemble_label_tb_6, test_assemble_label_line_1
    +test_assemble_label $0A, test_assemble_label_tb_8, test_assemble_label_tb_end, test_assemble_label_line_2, 0, 3, 0, 1, 1, $aabb, 0

    +print_chr chr_cr
    +print_strlit_line "-- all tests passed --"
    rts
