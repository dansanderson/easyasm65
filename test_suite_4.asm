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
    ldx #5
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


!macro set_pc_for_test .addr {
    ldx #<.addr
    ldy #>.addr
    jsr set_pc
}

!macro test_assemble_instruction .tnum, .str, .ec, .eerr, .epc, .ebytes, .ebytes_len {
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
    jsr init_symbol_table
    jsr init_segment_table
    jsr init_forced16

    ldx #0
    stx pass
    stx tok_pos
    stx stmt_tokpos
    stx err_code
    stx asm_flags
    +set_pc_for_test $0010
    jsr assemble_instruction

!if .ec {
    +assert_cs test_msg_ecs
    +assert_mem_eq_byte tok_pos, 0, test_msg_tokpos_zero
    +assert_mem_eq_byte err_code, .eerr, test_msg_wrong_err_code
} else {
    +assert_cc test_msg_ecc
    +assert_mem_eq_word program_counter, .epc, test_msg_wrong_pc

    lda #<.ebytes
    sta code_ptr
    lda #>.ebytes
    sta code_ptr+1
    ldx #0
    ldz #.ebytes_len-1
    jsr strbuf_cmp_code_ptr
    cmp #0
    +assert_eq test_msg_wrong_strbuf
}

    +test_end
}

ai_1: !pet "inx",0
ai_1_bytes: !byte $e8
ai_1_bytes_end:
ai_2: !pet "lda #7",0
ai_2_bytes: !byte $a9, $07
ai_2_bytes_end:
ai_3: !pet "lda [$fc],z",0
ai_3_bytes: !byte $ea, $b2, $fc
ai_3_bytes_end:
ai_4: !pet "sta $fc",0
ai_4_bytes: !byte $85, $fc
ai_4_bytes_end:
ai_5: !pet "sta $1600",0
ai_5_bytes: !byte $8d, $00, $16
ai_5_bytes_end:
ai_6: !pet "sta ($fc),y",0
ai_6_bytes: !byte $91, $fc
ai_6_bytes_end:
ai_7: !pet "sta [$fc]",0
ai_7_bytes: !byte $ea, $92, $fc
ai_7_bytes_end:
ai_8: !pet "sta ([$fc]),y",0
ai_8_bytes: !byte $91, $fc
ai_8_bytes_end:
ai_9: !pet "stq $fc",0
ai_9_bytes: !byte $42, $42, $85, $fc
ai_9_bytes_end:
ai_10: !pet "stq $1600",0
ai_10_bytes: !byte $42, $42, $8d, $00, $16
ai_10_bytes_end:
ai_11: !pet "stq ($fc)",0
ai_11_bytes: !byte $42, $42, $92, $fc
ai_11_bytes_end:
ai_12: !pet "stq [$fc]",0
ai_12_bytes: !byte $42, $42, $ea, $92, $fc
ai_12_bytes_end:
ai_13: !pet "sta+2 $fc",0
ai_13_bytes: !byte $8d, $fc, 00
ai_13_bytes_end:
ai_14: !pet "sta+1 $00fc",0
ai_14_bytes: !byte $85, $fc
ai_14_bytes_end:

; (Test sets PC to $0010.)
ai_15: !pet "bra $0010",0
ai_15_bytes: !byte $80, $fe
ai_15_bytes_end:
ai_16: !pet "bra $0008",0
ai_16_bytes: !byte $80, $fc
ai_16_bytes_end:
ai_17: !pet "bra $0013",0
ai_17_bytes: !byte $80, $01
ai_17_bytes_end:
ai_18: !pet "bra $C013",0  ; out of range error
ai_19: !pet "lbra $0010",0
ai_19_bytes: !byte $83, $fe, $ff
ai_19_bytes_end:
ai_20: !pet "lbra $0008",0
ai_20_bytes: !byte $83, $f6, $ff
ai_20_bytes_end:
ai_21: !pet "lbra $0013",0
ai_21_bytes: !byte $83, $01, $00
ai_21_bytes_end:
ai_22: !pet "lbra $C013",0  ; no error
ai_22_bytes: !byte $83, $01, $c0
ai_22_bytes_end:
ai_23: !pet "lbra $1C013",0  ; out of range error
ai_24: !pet "bbs0 $fc,$0010",0
ai_24_bytes: !byte $8f, $fc, $fe
ai_24_bytes_end:


!macro test_assemble_line .tnum, .line_addr, .ec, .eerr, .epos {
    +test_start .tnum

    jsr init_symbol_table
    jsr init_segment_table
    lda #0
    sta pass
    sta err_code
    jsr init_pass

    ; Fake assembly location in bank 5
    lda #5
    sta bas_ptr+2
    lda #0
    sta bas_ptr+3
    lda #<(.line_addr)  ; (test data will provide bytes 0-3 this time)
    sta line_addr
    lda #>(.line_addr)
    sta line_addr+1

    ldx #$bb
    ldy #$aa
    jsr set_pc

    jsr assemble_line

!if .ec {
    +assert_cs test_msg_ecs
    +assert_mem_eq_byte err_code, .eerr, test_msg_wrong_err_code
    +assert_mem_eq_byte line_pos, .epos, test_msg_wrong_err_pos
} else {
    +assert_cc test_msg_ecc
}

    +test_end
}

test_assemble_line_1: !pet 1,2,3,4,"; comment", 0
test_assemble_line_2: !pet 1,2,3,4,"* = $c000 ; comment",0
test_assemble_line_3: !pet 1,2,3,4,"label  ; comment",0
test_assemble_line_4: !pet 1,2,3,4,"label = 12345 ; comment",0
test_assemble_line_5: !pet 1,2,3,4,"  lda #7  ; comment",0
test_assemble_line_6: !pet 1,2,3,4,"label lda #7 ; comment",0
test_assemble_line_7: !pet 1,2,3,4,"label: lda #7 ; comment",0
test_assemble_line_8: !pet 1,2,3,4," inx : iny : inz ",0

test_assemble_line_9: !pet 1,2,3,4," inx iny inz ",0
test_assemble_line_10: !pet 1,2,3,4,"*=$$$",0
test_assemble_line_11: !pet 1,2,3,4,"= $c000",0
test_assemble_line_12: !pet 1,2,3,4,"label1 label2",0

test_assemble_line_13: !byte 0,0,0,0,0

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
    +print_strlit_line "test-assemble-instruction"
    ; .tnum, .str, .ec, .eerr, .epc, .ebytes, .ebytes_len
    +test_assemble_instruction $01, ai_1, 0, 0, $0011, ai_1_bytes, ai_1_bytes_end-ai_1_bytes
    +test_assemble_instruction $02, ai_2, 0, 0, $0012, ai_2_bytes, ai_2_bytes_end-ai_2_bytes
    +test_assemble_instruction $03, ai_3, 0, 0, $0013, ai_3_bytes, ai_3_bytes_end-ai_3_bytes
    +test_assemble_instruction $04, ai_4, 0, 0, $0012, ai_4_bytes, ai_4_bytes_end-ai_4_bytes
    +test_assemble_instruction $05, ai_5, 0, 0, $0013, ai_5_bytes, ai_5_bytes_end-ai_5_bytes
    +test_assemble_instruction $06, ai_6, 0, 0, $0012, ai_6_bytes, ai_6_bytes_end-ai_6_bytes
    +test_assemble_instruction $07, ai_7, 0, 0, $0013, ai_7_bytes, ai_7_bytes_end-ai_7_bytes
    +test_assemble_instruction $08, ai_8, 0, 0, $0012, ai_8_bytes, ai_8_bytes_end-ai_8_bytes
    +test_assemble_instruction $09, ai_9, 0, 0, $0014, ai_9_bytes, ai_9_bytes_end-ai_9_bytes
    +test_assemble_instruction $0a, ai_10, 0, 0, $0015, ai_10_bytes, ai_10_bytes_end-ai_10_bytes
    +test_assemble_instruction $0b, ai_11, 0, 0, $0014, ai_11_bytes, ai_11_bytes_end-ai_11_bytes
    +test_assemble_instruction $0c, ai_12, 0, 0, $0015, ai_12_bytes, ai_12_bytes_end-ai_12_bytes
    +test_assemble_instruction $0d, ai_13, 0, 0, $0013, ai_13_bytes, ai_13_bytes_end-ai_13_bytes
    +test_assemble_instruction $0e, ai_14, 0, 0, $0012, ai_14_bytes, ai_14_bytes_end-ai_14_bytes

    +test_assemble_instruction $0f, ai_15, 0, 0, $0012, ai_15_bytes, ai_15_bytes_end-ai_15_bytes
    +test_assemble_instruction $10, ai_16, 0, 0, $0012, ai_16_bytes, ai_16_bytes_end-ai_16_bytes
    +test_assemble_instruction $11, ai_17, 0, 0, $0012, ai_17_bytes, ai_17_bytes_end-ai_17_bytes
    +test_assemble_instruction $12, ai_18, 1, err_value_out_of_range, 0, 0, 0
    +test_assemble_instruction $13, ai_19, 0, 0, $0013, ai_19_bytes, ai_19_bytes_end-ai_19_bytes
    +test_assemble_instruction $14, ai_20, 0, 0, $0013, ai_20_bytes, ai_20_bytes_end-ai_20_bytes
    +test_assemble_instruction $15, ai_21, 0, 0, $0013, ai_21_bytes, ai_21_bytes_end-ai_21_bytes
    +test_assemble_instruction $16, ai_22, 0, 0, $0013, ai_22_bytes, ai_22_bytes_end-ai_22_bytes
    +test_assemble_instruction $17, ai_23, 1, err_value_out_of_range, 0, 0, 0
    +test_assemble_instruction $18, ai_24, 0, 0, $0013, ai_24_bytes, ai_24_bytes_end-ai_24_bytes

    +print_chr chr_cr
    +print_strlit_line "test-assemble-line"
    ; .tnum, .line_addr, .ec, .eerr, .epos
    +test_assemble_line $01, test_assemble_line_1, 0, 0, 0
    +test_assemble_line $02, test_assemble_line_2, 0, 0, 0
    +test_assemble_line $03, test_assemble_line_3, 0, 0, 0
    +test_assemble_line $04, test_assemble_line_4, 0, 0, 0
    +test_assemble_line $05, test_assemble_line_5, 0, 0, 0
    +test_assemble_line $06, test_assemble_line_6, 0, 0, 0
    +test_assemble_line $07, test_assemble_line_7, 0, 0, 0
    +test_assemble_line $08, test_assemble_line_8, 0, 0, 0
    +test_assemble_line $09, test_assemble_line_9, 1, err_syntax, 5+4
    +test_assemble_line $0A, test_assemble_line_10, 1, err_syntax, 2+4
    +test_assemble_line $0B, test_assemble_line_11, 1, err_syntax, 0+4
    +test_assemble_line $0C, test_assemble_line_12, 1, err_syntax, 0+4
    +test_assemble_line $0D, test_assemble_line_13, 1, 0, 0+4

    ; TODO: assert segment bytes for successful statements


    +print_chr chr_cr
    +print_strlit_line "-- all tests passed --"
    rts
