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

test_assemble_instruction_1: !pet "inx",0
test_assemble_instruction_1_bytes: !byte $e8
test_assemble_instruction_1_bytes_end:
test_assemble_instruction_2: !pet "lda #7",0
test_assemble_instruction_2_bytes: !byte $a9, $07
test_assemble_instruction_2_bytes_end:
test_assemble_instruction_3: !pet "lda [$fc],z",0
test_assemble_instruction_3_bytes: !byte $ea, $b2, $fc
test_assemble_instruction_3_bytes_end:
test_assemble_instruction_4: !pet "sta $fc",0
test_assemble_instruction_4_bytes: !byte $85, $fc
test_assemble_instruction_4_bytes_end:
test_assemble_instruction_5: !pet "sta $1600",0
test_assemble_instruction_5_bytes: !byte $8d, $00, $16
test_assemble_instruction_5_bytes_end:
test_assemble_instruction_6: !pet "sta ($fc),y",0
test_assemble_instruction_6_bytes: !byte $91, $fc
test_assemble_instruction_6_bytes_end:
test_assemble_instruction_7: !pet "sta [$fc]",0
test_assemble_instruction_7_bytes: !byte $ea, $92, $fc
test_assemble_instruction_7_bytes_end:
test_assemble_instruction_8: !pet "sta ([$fc]),y",0
test_assemble_instruction_8_bytes: !byte $91, $fc
test_assemble_instruction_8_bytes_end:
test_assemble_instruction_9: !pet "stq $fc",0
test_assemble_instruction_9_bytes: !byte $42, $42, $85, $fc
test_assemble_instruction_9_bytes_end:
test_assemble_instruction_10: !pet "stq $1600",0
test_assemble_instruction_10_bytes: !byte $42, $42, $8d, $00, $16
test_assemble_instruction_10_bytes_end:
test_assemble_instruction_11: !pet "stq ($fc)",0
test_assemble_instruction_11_bytes: !byte $42, $42, $92, $fc
test_assemble_instruction_11_bytes_end:
test_assemble_instruction_12: !pet "stq [$fc]",0
test_assemble_instruction_12_bytes: !byte $42, $42, $ea, $92, $fc
test_assemble_instruction_12_bytes_end:
test_assemble_instruction_13: !pet "sta+2 $fc",0
test_assemble_instruction_13_bytes: !byte $8d, $fc, 00
test_assemble_instruction_13_bytes_end:
test_assemble_instruction_14: !pet "sta+1 $00fc",0
test_assemble_instruction_14_bytes: !byte $85, $fc
test_assemble_instruction_14_bytes_end:


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

run_test_suite_cmd:
    +print_strlit_line "-- test suite --"

    +print_chr chr_cr
    +print_strlit_line "test-assemble-instruction"
    ; .tnum, .str, .ec, .eerr, .epc, .ebytes, .ebytes_len
    +test_assemble_instruction $01, test_assemble_instruction_1, 0, 0, $0011, test_assemble_instruction_1_bytes, test_assemble_instruction_1_bytes_end-test_assemble_instruction_1_bytes
    +test_assemble_instruction $02, test_assemble_instruction_2, 0, 0, $0012, test_assemble_instruction_2_bytes, test_assemble_instruction_2_bytes_end-test_assemble_instruction_2_bytes
    +test_assemble_instruction $03, test_assemble_instruction_3, 0, 0, $0013, test_assemble_instruction_3_bytes, test_assemble_instruction_3_bytes_end-test_assemble_instruction_3_bytes
    +test_assemble_instruction $04, test_assemble_instruction_4, 0, 0, $0012, test_assemble_instruction_4_bytes, test_assemble_instruction_4_bytes_end-test_assemble_instruction_4_bytes
    +test_assemble_instruction $05, test_assemble_instruction_5, 0, 0, $0013, test_assemble_instruction_5_bytes, test_assemble_instruction_5_bytes_end-test_assemble_instruction_5_bytes
    +test_assemble_instruction $06, test_assemble_instruction_6, 0, 0, $0012, test_assemble_instruction_6_bytes, test_assemble_instruction_6_bytes_end-test_assemble_instruction_6_bytes
    +test_assemble_instruction $07, test_assemble_instruction_7, 0, 0, $0013, test_assemble_instruction_7_bytes, test_assemble_instruction_7_bytes_end-test_assemble_instruction_7_bytes
    +test_assemble_instruction $08, test_assemble_instruction_8, 0, 0, $0012, test_assemble_instruction_8_bytes, test_assemble_instruction_8_bytes_end-test_assemble_instruction_8_bytes
    +test_assemble_instruction $09, test_assemble_instruction_9, 0, 0, $0014, test_assemble_instruction_9_bytes, test_assemble_instruction_9_bytes_end-test_assemble_instruction_9_bytes
    +test_assemble_instruction $0a, test_assemble_instruction_10, 0, 0, $0015, test_assemble_instruction_10_bytes, test_assemble_instruction_10_bytes_end-test_assemble_instruction_10_bytes
    +test_assemble_instruction $0b, test_assemble_instruction_11, 0, 0, $0014, test_assemble_instruction_11_bytes, test_assemble_instruction_11_bytes_end-test_assemble_instruction_11_bytes
    +test_assemble_instruction $0c, test_assemble_instruction_12, 0, 0, $0015, test_assemble_instruction_12_bytes, test_assemble_instruction_12_bytes_end-test_assemble_instruction_12_bytes
    +test_assemble_instruction $0d, test_assemble_instruction_13, 0, 0, $0013, test_assemble_instruction_13_bytes, test_assemble_instruction_13_bytes_end-test_assemble_instruction_13_bytes
    +test_assemble_instruction $0e, test_assemble_instruction_14, 0, 0, $0012, test_assemble_instruction_14_bytes, test_assemble_instruction_14_bytes_end-test_assemble_instruction_14_bytes

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

    +print_chr chr_cr
    +print_strlit_line "-- all tests passed --"
    rts
