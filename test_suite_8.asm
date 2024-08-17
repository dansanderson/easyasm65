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
ai_25: !pet "adc $fc,y",0  ; coerce to 16-bit
ai_25_bytes: !byte $79, $fc
ai_25_bytes_end:
ai_26: !pet "ldx $fc,y",0  ; do not coerce to 16-bit
ai_26_bytes: !byte $b6, $fc
ai_26_bytes_end:
ai_27: !pet "ldx $00fc,y",0  ; 16-bit
ai_27_bytes: !byte $be, $fc
ai_27_bytes_end:
tee_line_1: !pet "label",0

run_test_suite_cmd:
    +print_strlit_line "-- test suite --"

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
    +test_assemble_instruction $19, ai_25, 0, 0, $0013, ai_25_bytes, ai_25_bytes_end-ai_25_bytes
    +test_assemble_instruction $1A, ai_26, 0, 0, $0012, ai_26_bytes, ai_26_bytes_end-ai_26_bytes
    +test_assemble_instruction $1B, ai_27, 0, 0, $0013, ai_27_bytes, ai_27_bytes_end-ai_27_bytes

    +print_chr chr_cr
    +print_strlit_line "-- all tests passed --"
    rts
