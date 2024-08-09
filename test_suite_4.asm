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

    ldx #0
    stx pass
    stx tok_pos
    stx stmt_tokpos
    stx err_code
    stx asm_flags
    +set_pc_for_test $0000
    jsr assemble_instruction

!if .ec {
    bcs +
    +print_strlit_line "... fail, expected carry set"
    brk
+   ldx tok_pos
    beq +
    +print_strlit_line "... fail, expected tok-pos zero"
    brk
+   lda err_code
    cmp #.eerr
    beq +
    +print_strlit_line "... fail, wrong errcode"
    brk
+
} else {
    bcc +
    +print_strlit_line "... fail, expected carry clear"
    brk
+   lda program_counter
    cmp #<.epc
    beq +
    +print_strlit_line "... fail, wrong pc (0)"
    brk
+   lda program_counter+1
    cmp #>.epc
    beq +
    +print_strlit_line "... fail, wrong pc (1)"
    brk
+

    lda #<.ebytes
    sta code_ptr
    lda #>.ebytes
    sta code_ptr+1
    ldx #0
    ldz #.ebytes_len-1
    jsr strbuf_cmp_code_ptr
    cmp #0
    beq +
    +print_strlit_line "... fail, wrong strbuf (bytes)"
    brk
+
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

; TODO: set a label; lda #label


run_test_suite_cmd:
    +print_strlit_line "-- test suite --"

    +print_chr chr_cr
    +print_strlit_line "test-assemble-instruction"
    ; .tnum, .str, .ec, .eerr, .epc, .ebytes, .ebytes_len
    +test_assemble_instruction $01, test_assemble_instruction_1, 0, 0, 1, test_assemble_instruction_1_bytes, test_assemble_instruction_1_bytes_end-test_assemble_instruction_1_bytes
    +test_assemble_instruction $02, test_assemble_instruction_2, 0, 0, 2, test_assemble_instruction_2_bytes, test_assemble_instruction_2_bytes_end-test_assemble_instruction_2_bytes
    +test_assemble_instruction $03, test_assemble_instruction_3, 0, 0, 3, test_assemble_instruction_3_bytes, test_assemble_instruction_3_bytes_end-test_assemble_instruction_3_bytes
    +test_assemble_instruction $04, test_assemble_instruction_4, 0, 0, 2, test_assemble_instruction_4_bytes, test_assemble_instruction_4_bytes_end-test_assemble_instruction_4_bytes
    +test_assemble_instruction $05, test_assemble_instruction_5, 0, 0, 3, test_assemble_instruction_5_bytes, test_assemble_instruction_5_bytes_end-test_assemble_instruction_5_bytes
    +test_assemble_instruction $06, test_assemble_instruction_6, 0, 0, 2, test_assemble_instruction_6_bytes, test_assemble_instruction_6_bytes_end-test_assemble_instruction_6_bytes
    +test_assemble_instruction $07, test_assemble_instruction_7, 0, 0, 3, test_assemble_instruction_7_bytes, test_assemble_instruction_7_bytes_end-test_assemble_instruction_7_bytes
    +test_assemble_instruction $08, test_assemble_instruction_8, 0, 0, 2, test_assemble_instruction_8_bytes, test_assemble_instruction_8_bytes_end-test_assemble_instruction_8_bytes
    +test_assemble_instruction $09, test_assemble_instruction_9, 0, 0, 4, test_assemble_instruction_9_bytes, test_assemble_instruction_9_bytes_end-test_assemble_instruction_9_bytes
    +test_assemble_instruction $0a, test_assemble_instruction_10, 0, 0, 5, test_assemble_instruction_10_bytes, test_assemble_instruction_10_bytes_end-test_assemble_instruction_10_bytes
    +test_assemble_instruction $0b, test_assemble_instruction_11, 0, 0, 4, test_assemble_instruction_11_bytes, test_assemble_instruction_11_bytes_end-test_assemble_instruction_11_bytes
    +test_assemble_instruction $0c, test_assemble_instruction_12, 0, 0, 5, test_assemble_instruction_12_bytes, test_assemble_instruction_12_bytes_end-test_assemble_instruction_12_bytes

    +print_chr chr_cr
    +print_strlit_line "-- all tests passed --"
    rts
