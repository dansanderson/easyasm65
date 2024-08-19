!macro test_tokenize .tnum, .str, .etokbuf, .etokbuf_end, .eerror, .eerror_pos {
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

!if .eerror {
    +assert_mem_eq_byte err_code, .eerror, test_msg_wrong_err_code
    +assert_mem_eq_byte line_pos, .eerror_pos, test_msg_wrong_err_pos
} else {
    ldx #.etokbuf_end-.etokbuf
-   dex
    lda .etokbuf,x
    cmp tokbuf,x
    bne +
    cpx #0
    beq ++
    bra -
+
    brk
++
}

    +test_end
}

test_tokenize_1: !pet 0
test_tokenize_1e: !byte 0, $ff
test_tokenize_2: !pet "    ; comment only",0
test_tokenize_2e: !byte 0, $ff
test_tokenize_3: !pet "\"string literal\"",0
test_tokenize_3e: !byte tk_string_literal, 4, 14, 0, $ff
test_tokenize_4: !pet "12345",0
test_tokenize_4e:
    !byte tk_number_literal, 4
    !32 12345
    !byte 0, $ff
test_tokenize_5: !pet "$DeAdBeEf",0
test_tokenize_5e:
    !byte tk_number_literal, 4
    !32 $deadbeef
    !byte 0, $ff
test_tokenize_6: !pet "tZa",0
test_tokenize_6e: !byte mnemonic_tza, 4, 0, 0, $ff
test_tokenize_7: !pet "!wOrD",0
test_tokenize_7e: !byte po_word, 4, 0, $ff
test_tokenize_8: !pet "xOr",0
test_tokenize_8e: !byte tk_label_or_reg, 4, 3, 0, $ff
test_tokenize_9: !pet ">>>",0
test_tokenize_9e: !byte tk_lsr, 4, 0, $ff
test_tokenize_10: !pet "label",0
test_tokenize_10e: !byte tk_label_or_reg, 4, 5, 0, $ff
test_tokenize_11: !pet "@label",0
test_tokenize_11e: !byte tk_label_or_reg, 4, 6, 0, $ff
test_tokenize_12: !pet "label: lda (45, sp), y  ; comment",0
test_tokenize_12e:
    !byte tk_label_or_reg, 4, 5
    !byte tk_colon, 9
    !byte mnemonic_lda, 11, 0
    !byte tk_lparen, 15
    !byte tk_number_literal, 16
    !32 45
    !byte tk_comma, 18
    !byte tk_label_or_reg, 20, 2
    !byte tk_rparen, 22
    !byte tk_comma, 23
    !byte tk_label_or_reg, 25, 1
    !byte 0, $ff
test_tokenize_13: !pet "label = *+4",0
test_tokenize_13e:
    !byte tk_label_or_reg, 4, 5
    !byte tk_equal, 10
    !byte tk_multiply, 12
    !byte tk_pluses, 13, 1
    !byte tk_number_literal, 14
    !32 4
    !byte 0, $ff
test_tokenize_14: !pet "* = $d000",0
test_tokenize_14e:
    !byte tk_multiply, 4
    !byte tk_equal, 6
    !byte tk_number_literal, 8
    !32 $d000
    !byte 0, $ff
test_tokenize_15: !pet "!to \"lda\", runnable",0
test_tokenize_15e:
    !byte po_to, 4
    !byte tk_string_literal, 8, 3
    !byte tk_comma, 13
    !byte tk_label_or_reg, 15, 8
    !byte 0, $ff
test_tokenize_16: !pet "lda $000a",0
test_tokenize_16e:
    !byte mnemonic_lda, 4, 0
    !byte tk_number_literal_leading_zero, 8
    !32 $000a
    !byte 0, $ff
test_tokenize_17: !pet "!warn $cc | $9d",0
test_tokenize_17e:
    !byte po_warn, 4
    !byte tk_number_literal, 10
    !32 $cc
    !byte tk_pipe2, 14
    !byte tk_number_literal, 16
    !32 $9d
    !byte 0, $ff
test_tokenize_18: !pet "!warn $cc ", 220, " $9d",0  ; other pipe
test_tokenize_18e:
    !byte po_warn, 4
    !byte tk_number_literal, 10
    !32 $cc
    !byte tk_pipe, 14
    !byte tk_number_literal, 16
    !32 $9d
    !byte 0, $ff
test_tokenize_last:

test_tokenize_error_1: !pet "$$$",0

run_test_suite_cmd:
    +print_strlit_line "-- test suite --"

    +print_chr chr_cr
    +print_strlit_line "tokenize"
    +test_tokenize $01, test_tokenize_1, test_tokenize_1e, test_tokenize_2, 0, 0
    +test_tokenize $02, test_tokenize_2, test_tokenize_2e, test_tokenize_3, 0, 0
    +test_tokenize $03, test_tokenize_3, test_tokenize_3e, test_tokenize_4, 0, 0
    +test_tokenize $04, test_tokenize_4, test_tokenize_4e, test_tokenize_5, 0, 0
    +test_tokenize $05, test_tokenize_5, test_tokenize_5e, test_tokenize_6, 0, 0
    +test_tokenize $06, test_tokenize_6, test_tokenize_6e, test_tokenize_7, 0, 0
    +test_tokenize $07, test_tokenize_7, test_tokenize_7e, test_tokenize_8, 0, 0
    +test_tokenize $08, test_tokenize_8, test_tokenize_8e, test_tokenize_9, 0, 0
    +test_tokenize $09, test_tokenize_9, test_tokenize_9e, test_tokenize_10, 0, 0
    +test_tokenize $0A, test_tokenize_10, test_tokenize_10e, test_tokenize_11, 0, 0
    +test_tokenize $0B, test_tokenize_11, test_tokenize_11e, test_tokenize_12, 0, 0
    +test_tokenize $0C, test_tokenize_error_1, 0, 0, 1, 4
    +test_tokenize $0D, test_tokenize_12, test_tokenize_12e, test_tokenize_13, 0, 0
    +test_tokenize $0E, test_tokenize_13, test_tokenize_13e, test_tokenize_14, 0, 0
    +test_tokenize $0F, test_tokenize_14, test_tokenize_14e, test_tokenize_15, 0, 0
    +test_tokenize $10, test_tokenize_15, test_tokenize_15e, test_tokenize_16, 0, 0
    +test_tokenize $11, test_tokenize_16, test_tokenize_16e, test_tokenize_17, 0, 0
    +test_tokenize $12, test_tokenize_17, test_tokenize_17e, test_tokenize_18, 0, 0
    +test_tokenize $13, test_tokenize_18, test_tokenize_18e, test_tokenize_last, 0, 0

    +print_chr chr_cr
    +print_strlit_line "-- all tests passed --"
    rts
