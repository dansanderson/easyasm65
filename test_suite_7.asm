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


!macro test_file_table .tnum, .count {
    +test_start .tnum

    jsr init_file_table
    ldy #.count
-   cpy #0
    beq +
    ldz #0
    lda #1
    sta [current_file],z ; make entry non-zero
    phy
    jsr next_file_entry
    jsr make_current_file_zero
    ply
    dey
    bra -
+
    jsr first_file_entry
    ldx #0
-   jsr file_entry_is_null
    beq +
    inx
    phx
    jsr next_file_entry
    plx
    bra -
+   cpx #.count
    +assert_eq test_msg_wrong_result

    +test_end
}

run_test_suite_cmd:
    +print_strlit_line "-- test suite --"

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
    +print_strlit_line "test-file-table"
    +test_file_table $01, 0
    +test_file_table $02, 1
    +test_file_table $03, 3

    +print_chr chr_cr
    +print_strlit_line "-- all tests passed --"
    rts
