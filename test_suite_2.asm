!macro start_test_expect_expr {
    jsr init_symbol_table

    ; Fake assembly location in bank 5
    lda #5
    sta bas_ptr+2
    lda #0
    sta bas_ptr+3

    sta err_code
    sta asm_flags
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
    bcs +
    +print_strlit_line "... fail, expected carry set"
    brk
+   ldx tok_pos
    beq +
    +print_strlit_line "... fail, expected tok-pos zero"
    brk
+
} else {
    bcc +
    +print_strlit_line "... fail, expected carry clear"
    brk
+   lda tok_pos
    cmp #.etokpos
    beq +
    +print_strlit_line "... fail, wrong tokpos (a)"
    brk
+
    !if .eflags != F_EXPR_UNDEFINED {
    lda expr_result
    cmp #<.eresult
    beq +
    +print_strlit_line "... fail, wrong result (0; a)"
    brk
+   lda expr_result+1
    cmp #>.eresult
    beq +
    +print_strlit_line "... fail, wrong result (1; a)"
    brk
+   lda expr_result+2
    cmp #^.eresult
    beq +
    +print_strlit_line "... fail, wrong result (2; a)"
    brk
+   lda expr_result+3
    cmp #<(.eresult >>> 24)
    beq +
    +print_strlit_line "... fail, wrong result (3; a)"
    brk
+
    }
    lda expr_flags
    cmp #.eflags
    beq +
    +print_strlit_line "... fail, wrong expr flags (a)"
    brk
+
}
    +test_end
}

test_expect_expr_tb_1: !byte 0, $ff
test_expect_expr_tb_2: !byte tk_number_literal, 0, $dd, $cc, $bb, $aa, 0, $ff
test_expect_expr_tb_3: !byte tk_number_literal_leading_zero, 0, $dd, $cc, $bb, $0a, 0, $ff
test_expect_expr_tb_4: !byte tk_label_or_reg, 0, 5, 0, $ff
test_expect_expr_tb_end:
test_expect_expr_line_1: !pet "label",0

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
+   lda tok_pos
    cmp #.etokpos
    beq +
    +print_strlit_line "... fail, wrong tokpos (a)"
    brk
+   lda program_counter
    cmp #<.epc
    beq +
    brk
+   lda program_counter+1
    cmp #>.epc
    beq +
    brk
+
!if .edefined {
    lda asm_flags
    and #F_ASM_PC_DEFINED
    bne +
    +print_strlit_line "... fail, expected defined pc"
    brk
+
} else {
    lda asm_flags
    and #F_ASM_PC_DEFINED
    beq +
    +print_strlit_line "... fail, expected undefined pc"
    brk
+
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


!macro test_assemble_label .tnum, .tokbuf, .tokbufend, .lineaddr, .ec, .etokpos, .eerr, .ez, .edefined, .evalue, .ezero {
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
    jsr assemble_label

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
+   lda tok_pos
    cmp #.etokpos
    beq +
    +print_strlit_line "... fail, wrong tokpos (a)"
    brk
+   cpz #.ez
    beq +
    +print_strlit_line "... fail, wrong z"
    brk
+
    lda #<.lineaddr
    sta bas_ptr
    lda #>.lineaddr
    sta bas_ptr+1
    ldx #5
    jsr find_symbol
    jsr get_symbol_value
!if .edefined {
    cmp #<.evalue
    beq +
    +print_strlit_line "... fail, wrong value (a)"
    brk
+   cpx #>.evalue
    beq +
    +print_strlit_line "... fail, wrong value (b)"
    brk
+   cpy #^.evalue
    beq +
    +print_strlit_line "... fail, wrong value (c)"
    brk
+   cpz #<(.evalue >>> 24)
    beq +
    +print_strlit_line "... fail, wrong value (d)"
    brk
+
    ldz #3
    lda [attic_ptr],z
    and #F_SYMTBL_DEFINED
    bne +
    +print_strlit_line "... fail, expected value defined"
    brk
+
!if .ezero {
    ldz #3
    lda [attic_ptr],z
    and #F_SYMTBL_LEADZERO
    bne +
    +print_strlit_line "... fail, expected value leading zero"
    brk
+
}
} else {
    ldz #3
    lda [attic_ptr],z
    and #F_SYMTBL_DEFINED
    beq +
    +print_strlit_line "... fail, expected value undefined"
    brk
+
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
test_assemble_label_tb_end:
test_assemble_label_line_1: !pet "label = label2",0


!macro test_expect_addressing_expr .tnum, .str, .ec, .emode, .eresult, .eflags, .etokpos, .eerror, .eerror_pos {
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
    bne +
    +print_strlit_line "...test error: tokenize failed"
    brk
+
    ldz #2
    stz tok_pos
    jsr expect_addressing_expr
!if .ec {
    bcs +
    +print_strlit_line "...fail: expected carry set"
    brk
+   lda err_code
    cmp #.eerror
    beq +
    +print_strlit_line "...fail: wrong error code"
    brk
+   lda line_pos
    cmp #.eerror_pos
    beq +
    +print_strlit_line "...fail: wrong error pos"
    brk
+
} else {
    bcc +
    +print_strlit_line "...fail: expected carry clear"
    brk
+   cpx #<.emode
    beq +
    +print_strlit_line "...fail: wrong mode (x)"
    brk
+   cpy #>.emode
    beq +
    +print_strlit_line "...fail: wrong mode (y)"
    brk
+   lda expr_result
    cmp #<.eresult
    beq +
    +print_strlit_line "...fail: wrong result (0)"
    brk
+   lda expr_result+1
    cmp #>.eresult
    beq +
    +print_strlit_line "...fail: wrong result (1)"
    brk
+   lda expr_result+2
    cmp #^.eresult
    beq +
    +print_strlit_line "...fail: wrong result (2)"
    brk
+   lda expr_result+3
    cmp #<(.eresult >>> 24)
    beq +
    +print_strlit_line "...fail: wrong result (3)"
    brk
+   lda expr_flags
    cmp #.eflags
    beq +
    +print_strlit_line "...fail: wrong flags"
    brk
+   lda tok_pos
    cmp #.etokpos
    beq +
    +print_strlit_line "...fail: wrong tokpos"
    brk
+
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
test_expect_addressing_expr_26: !pet "lda $$$",0
test_expect_addressing_expr_27: !pet "lda ($$$,x)",0
test_expect_addressing_expr_28: !pet "lda ($fe x)",0
test_expect_addressing_expr_29: !pet "lda ($fe,y)",0
test_expect_addressing_expr_30: !pet "lda ($fe,x",0
test_expect_addressing_expr_31: !pet "lda (4,sp",0
test_expect_addressing_expr_32: !pet "lda (4,sp)",0
test_expect_addressing_expr_33: !pet "lda (4,sp),",0
test_expect_addressing_expr_34: !pet "lda (4,sp),x",0
; Other errors
test_expect_addressing_expr_35: !pet "lda [$fffe],z"  ; err_value_out_of_range


run_test_suite_cmd:
    +print_strlit_line "-- test suite --"

    +print_chr chr_cr
    +print_strlit_line "test-expr"

    +start_test_expect_expr
    +test_expect_expr $01, "empty", test_expect_expr_tb_1, test_expect_expr_tb_2, test_expect_expr_line_1, 1, 0, 0, 0
    +test_expect_expr $02, "literal", test_expect_expr_tb_2, test_expect_expr_tb_3, test_expect_expr_line_1, 0, 6, $aabbccdd, 0
    +test_expect_expr $03, "literal w zero", test_expect_expr_tb_3, test_expect_expr_tb_4, test_expect_expr_line_1, 0, 6, $0abbccdd, F_EXPR_BRACKET_ZERO

    +start_test_expect_expr
    +test_expect_expr $04, "label undef", test_expect_expr_tb_4, test_expect_expr_tb_end, test_expect_expr_line_1, 0, 3, 0, F_EXPR_UNDEFINED

    +start_test_expect_expr
    +create_undefined_symbol_for_test test_expect_expr_line_1, 5
    +test_expect_expr $05, "label undef in tbl", test_expect_expr_tb_4, test_expect_expr_tb_end, test_expect_expr_line_1, 0, 3, 0, F_EXPR_UNDEFINED

    +start_test_expect_expr
    +set_symbol_for_test test_expect_expr_line_1, 5, 98765
    +test_expect_expr $06, "label def", test_expect_expr_tb_4, test_expect_expr_tb_end, test_expect_expr_line_1, 0, 3, 98765, 0

    ; -----------------------------------

    +print_chr chr_cr
    +print_strlit_line "test-assemble-pc"
    +start_test_expect_expr
    +test_assemble_pc_assign $01, test_assemble_pc_assign_tb_1, test_assemble_pc_assign_tb_2, test_assemble_pc_assign_line_1, 1, 0, 0, 0, 0
    +test_assemble_pc_assign $02, test_assemble_pc_assign_tb_2, test_assemble_pc_assign_tb_3, test_assemble_pc_assign_line_1, 0, 10, 0, $0000ccdd, 1

    +start_test_expect_expr
    +test_assemble_pc_assign $03, test_assemble_pc_assign_tb_3, test_assemble_pc_assign_tb_4, test_assemble_pc_assign_line_1, 1, 0, err_pc_undef, 0, 0

    +start_test_expect_expr
    +set_symbol_for_test test_assemble_pc_assign_line_1 + 4, 5, $0000aabb
    +test_assemble_pc_assign $04, test_assemble_pc_assign_tb_3, test_assemble_pc_assign_tb_4, test_assemble_pc_assign_line_1, 0, 7, 0, $0000aabb, 1

    +start_test_expect_expr
    +test_assemble_pc_assign $05, test_assemble_pc_assign_tb_4, test_assemble_pc_assign_tb_end, test_assemble_pc_assign_line_1, 1, 0, err_value_out_of_range, 0, 0

    ; -----------------------------------

    +print_chr chr_cr
    +print_strlit_line "test-assemble-label"
    +start_test_expect_expr

    +test_assemble_label $01, test_assemble_label_tb_1, test_assemble_label_tb_2, test_assemble_label_line_1, 1, 0, 0, 0, 0, 0, 0
    +start_test_expect_expr
    +test_assemble_label $02, test_assemble_label_tb_2, test_assemble_label_tb_3, test_assemble_label_line_1, 0, 11, 0, 0, 1, $ccdd, 0

    +start_test_expect_expr
    +test_assemble_label $03, test_assemble_label_tb_3, test_assemble_label_tb_4, test_assemble_label_line_1, 0, 11, 0, 0, 1, $ccdd, 1

    +start_test_expect_expr
    +test_assemble_label $04, test_assemble_label_tb_4, test_assemble_label_tb_5, test_assemble_label_line_1, 0, 8, 0, 0, 0, 0, 0

    +start_test_expect_expr
    jsr init_pass
    +test_assemble_label $05, test_assemble_label_tb_5, test_assemble_label_tb_6, test_assemble_label_line_1, 1, 0, err_pc_undef, 0, 0, 0, 0

    +start_test_expect_expr
    jsr init_pass
    ldx #$bb
    ldy #$aa
    jsr set_pc
    +test_assemble_label $06, test_assemble_label_tb_5, test_assemble_label_tb_6, test_assemble_label_line_1, 0, 3, 0, 1, 1, $aabb, 0

    +start_test_expect_expr
    jsr init_pass
    ldx #$bb
    ldy #$aa
    jsr set_pc
    +test_assemble_label $07, test_assemble_label_tb_6, test_assemble_label_tb_end, test_assemble_label_line_1, 0, 3, 0, 1, 1, $aabb, 0

    +start_test_expect_expr
    +set_symbol_for_test test_expect_expr_line_1, 5, 98765
    +test_assemble_label $08, test_assemble_label_tb_2, test_assemble_label_tb_3, test_assemble_label_line_1, 1, 0, err_already_defined, 0, 0, 0, 0

    ; -----------------------------------

    +print_chr chr_cr
    +print_strlit_line "test-expect-addressing-expr"
    ; .tnum, .str, .ec, .emode, .eresult, .eflags, .etokpos, .eerror, .eerror_pos
    +test_expect_addressing_expr $01, test_expect_addressing_expr_1, 0, MODE_IMPLIED, 0, 0, 2, 0, 0
    +test_expect_addressing_expr $02, test_expect_addressing_expr_2, 0, MODE_IMPLIED, 0, 0, 2, 0, 0
    +test_expect_addressing_expr $03, test_expect_addressing_expr_3, 0, MODE_IMMEDIATE, 13, 0, 10, 0, 0
    +test_expect_addressing_expr $04, test_expect_addressing_expr_4, 0, MODE_IMMEDIATE_WORD, $aabb, 0, 10, 0, 0
    +test_expect_addressing_expr $05, test_expect_addressing_expr_5, 0, MODE_BASE_PAGE, $fe, 0, 8, 0, 0
    +test_expect_addressing_expr $06, test_expect_addressing_expr_6, 0, MODE_BASE_PAGE_X, $fe, 0, 13, 0, 0
    +test_expect_addressing_expr $07, test_expect_addressing_expr_7, 0, MODE_BASE_PAGE_Y, $fe, 0, 13, 0, 0
    +test_expect_addressing_expr $08, test_expect_addressing_expr_8, 0, MODE_ABSOLUTE, $d020, 0, 8, 0, 0
    +test_expect_addressing_expr $09, test_expect_addressing_expr_9, 0, MODE_ABSOLUTE_X, $d020, 0, 13, 0, 0
    +test_expect_addressing_expr $0a, test_expect_addressing_expr_10, 0, MODE_ABSOLUTE_Y, $d020, 0, 13, 0, 0
    +test_expect_addressing_expr $0b, test_expect_addressing_expr_11, 0, MODE_ABSOLUTE_X, $fe, F_EXPR_BRACKET_ZERO, 13, 0, 0
    +test_expect_addressing_expr $0c, test_expect_addressing_expr_12, 0, MODE_ABSOLUTE_IND, $fffe, 0, 12, 0, 0
    +test_expect_addressing_expr $0d, test_expect_addressing_expr_13, 0, MODE_ABSOLUTE_IND_X, $c000, 0, 16, 0, 0
    +test_expect_addressing_expr $0e, test_expect_addressing_expr_14, 0, MODE_BASE_PAGE_IND_X, $fe, 0, 16, 0, 0
    +test_expect_addressing_expr $0f, test_expect_addressing_expr_15, 0, MODE_BASE_PAGE_IND_Y, $fe, 0, 17, 0, 0
    +test_expect_addressing_expr $10, test_expect_addressing_expr_16, 0, MODE_BASE_PAGE_IND_Z, $fe, 0, 17, 0, 0
    +test_expect_addressing_expr $11, test_expect_addressing_expr_17, 0, MODE_32BIT_IND, $fe, 0, 17, 0, 0
    +test_expect_addressing_expr $12, test_expect_addressing_expr_18, 0, MODE_32BIT_IND, $fe, 0, 12, 0, 0
    +test_expect_addressing_expr $13, test_expect_addressing_expr_19, 0, MODE_STACK_REL, 4, 0, 22, 0, 0
    +test_expect_addressing_expr $14, test_expect_addressing_expr_20, 0, MODE_STACK_REL, 4, 0, 22, 0, 0
    +test_expect_addressing_expr $15, test_expect_addressing_expr_21, 1, 0, 0, 0, 0, err_syntax, 6
    +test_expect_addressing_expr $16, test_expect_addressing_expr_22, 1, 0, 0, 0, 0, err_syntax, 9
    +test_expect_addressing_expr $17, test_expect_addressing_expr_23, 1, 0, 0, 0, 0, err_syntax, 11
    +test_expect_addressing_expr $18, test_expect_addressing_expr_24, 1, 0, 0, 0, 0, err_syntax, 11
    +test_expect_addressing_expr $19, test_expect_addressing_expr_25, 1, 0, 0, 0, 0, err_syntax, 11
    +test_expect_addressing_expr $1a, test_expect_addressing_expr_26, 1, 0, 0, 0, 0, err_syntax, 4
    +test_expect_addressing_expr $1b, test_expect_addressing_expr_27, 1, 0, 0, 0, 0, err_syntax, 5
    +test_expect_addressing_expr $1c, test_expect_addressing_expr_28, 1, 0, 0, 0, 0, err_syntax, 9
    +test_expect_addressing_expr $1d, test_expect_addressing_expr_29, 1, 0, 0, 0, 0, err_syntax, 10
    +test_expect_addressing_expr $1e, test_expect_addressing_expr_30, 1, 0, 0, 0, 0, err_syntax, 11
    +test_expect_addressing_expr $1f, test_expect_addressing_expr_31, 1, 0, 0, 0, 0, err_syntax, 10
    +test_expect_addressing_expr $20, test_expect_addressing_expr_32, 1, 0, 0, 0, 0, err_syntax, 11
    +test_expect_addressing_expr $21, test_expect_addressing_expr_33, 1, 0, 0, 0, 0, err_syntax, 12
    +test_expect_addressing_expr $22, test_expect_addressing_expr_34, 1, 0, 0, 0, 0, err_syntax, 13
    +test_expect_addressing_expr $23, test_expect_addressing_expr_35, 1, 0, 0, 0, 0, err_value_out_of_range, 5

    +print_chr chr_cr
    +print_strlit_line "-- all tests passed --"
    rts
