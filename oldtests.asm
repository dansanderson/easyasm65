test_accept_ident_5: !pet "@label",0
test_accept_ident_6: !pet "+",0
test_accept_ident_7: !pet "++++",0
test_accept_ident_8: !pet "-",0
test_accept_ident_9: !pet "----",0
test_accept_ident_10: !pet "++++:",0
test_accept_ident_11: !pet "++++ ",0
test_accept_ident_12: !pet "!!label",0

!macro test_accept_label_mnemonic_pseudoop .tnum, .basptr, .c, .ec, .ez {
    +test_start .tnum
    lda #<.basptr
    sta bas_ptr
    lda #>.basptr
    sta bas_ptr+1
    lda #$05
    sta bas_ptr+2   ; test data in bank 5
    ldz #0
!if .c {
    sec
} else {
    clc
}
    jsr accept_label_mnemonic_pseudoop
!if .ec {
    bcs +
    brk
+
} else {
    bcc +
    brk
+
}
    cpz #.ez
    beq +
    brk
+
    +test_end
}

!macro test_code_to_strbuf .tnum, .basptr, .pos, .endpos, .eptr, .ez {
    +test_start .tnum
    lda #<.basptr
    sta bas_ptr
    lda #>.basptr
    sta bas_ptr+1
    lda #$05
    sta bas_ptr+2   ; test data in bank 5
    ldz #.endpos
    stz line_pos
    ldz #.pos
    jsr code_to_strbuf
    +test_match_strbuf .eptr
    beq +
    brk
+
    cpz #.ez
    beq +
    brk
+
    +test_end
}

!macro test_get_mnemonic_for_strbuf .tnum, .straddr, .ec, .erowaddr {
    +test_start .tnum
    +test_copy_to_strbuf .straddr
    clc
    jsr get_mnemonic_for_strbuf
!if .ec {
    bcs +
    brk
+
    lda code_ptr
    cmp #<.erowaddr
    beq +
    brk
+
    lda code_ptr+1
    cmp #>.erowaddr
    beq +
    brk
+
} else {
    bcc +
    brk
+
}
    +test_end
}

test_get_mnemonic_for_strbuf_1: !pet "cpq",0
test_get_mnemonic_for_strbuf_2: !pet "adc",0
test_get_mnemonic_for_strbuf_3: !pet "ggg",0
test_get_mnemonic_for_strbuf_4: !pet "ldax",0

!macro test_get_pseudoop_for_strbuf .tnum, .straddr, .ec, .ea {
    +test_start .tnum
    +test_copy_to_strbuf .straddr
    jsr get_pseudoop_for_strbuf
!if .ec {
    bcs +
    brk
+   cmp #.ea
    beq +
    brk
+
} else {
    bcc +
    brk
+
}
    +test_end
}

test_get_pseudoop_for_strbuf_1: !pet "!to",0
test_get_pseudoop_for_strbuf_2: !pet "!warn",0
test_get_pseudoop_for_strbuf_3: !pet "!8",0
test_get_pseudoop_for_strbuf_4: !pet "warn",0
test_get_pseudoop_for_strbuf_5: !pet "lda",0

!macro test_accept_addressing_operand .tnum, .lineaddr, .ec, .eresult, .ex, .ey, .epos {
    +test_start .tnum
    lda #<.lineaddr
    sta bas_ptr
    lda #>.lineaddr
    sta bas_ptr+1
    lda #$05
    sta bas_ptr+2   ; test data in bank 5
    ldz #0
    stz line_pos
    jsr accept_addressing_operand

!if .ec {
    bcs +
    +print_strlit_line "... fail ec should be 1, is 0"
    brk
+   lda expr_result
    cmp #<.eresult
    beq +
    +print_strlit_line "... fail result byte 0"
    brk
+   lda expr_result+1
    cmp #>.eresult
    beq +
    +print_strlit_line "... fail result byte 1"
    brk
+   lda expr_result+2
    cmp #^.eresult
    beq +
    +print_strlit_line "... fail result byte 2"
    brk
+   lda expr_result+3
    cmp #<(.eresult >>> 24)
    beq +
    +print_strlit_line "... fail result byte 3"
    brk
+   cpx #.ex
    beq +
    +print_strlit_line "... fail x"
    brk
+   cpy #.ey
    beq +
    +print_strlit_line "... fail y"
    brk
+   lda line_pos
    cmp #.epos
    beq +
    +print_strlit_line "... fail line-pos"
    brk
+
} else {
    bcc +
    +print_strlit_line "... fail ec should be 0, is 1"
    brk
+
}

    +test_end
}

test_accept_addressing_operand_1: !pet "   ",0
test_accept_addressing_operand_2: !pet 0
test_accept_addressing_operand_3: !pet "     ; comment",0

test_accept_addressing_operand_4: !pet "#$ff",0
test_accept_addressing_operand_5: !pet "#$ff  ; comment",0
test_accept_addressing_operand_6: !pet "#255",0
test_accept_addressing_operand_7: !pet "#%01010101",0
test_accept_addressing_operand_8: !pet "#$10ff",0
test_accept_addressing_operand_9: !pet "#$00ff",0
test_accept_addressing_operand_10: !pet "#256",0

test_accept_addressing_operand_11: !pet "$ff",0
test_accept_addressing_operand_12: !pet "$ff,x",0
test_accept_addressing_operand_13: !pet "$FF,X",0
test_accept_addressing_operand_14: !pet "  $ff  ,  x  ",0
test_accept_addressing_operand_15: !pet "$ff,y",0
test_accept_addressing_operand_16: !pet "$FF,Y",0
test_accept_addressing_operand_17: !pet "  $ff  ,  y",0

test_accept_addressing_operand_18: !pet "$ffff",0
test_accept_addressing_operand_19: !pet "$00ff",0

test_accept_addressing_operand_20: !pet "$ffff,x",0
test_accept_addressing_operand_21: !pet "$fFFf,X",0
test_accept_addressing_operand_22: !pet "$ffff  ,  x",0

test_accept_addressing_operand_23: !pet "$ffff,y",0
test_accept_addressing_operand_24: !pet "$fFFf,Y",0
test_accept_addressing_operand_25: !pet "$ffff  ,  y",0

test_accept_addressing_operand_26: !pet "($ffff)",0
test_accept_addressing_operand_27: !pet "(  $fFFf  )",0

test_accept_addressing_operand_28: !pet "($ffff,x)",0
test_accept_addressing_operand_29: !pet "(  $fFFf  ,  X  )",0

test_accept_addressing_operand_30: !pet "($ff,x)",0
test_accept_addressing_operand_31: !pet "($FF,X)",0
test_accept_addressing_operand_32: !pet "(  $ff  ,  x  )",0

test_accept_addressing_operand_33: !pet "($ff),y",0
test_accept_addressing_operand_34: !pet "($FF),Y",0
test_accept_addressing_operand_35: !pet "(  $ff  )  ,  y",0

test_accept_addressing_operand_36: !pet "($ff),z",0
test_accept_addressing_operand_37: !pet "($FF),Z",0
test_accept_addressing_operand_38: !pet "(  $ff  )  ,  z",0
test_accept_addressing_operand_39: !pet "($ff)  ; comment",0

test_accept_addressing_operand_40: !pet "[$ff],z",0
test_accept_addressing_operand_41: !pet "[$fF],Z",0
test_accept_addressing_operand_42: !pet "[  $ff  ]  ,  z",0
test_accept_addressing_operand_43: !pet "[$ff]  ; comment",0

test_accept_addressing_operand_44: !pet "(123,sp),y",0
test_accept_addressing_operand_45: !pet "(123,Sp),Y",0
test_accept_addressing_operand_46: !pet "(  123  ,  sp  )  ,  y",0

test_accept_addressing_operand_47: !pet "($ff",0
test_accept_addressing_operand_48: !pet "[$ff",0
test_accept_addressing_operand_49: !pet "(123,s),y",0
test_accept_addressing_operand_50: !pet "(123,sp)y",0
test_accept_addressing_operand_51: !pet "(123,sp)",0


!macro test_assemble_line .tnum, .lineaddr, .ecode, .epos {
    +test_start .tnum

    lda #$05
    sta bas_ptr+2   ; Test code in bank 5
    lda #<(.lineaddr-4)
    sta line_addr
    lda #>(.lineaddr-4)
    sta line_addr+1
    jsr assemble_line
    lda err_code
    cmp #.ecode
    beq +
    +print_strlit_line "... fail, unexpected errcode"
    brk
+   lda err_code
    beq +
    lda line_pos
    cmp #.epos
    beq +
    +print_strlit_line "... fail, unexpected line pos"
    brk
+
    +test_end
}

test_assemble_line_1: !pet 0
test_assemble_line_2: !pet "   ",0
test_assemble_line_3: !pet "   ; comment",0
test_assemble_line_4: !pet "label",0
test_assemble_line_5: !pet "label:",0
test_assemble_line_6: !pet "+++",0
test_assemble_line_7: !pet "+++:",0
test_assemble_line_8: !pet "label = 12345",0
test_assemble_line_9: !pet "lda #1",0
test_assemble_line_10: !pet "inx",0
test_assemble_line_11: !pet "   lda #1  ; comment",0
test_assemble_line_12: !pet "label lda #1",0
test_assemble_line_13: !pet "label: lda #1",0
test_assemble_line_14: !pet "label: lda #1  ; comment",0
test_assemble_line_15: !pet "label: Lda #1  ; comment",0
test_assemble_line_16: !pet "!to \"filename\",cbm",0
test_assemble_line_17: !pet "!TO \"filename\",cbm",0
test_assemble_line_18: !pet "!8 1,2,3",0
test_assemble_line_19: !pet "label problem",0


    +print_chr chr_cr
    +print_strlit_line "accept-label-mnemonic-pseudoop"
    +test_accept_label_mnemonic_pseudoop $01, test_accept_ident_1, 0, 1, 5
    +test_accept_label_mnemonic_pseudoop $02, test_accept_ident_2, 0, 1, 10
    +test_accept_label_mnemonic_pseudoop $03, test_accept_ident_3, 0, 0, 0
    +test_accept_label_mnemonic_pseudoop $04, test_accept_ident_4, 0, 1, 6
    +test_accept_label_mnemonic_pseudoop $04, test_accept_ident_4, 1, 0, 0
    +test_accept_label_mnemonic_pseudoop $05, test_accept_ident_5, 0, 1, 6
    +test_accept_label_mnemonic_pseudoop $06, test_accept_ident_6, 0, 1, 1
    +test_accept_label_mnemonic_pseudoop $07, test_accept_ident_7, 0, 1, 4
    +test_accept_label_mnemonic_pseudoop $08, test_accept_ident_8, 0, 1, 1
    +test_accept_label_mnemonic_pseudoop $09, test_accept_ident_9, 0, 1, 4
    +test_accept_label_mnemonic_pseudoop $0A, test_accept_ident_10, 0, 1, 4
    +test_accept_label_mnemonic_pseudoop $0B, test_accept_ident_11, 0, 1, 4
    +test_accept_label_mnemonic_pseudoop $0C, test_accept_ident_12, 1, 0, 0

    +print_chr chr_cr
    +print_strlit_line "code-to-strbuf"
    +test_code_to_strbuf $01, test_accept_whitespace_and_comment_comment, 4, 37, test_accept_whitespace_and_comment_comment+4, 37

    +print_chr chr_cr
    +print_strlit_line "get-mnemonic-for-strbuf"
    +test_get_mnemonic_for_strbuf $01, test_get_mnemonic_for_strbuf_1, 1, mnemonics+(45*8)
    +test_get_mnemonic_for_strbuf $02, test_get_mnemonic_for_strbuf_2, 1, mnemonics
    +test_get_mnemonic_for_strbuf $03, test_get_mnemonic_for_strbuf_3, 0, 0
    +test_get_mnemonic_for_strbuf $04, test_get_mnemonic_for_strbuf_4, 0, 0

    +print_chr chr_cr
    +print_strlit_line "get-pseudoop-for-strbuf"
    +test_get_pseudoop_for_strbuf $01, test_get_pseudoop_for_strbuf_1, 1, 1
    +test_get_pseudoop_for_strbuf $02, test_get_pseudoop_for_strbuf_2, 1, 12
    +test_get_pseudoop_for_strbuf $03, test_get_pseudoop_for_strbuf_3, 1, 3
    +test_get_pseudoop_for_strbuf $04, test_get_pseudoop_for_strbuf_4, 0, 0
    +test_get_pseudoop_for_strbuf $05, test_get_pseudoop_for_strbuf_5, 0, 0

    +print_chr chr_cr
    +print_strlit_line "accept-addressing-operand"
                                                                        ;  C  R  X    Y  L
    +test_accept_addressing_operand $01, test_accept_addressing_operand_1, 1, 0, 128, 0, 3
    +test_accept_addressing_operand $02, test_accept_addressing_operand_2, 1, 0, 128, 0, 0
    +test_accept_addressing_operand $03, test_accept_addressing_operand_3, 1, 0, 128, 0, 14

    +test_accept_addressing_operand $04, test_accept_addressing_operand_4, 1, $ff, 64, 0, 4
    +test_accept_addressing_operand $05, test_accept_addressing_operand_5, 1, $ff, 64, 0, 4
    +test_accept_addressing_operand $06, test_accept_addressing_operand_6, 1, 255, 64, 0, 4
    +test_accept_addressing_operand $07, test_accept_addressing_operand_7, 1, %01010101, 64, 0, 10
    +test_accept_addressing_operand $08, test_accept_addressing_operand_8, 1, $10ff, 32, 0, 6
    +test_accept_addressing_operand $09, test_accept_addressing_operand_9, 1, $00ff, 32, 0, 6
    +test_accept_addressing_operand $0A, test_accept_addressing_operand_10, 1, 256, 32, 0, 4

    +test_accept_addressing_operand $0B, test_accept_addressing_operand_11, 1, 255, 16, 0, 3
    +test_accept_addressing_operand $0C, test_accept_addressing_operand_12, 1, 255, 8, 0, 5
    +test_accept_addressing_operand $0D, test_accept_addressing_operand_13, 1, 255, 8, 0, 5
    +test_accept_addressing_operand $0E, test_accept_addressing_operand_14, 1, 255, 8, 0, 11
    +test_accept_addressing_operand $0F, test_accept_addressing_operand_15, 1, 255, 4, 0, 5
    +test_accept_addressing_operand $10, test_accept_addressing_operand_16, 1, 255, 4, 0, 5
    +test_accept_addressing_operand $11, test_accept_addressing_operand_17, 1, 255, 4, 0, 11

    +print_chr chr_cr
    +print_strlit_line "assemble-line"
    +test_assemble_line $01, test_assemble_line_1, 0, 0
    +test_assemble_line $02, test_assemble_line_2, 0, 0
    +test_assemble_line $03, test_assemble_line_3, 0, 0
    +test_assemble_line $04, test_assemble_line_4, 3, 9
    +test_assemble_line $05, test_assemble_line_5, 3, 10
    +test_assemble_line $06, test_assemble_line_6, 3, 7
    +test_assemble_line $07, test_assemble_line_7, 3, 8
    +test_assemble_line $08, test_assemble_line_8, 2, 10
    +test_assemble_line $09, test_assemble_line_9, 4, 7
    +test_assemble_line $0a, test_assemble_line_10, 4, 7
    +test_assemble_line $0b, test_assemble_line_11, 4, 10
    +test_assemble_line $0c, test_assemble_line_12, 4, 13
    +test_assemble_line $0d, test_assemble_line_13, 4, 14
    +test_assemble_line $0e, test_assemble_line_14, 4, 14
    +test_assemble_line $0f, test_assemble_line_15, 4, 14
    +test_assemble_line $10, test_assemble_line_16, 5, 7
    +test_assemble_line $11, test_assemble_line_17, 5, 7
    +test_assemble_line $12, test_assemble_line_18, 5, 6
    +test_assemble_line $13, test_assemble_line_19, 1, 10
