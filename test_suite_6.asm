test_symbol_table:
    ; label
    !byte <attic_symbol_names, >attic_symbol_names, ^attic_symbol_names
    !byte F_SYMTBL_DEFINED
    !32 12345
    ; Alpha
    !byte <(attic_symbol_names+6), >(attic_symbol_names+6), ^(attic_symbol_names+6)
    !byte F_SYMTBL_DEFINED
    !32 23456
    ; Alph
    !byte <(attic_symbol_names+12), >(attic_symbol_names+12), ^(attic_symbol_names+12)
    !byte F_SYMTBL_DEFINED
    !32 34567
    ; Beta
    !byte <(attic_symbol_names+17), >(attic_symbol_names+17), ^(attic_symbol_names+17)
    !byte F_SYMTBL_DEFINED
    !32 45678
    ; BetaZ
    !byte <(attic_symbol_names+22), >(attic_symbol_names+22), ^(attic_symbol_names+22)
    !byte F_SYMTBL_DEFINED
    !32 56789
    ; GAMMA
    !byte <(attic_symbol_names+28), >(attic_symbol_names+28), ^(attic_symbol_names+28)
    !byte F_SYMTBL_DEFINED
    !32 99999
    ; (END)
    !byte 0,0,0,0,0,0,0,0
test_symbol_table_end:
test_symbol_table_last_addr = test_symbol_table_end-test_symbol_table+attic_symbol_table-SYMTBL_ENTRY_SIZE
test_symbol_names:
    !pet "label",0
    !pet "Alpha",0
    !pet "Alph",0
    !pet "Beta",0
    !pet "BetaZ",0
    !pet "GAMMA",0
test_symbol_names_end:

test_set_up_symbol_data:
    jsr init_symbol_table

    lda #<test_symbol_table
    sta code_ptr
    lda #>test_symbol_table
    sta code_ptr+1
    lda #<attic_symbol_table
    ldx #>attic_symbol_table
    ldy #^attic_symbol_table
    ldz #$08
    stq attic_ptr
    ldy #0
    ldz #0
-   lda (code_ptr),y
    sta [attic_ptr],z
    inw code_ptr
    inw attic_ptr
    lda code_ptr+1
    cmp #>test_symbol_names
    bcc -
    lda code_ptr
    cmp #<test_symbol_names
    bcc -

    lda #<test_symbol_names
    sta code_ptr
    lda #>test_symbol_names
    sta code_ptr+1
    lda #<attic_symbol_names
    ldx #>attic_symbol_names
    ldy #^attic_symbol_names
    ldz #$08
    stq attic_ptr
    ldy #0
    ldz #0
-   lda (code_ptr),y
    sta [attic_ptr],z
    inw code_ptr
    inw attic_ptr
    lda code_ptr+1
    cmp #>test_symbol_names_end
    bcc -
    lda code_ptr
    cmp #<test_symbol_names_end
    bcc -
    lda attic_ptr
    sta symtbl_next_name
    lda attic_ptr+1
    sta symtbl_next_name+1
    lda attic_ptr+2
    sta symtbl_next_name+2
    rts

!macro test_find_symbol .tnum, .str, .length, .ec, .eatticptr {
    +test_start .tnum

    jsr test_set_up_symbol_data

    ; Fake assembly location in bank 5
    lda #<.str
    sta bas_ptr
    lda #>.str
    sta bas_ptr+1
    lda #5
    sta bas_ptr+2
    lda #0
    sta bas_ptr+3

    ldx #.length
    jsr find_symbol

    ; (C set = fail)
!if .ec {
    bcs +
    brk
+
    lda #<test_symbol_table_last_addr
    ldx #>test_symbol_table_last_addr
    ldy #^test_symbol_table_last_addr
    ldz #$08
    cpq attic_ptr
    beq +
    brk
+
} else {
    bcc +
    brk
+
    lda #<.eatticptr
    ldx #>.eatticptr
    ldy #^.eatticptr
    ldz #$08
    cpq attic_ptr
    beq +
    brk
+
}

    +test_end
}

test_find_symbol_1: !pet "label = 999",0
test_find_symbol_2: !pet "Alpha",0
test_find_symbol_3: !pet "Alph",0
test_find_symbol_4: !pet "Beta",0
test_find_symbol_5: !pet "BetaZ",0
test_find_symbol_6: !pet "GAMMA",0
test_find_symbol_7: !pet "GAMMB",0

!macro test_find_or_add_symbol .tnum, .str, .length, .ec, .eatticptr {
    +test_start .tnum

    jsr test_set_up_symbol_data

    ; Fake assembly location in bank 5
    lda #<.str
    sta bas_ptr
    lda #>.str
    sta bas_ptr+1
    lda #5
    sta bas_ptr+2
    lda #0
    sta bas_ptr+3

    ldx #.length
    jsr find_or_add_symbol

    ; (C set = fail)
!if .ec {
    bcs +
    brk
+
    ; TODO: confirm out of memory conditions
} else {
    bcc +
    brk
+
    lda #<.eatticptr
    ldx #>.eatticptr
    ldy #^.eatticptr
    ldz #$08
    cpq attic_ptr
    beq +
    brk
+
    ; TODO: test symtbl_next_name has advanced
}

    +test_end
}

!macro test_get_symbol_value .tnum, .str, .length, .ec, .eq {
    +test_start .tnum

    jsr test_set_up_symbol_data

    ; Fake assembly location in bank 5
    lda #<.str
    sta bas_ptr
    lda #>.str
    sta bas_ptr+1
    lda #5
    sta bas_ptr+2
    lda #0
    sta bas_ptr+3

    ldx #.length
    jsr find_or_add_symbol
    jsr get_symbol_value

!if .ec {
    bcs +
    brk
+
} else {
    bcc +
    brk
+   cmp #<.eq
    beq +
    brk
+   cpx #>.eq
    beq +
    brk
+   cpy #^.eq
    beq +
    brk
+   cpz #<(.eq >>> 24)
    beq +
    brk
+
}

    +test_end
}

!macro test_set_symbol_value .tnum, .str, .length, .q {
    +test_start .tnum

    jsr test_set_up_symbol_data

    ; Fake assembly location in bank 5
    lda #<.str
    sta bas_ptr
    lda #>.str
    sta bas_ptr+1
    lda #5
    sta bas_ptr+2
    lda #0
    sta bas_ptr+3

    ldx #.length
    jsr find_or_add_symbol
    lda #<.q
    ldx #>.q
    ldy #^.q
    ldz #<(.q >>> 24)
    jsr set_symbol_value

    ldz #3
    lda [attic_ptr],z
    and #F_SYMTBL_DEFINED
    bne +
    brk
+   inz
    lda [attic_ptr],z
    cmp #<.q
    beq +
    brk
+   inz
    lda [attic_ptr],z
    cmp #>.q
    beq +
    brk
+   inz
    lda [attic_ptr],z
    cmp #^.q
    beq +
    brk
+   inz
    lda [attic_ptr],z
    cmp #<(.q >>> 24)
    beq +
    brk
+
    +test_end
}

!macro test_do_assemble_bytes .x {
!if .x > 0 {
    ldx #.x-1
-   txa
    sta strbuf,x
    dex
    bpl -
}
    ldx #.x
    jsr assemble_bytes
}

!macro test_assemble_bytes .tnum, .pass, .pc, .x, .ec, .epc {
    +test_start .tnum
    jsr init_segment_table
    lda #.pass
    sta pass
    jsr init_pass
!if .pc {
    ldx #<.pc
    ldy #>.pc
    jsr set_pc
}
    +test_do_assemble_bytes .x

!if .ec {
    bcs +
    brk
+
} else {
    bcc +
    brk
+
}

!if .epc {
    lda program_counter
    cmp #<.epc
    bne +
    lda program_counter+1
    cmp #>.epc
    bne +
    bra ++
+   brk
++
    lda next_segment_pc
    cmp #<.epc
    bne +
    lda next_segment_pc+1
    cmp #>.epc
    bne +
    bra ++
+   brk
++
}

!if .pass = $ff {
    lda #<attic_segments
    ldx #>attic_segments
    ldy #^attic_segments
    ldz #<(attic_segments >>> 24)
    stq attic_ptr
    ldz #0
    lda [attic_ptr],z
    cmp #<.pc
    beq +
    brk
+   inz
    lda [attic_ptr],z
    cmp #>.pc
    beq +
    brk
+   inz
    lda [attic_ptr],z
    cmp #<.x
    beq +
    brk
+   inz
    lda [attic_ptr],z
    cmp #>.x
    beq +
    brk
+   inz

    ldy #.x
    ldx #0
-   txa
    cmp [attic_ptr],z
    beq +
    brk
+
    inz
    inx
    dey
    bne -
}

    +test_end
}

!macro test_assemble_bytes_twice .tnum, .pass, .pc1, .pc2, .x, .epc, .edata, .edataend {
    +test_start .tnum
    jsr init_segment_table
    lda #.pass
    sta pass
    jsr init_pass
    ldx #<.pc1
    ldy #>.pc1
    jsr set_pc
    +test_do_assemble_bytes .x
    bcc +
    brk
+
    ldx #<.pc2
    ldy #>.pc2
    jsr set_pc
    +test_do_assemble_bytes .x
    bcc +
    brk
+
    lda program_counter
    cmp #<.epc
    bne +
    lda program_counter+1
    cmp #>.epc
    bne +
    bra ++
+   brk
++
    lda next_segment_pc
    cmp #<.epc
    bne +
    lda next_segment_pc+1
    cmp #>.epc
    bne +
    bra ++
+   brk
++
!if .pass = $ff {
    lda #<attic_segments
    ldx #>attic_segments
    ldy #^attic_segments
    ldz #<(attic_segments >>> 24)
    stq attic_ptr

    ldx #.edataend-.edata-1
-   txa
    taz
    lda .edata,x
    cmp [attic_ptr],z
    beq +
    brk
+
    dex
    bpl -
}
    +test_end
}

test_assemble_bytes_twice_1:
    !word $c000
    !word 10
    !byte 0, 1, 2, 3, 4, 0, 1, 2, 3, 4
test_assemble_bytes_twice_2:
    !word $c000
    !word 5
    !byte 0, 1, 2, 3, 4
    !word $d000
    !word 5
    !byte 0, 1, 2, 3, 4
test_assemble_bytes_twice_2_end:


!macro test_expect_token .tnum, .a, .tokbuf, .tokbufend, .ec, .etokpos {
    +test_start .tnum
    ldx #.tokbufend-.tokbuf
    dex
-   lda .tokbuf,x
    sta tokbuf,x
    dex
    bpl -

    lda #.a
    ldx #0
    stx tok_pos
    jsr expect_token
!if .ec {
    bcs +
    brk
+   ldx tok_pos
    beq +
    brk
+
} else {
    bcc +
    brk
+   ldx tok_pos
    cpx #.etokpos
    beq +
    brk
+
}

    +test_end
}

test_expect_token_1: !byte 0, $ff
test_expect_token_2: !byte 1, 4, 0, $ff
test_expect_token_end:

!macro test_expect_label .tnum, .tokbuf, .tokbufend, .ec, .etokpos, .ex, .ey {
    +test_start .tnum
    ldx #.tokbufend-.tokbuf
    dex
-   lda .tokbuf,x
    sta tokbuf,x
    dex
    bpl -

    ldx #0
    stx tok_pos
    jsr expect_label
!if .ec {
    bcs +
    brk
+   ldx tok_pos
    beq +
    brk
+
} else {
    bcc +
    brk
+   cpx #.ex
    beq +
    brk
+   cpy #.ey
    beq +
    brk
+   ldx tok_pos
    cpx #.etokpos
    beq +
    brk
+
}

    +test_end
}

test_expect_label_1: !byte 0, $ff
test_expect_label_2: !byte tk_label_or_reg, 99, 5, 0, $ff
test_expect_label_end:


; Input: X=segment count
; Output: Initialized segment table with X segments,
;   with starting addresses of $ii00 and legnth of 3,
;   created in descending order i=X to 1,
;   e.g. $3300, $2200, $1100. If X=0, no segments
;   are created.
set_up_segtable_for_test:
    phx
    jsr init_segment_table

    ; Three bytes in strbuf
    lda #1
    ldx #0
    sta strbuf,x
    inc
    inx
    sta strbuf,x
    inc
    inx
    sta strbuf,x

    ply
-   cpy #0
    beq +
    sty program_counter+1
    lda #0
    sta program_counter
    lda asm_flags
    ora #F_ASM_PC_DEFINED
    sta asm_flags
    phy
    ldx #3
    jsr assemble_bytes
    ply
    dey
    bra -

+   rts

!macro create_test_segment_tables .count {
    ldx #.count
    jsr set_up_segtable_for_test
}

!macro test_segment_traversal .tnum, .count {
    +test_start .tnum

    +create_test_segment_tables .count
    lda #0
    sta bas_ptr  ; repurposes bas_ptr.0 as a segment counter
    jsr start_segment_traversal
-   jsr is_end_segment_traversal
    bcs +
    inc bas_ptr
    jsr next_segment_traversal
    bra -
+   lda bas_ptr
    cmp #.count
    +assert_eq test_msg_wrong_result

    +test_end
}

!macro test_segment_overlap .tnum, .start, .length, .c, .set_expr_result, .ec {
    +test_start .tnum

    +create_test_segment_tables 5

    !if .set_expr_result {
        jsr start_segment_traversal
        jsr next_segment_traversal
        jsr next_segment_traversal
        ldq current_segment
        stq expr_result
    } else {
        lda #0
        sta expr_result
        sta expr_result+1
        sta expr_result+2
        sta expr_result+3
    }

    lda #<.start
    ldx #>.start
    ldy #<.length
    ldz #>.length
    !if .c { sec } else { clc }
    jsr does_a_segment_overlap
    !if .ec {
        +assert_cs test_msg_ecs
    } else {
        +assert_cc test_msg_ecc
    }

    +test_end
}

run_test_suite_cmd:
    +print_strlit_line "-- test suite --"

    +print_chr chr_cr
    +print_strlit_line "test-find-symbol"
    +test_find_symbol $01, test_find_symbol_1, 5, 0, attic_symbol_table+(8*0)
    +test_find_symbol $02, test_find_symbol_2, 5, 0, attic_symbol_table+(8*1)
    +test_find_symbol $03, test_find_symbol_3, 4, 0, attic_symbol_table+(8*2)
    +test_find_symbol $04, test_find_symbol_4, 4, 0, attic_symbol_table+(8*3)
    +test_find_symbol $05, test_find_symbol_5, 5, 0, attic_symbol_table+(8*4)
    +test_find_symbol $06, test_find_symbol_6, 5, 0, attic_symbol_table+(8*5)
    +test_find_symbol $07, test_find_symbol_7, 5, 1, 0

    +print_chr chr_cr
    +print_strlit_line "test-find-or-add-symbol"
    +test_find_or_add_symbol $01, test_find_symbol_3, 4, 0, attic_symbol_table+(8*2)
    +test_find_or_add_symbol $02, test_find_symbol_7, 5, 0, attic_symbol_table+(8*6)

    +print_chr chr_cr
    +print_strlit_line "test-get-symbol-value"
    +test_get_symbol_value $01, test_find_symbol_2, 5, 0, 23456
    +test_get_symbol_value $02, test_find_symbol_7, 5, 1, 0

    +print_chr chr_cr
    +print_strlit_line "test-set-symbol-value"
    +test_set_symbol_value $01, test_find_symbol_2, 5, 98765
    +test_set_symbol_value $02, test_find_symbol_7, 5, 87654

    +print_chr chr_cr
    +print_strlit_line "test-assemble-bytes"
    +test_assemble_bytes $01, 0, 0, 1, 1, 0  ; undefined PC is error
    +test_assemble_bytes $02, 0, $c000, 0, 0, 0  ; zero length is ok
    +test_assemble_bytes $03, 0, $c000, 5, 0, $c005
    +test_assemble_bytes $04, $ff, $c000, 5, 0, $c005
    +test_assemble_bytes_twice $05, 0, $c000, $c005, 5, $c00a, 0, 0
    +test_assemble_bytes_twice $06, 0, $c000, $d000, 5, $d005, 0, 0
    +test_assemble_bytes_twice $07, $ff, $c000, $c005, 5, $c00a, test_assemble_bytes_twice_1, test_assemble_bytes_twice_2
    +test_assemble_bytes_twice $08, $ff, $c000, $d000, 5, $d005, test_assemble_bytes_twice_2, test_assemble_bytes_twice_2_end

    +print_chr chr_cr
    +print_strlit_line "test-expect-token"
    +test_expect_token $01, 1, test_expect_token_1, test_expect_token_2, 1, 0
    +test_expect_token $02, 1, test_expect_token_2, test_expect_token_end, 0, 2
    +test_expect_token $03, 4, test_expect_token_2, test_expect_token_end, 1, 0

    +print_chr chr_cr
    +print_strlit_line "test-expect-label"
    +test_expect_label $01, test_expect_label_1, test_expect_label_2, 1, 0, 0, 0
    +test_expect_label $02, test_expect_label_2, test_expect_label_end, 0, 3, 99, 5

    +print_chr chr_cr
    +print_strlit_line "test-segment-traversal"
    +test_segment_traversal $01, 0
    +test_segment_traversal $02, 1
    +test_segment_traversal $03, 5

    +print_chr chr_cr
    +print_strlit_line "test-segment-overlap"

    ; $0100 cmp $0080 -> Z=0 C=1
    ; lda #$00 : sta expr_a : lda #$01 : sta expr_a+1 : lda #$80 : sta expr_a+2 : lda #$00 : sta expr_a+3
    ; +cmp16 expr_a, expr_a+2
    ; $007F cmp $0080 -> Z=0 C=0
    ; lda #$7F : sta expr_a : lda #$00 : sta expr_a+1 : lda #$80 : sta expr_a+2 : lda #$00 : sta expr_a+3
    ; +cmp16 expr_a, expr_a+2
    ; $0080 cmp $0080 -> Z=1 C=1
    ; lda #$80 : sta expr_a : lda #$00 : sta expr_a+1 : lda #$80 : sta expr_a+2 : lda #$00 : sta expr_a+3
    ; +cmp16 expr_a, expr_a+2

    ; Five test segments: 0100-0102, ..., 0500-0502
    ; .start, .length, .c, .set_expr_result, .ec
    +test_segment_overlap $01, $0204, $00ef, 0, 0, 0
    +test_segment_overlap $02, $0202, $00ef, 0, 0, 1
    +test_segment_overlap $03, $0204, $00fd, 0, 0, 1
    +test_segment_overlap $04, $0302, $00ef, 0, 1, 1
    +test_segment_overlap $05, $0302, $00ef, 1, 1, 0
    +test_segment_overlap $06, $0302, $00ef, 1, 0, 1
    +test_segment_overlap $07, $01ff, $00ef, 0, 0, 1

    +print_chr chr_cr
    +print_strlit_line "-- all tests passed --"
    rts
