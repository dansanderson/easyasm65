!macro print_chr .chr {
    lda #.chr
    +kcall bsout
}

!macro print_strlit .strlit {
    +kprimm_start
    !pet .strlit,0
    +kprimm_end
}

!macro print_strlit_line .strlit {
    +kprimm_start
    !pet .strlit,13,0
    +kprimm_end
}

!macro test_start .tnum {
    +print_strlit "  test "
    lda #.tnum
    jsr print_hex8
}
!macro test_end {
    +print_strlit_line "... ok"
}

; Input: code_ptr = first char of null-terminated string
; Output: strbuf = same string
; (This is similar to code_to_strbuf except it copies from code
; memory, i.e. test data.)
copy_ptr_to_strbuf:
    ldx #0
    ldy #0
-   lda (code_ptr),y
    sta strbuf,x
    beq +
    iny
    inx
    bra -
+   rts

; Input: code_ptr = first char of null-terminated string
; Output: Z=0 if matches strbuf exactly, otherwise Z=1
match_ptr_strbuf:
    ldx #0
    ldy #0
-   lda (code_ptr),y
    cmp strbuf,x
    bne +
    cmp #0
    beq +
    iny
    inx
    bra -
+   rts

!macro test_copy_to_strbuf .straddr {
    lda #<.straddr
    sta code_ptr
    lda #>.straddr
    sta code_ptr+1
    jsr copy_ptr_to_strbuf
}

!macro test_match_strbuf .straddr {
    lda #<.straddr
    sta code_ptr
    lda #>.straddr
    sta code_ptr+1
    jsr match_ptr_strbuf
}
