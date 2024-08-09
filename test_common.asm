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

!macro print_str .addr {
    ldx #<.addr
    ldy #>.addr
    jsr print_cstr
}

!macro print_fail_line .addr {
    +print_str test_msg_fail
    +print_str .addr
    lda #13
    +kcall bsout
}

!macro test_start .tnum {
    +print_strlit "  test "
    lda #.tnum
    jsr print_hex8
}
!macro test_end {
    +print_strlit_line "... ok"
}

!macro assert_eq .msg {
    beq +
    +print_fail_line .msg
    brk
+
}

!macro assert_ne .msg {
    bne +
    +print_fail_line .msg
    brk
+
}

!macro assert_cc .msg {
    bcc +
    +print_fail_line .msg
    brk
+
}

!macro assert_cs .msg {
    bcs +
    +print_fail_line .msg
    brk
+
}

!macro assert_mem_eq_byte .addr, .val, .msg {
    lda .addr
    cmp #.val
    +assert_eq .msg
}

!macro assert_mem_eq_word .addr, .val, .msg {
    lda .addr
    cmp #<.val
    +assert_eq .msg
    lda .addr+1
    cmp #>.val
    +assert_eq .msg
}

!macro assert_mem_eq_32 .addr, .val, .msg {
    lda .addr
    cmp #<.val
    +assert_eq .msg
    lda .addr+1
    cmp #>.val
    +assert_eq .msg
    lda .addr+2
    cmp #^.val
    +assert_eq .msg
    lda .addr+3
    cmp #<(.val >>> 24)
    +assert_eq .msg
}

!macro assert_q_eq_32 .val, .msg {
    cmp #<.evalue
    +assert_eq .msg
    cpx #>.evalue
    +assert_eq .msg
    cpy #^.evalue
    +assert_eq .msg
    cpz #<(.evalue >>> 24)
    +assert_eq .msg
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

test_msg_fail: !pet "...fail: ",0
test_msg_ecs: !pet "expected carry set",0
test_msg_ecc: !pet "expected carry clear",0
test_msg_tokenize: !pet "tokenize failed",0
test_msg_wrong_err_code: !pet "wrong error code",0
test_msg_wrong_err_pos: !pet "wrong error pos",0
test_msg_wrong_mode: !pet "wrong mode",0
test_msg_wrong_result: !pet "wrong result",0
test_msg_wrong_flags: !pet "wrong flags",0
test_msg_wrong_tokpos: !pet "wrong tokpos",0
test_msg_tokpos_zero: !pet "expected tokpos zero",0
test_msg_wrong_pc: !pet "wrong pc",0
test_msg_wrong_strbuf: !pet "wrong strbuf",0
test_msg_expected_defined_pc: !pet "expected defined pc",0
test_msg_expected_undefined_pc: !pet "expected undefined pc",0
test_msg_wrong_z: !pet "wrong z",0
test_msg_wrong_value: !pet "wrong value",0
test_msg_expected_defined_value: !pet "expected defined value",0
test_msg_expected_undefined_value: !pet "expected undefined value",0
test_msg_expected_leading_zero_value: !pet "expected value leading zero",0
