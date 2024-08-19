!macro start_test_rellabel {
    jsr init_rellabel_table
}

!macro add_rellabel_for_test .plusminus, .length, .pc {
    !if .plusminus = 1 {
        sec
    } else {
        clc
    }
    lda #.length
    ldx #<.pc
    ldy #>.pc
    jsr add_rellabel
    +assert_cc test_msg_ecc
}

!macro test_add_rellabel .tnum, .ebytes_start, .ebytes_end {
    +test_start .tnum

    jsr start_rellabel_table
    ldx #.ebytes_end-.ebytes_start-1
-   txa
    taz
    lda [attic_ptr],z
    cmp .ebytes_start,x
    +assert_eq test_msg_wrong_result
    dex
    bpl -

    +test_end
}

!macro test_eval_rellabel .tnum, .plusminus, .length, .pc, .ec, .epc {
    +test_start .tnum

    !if .plusminus = 1 {
        sec
    } else {
        clc
    }
    lda #.length
    ldx #<.pc
    ldy #>.pc
    jsr eval_rellabel

    !if .ec {
        +assert_cs test_msg_ecs
        cpx #<.epc
        +assert_eq test_msg_wrong_pc
        cpy #>.epc
        +assert_eq test_msg_wrong_pc
    } else {
        +assert_cc test_msg_ecc
    }

    +test_end
}

test_add_rellabel_1: !byte $00, $00, $00, $00, $00, $00
test_add_rellabel_2: !byte $00, $00, $00, $83, $00, $16, $00, $00, $00
test_add_rellabel_3: !byte $00, $00, $00, $83, $00, $16, $02, $04, $16, $81, $08, $16, $00, $00, $00
test_add_rellabel_end:

run_test_suite_cmd:
    +print_strlit_line "-- test suite --"

    +print_chr chr_cr
    +print_strlit_line "test rellabels"

    +start_test_rellabel
    +test_add_rellabel $01, test_add_rellabel_1, test_add_rellabel_2

    +start_test_rellabel
    +add_rellabel_for_test 1, 3, $1600
    +test_add_rellabel $02, test_add_rellabel_2, test_add_rellabel_3

    +start_test_rellabel
    +add_rellabel_for_test 1, 3, $1600
    +add_rellabel_for_test 0, 2, $1604
    +add_rellabel_for_test 1, 1, $1608
    +test_add_rellabel $03, test_add_rellabel_3, test_add_rellabel_end

    +start_test_rellabel
    ; test_eval_rellabel $04, .plusminus, .length, .pc, .ec, .epc
    +test_eval_rellabel $04, 0, 1, $1600, 0, 0
    +test_eval_rellabel $05, 1, 1, $1600, 0, 0

    ; one entry, minus, same pc (yes)
    +start_test_rellabel
    +add_rellabel_for_test 1, 1, $1600
    +test_eval_rellabel $06, 1, 1, $1600, 1, $1600

    ; one entry, plus,  same pc (no)
    +start_test_rellabel
    +add_rellabel_for_test 0, 1, $1600
    +test_eval_rellabel $07, 0, 1, $1600, 0, 0

    ; one entry, minus, pc+1    (no)
    +start_test_rellabel
    +add_rellabel_for_test 1, 1, $1601
    +test_eval_rellabel $08, 1, 1, $1600, 0, 0

    ; one entry, plus,  pc+1    (yes)
    +start_test_rellabel
    +add_rellabel_for_test 0, 1, $1601
    +test_eval_rellabel $09, 0, 1, $1600, 1, $1601

    ; one entry, minus, pc-1    (yes)
    +start_test_rellabel
    +add_rellabel_for_test 1, 1, $15ff
    +test_eval_rellabel $0A, 1, 1, $1600, 1, $15ff

    ; one entry, plus,  pc-1    (no)
    +start_test_rellabel
    +add_rellabel_for_test 0, 1, $15ff
    +test_eval_rellabel $0B, 0, 1, $1600, 0, 0

    ; three entries, two match, minus...
    +start_test_rellabel
    +add_rellabel_for_test 1, 1, $15fe
    +add_rellabel_for_test 1, 1, $15ff
    +add_rellabel_for_test 1, 1, $1601
    +test_eval_rellabel $0C, 1, 1, $1600, 1, $15ff

    ; three entries, two match, plus...
    +start_test_rellabel
    +add_rellabel_for_test 0, 1, $1600
    +add_rellabel_for_test 0, 1, $1601
    +add_rellabel_for_test 0, 1, $1602
    +test_eval_rellabel $0D, 0, 1, $1600, 1, $1601

    ; three entries, zero matches, minus...
    +start_test_rellabel
    +add_rellabel_for_test 1, 2, $15fe
    +add_rellabel_for_test 1, 3, $15ff
    +add_rellabel_for_test 1, 1, $1601
    +test_eval_rellabel $0E, 1, 1, $1600, 0, 0

    ; three entries, zero matches, plus...
    +start_test_rellabel
    +add_rellabel_for_test 0, 1, $1600
    +add_rellabel_for_test 0, 2, $1601
    +add_rellabel_for_test 0, 3, $1602
    +test_eval_rellabel $0F, 0, 1, $1600, 0, 0

    +print_chr chr_cr
    +print_strlit_line "-- all tests passed --"
    rts
