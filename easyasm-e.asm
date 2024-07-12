!cpu m65

; EasyAsm uses page $1E00-$1EFF for dispatch code and variable space.
; Set EXPECTED_END_OF_DISPATCH according to BP variables declared in
; easyasm.asm. (On error, it may be able to raise this limit, just
; check easyasm.asm first.)
EXPECTED_END_OF_DISPATCH = $1e5f

* = $1e00

    ; $1E00: Assemble to memory.
    lda #$01
    !byte $2c    ; "bit" instruction skip trick
    ; $1E03: Assemble to disk.
    lda #$02
    !byte $2c
    ; $1E06: Restore source.
    lda #$03

    pha

    ; Copy EasyAsm from $8700000 to $52000
    lda #0
    sta $d704
    lda #^install_dma
    sta $d702
    lda #>install_dma
    sta $d701
    lda #<install_dma
    sta $d705

    ; MAP $2000-$7FFF to bank 5
    ; Keep KERNAL in $E000-$FFFF
    lda #$00
    ldx #$0f
    ldy #$00
    ldz #$0f
    map
    lda #$00
    ldx #$E5
    ldy #$00
    ldz #$83
    map
    eom

    ; Set B = $1Exx
    lda #$1e
    tab

    pla
    jsr $2000     ; $52000, EasyAsm dispatch

    ; Restore the SYS map
    lda #$00
    ldx #$E0
    ldy #$00
    ldz #$83
    map
    eom

    ; Restore B.
    lda #$00
    tab
    rts

install_dma:
    !byte $80, $87       ; src mb  = $(0)87
    !byte $81, $00       ; dest mb = $(0)00
    !byte $0b, $00
    !byte $00, $00, $60  ; $6000 bytes
    !byte $00, $00, $00  ; src  $(087)00000
    !byte $00, $20, $05  ; dest $(000)52000
    !byte $00, $00, $00


!if * > EXPECTED_END_OF_DISPATCH {
    !error "EasyAsm dispatch code ends at address ", *
}
