!cpu m65

; EasyAsm Attic map
attic_bank = $0870
; $08700000 - $08706000  EasyAsm program
attic_data_start = $6000

; KERNAL routines
bsout = $ffd2
primm = $ff7d

!macro kcall .kaddr {
    pha
    lda #0
    tab
    pla
    jsr .kaddr
    pha
    lda #$1e
    tab
    pla
}

!macro kprimm_start {
    pha
    lda #0
    tab
    pla
    jsr primm
}
!macro kprimm_end {
    pha
    lda #$1e
    tab
    pla
}

* = $2000    ; Actually $52000

    jmp dispatch
id_string:
    !pet "easyasm v0.1",0

; All entry into EasyAsm comes through here.
; MAPL = (E)500  MAPH = (8)300  B = $1Exx
; A = dispatch index (1-indexed)
dispatch:
    dec
    asl
    tax   ; X = (A-1)*2
    jmp (dispatch_jump,x)
dispatch_jump:
    !word assemble_to_memory
    !word assemble_to_disk
    !word restore_source

assemble_to_memory:
    +kprimm_start
    !pet "debug: assemble to memory",13,0
    +kprimm_end

    rts

assemble_to_disk:
    +kprimm_start
    !pet "debug: assemble to disk",13,0
    +kprimm_end

    rts

restore_source:
    +kprimm_start
    !pet "debug: restore source",13,0
    +kprimm_end

    rts


!if * > $7fff {
    !error "EasyAsm code has outgrown $5.2000-$5.7FFF"
}
