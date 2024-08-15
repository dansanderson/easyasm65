; EasyAsm, an assembler for the MEGA65
; Copyright © 2024  Dan Sanderson
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;
; ---------------------------------------------------------
; easyasm-e : Dispatch routines
; ---------------------------------------------------------

!cpu m65

; EasyAsm uses page $1E00-$1EFF for dispatch code and variable space.
; Set EXPECTED_END_OF_DISPATCH according to BP variables declared in
; easyasm.asm. (On error, it may be able to raise this limit, just
; check easyasm.asm first.)
EXPECTED_END_OF_DISPATCH = $1e9f

* = $1e00

    ; $1E00: Launch menu.
    lda #$00
    ldx #$00
    ; $1E04: Run menu option A, argument X.
    pha
    phx
    bra install
    ; $1E08: Execute user's program.
    ; A/X = address
execute_user_program:
    sta user_address
    stx user_address+1
    jsr easyasm_to_sys
    jsr (user_address)
    jsr sys_to_easyasm
    rts
user_address: !byte $00, $00

install:
    ; Copy EasyAsm from $8700000 to $52000
    lda #0
    sta $d704
    lda #^install_dma
    sta $d702
    lda #>install_dma
    sta $d701
    lda #<install_dma
    sta $d705

    jsr sys_to_easyasm

    plx
    pla
    jsr $2000     ; $52000, EasyAsm dispatch

    jsr easyasm_to_sys
    rts

sys_to_easyasm:
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
    rts

easyasm_to_sys:
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
