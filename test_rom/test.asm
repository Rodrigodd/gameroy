INCLUDE "hardware.inc"


SECTION "RST38", ROM0[$38]
    ret

SECTION "VBlank", ROM0[$40]
    reti

SECTION "LCD STAT", ROM0[$48]
    reti

SECTION "Timer", ROM0[$50]
    reti

SECTION "Serial", ROM0[$58]
    reti

SECTION "Joypad", ROM0[$60]
    reti

SECTION "Header", ROM0[$100]

	jp EntryPoint

	ds $150 - @, 0 ; Make room for the header

EntryPoint:
	; Shut down audio circuitry
	ld a, 0
	ld [rNR52], a

    ; Test Dissasembly

    ; Test if write invalidate reg A
    ld a, $03
    ld [$2000], a
    call EmptyFunction3
    ld b, $04
    ld a, b ; invalidate A
    ld [$2000], a
    call EmptyFunction4

    ; Test if call invalidate bank
    ld a, $03
    ld [$2000], a
    call EmptyFunction3
    call EmptyFunction3
    call ChangeBank ; invalidate bank
    call EmptyFunction2

    ld a, $04
    ld [$2000], a
    call EmptyFunction4
    call NotChangeBank ; invalidate bank, because dissasembler isn't smart
    call EmptyFunction4

    ; Test if read invalidate bank
    ld a, $04
    ld [$2000], a
    call EmptyFunction4

    ld [$C000], a
    ld a, [$C000] ; invalidate A
    ld [$2000], a
    call EmptyFunction4

    ; Test if write invalidate bank
    ld b, $20
    ld c, $00

    ld a, $03
    ld [$2000], a
    call EmptyFunction3

    ld [bc], a ; invalidate bank
    call EmptyFunction3

    ld a, $02
    ld [$2000], a
    call TestInsideBank

    rst $38

    jp Done

ChangeBank:
    ld a, $02
    ld [$2000], a
    ret


NotChangeBank:
    ret

Done:
	jp Done


SECTION "bank2", ROMX, BANK[2]

TestInsideBank:
    ; Test if dissasembler don't invalidate bank, inside a switchable bank

    ; Test if call invalidate bank
    call EmptyFunction2
    call NotChangeBank
    call EmptyFunction2

    ; Test if write invalidate bank
    ld b, $C0
    ld c, $00

    ld [bc], a
    call EmptyFunction2

    ret

EmptyFunction2:
    ret

SECTION "bank3", ROMX, BANK[3]
    ds 20
EmptyFunction3:
    ret


SECTION "bank4", ROMX, BANK[4]
    ds 30
EmptyFunction4:
    ret
