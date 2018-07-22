.nolist
#include "ti84pce.inc"
#define dw .dw ; convpng -> compiler compatibility
#define db .db

lineDistance .equ lcdWidth/10
lineXStart   .equ lineDistance/2

lineYStart   .equ 100
lineHeight   .equ lcdHeight - lineYStart

fishStart    .equ pixelShadow + 1
fishData0    .equ pixelShadow + 1 ; these hold the binary values with the fish
fishData1    .equ pixelShadow + 2
fishData2    .equ pixelShadow + 3
fishData3    .equ pixelShadow + 4
fishData4    .equ pixelShadow + 5
fishData5    .equ pixelShadow + 6
fishData6    .equ pixelShadow + 7

nextFish     .equ pixelShadow + 8
numFishRows  .equ 7 ; fishData0 to fishData6

playerPosOld .equ pixelShadow + 9
playerPos    .equ pixelShadow + 10

rng_seed_location .equ pixelShadow + 11

timerMax     .equ pixelShadow + 12
timer        .equ pixelShadow + 15

score        .equ pixelShadow + 18
multiplier   .equ pixelShadow + 21

health       .equ pixelShadow + 22

hudSize      .equ lcdWidth * 32

healthBarOffset     .equ lcdWidth * 8 + 80
healthSegmentWidth  .equ (2*(lcdWidth-80)/3)/8 ; 8 is health max
healthSegmentHeight .equ 8

piranhaSize  .equ 16

; palette equates
black        .equ $00
white        .equ $09
green        .equ $0a

playerHeight .equ 37 ; yeah it's a weird number, but it's what I ended up with.
playerWidth  .equ 16
.list
  
  .org UserMem-2
  .db tExtTok,tAsm84CeCmp
prgm_start:
  call _RunIndicOff ; we'll clear vRAM later
  
; <init palette>
  ld hl, palette
  ld de, mpLcdPalette
  ld bc, _palette_end-palette
  ldir
; </init palette>
  
  ld a, lcdBpp8
  ld (mpLcdCtrl), a
  
  ld hl, %1000100001110100 ; just 2 arbitrarily chosen bytes, generated with https://www.random.org/cgi-bin/randbyte?nbytes=2&format=b
  ld (rng_seed_location), hl
  
  ld hl, fishStart
  ld b, numFishRows
_init_nextRow:
  push bc
    push hl
      call rng
    pop hl
    ld (hl), a
    inc hl ; move to next row
  pop bc
  djnz _init_nextRow
  
  ld hl, $60ff
  ld (timerMax), hl
  
  ld a, 8
  ld (health), a
  
; draw health bar
  ld c, healthSegmentWidth
  ld a, (health)
  ld b, a
  mlt bc ; health * width per health
  ld a, healthSegmentHeight
  ld hl, vRAM + healthBarOffset
  ld de, vRAM + healthBarOffset + 1
_init_nextHealthBarLine:
  ld (hl), green
  push bc
    ldir
  pop bc
  push bc
    push hl
      ld hl, lcdWidth
      or a, a
      sbc hl, bc
      push hl
      pop bc
    pop hl
    add hl, bc
    ex de, hl
    add hl, bc
    ex de, hl
  pop bc
  dec a 
  jr nz, _init_nextHealthBarLine
  
  ld a, $1
  ld (playerPos), a
time_up:
  call moveFishies
  call drawFishies
  ld hl, (timerMax)
  ld (timer), hl
mainLoop:
  jp movePlayer
quit:
  ld a, lcdBpp16 ; 16 bit
  ld (mpLcdCtrl), a
  call	_DrawStatusBar
  ret
movePlayer:
  ld hl, (timer)
  ld de, 1
_getKeyCode:
  push hl
    call _GetCSC
  pop hl
  or a, a
  sbc hl, de ; de=1
  jr z, time_up
  ld (timer), hl
  or a, a ; set to zero if a is zero
  jr z, _getKeyCode
  dec a
  ld b, a
  ld a, (playerPos)
  djnz _chkRight
  ld b, a
  rrca
  ld (playerPos), a
  call redrawPlayer
  jr movePlayer
_chkRight:
  djnz _chkQuit
  ld b, a
  rlca
  ld (playerPos), a
  call redrawPlayer
  jr movePlayer
_chkQuit:
  djnz _getKeyCode
  jp quit
redrawPlayer:
; input of current player position in a
; previous position in b
  push af
    ld a, b
    call a_log_2
    ld d, lineDistance
    mlt de
    ld hl, (lineYStart - playerHeight - 4) * lcdWidth + vRAM + lineXStart - 8 ; playerwidth/2
    add hl, de
    ld de, lcdWidth - playerWidth
    ld a, playerHeight
playerEraseOuter:
    ld b, playerWidth
playerEraseInner:
    ld (hl), white
    inc hl
    djnz playerEraseInner
    add hl, de
    dec a
    jr nz, playerEraseOuter
  pop af
drawPlayer:
  call a_log_2
  ld d, lineDistance
  mlt de
  ld hl, (lineYStart - playerHeight - 4) * lcdWidth + vRAM + lineXStart - 8 ; playerwidth/2
  add hl, de
  ld a, playerHeight
  ld de, player_standing_sprite
  ex de, hl
playerDrawLoop:
  ld bc, playerWidth
  ldir
  ex de, hl
  ld bc, lcdWidth - playerWidth
  add hl, bc
  ex de, hl
  dec a
  jr nz, playerDrawLoop
  ret
  
drawFishies:
; draws our cute bloodthirsty monsters
; just like them, it destroys everything.
  
; erase
  ld hl, vRAM + hudSize
  ld de, vRAM + hudSize + 1
  ld bc, lcdWidth * lcdHeight - hudSize - 1
  ld (hl), white
  ldir
; draw player
  ld a, (playerPos)
  call drawPlayer
; draw lines
  or a, a
  ld bc, lineDistance
  ld hl, lineYStart * lcdWidth + vRAM + lineDistance + lineXStart
  ld a, lineHeight
_lineLoopOuter:
  push af
    ld a,8
_lineLoopInner:
    ld (hl), $01 ; grey-ish
    add hl, bc
    dec a
    jr nz, _lineLoopInner
  pop af
  add hl, bc
  add hl, bc
  dec a
  jr nz, _lineLoopOuter
  ld bc, numFishRows ; set c to numFishRows, clear upper byte. 
_nextRow:
  push bc
    ld hl, fishStart
    ld b, 0
    dec c
    add hl, bc
    ld a, (hl)
    ld h, lcdWidth/2
    ld l, c
    mlt hl
    add hl, hl ; get rid of /2 above
    add hl, hl
    add hl, hl
    push bc
      push hl
      pop bc
      add hl, hl
      add hl, hl ; multiply hl by 16
      add hl, bc ; add hl * 4, we are doing hl * 20
    pop bc
    ex de, hl
    ld b, 8
_nextFish:
    rlca
    jr nc, _noFish
_yesFish:
    push bc
      ld c, lineDistance
      mlt bc
      ld hl, lineYStart * lcdWidth + vRAM + (lineXStart - 8)
      add hl, bc
      add hl, de 
      push af
        ld a, piranhaSize
_fishLoopOuter:
        ld b, piranhaSize
_fishLoopInner:
        ld (hl), $02 ; red
        inc hl
        djnz _fishLoopInner
        ld bc, lcdWidth - piranhaSize
        add hl, bc
        dec a
        jr nz, _fishLoopOuter
      pop af
    pop bc
_noFish:
    djnz _nextFish
  pop bc
  ld b, c
  dec c
  djnz _nextRow
  ret
moveFishies:
  ld a, (fishData0)
  ld b, a
  ld a, (playerPos)
  and a, b
  jr z, doScoring
; damage player
  ld hl, health
  dec (hl)
; pop off the return location so we don't have to press up to quit
  pop hl ; prettifier-no-indent-change
  jp z, quit
; push it back on if we didn't run out of health so we can ret later.
  push hl ; prettifier-no-indent-change
; draw health bar change
  ld c, healthSegmentWidth
  ld a, (health)
  ld b, a
  mlt bc ; health * width per health
  ld a, healthSegmentHeight
  ld hl, vRAM + healthBarOffset
  ld de, vRAM + healthBarOffset + 1
  add hl, bc
  ex de, hl
  add hl, bc
  ex de, hl
_:
  ld (hl), white
  ld bc, healthSegmentWidth
  ldir
  ld bc, lcdWidth - healthSegmentWidth
  add hl, bc
  ex de, hl
  add hl, bc
  ex de, hl
  dec a
  jr nz, -_
doScoring:
; scoring
  ld hl, fishStart
  ld b, numFishRows + 1
  ld c, 0 ; this will be the change in score
_scoreFish:
  ld a, (playerPos)
  and (hl) ; hl is the row of fish
  inc hl ; this doesn't alter flags
  jr z, _moreFish
  inc c
_moreFish:
  djnz _scoreFish
  ld a, (multiplier)
  ld b, a
  mlt bc
  ld hl, (score)
  add hl, bc
  ld (score), hl
  call rng
  ld (nextFish), a
; shift the rows
  ld hl, fishData1
  ld de, fishData0
  ld bc, numFishRows
  ldir
  xor a, a
  ret
rng:
; generates a random number, %00000000 to %11111110 with the result in a
; slight bias towards %00000000
; destroys hl, bc
; to seed the rng (16-bit seed in hl):
; ld (rng_seed_location), hl
  ld b, 8 ; max output of rng = 2^b (0 <= b <= 7)
  ld hl, (rng_seed_location) ; seed rng with previous output
  ld c, $00 ; we'll be using c to hold the output, because we need a for other things.
_rng:
; A modified Galois LFSR. Nicely, the current
; example source code on wikipedia pretty much directly ports over.
  srl h ; shifts h, fills gap with 0. sets carry flag to what "fell off".
  rr l ; shifts l, fills gap with what fell off h.
  rl c ; output here, this is what's different from the wikipedia code. Carry flag (what fell off l) is pushed into bit 0 of c. I love this.
  bit 0, c
  jr nz, _
  ld a, h
  xor $B4 ; magic # from wiki article.
  ld h, a
_:
  djnz _rng
  ld (rng_seed_location), hl
  ld a, c
  inc c
  ret nz
  xor a, a
  ret
a_log_2:
  push af
    ld e,$00 
    inc e
    rrca 
    jr nc,$-2 
  pop af
  ret
code_end:
; code ends here, sprite stuff starts here. All of this was converted with ConvPNG
palette:
  dw 00000h ; 00 :: rgb(0,0,0)
  dw 0E690h ; 01 :: rgb(209,164,130)
  dw 04800h ; 02 :: rgb(146,0,0)
  dw 0624Dh ; 03 :: rgb(196,147,104)
  dw 01074h ; 04 :: rgb(37,23,163)
  dw 03C21h ; 05 :: rgb(122,10,12)
  dw 01C81h ; 06 :: rgb(59,31,10)
  dw 01861h ; 07 :: rgb(48,26,8)
  dw 09852h ; 08 :: rgb(51,22,145)
  dw 0FFFFh ; 09 :: rgb(255,255,255)
  dw 001E0h ; 0a :: rgb(0,255,0)
_palette_end:
player_standing_sprite:
  db 009h,009h,009h,009h,009h,009h,009h,009h,009h,009h,009h,009h,009h,009h,009h,009h
  db 009h,009h,009h,009h,009h,009h,009h,009h,009h,009h,009h,009h,006h,009h,009h,009h
  db 009h,009h,009h,009h,009h,009h,007h,006h,006h,006h,006h,006h,009h,009h,009h,009h
  db 009h,009h,009h,009h,009h,007h,007h,007h,007h,007h,006h,009h,009h,009h,009h,009h
  db 009h,009h,009h,009h,007h,007h,001h,001h,001h,001h,001h,009h,009h,009h,009h,009h
  db 009h,009h,009h,009h,007h,001h,009h,009h,001h,009h,009h,001h,009h,009h,009h,009h
  db 009h,009h,009h,009h,003h,001h,009h,000h,001h,000h,009h,001h,009h,009h,009h,009h
  db 009h,009h,009h,009h,003h,001h,001h,001h,001h,001h,001h,001h,009h,009h,009h,009h
  db 009h,009h,009h,009h,003h,001h,001h,000h,000h,000h,001h,001h,009h,009h,009h,009h
  db 009h,009h,009h,009h,009h,001h,000h,002h,002h,000h,001h,003h,009h,009h,009h,009h
  db 009h,009h,009h,009h,009h,003h,001h,001h,001h,001h,003h,009h,009h,009h,009h,009h
  db 009h,009h,009h,009h,009h,009h,003h,003h,003h,003h,009h,009h,009h,009h,009h,009h
  db 009h,009h,009h,009h,005h,005h,002h,003h,003h,002h,002h,009h,009h,009h,009h,009h
  db 009h,009h,005h,005h,002h,002h,002h,002h,002h,002h,002h,005h,005h,009h,009h,009h
  db 009h,005h,002h,002h,002h,002h,002h,002h,002h,002h,002h,002h,002h,005h,009h,009h
  db 009h,002h,002h,002h,002h,002h,002h,002h,002h,002h,002h,002h,002h,002h,009h,009h
  db 009h,002h,002h,005h,002h,002h,002h,002h,002h,002h,002h,005h,002h,002h,009h,009h
  db 009h,002h,002h,005h,002h,002h,002h,002h,002h,002h,002h,005h,002h,002h,009h,009h
  db 009h,002h,002h,005h,002h,002h,002h,002h,002h,002h,002h,005h,002h,002h,009h,009h
  db 009h,002h,002h,005h,002h,002h,005h,005h,005h,002h,002h,005h,002h,002h,009h,009h
  db 009h,002h,002h,005h,002h,005h,002h,002h,002h,005h,002h,005h,002h,002h,009h,009h
  db 009h,002h,002h,005h,002h,005h,005h,005h,005h,005h,002h,005h,002h,002h,009h,009h
  db 009h,002h,002h,005h,002h,002h,002h,002h,002h,002h,002h,005h,002h,002h,009h,009h
  db 009h,002h,002h,009h,008h,004h,004h,004h,004h,004h,004h,009h,002h,002h,009h,009h
  db 009h,003h,003h,009h,008h,004h,004h,004h,004h,004h,004h,009h,003h,003h,009h,009h
  db 009h,001h,001h,003h,008h,004h,004h,004h,004h,004h,004h,003h,001h,001h,009h,009h
  db 009h,003h,001h,009h,008h,004h,004h,009h,004h,004h,004h,009h,003h,003h,009h,009h
  db 009h,009h,009h,009h,008h,004h,009h,009h,009h,004h,004h,009h,009h,009h,009h,009h
  db 009h,009h,009h,009h,001h,001h,009h,009h,009h,003h,001h,009h,009h,009h,009h,009h
  db 009h,009h,009h,009h,001h,001h,009h,009h,009h,003h,001h,009h,009h,009h,009h,009h
  db 009h,009h,009h,009h,001h,003h,009h,009h,009h,003h,001h,009h,009h,009h,009h,009h
  db 009h,009h,009h,009h,001h,003h,009h,009h,009h,003h,001h,009h,009h,009h,009h,009h
  db 009h,009h,009h,009h,001h,003h,009h,009h,009h,003h,001h,009h,009h,009h,009h,009h
  db 009h,009h,009h,009h,001h,003h,009h,009h,009h,003h,001h,009h,009h,009h,009h,009h
  db 009h,009h,009h,009h,000h,000h,009h,009h,009h,003h,001h,009h,009h,009h,009h,009h
  db 009h,009h,009h,001h,001h,001h,009h,009h,009h,001h,001h,000h,000h,001h,009h,009h
  db 009h,009h,009h,000h,000h,000h,009h,009h,000h,000h,000h,000h,000h,000h,009h,009h
prgm_end:
.nolist
.echo "====="
.echo "Total program size: ", prgm_end-prgm_start, " bytes"
.echo "Code-only size: ", code_end-prgm_start, " bytes"
.echo "====="