.nolist
#include "ti84pce.inc"
#define dw .dw ; convpng -> compiler compatibility
#define db .db

; text width information
_text_title_width   .equ 170
_text_play_width    .equ 42
_text_exit_width  .equ 31
_text_credits_width .equ 159

; snail positions
snailStart    .equ pixelShadow + 1
snailData0    .equ pixelShadow + 1
snailData1    .equ pixelShadow + 2
snailData2    .equ pixelShadow + 3
snailData3    .equ pixelShadow + 4
snailData4    .equ pixelShadow + 5

; other snail position equates
nextSnail     .equ pixelShadow + 6
numSnailRows  .equ 5 ; snailData0 to snailData6

; player positioning locations
playerPosOld  .equ pixelShadow + 9
playerPos     .equ pixelShadow + 10

; rng seed location
rng_seed_location .equ pixelShadow + 11

; timer locations
timerMax      .equ pixelShadow + 15
timer         .equ pixelShadow + 16
moreTimer     .equ pixelShadow + 17
evenMoreTimer .equ pixelShadow + 18

; scoring locations
score         .equ pixelShadow + 19 ; 24 bits
multiplier    .equ pixelShadow + 22
round         .equ pixelShadow + 23 ; 8 bits, loops every 8 rounds.

health        .equ pixelShadow + 24

; other
tempStorage   .equ pixelShadow + 25 ; 24 bits, used in font routine

; hud equates
hudSize       .equ lcdWidth * 32

; health bar equates
healthBarOffset     .equ lcdWidth * 8 + 80
healthSegmentWidth  .equ (2*(lcdWidth-80)/3)/8 ; 8 is health max
healthSegmentHeight .equ 8

; menu text equates
menuTextLeft .equ (lcdWidth/2)-_text_play_width
menuTextTop  .equ 64

; carat equates
caratWidth   .equ $07 ; width of '>' char
caratChar    .equ $19 ; '>'
caratPos     .equ pixelShadow + 28

; snail equates
snailHeight  .equ 15
snailWidth   .equ 6
snailXOffset .equ 4
snailSpacing .equ 5

; font sizing equates
glyphHeight  .equ 12
glyphSpacing .equ 1

; palette equates
black        .equ $00
white        .equ $09
green        .equ $0a

; player size equates
playerHeight .equ 37 - 1 ; yeah it's a weird number, but it's what I ended up with.
playerWidth  .equ 16 - 2
spriteCenteringConstant .equ 8

; snail column equates
lineDistance  .equ lcdWidth/10
lineXStart    .equ lineDistance/2

lineHeight    .equ (snailHeight + snailSpacing) * numSnailRows
lineYStart    .equ lcdHeight - lineHeight - 16 ; 16 pixels of padding on the bottom

; animation timings - each tick is effectively 86 clock cycles, or about 1.8ms
snailAttackFrame .equ 255 ; 0-255 "ticks" (not CC's). Time between redraws of the animation
snailAttackPause .equ 255 ; 0-255 "ticks". Time before snail attack animation plays

; this is arbitrary, it's just the starting "difficulty"
; if you decrease it, the game will be harder from the start
; increasing it does the opposite (it'll be easier)
startingDifficulty  .equ 255
.list
  
  .org UserMem-2
  .db tExtTok,tAsm84CeCmp
prgm_start:
init:
	call	_RunIndicOff ; we'll clear vRAM later
  
; initialize font:
	ld	hl, pixelShadow2-1
	ld	de, _font_start-1
	ld	bc, _font_end-_font_start
	inc	b
_init_font__next_byte:
	push	bc
	inc	de
	ld	a, (de)
	ld	b, $8
_init_font__next_bit:
	inc	hl
	ld	(hl), white
	rlca
	jr	nc, _
	ld	(hl), black
_:
	djnz	_init_font__next_bit
	pop	bc
	dec	c
	jr	nz, _init_font__next_byte
	djnz	_init_font__next_byte
  
; <init palette>
	ld	hl, palette
	ld	de, mpLcdPalette
	ld	bc, _palette_end-palette
	ldir
; </init palette>
  
	ld	a, lcdBpp8
	ld	(mpLcdCtrl), a
	xor	a, a
	ld	(caratPos), a
  
main_menu:
	ld	hl, vRAM
	ld	de, vRAM+1
	ld	bc, lcdWidth * lcdHeight - 1
	ld	(hl), white
	ldir
  
	ld	hl, _text_title_start
	ld	bc, _text_title_end-_text_title_start
	call	load_text
	ld	hl, (lcdWidth-_text_title_width)/2
	ld	e, 0
	call	draw_text
  
	ld	hl, _text_play_start
	ld	bc, _text_play_end-_text_play_start
	call	load_text
	ld	hl, menuTextLeft
	ld	e, menuTextTop
	call	draw_text
  
	ld	hl, _text_exit_start
	ld	bc, _text_exit_end-_text_exit_start
	call	load_text
	ld	hl, menuTextLeft + caratWidth ; align it with the "PLAY" sign.
	ld	e, menuTextTop+glyphHeight
	call	draw_text
  
	ld	hl, _text_credits_start
	ld	bc, _text_credits_end-_text_credits_start
	call	load_text
	or	a, a
	sbc	hl, hl
	ld	(score), hl ; hl is set to zero, why not use it to initialize the score and save some bytes.
	ld	e, lcdHeight-glyphHeight
	call	draw_text
  
_menu_key_wait:
	call	_GetCSC
	or	a, a
	jr	z, _menu_key_wait
	cp	a, skDown
	jr	nz, _
	call	_menu_swap_cursor
	jr	_menu_key_wait
_:
	cp	a, skUp
	jr	nz, _
	call	_menu_swap_cursor
	jr	_menu_key_wait
_:
	cp	a, skEnter
	jr	nz, _menu_key_wait
	jr	menu_end
_menu_swap_cursor:
; swaps the carat
; destroys all
	ld	a, (caratPos)
	ld	l, a
	ld	h, glyphHeight
	mlt	hl
	ld	h, lcdWidth/2
	mlt	hl
	add	hl, hl
	ld	de, lcdWidth * menuTextTop + menuTextLeft + vRAM
	add	hl, de
	push	hl
	pop	de
	inc	de
	ld	b, glyphHeight
_menu_erase_nextRow:
	push	bc
	ld	b, 0
	ld	(hl), white
	ld	c, caratWidth-1
	ldir
	ld	bc, lcdWidth-caratWidth+1
	add	hl, bc
	ex	de, hl
	add	hl, bc
	ex	de, hl
	pop	bc
	djnz	_menu_erase_nextRow
	ld	a, (caratPos)
	xor	a, 1
	ld	(caratPos), a
	ld	l, a
	ld	h, glyphHeight
	mlt	hl
	ld	a, l
	add	a, menuTextTop
	ld	e, a
	ld	a, caratChar
	ld	(OP1), a
	ld	a, b
	ld	b, 1
	ld	hl, menuTextLeft
	call	draw_text
	ret
menu_end:
	ld	a, (caratPos)
	xor	a, 1
	jp	z, quit
; clear the hud area.
	ld	hl, vRAM
	ld	de, vRAM + 1
	ld	(hl), white
	ld	bc, hudSize
	ldir
  
	ld	hl, %1000100001110100 ; just 2 arbitrarily chosen bytes, generated with https://www.random.org/cgi-bin/randbyte?nbytes=2&format=b
	ld	(rng_seed_location), hl
  
	ld	hl, snailStart
	ld	b, numSnailRows
_init_nextRow:
	push	bc
	push	hl
	call	rng
	pop	hl
	ld	(hl), a
	inc	hl ; move to next row
	pop	bc
	djnz	_init_nextRow
  
	ld	a, startingDifficulty
	ld	(timerMax), a
  
	ld	a, 8
	ld	(health), a
  
	ld	a, 1
	ld	(round), a
	ld	(multiplier), a
	ld	(evenMoreTimer), a
	ld	(playerPos), a
	call	drawMultiplier
  
; draw health bar
	ld	c, healthSegmentWidth
	ld	b, 8
	mlt	bc ; health * width per health
	ld	a, healthSegmentHeight
	ld	hl, vRAM + healthBarOffset
	push	hl
	pop	de
	inc	de
_init_nextHealthBarLine:
	ld	(hl), green
	push	bc
	ldir
	pop	bc
	push	bc
	push	hl
	ld	hl, lcdWidth
	or	a, a
	sbc	hl, bc
	push	hl
	pop	bc
	pop	hl
	add	hl, bc
	ex	de, hl
	add	hl, bc
	ex	de, hl
	pop	bc
	dec	a 
	jr	nz, _init_nextHealthBarLine
	push	af ; prettifier-no-indent-change (bit of a hack)
time_up:
	pop	af ; prettifier-no-indent-change
	call	moveSnailies
	call	drawSnailies
	ld	a, 20
	ld	(moreTimer), a
	ld	a, (timerMax)
	ld	(timer), a
	ld	hl, round
	ld	de, 1
	ld	a, (hl)
	rlca
	ld	(hl), a
	jr	nc, mainLoop
	ld	hl, timerMax
; if it got to here, the round counter looped and we should increase the difficulty.
	dec	(hl)
	ld	hl, multiplier
	inc	(hl)
	call	drawMultiplier
mainLoop:
	jp	movePlayer
quit:
	ld	a, lcdBpp16 ; 16 bits per pixel
	ld	(mpLcdCtrl), a
; clear screen
	ld	hl, vRAM
	ld	de, vRAM + 1
	ld	bc, lcdWidth * lcdHeight
	ld	(hl), $ff ; white
	ldir
; draw status bar
	call	_DrawStatusBar
	ret
movePlayer:
	ld	a, (timer)
	ld	l, a
	ld	a, (moreTimer)
	ld	h, a
_getKeyCode:
	push	hl
	call	_GetCSC
	ld	hl, evenMoreTimer
	rlc	(hl)
	pop	hl
	jr	nc, _gck_skip
	push	af
	dec	l
	jr	nz, _
	dec	h
	jr	z, time_up
; shift the snails up a row.
	push	hl
	ld	hl, (lineYStart - 20) * lcdWidth + vRAM ; 20 comes from snail height + snail spacing
	ld	de, (lineYStart - 21) * lcdWidth + vRAM
	ld	bc, (lineHeight + 20) * lcdWidth
	ldir
	pop	hl
	ld	a, h
	ld	(moreTimer), a
	ld	a, (timerMax)
	ld	l, a
_:
	ld	a, l
	ld	(timer), a
	pop	af
_gck_skip:
	or	a, a ; update z flag
	jr	z, _getKeyCode
	dec	a
	ld	b, a
	push	hl ; prettifier-no-indent-change
	ld	hl, playerPos
	djnz	_chkRight
	ld	b, (hl)
	rrc	(hl)
	pop	hl ; prettifier-no-indent-change
	call	redrawPlayer
	jr	movePlayer
_chkRight:
	djnz	_chkQuit
	ld	b, (hl)
	rlc	(hl)
	pop	hl ; prettifier-no-indent-change
	call	redrawPlayer
	jr	movePlayer
_chkQuit:
	pop	hl ; prettifier-no-indent-change
	djnz	_getKeyCode
	jp	quit
erasePlayer:
	ld	a, (playerPos)
	ld	b, a
	xor	a, a ; set zero flag
redrawPlayer:
; previous position in b
; for erase only, set zero flag
	push	af ; preserve flags
	ld	a, b
	call	playerPosConvert
	ld	de, lcdWidth - playerWidth
playerEraseOuter:
	ld	b, playerWidth
playerEraseInner:
	ld	(hl), white
	inc	hl
	djnz	playerEraseInner
	add	hl, de
	dec	a
	jr	nz, playerEraseOuter
	pop	af
	ret	z
drawPlayer:
	ld	a, (playerPos)
	call	playerPosConvert
	ld	de, player_standing_sprite
	ex	de, hl
playerDrawLoop:
	ld	bc, playerWidth
	ldir
	ex	de, hl
	ld	bc, lcdWidth - playerWidth
	add	hl, bc
	ex	de, hl
	dec	a
	jr	nz, playerDrawLoop
	ret
playerPosConvert:
; converts the playerPos into a position in vRAM
; inputs:
;   playerPos in a
; results:
;   hl = pointer to player location in vRAM
;   de destroyed
;   a = playerHeight
  
_a_log_2:
; gets a log_2, assuming a is of the form 2^n
; output in e, because it's used as part of a subroutine.
; corrupts de (ofc)
	push	af
	ld	e,$00
	inc	e
	rrca	
	jr	nc,$-2
	pop	af 
; </a_log_2>
	ld	d, lineDistance
	mlt	de
	ld	hl, (lineYStart - playerHeight - snailSpacing - snailHeight - 4) * lcdWidth + vRAM + lineXStart - spriteCenteringConstant ; 4 is padding
	add	hl, de
	ld	a, playerHeight
	ret
drawSnailies:
; draws our cute bloodthirsty monsters
; just like them, it destroys everything.
  
; erase
	ld	hl, vRAM + hudSize
	ld	de, vRAM + hudSize + 1
	ld	bc, lcdWidth * lcdHeight - hudSize - 1
	ld	(hl), white
	ldir
; draw player
	call	drawPlayer
	ld	bc, numSnailRows ; set c to numSnailRows, clear upper byte. 
_nextRow:
	push	bc
	ld	hl, snailStart
	ld	b, 0
	dec	c
	add	hl, bc
	ld	a, (hl)
	ld	hl, lcdWidth/2 * 256 ; clear hlu, set h to lcdWidth/2
	ld	l, c
	mlt	hl
	add	hl, hl ; get rid of /2 above
	add	hl, hl
	add	hl, hl
	push	bc
	push	hl
	pop	bc
	add	hl, hl
	add	hl, hl ; multiply hl by 16
	add	hl, bc ; add hl * 4, we are doing hl * 20
	pop	bc
	ex	de, hl
	ld	b, 8
_nextSnail:
	rlca
	jr	nc, _noSnail
_yesSnail:
	push	bc
	push	de
	ld	c, lineDistance
	mlt	bc
	ld	hl, lineYStart * lcdWidth + vRAM + (lineXStart - spriteCenteringConstant) + snailXOffset
	add	hl, bc
	add	hl, de
	ex	de, hl
	ld	bc, 0
	push	af
	ld	hl, snail_sprite
	ld	a, snailHeight
_snailLoop:
	ld	c, snailWidth
	ldir
	ld	bc, lcdWidth - snailWidth
	ex	de, hl
	add	hl, bc
	ex	de, hl
	ld	b, 0
	dec	a
	jr	nz, _snailLoop
	pop	af
	pop	de
	pop	bc
_noSnail:
	djnz	_nextSnail
	pop	bc
	ld	b, c
	dec	c
	djnz	_nextRow
	ret
moveSnailies:
	ld	a, (snailData0)
	ld	b, a
	ld	a, (playerPos)
	and	a, b
	jr	z, doScoring
; damage player
	ld	hl, health
	dec	(hl)
; clear multiplier + counter
	ld	a, 1
	ld	(multiplier), a
	ld	(round), a
; clear round counter
; pop off the return location so we don't have to press up to quit
	pop	hl ; prettifier-no-indent-change
	jp	z, quit
; push it back on if we didn't run out of health so we can ret later.
	push	hl ; prettifier-no-indent-change
; draw multiplier change
	call	drawMultiplier
; draw health bar change
	ld	c, healthSegmentWidth
	ld	a, (health)
	ld	b, a
	mlt	bc ; health * width per health
	ld	a, healthSegmentHeight
	ld	hl, vRAM + healthBarOffset
	ld	de, vRAM + healthBarOffset + 1
	add	hl, bc
	ex	de, hl
	add	hl, bc
	ex	de, hl
_:
	ld	(hl), white
	ld	bc, healthSegmentWidth
	ldir
	ld	bc, lcdWidth - healthSegmentWidth
	add	hl, bc
	ex	de, hl
	add	hl, bc
	ex	de, hl
	dec	a
	jr	nz, -_
doScoring:
; scoring
	ld	hl, snailStart
	ld	b, numSnailRows + 1
	ld	c, 0 ; this will be the change in score
_scoreSnail:
	ld	a, (playerPos)
	and	a, (hl) ; hl is the row of snails
	inc	hl ; this doesn't alter flags
	jr	z, _moreSnail
	inc	c
_moreSnail:
	djnz	_scoreSnail
	ld	a, (multiplier)
	ld	b, a
	mlt	bc
	ld	hl, (score)
	add	hl, bc
	ld	(score), hl
	push	bc
	push	de
	push	hl
	pop	bc
	xor	a, a
	ld	de, 0
	call	drawScore
	pop	de
	pop	bc
	call	rng
	ld	(nextSnail), a
; shift the rows
	ld	hl, snailData1
	ld	de, snailData0
	ld	bc, numSnailRows
	ldir
; animations:
	call	wait
	ld	b, 10
_attackAnimation:
; leap forward
	push	bc
	call	erasePlayer
	ld	hl, (lineYStart - 30) * lcdWidth + vRAM ; 30 comes from snail height + snail spacing + the 10 spaces we are shifting them up
	ld	de, (lineYStart - 31) * lcdWidth + vRAM
	ld	bc, (snailHeight + snailSpacing + 11) * lcdWidth
	ldir
	call	drawPlayer
	call	wait
	pop	bc
	djnz	_attackAnimation
	call	wait
	ld	b, 10
_attackAnimation_2:
; leap back
	push	bc
	call	erasePlayer
	ld	hl, (lineYStart - 1) * lcdWidth + vRAM ; 30 comes from snail height + snail spacing + the 10 spaces we are shifting them up
	ld	de, lineYStart * lcdWidth + vRAM
	ld	bc, (snailHeight + snailSpacing + 10) * lcdWidth
	lddr
	call	drawPlayer
	call	wait
	pop	bc
	djnz	_attackAnimation_2
	ld	b, (lineDistance * 8 + lineXStart)/2
_attackAnimation_3:
; go out to the left
	push	bc
	ld	b, snailHeight
_shiftLeftNextRow:
	push	bc
	ld	c, lcdWidth/2
	mlt	bc
	ld	hl, (lineYStart - snailHeight - snailSpacing) * lcdWidth + vRAM + 2
	or	a, a
	adc	hl, bc
	adc	hl, bc
	push	hl
	pop	de
	dec	de
	dec	de ; de = hl - 2
	ld	bc, lineDistance * 8 + lineXStart + 3 ; +3 is to make sure it erases properly
	ldir
	pop	bc
	djnz	_shiftLeftNextRow
	ld	b, 128
	call	customWait
	pop	bc
	djnz	_attackAnimation_3
attackAnimationEnd:
	xor	a, a
	ret
rng:
; generates a random number, %00000000 to %11111110 with the result in a
; slight bias towards %00000000
; destroys hl, bc
; to seed the rng (16-bit seed in hl):
; ld (rng_seed_location), hl
	ld	b, 8 ; max output of rng = 2^b (0 <= b <= 7)
	ld	hl, (rng_seed_location) ; seed rng with previous output
	ld	c, $00 ; we'll be using c to hold the output, because we need a for other things.
_rng:
; A modified Galois LFSR. Nicely, the current
; example source code on wikipedia pretty much directly ports over.
	srl	h ; shifts h, fills gap with 0. sets carry flag to what "fell off".
	rr	l ; shifts l, fills gap with what fell off h.
	rl	c ; output here, this is what's different from the wikipedia code. Carry flag (what fell off l) is pushed into bit 0 of c. I love this.
	bit	0, c
	jr	nz, _
	ld	a, h
	xor	$B4 ; magic # from wiki article.
	ld	h, a
_:
	djnz	_rng
	ld	(rng_seed_location), hl
	ld	a, c
	inc	c
	ret	nz
	xor	a, a
	ret
load_text:
; loads text into op1
; length in bc
; pointer to start of text in hl
; destroys de, hl=hl+bc, b = length of text
	push	bc
	ld	de, OP1
	ldir
	pop	bc
	ld	b, c
	ret
draw_text:
; draws text to the screen
; text in OP1, length in b
; text should be reversed! (i.e. "ABC" in OP1 draws as "CBA" on-screen)
; position is (hl, e), zero-indexed to the top left of the screen
; position = top left of character
; uses SnailFont
	ld	d, lcdWidth/2
	mlt	de
	ex	de, hl
	add	hl, hl
	add	hl, de
	ld	de, vRAM
	add	hl, de
	ex	de, hl ; de = position in vRAM
_draw_next_char:
	push	bc
	push	de
	ld	c, b
	ld	b, 0
	ld	hl, OP1
	dec	hl
	add	hl, bc
	ld	c, (hl)
	push	bc
	pop	hl
	ld	(tempStorage), hl
	ld	h, 17*8
	mlt	hl
	ld	bc, pixelShadow2
	add	hl, bc
	ld	bc, glyphHeight * 256 ; bcu=0; c=0; b=glyphHeight
	pop	de
_next_char_row:
	push	bc
	ld	b, 0
	push	hl
	push	bc
	ld	hl, (tempStorage)
	ld	bc, _char_widths
	add	hl, bc
	pop	bc
	ld	a, (hl)
	ld	c, a
	pop	hl
	ldir
	push	hl
	ex	de, hl
	ld	de, lcdWidth
	add	hl, de
	ld	c, a
	or	a, a
	sbc	hl, bc
	ex	de, hl
	pop	hl
	pop	bc
	djnz	_next_char_row
	or	a, a
	ld	bc, lcdWidth * glyphHeight - glyphSpacing
	push	hl
	push	bc
	pop	hl
	ld	b, 0
	ld	c, a
	or	a, a
	sbc	hl, bc
	push	hl
	pop	bc
	pop	hl
	ex	de, hl
	sbc	hl, bc
	ex	de, hl
	pop	bc
	djnz	_draw_next_char
	ret
drawScore:
; draws the decimal equivalent of the number in bc, 
; padded with zeros to be exactly 4 digits long
; position is (de, a), zero-indexed to the top left of the screen
; position = top left of character
; uses SnailFont
	ld	l, a
	ld	h, lcdWidth/2
	mlt	hl
	add	hl, hl
	add	hl, de
	ld	de, vRAM
	add	hl, de
	ex	de, hl
	push	de
; bc -> OP1
	push	bc
	pop	hl
	ld	b, 4
_calculateNextDigit:
	call	_DivHLBy10_s
	ld	d, a
	ld	e, 12 ; size of each char, in bytes
	mlt	de
	push	hl
	push	bc
	push	af
	ld	a, 5
	sub	b
	ld	b, a
	call	_OP1plusB
	pop	af
	pop	bc
	ld	(hl), e
	pop	hl
	djnz	_calculateNextDigit
	pop	de
	ld	b, 4
_loadNextNumber:
	call	_OP1plusB
	push	bc
	ld	c, (hl)
	ld	b, 8
	mlt	bc ; bits->bytes
	ld	hl, numberCharsOffset+pixelShadow2
	add	hl, bc ; hl = location of expanded char in memory
	ld	b, glyphHeight
_drawNextNumberRow:
	push	bc
	ld	bc, 8
	ldir
	push	hl
	ex	de, hl
	ld	de, lcdWidth-8 ; 8 = number width
	add	hl, de
	ex	de, hl
	pop	hl
	pop	bc
	djnz	_drawNextNumberRow
_nextNumber:
	ld	bc, (glyphHeight - 1) * lcdWidth - glyphSpacing + lcdWidth - 8
	ex	de, hl
	or	a, a
	sbc	hl, bc
	ex	de, hl
	pop	bc
	djnz	_loadNextNumber
	ret
_OP1plusB:
; gets the position of the Bth char in OP1, and puts it into hl.
; preserves bc
	ld	hl, OP1
	push	bc
	ld	c, b
	ld	b, 0
	add	hl, bc
	pop	bc
	ret
drawMultiplier:
	ld	a, $0d ; "x"
	ld	(OP1), a
	ld	b, 1
	ld	hl, 0
	ld	e, glyphHeight
	call	draw_text
	ld	de, 7 ; width of "x"
	ld	bc, 0
	ld	a, (multiplier)
	ld	c, a
	ld	a, glyphHeight
	call	drawScore
	ret
wait:
; wait routine
; waits for about 1 ms (gg)
	ld	b, 255
customWait:
_:
	ex	(sp), hl
	ex	(sp), hl
	ex	(sp), hl
	ex	(sp), hl
	ex	(sp), hl
	ex	(sp), hl
	ex	(sp), hl
	ex	(sp), hl ; ex (sp), hl is the same length (in bytes) as nop, but it takes longer :)
	djnz	-_
	ret
code_end:
; code ends here, sprite stuff starts here. All of this was converted with ConvPNG
palette:
	dw	00000h ; 00 :: rgb(0,0,0)
	dw	0E690h ; 01 :: rgb(209,164,130)
	dw	04800h ; 02 :: rgb(146,0,0)
	dw	0624Dh ; 03 :: rgb(196,147,104)
	dw	01074h ; 04 :: rgb(37,23,163)
	dw	03C21h ; 05 :: rgb(122,10,12)
	dw	01C81h ; 06 :: rgb(59,31,10)
	dw	01861h ; 07 :: rgb(48,26,8)
	dw	09852h ; 08 :: rgb(51,22,145)
	dw	0FFFFh ; 09 :: rgb(255,255,255)
	dw	001E0h ; 0a :: rgb(0,255,0)
_palette_end:
player_standing_sprite:
	db	009h,009h,009h,009h,009h,009h,009h,009h,009h,009h,009h,009h,006h,009h
	db	009h,009h,009h,009h,009h,009h,007h,006h,006h,006h,006h,006h,009h,009h
	db	009h,009h,009h,009h,009h,007h,007h,007h,007h,007h,006h,009h,009h,009h
	db	009h,009h,009h,009h,007h,007h,001h,001h,001h,001h,001h,009h,009h,009h
	db	009h,009h,009h,009h,007h,001h,009h,009h,001h,009h,009h,001h,009h,009h
	db	009h,009h,009h,009h,003h,001h,009h,000h,001h,000h,009h,001h,009h,009h
	db	009h,009h,009h,009h,003h,001h,001h,001h,001h,001h,001h,001h,009h,009h
	db	009h,009h,009h,009h,003h,001h,001h,000h,000h,000h,001h,001h,009h,009h
	db	009h,009h,009h,009h,009h,001h,000h,002h,002h,000h,001h,003h,009h,009h
	db	009h,009h,009h,009h,009h,003h,001h,001h,001h,001h,003h,009h,009h,009h
	db	009h,009h,009h,009h,009h,009h,003h,003h,003h,003h,009h,009h,009h,009h
	db	009h,009h,009h,009h,005h,005h,002h,003h,003h,002h,002h,009h,009h,009h
	db	009h,009h,005h,005h,002h,002h,002h,002h,002h,002h,002h,005h,005h,009h
	db	009h,005h,002h,002h,002h,002h,002h,002h,002h,002h,002h,002h,002h,005h
	db	009h,002h,002h,002h,002h,002h,002h,002h,002h,002h,002h,002h,002h,002h
	db	009h,002h,002h,005h,002h,002h,002h,002h,002h,002h,002h,005h,002h,002h
	db	009h,002h,002h,005h,002h,002h,002h,002h,002h,002h,002h,005h,002h,002h
	db	009h,002h,002h,005h,002h,002h,002h,002h,002h,002h,002h,005h,002h,002h
	db	009h,002h,002h,005h,002h,002h,005h,005h,005h,002h,002h,005h,002h,002h
	db	009h,002h,002h,005h,002h,005h,002h,002h,002h,005h,002h,005h,002h,002h
	db	009h,002h,002h,005h,002h,005h,005h,005h,005h,005h,002h,005h,002h,002h
	db	009h,002h,002h,005h,002h,002h,002h,002h,002h,002h,002h,005h,002h,002h
	db	009h,002h,002h,009h,008h,004h,004h,004h,004h,004h,004h,009h,002h,002h
	db	009h,003h,003h,009h,008h,004h,004h,004h,004h,004h,004h,009h,003h,003h
	db	009h,001h,001h,003h,008h,004h,004h,004h,004h,004h,004h,003h,001h,001h
	db	009h,003h,001h,009h,008h,004h,004h,009h,004h,004h,004h,009h,003h,003h
	db	009h,009h,009h,009h,008h,004h,009h,009h,009h,004h,004h,009h,009h,009h
	db	009h,009h,009h,009h,001h,001h,009h,009h,009h,003h,001h,009h,009h,009h
	db	009h,009h,009h,009h,001h,001h,009h,009h,009h,003h,001h,009h,009h,009h
	db	009h,009h,009h,009h,001h,003h,009h,009h,009h,003h,001h,009h,009h,009h
	db	009h,009h,009h,009h,001h,003h,009h,009h,009h,003h,001h,009h,009h,009h
	db	009h,009h,009h,009h,001h,003h,009h,009h,009h,003h,001h,009h,009h,009h
	db	009h,009h,009h,009h,001h,003h,009h,009h,009h,003h,001h,009h,009h,009h
	db	009h,009h,009h,009h,000h,000h,009h,009h,009h,003h,001h,009h,009h,009h
	db	009h,009h,009h,001h,001h,001h,009h,009h,009h,001h,001h,000h,000h,001h
	db	009h,009h,009h,000h,000h,000h,009h,009h,000h,000h,000h,000h,000h,000h
snail_sprite:
	db	001h,009h,009h,009h,009h,001h
	db	009h,001h,009h,009h,001h,009h
	db	009h,001h,009h,009h,001h,009h
	db	009h,000h,003h,003h,000h,009h
	db	009h,009h,003h,003h,009h,009h
	db	009h,000h,000h,000h,009h,009h
	db	000h,001h,001h,001h,000h,009h
	db	000h,001h,001h,000h,000h,009h
	db	000h,001h,000h,001h,001h,000h
	db	000h,000h,001h,001h,000h,000h
	db	000h,001h,001h,000h,001h,000h
	db	009h,000h,000h,001h,000h,009h
	db	009h,003h,003h,000h,009h,009h
	db	009h,003h,003h,009h,009h,009h
	db	003h,003h,009h,009h,009h,009h
  #include "font.asm"
_text_title_start:
	db	$0a,$07,$05,$00,$08,$0a,$10,$02,$04,$0b,$10,$03,$09,$10,$06,$01,$00,$0c,$0c,$00 ; "ATTACK of the SNAILS"
_text_title_end:
_text_play_start:
	db	$12,$13,$1b,$0e,$10,$19 ; "> Play"
_text_play_end:
_text_exit_start:
	db	$0b,$16,$0d,$1a,$10 ; " Exit"
_text_exit_end:
_text_credits_start:
	db	$11,$0d,$16,$17,$02,$09,$04,$0e,$16,$11,$10,$12,$14,$10,$15,$02,$0b,$13,$02,$18,$01 ; "Created by _iPhoenix_"
_text_credits_end:
prgm_end:
.nolist ; prettifier-decrease-indent
.echo "====="
.echo "Total program size: ", prgm_end-prgm_start, " bytes"
.echo "Code-only size: ", code_end-prgm_start, " bytes"
.echo "====="
