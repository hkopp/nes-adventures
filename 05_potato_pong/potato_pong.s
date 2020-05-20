; Parts of this have been taken from https://github.com/algofoogle/nes-gamedev-examples/blob/master/part01/ex02-asm-example/test.s
; This example makes use of nesfile.ini (i.e. a configuration file for ld65).

;  # Assemble:
;  ca65 controller.s -o controller.o
;   # Link, to create controller.nes:
;  ld65 controller.o -o controller.nes -C nesfile.ini
; ...and then run it with FCEUX.

; =====  Includes ===============================================================

.include "nes.inc"    ; This is found in cc65's "asminc" dir.
.include "nesdefs.inc"  ; General NES definitions

; Game specific Definitions
RIGHT_WALL = 224
TOP_WALL = 24
BOTTOM_WALL = 204
PADDLE_X = 16 +8 ; the x position of the right border of the paddle
                 ; for collision detection, not for placement of the paddle


; ===== Local macros ===========================================================

; This waits for a change in the value of the NMI counter.
; It destroys the A register.
.macro wait_for_vblank
  : bit PPU_STATUS    ; P.V (overflow) <- bit 6 (S0 hit); P.N (negative) <- bit 7 (VBLANK).
    bpl  :-        ; Keep checking until bit 7 (VBLANK) is asserted.
.endmacro

; =====  iNES header ============================================================

.segment "INESHDR"
  .byt "NES",$1A
  .byt 1         ; 1 x 16kB PRG block.
  .byt 1         ; 1 x 8kB CHR block.
  ; Rest of iNES header defaults to 0, indicating mapper 0, standard RAM size, etc.

; =====  Interrupt vectors ======================================================

.segment "VECTORS"
  .addr nmi, reset, irq_isr
  ;when an NMI happens (once per frame if enabled) the
  ;processor will jump to the label nmi
  ;when the processor first turns on or is reset, it will jump
  ;to the label reset:
  ;external interrupt IRQ is not used here


; =====  Zero-page RAM ==========================================================

.segment "ZEROPAGE"
player1_buttons:    .res 1  ; Buttons of player1.
                            ; Bits are set as follows:
                            ; A|B|Select|Start|Up|Down|Left|Right

pointerLo: .res 1   ; pointers for indexing the nametable
pointerHi: .res 1
counterLo: .res 1   ; a 16 bit loop counter
counterHi: .res 1
ballSpeedX: .res 1  ; ball speed in x direction
ballSpeedY: .res 1

; =====  General RAM ============================================================

.segment "BSS"
; Put labels with .res statements here.

; =====  Program data (read-only) ===============================================

.segment "RODATA"

palette_data:
; Colours available in the NES palette are:
; http://bobrost.com/nes/files/NES_Palette.png
.repeat 2
  .byte $21, $0f, $00, $10
  .byte $21, $17, $27, $37
  .byte $21, $06, $16, $26
  .byte $21, $09, $19, $29
.endrepeat

sprites:
  ;y tile attr x
  .byte $80, $02, $00, $10 ; sprite 0, top of paddle
  .byte $88, $02, $00, $10 ; sprite 1, middle paddle
  .byte $90, $02, $00, $10 ; sprite 2, bottom paddle
  .byte $80, $01, $01, $80 ; sprite 3, the potato

title_screen:
; This is the titlescreen as nametable with attributes at the end.
; Actually, this is nametables/titlescreen.nam
; Could have been incbinned, but this is simply another way of doing it
  .byte $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08
  .byte $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08
  .byte $08, $08, $08, $05, $05, $05, $05, $05, $05, $05, $05, $05, $05, $05, $05, $05, $05, $05, $05, $05, $05, $05, $05, $05, $05, $05, $05, $05, $05, $08, $08, $08
  .byte $08, $08, $09, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $07, $08, $08
  .byte $08, $08, $09, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $07, $08, $08
  .byte $08, $08, $09, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $07, $08, $08
  .byte $08, $08, $09, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $07, $08, $08
  .byte $08, $08, $09, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $07, $08, $08
  .byte $08, $08, $09, $00, $00, $03, $00, $03, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $07, $08, $08
  .byte $08, $08, $09, $00, $00, $03, $00, $03, $00, $03, $00, $03, $03, $03, $00, $03, $00, $03, $03, $03, $00, $03, $03, $00, $00, $00, $00, $00, $00, $07, $08, $08
  .byte $08, $08, $09, $00, $00, $03, $03, $00, $03, $00, $03, $00, $03, $00, $03, $00, $03, $00, $03, $00, $03, $00, $03, $00, $00, $00, $00, $00, $00, $07, $08, $08
  .byte $08, $08, $09, $00, $00, $03, $00, $00, $03, $03, $03, $00, $03, $00, $03, $03, $03, $00, $03, $00, $03, $03, $03, $00, $00, $00, $00, $00, $00, $07, $08, $08
  .byte $08, $08, $09, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $07, $08, $08
  .byte $08, $08, $09, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $07, $08, $08
  .byte $08, $08, $09, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $07, $08, $08
  .byte $08, $08, $09, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $07, $08, $08
  .byte $08, $08, $09, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $07, $08, $08
  .byte $08, $08, $09, $00, $00, $00, $00, $00, $03, $03, $03, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $07, $08, $08
  .byte $08, $08, $09, $00, $00, $00, $00, $00, $03, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $07, $08, $08
  .byte $08, $08, $09, $00, $00, $00, $00, $00, $03, $00, $03, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $07, $08, $08
  .byte $08, $08, $09, $00, $00, $00, $00, $00, $03, $03, $03, $00, $03, $00, $03, $03, $03, $00, $03, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $07, $08, $08
  .byte $08, $08, $09, $00, $00, $00, $00, $00, $03, $00, $00, $03, $00, $03, $03, $00, $03, $03, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $07, $08, $08
  .byte $08, $08, $09, $00, $00, $00, $00, $00, $03, $00, $00, $03, $03, $00, $03, $00, $03, $00, $03, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $07, $08, $08
  .byte $08, $08, $09, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $07, $08, $08
  .byte $08, $08, $09, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $03, $03, $03, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $07, $08, $08
  .byte $08, $08, $09, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $07, $08, $08
  .byte $08, $08, $09, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $07, $08, $08
  .byte $08, $08, $08, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $08, $08, $08
  .byte $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08
  .byte $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08
  .byte $aa, $aa, $aa, $aa, $aa, $aa, $aa, $aa, $aa, $40, $00, $00, $00, $00, $00, $aa, $aa, $55, $55, $55, $55, $55, $00, $aa, $aa, $00, $00, $01, $00, $00, $00, $aa
  .byte $aa, $00, $55, $00, $50, $10, $00, $aa, $aa, $00, $55, $55, $55, $01, $00, $aa, $aa, $a0, $a0, $a4, $a5, $a0, $a0, $aa, $0a, $0a, $0a, $0a, $0a, $0a, $0a, $0a

game_screen:
; This is the game screen as nametable with attributes at the end.
  .incbin "nametables/gamescreen.nam"


; =====  Main code ==============================================================

.segment "CODE"

; Use of .proc means labels are specific to this scope.

; NMI. This is executed during each vblank
.proc nmi
  ; Update the screen
  lda #$00
  sta PPU_OAM_ADDR  ; set low byte of OAM RAM address
  lda #$02
  sta OAM_DMA       ; set high byte of OAM RAM address and start the transfer
  ; Next, we read out the controllers and store the positions in
  ; a variable so the game loop can read them out.
LatchController:    ; tell both controllers to latch buttons
  lda #$01
  sta $4016
  lda #$00
  sta $4016
  ldx #$08
ReadControllerLoop:
  ; This idea is from the nerdy nights tutorial series.
  lda $4016
  lsr a                ; least significant byte is stored in carry
  rol player1_buttons  ; and shifted into the variable
  dex
  bne ReadControllerLoop
  rti ; return
.endproc

; IRQ/BRK ISR:
.proc irq_isr
  ; Handle IRQ/BRK here.
  rti
.endproc


; MAIN PROGRAM START: The 'reset' address.
.proc reset

  ; Disable interrupts:
  sei

  ; Basic init:
  ldx #0
  stx PPU_CTRL            ; General init state; NMIs (bit 7) disabled.
  stx PPU_MASK            ; Disable rendering, i.e. turn off background & sprites.

  ; Set stack pointer:
  dex                     ; X = $FF
  txs                     ; Stack pointer = $FF

  ; Clear lingering interrupts since before reset:
  bit PPU_STATUS    ; Ack VBLANK NMI (if one was left over after reset); bit 7.

  ; PPU warm-up: Wait 1 full frame for the PPU to become stable, by watching VBLANK.
  wait_for_vblank
  ; First PPU frame has reached VBLANK.

  ; Clear zeropage
  ldx #0
  txa
: sta $00,x
  inx
  bne :-

  ; Disable 'decimal' mode.
  cld

  ; Clear memory
clrmem:
  lda #00
  sta $0100, x
  sta $0200, x
  sta $0300, x
  sta $0400, x
  sta $0500, x
  sta $0600, x
  sta $0700, x
  lda #$fe
  sta $0300, x
  inx
  bne clrmem

  ; Wait for second VBLANK:
  wait_for_vblank
  ; VLBANK asserted: PPU is now fully stabilised.

  ; --- We're still in VBLANK for a short while, so do video prep now ---

  ; Load the main palette.
  ; $3F00-$3F1F in the PPU address space is where palette data is kept,
  ; organised as 2 sets (background & sprite sets) of 4 palettes, each
  ; being 4 bytes long (but only the upper 3 bytes of each being used).
  ; That is 2(sets) x 4(palettes) x 3(colours). $3F00 itself is the
  ; "backdrop" colour, or the universal background colour.
  ppu_addr $3F00  ; Tell the PPU we want to access address $3F00 in its address space.
  ldx #0
: lda palette_data,x
  sta PPU_DATA
  inx
  cpx #32    ; P.C gets set if X>=M (i.e. X>=32).
  bcc :-     ; Loop if P.C is clear.
  ; NOTE: Trying to load the palette outside of VBLANK may lead to the colours being
  ; rendered as pixels on the screen. See:
  ; http://wiki.nesdev.com/w/index.php/Palette#The_background_palette_hack

  ; Load title screen
  
  lda #<(title_screen) ; the low byte of the title screen location
  sta pointerLo       ; put the low byte of the address of background into pointer
  lda #>(title_screen)
  sta pointerHi       ; put the high byte of the address into pointer

  jsr LoadScreen

  ; Activate VBLANK NMIs.
  lda #VBLANK_NMI
  sta PPU_CTRL

  ; Now wait until nmi_counter increments, to indicate the next VBLANK.
  wait_for_vblank
  ; By this point, we're in the 3rd VBLANK.

  ; Turn the screen on, by activating background
  lda #BG_ON
  sta PPU_MASK

  ; Wait until the screen refreshes.
  wait_for_vblank
  ; OK, at this point we know the screen is visible, ready, and waiting.

MainLoop:
  ; Show Title screen and switch to game when start is pressed

  ; we are simply waiting for the start button to be pressed and then
  ; switch to the game loop
  ; We could do a fancy fade-out effect,
  ; but that does not have priority.

WaitForStart:
  wait_for_vblank
  lda player1_buttons
  and #BUTTON_START
  beq WaitForStart ; if the button is not pressed, we are done
  ; Otherwise we set up the game screen.

  ;;;;;;;;;;;;;;;;;;;;
  ; Load game screen
  ;;;;;;;;;;;;;;;;;;;;
  ; compare with the loading of the title screen above

  ; Turn the screen off and disable NMI
  ldx #0
  stx PPU_CTRL            ; NMIs (bit 7) disabled.
  lda #BG_OFF|SPR_OFF
  sta PPU_MASK
  
  lda #<(game_screen) ; the low byte of the title screen location
  sta pointerLo       ; put the low byte of the address of background into pointer
  lda #>(game_screen)
  sta pointerHi       ; put the high byte of the address into pointer

  jsr LoadScreen

  ; Draw the Sprites (Bat + Ball)
  ; sprites are stores from $0200-$02FF
  ; and copied to PPU in each nmi
  ldx #$00
: lda sprites, x
  sta $0200, x
  inx
  cpx #16        ; we have 4 sprites with 4 bytes each
  bne :-

  ; Activate VBLANK NMIs.
  lda #VBLANK_NMI
  sta PPU_CTRL
  ; Turn the screen on, by activating background and sprites:
  lda #BG_ON|SPR_ON
  sta PPU_MASK

  ; initialize the ball movement
  lda #$01
  sta ballSpeedX
  sta ballSpeedY

GameLoop:
  ; read controllers and maybe move paddle
  lda player1_buttons
  and #BUTTON_UP
  beq HandlingUpDone
  jsr MovePaddleUp
HandlingUpDone:
  lda player1_buttons
  and #BUTTON_DOWN
  beq HandlingDownDone
  jsr MovePaddleDown
HandlingDownDone:

  ; Move Ball
  jsr MoveBall
  wait_for_vblank

  jmp GameLoop

.endproc

.proc LoadScreen
  ;;;;;;;;;;;;;;;;;;;;
  ; Load title screen
  ;;;;;;;;;;;;;;;;;;;;
  ; Each nametable is 1024 bytes of memory, arranged as 32 columns by 30 rows of
  ; tile references, for a total of 960 ($3C0) bytes. The remaining 64 bytes are
  ; for the attribute table of that nametable.
  ; Nametable 0 starts at PPU address $2000.
  ; For more information, see: http://wiki.nesdev.com/w/index.php/Nametable
  ; NOTE: We can only count up to 255 in a single loop, rather than 960.
  ; Thus we are using the technique explained in Nerdy Nights 8:
  ; Indirect Indexed mode
  
  ; To invoke this subroutine, you have to
  ;   - put the low byte of the screen location in pointerLo
  ;   - put the high byte of the screen location in pointerHi

  lda #$00
  sta counterLo       ; put the loop counter into 16 bit variable
  lda #$04
  sta counterHi       ; count = $0400 = 1KB, the whole screen at once including attributes
  ppu_addr $2000      ; Tell the PPU we want to access address $2000 in its address space.
                      ; That is where the nametable lies.

  ldy #$00            ; put y to 0 and don't change it
LoadScreenLoop:
  lda (pointerLo), y  ; a = the stuff at the 16 bit pointer starting with pointerLo + y
  sta $2007           ; copy one background byte

  lda pointerLo
  clc
  adc #$01
  sta pointerLo
  lda pointerHi
  adc #$00
  sta pointerHi       ; increment the pointer to the next byte
                      ; This is just adding 1 to a 16 bit variable
  lda counterLo
  sec
  sbc #$01
  sta counterLo
  lda counterHi
  sbc #$00
  sta counterHi       ; decrement the loop counter
                      ; This is just subtracting 1 from a 16 bit variable

  lda counterLo
  cmp #$00
  bne LoadScreenLoop
  lda counterHi
  cmp #$00
  bne LoadScreenLoop  ; if the loop counter isn't 0000, keep copyin
.endproc

.proc MovePaddleUp
  ; can probably be optimized in a loop
  lda $0200 ; move top of paddle
  sec
  sbc #$01
  sta $0200
  lda $0204 ; move middle of paddle
  sec
  sbc #$01
  sta $0204
  lda $0208 ; move bottom of paddle
  sec
  sbc #$01
  sta $0208
  rts
.endproc

.proc MovePaddleDown
  ; can probably be optimized in a loop
  lda $0200 ; move top of paddle
  clc
  adc #$01
  sta $0200
  lda $0204 ; move middle of paddle
  clc
  adc #$01
  sta $0204
  lda $0208 ; move bottom of paddle
  clc
  adc #$01
  sta $0208
  rts
.endproc

.proc MoveBall
  ; advance the ball
  lda $020f ; x position of ball
  clc
  adc ballSpeedX
  sta $020f
  lda $020c ; y position of ball
  clc
  adc ballSpeedY
  sta $020c

  ; if the ball touches something, adjust the speed
  ; bge (branch on greater or equal) = bcs
  ; blt (branch if less than) = bcc
  ; if the ball touches the right wall
  lda $020f ; x position of ball
  cmp #RIGHT_WALL
  bcc RightWallDone
  ; adjust speed
  lda #$ff
  sta ballSpeedX
RightWallDone:

  ; if the ball touches the top wall
  lda $020c ; y position of ball
  cmp #TOP_WALL
  bcs TopWallDone
  ; adjust speed
  lda #$01
  sta ballSpeedY
TopWallDone:

  ; if the ball touches the bottom wall
  lda $020c ; y position of ball
  cmp #BOTTOM_WALL
  bcc BottomWallDone
  ; adjust speed
  lda #$ff
  sta ballSpeedY
BottomWallDone:

  ; check if the ball touches the paddle
  ; the x position of a sprite it its leftmost position.
  ; the y position of a sprite is one lower than its highest pixel

  ; is ball behind the x-line of the paddle?
  lda $020f ; x position of ball
  cmp #PADDLE_X
  bcs PaddleDone ; no collision, we are done
  ; check y-collision
  ; ball below top of paddle?
  lda $020c ; y position of ball
  cmp $0200 ; y position of top of paddle sprite
  bcc PaddleDone ; blt (branch if less than) = bcc

  ; ball above bottom of paddle?
  ; note that the y position of the bottom of the paddle it its
  ; _uppermost_ pixel
  ; so we subtract 8 from the ball position
  sec
  sbc #$08
  cmp $0208 ; y position of bottom of paddle sprite
  bcs PaddleDone ; bge (branch on greater or equal) = bcs
  ; When we are here, we have a ball-paddle collision
  ; adjust speed
  lda #$01
  sta ballSpeedX
PaddleDone:
    ; if the left wall is hit, the game is over
    ; TODO
  rts
.endproc


; =====  CHR-ROM Pattern Tables =================================================

; ----- Pattern Table 0 --------------------------------------------------------

.segment "PATTERN0"

  .incbin "chr/potato_pong.chr"
