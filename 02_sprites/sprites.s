; Parts of this have been taken from https://github.com/algofoogle/nes-gamedev-examples/blob/master/part01/ex02-asm-example/test.s
; This example makes use of nesfile.ini (i.e. a configuration file for ld65).

;  # Assemble:
;  ca65 sprites.s -o sprites.o
;   # Link, to create test.nes:
;  ld65 sprites.o -o sprites.nes -C nesfile.ini
; ...and then run it with FCEUX.

; =====  Includes ===============================================================

.include "nes.inc"    ; This is found in cc65's "asminc" dir.
.include "nesdefs.inc"  ; This may be better than "nes.inc".

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

; =====  General RAM ============================================================

.segment "BSS"
; Put labels with .res statements here.

; =====  Program data (read-only) ===============================================

.segment "RODATA"

palette_data:
; Colours available in the NES palette are:
; http://bobrost.com/nes/files/NES_Palette.png
.repeat 2
  .byte $2c, $00, $10, $30
  .byte $00, $0f, $28, $30
  .byte $00, $06, $16, $26
  .byte $00, $09, $19, $29
.endrepeat

sprites:
  ;vert tile attr horiz
  .byte $80, $32, $00, $80 ; sprite 0
  .byte $80, $33, $00, $88 ; sprite 1
  .byte $88, $34, $00, $80 ; sprite 2
  .byte $88, $35, $00, $88 ; sprite 3

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

  ; Clear the first nametable.
  ; Each nametable is 1024 bytes of memory, arranged as 32 columns by 30 rows of
  ; tile references, for a total of 960 ($3C0) bytes. The remaining 64 bytes are
  ; for the attribute table of that nametable.
  ; Nametable 0 starts at PPU address $2000.
  ; For more information, see: http://wiki.nesdev.com/w/index.php/Nametable
  ; NOTE: In order to keep this loop tight (knowing we can only count up to
  ; 255 in a single loop, rather than 960), we just have one loop and do
  ; multiple writes in it.
  ppu_addr $2000
  lda #$fc      ; $fc is an empty tile in mario.chr
  ldx #32*30/4  ; Only need to repeat a quarter of the time, since the loop writes 4 times.
: sta PPU_DATA
  sta PPU_DATA
  sta PPU_DATA
  sta PPU_DATA
  dex
  bne :-

  ; Activate VBLANK NMIs.
  lda #VBLANK_NMI
  sta PPU_CTRL

  ; Now wait until nmi_counter increments, to indicate the next VBLANK.
  wait_for_vblank
  ; By this point, we're in the 3rd VBLANK.

  ; Wait until the screen refreshes.
  wait_for_vblank
  ; OK, at this point we know the screen is visible, ready, and waiting.

loadsprites:
  ; sprites will be stored from $0200-$02FF, see OAM_RAM in nesdefs.inc
  ldx #$00         ; start at 0
: lda sprites, x   ; load data from address (sprites + x)
  sta $0200, x     ; store into ram address ($0200 + x)
  inx
  cpx #32          ; loop 32 times
  bne :-

  ;lda #%10000000   ; enable NMI, sprites from Pattern Table 1
  ;sta PPU_CTRL

  ; Turn the screen on, by activating background and sprites:
  lda #BG_ON|SPR_ON
  sta PPU_MASK

  ; Wait until the screen refreshes.
  wait_for_vblank
  ; OK, at this point we know the screen is visible, ready, and waiting.

.endproc

; =====  CHR-ROM Pattern Tables =================================================

; ----- Pattern Table 0 --------------------------------------------------------

.segment "PATTERN0"

  .incbin "mario.chr"
