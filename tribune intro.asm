					//----------------------------------------------------------
					// unreleased tribune intro v1.0
					//----------------------------------------------------------
			        
					//----------------------------------------------------------
					// v1.0 - IC - initial re-write into kickassember
					//----------------------------------------------------------

					//----------------------------------------------------------
					// memory map
					// $1000-$1fff - music (standard init & play calls)
					// $2000-$21ff - 1x1 charset (?)
					// $2200-      - logo data in memory
					// $2800-$2fff - 4x4 charset data
					// $3000-$30ff - 4x4 lookup tables - length etc.
					// $3100-$34ff - 4x4 definition tables.
					// $3800-$3fff - scroll text
					//----------------------------------------------------------

					//----------------------------------------------------------
					// import music.
					//----------------------------------------------------------

					* = $1000 "Music"
					.var music = LoadSid("Stormlord_2_Demo.sid")

					//--------------------------------------------------------------------
					// import standard library definitions
					//--------------------------------------------------------------------
					#import "standardLibrary.asm"			
					//--------------------------------------------------------------------
					BasicUpstart2(start)
					//--------------------------------------------------------------------

					// logo data memory pointer
					.var logodata 		= $2200

					// top scroller line pointer
					.var scrollerline  	= 1024+(40*17)

					// define zero-page used
					.var apage    = $aa
					.var tpage    = apage+2
					.var newprog  = $3c00
					.var codearea = $0340

					//--------------------------------------------------------------------

start:				lda #$7b
					sta screenmode
					lda $3fff
					sta tempbyte
					lda #$ff
					sta $3fff
					
					sei
					ldx #$ff
					txs
					lda #$08
					sta screen
					sta 646
					jsr $e544

			        lda #BLACK
			        sta screen
			        sta border
					sta 650
					lda #$80
					sta 657
					
					lda #$4c
					sta case0
					sta case1

					ldy #8
					lda #$00
reset:   			sta delay,y
			        dey
			        bpl reset
			        
			        lda #$ad
			        sta fxbyte0
			        sta tbyte0
			        sta tbyte1
			        sta sbyte0
			        
			        lda #250
			        sta spriteYbyte+1
			        
			        lda#$00
			        sta col21+1
			        sta col22+1
			        sta col23+1
			        
			        jsr music.init

			        jsr drawlogo
			        jsr drawtext
			        jsr set4x4
			        jsr scrollerinit
			        jsr textinit
			        
			        lda #$7f
			        sta $dc0d
			        sta $dd0d
			        lda #$30
			        sta raster
			        lda #$01
			        sta irqflag
			        sta $d01a
			        ldx #<progirq
			        ldy #>progirq
			        stx $0314
			        sty $0315
			        ldx #<restore
			        ldy #>restore
			        stx $0318
			        sty $0319
			        cli
			        lda #$20
			        sta sbyte0

case0:   			jmp case0

delay0a: 			ldx #$70
			        ldy #$ff
			        dey
			        bne *-1
			        dex
			        bne delay0a+2
			        lda #$20
			        sta fxbyte0
case1:   			jmp case1

delay0b: 			ldx #$80
			        ldy #$ff
			        dey
			        bne *-1
			        dex
			        bne delay0b+2

			        lda #$20
			        sta tbyte0
			        lda #$20
			        sta tbyte1

keyscan: 			lda $dc01
			        and #$10
			        bne keyscan
			        lda tempbyte
			        sta $3fff

        			jmp $fce2

					//----------------------------------------------------------
					// the following code will kill everything and then transfer 
					// the next 'part / program' 
					//----------------------------------------------------------

         			sei
         			jsr $ffe4
         			lda #$00
         			ldx #$40
killsid:  			sta $d400,x
			        dex
			        bne killsid
			        sei
			        jsr $ff5b
			        jsr $fd15
			        jsr $e3bf
			        jsr $fda3
			        cli
			        ldx #$40
tranloop: 			lda linkcode,x
         			sta codearea,x
			        dex
			        bpl tranloop
			        lda $2e
			        sec
			        sbc #$0c
			        sta $2e
			        lda $2d
			        sec
			        sbc #$29
			        bcs tranloop2
			        dec $2e
tranloop2:		    sta $2d
			        lda #$08
			        sta $ba
			        jmp codearea
					//----------------------------------------------------------
linkcode:			sei
			        lda #$34
			        sta $01
			        ldx #$00
nextbyte: 			lda newprog,x
         			sta $0801,x
					ldy #$37
					sty $01
					bit $d418 // depack fx (optional)
					ldy #$34
					sty $01
			        inx
			        bne nextbyte
			        lda $0345
			        cmp #$00
			        beq tranexit
			        inc $0345
			        inc $0348
			        jmp $0341
tranexit: 			lda #$00
			        sta $0800
			        sta $d418
			        lda #$37
			        sta $01
			        cli
			        jsr $a65e
			        jsr $a68e
			        jmp $a7ae


					//----------------------------------------------------------
restore: 			rti
					//----------------------------------------------------------
					
					//----------------------------------------------------------
			        .byte $00
			        .text "> icc intro v1.0 by case/padua 2019 <"
			        .byte $00
					//----------------------------------------------------------
progirq: 
					lda #$1b
			        sta screenmode
			        lda #24
			        sta charset
			        lda #216
			        sta smoothpos

			        // set colours for logo

col21:			    lda #$00
			        sta screen
col22:				lda #$00
			        sta backcol0
col23:				lda #$00
			        sta backcol1


tbyte0:  			lda textfx

			        ldx #$72
			        cpx raster
			        bne *-3

			        ldx #$0a
			        dex
			        bne *-1
			        lda #BLACK
			        sta screen
			        sta border

			        lda #200
			        sta smoothpos

tbyte1:  			lda scroller

			        ldx #$b2
			        cpx raster
			        bne *-3
			        ldx #$08
			        dex
			        bne *-1

			        lda #216
			        sta smoothpos

			        lda #26
			        sta charset

sc21:		        lda #$06
			        sta screen
sc22:		        lda #$0e
			        sta backcol0
sc23:		        lda #$0f
			        sta backcol1

			        lda smoothpos
			        and #$f0
			        ora scrollXpos
			        sta smoothpos

			        ldy fldnum
			        ldx #$00
fldloop:			lda raster
					cmp raster
					beq *-3
					and #$07
					ora #$10
					sta screenmode
					inx
					txa 
					cmp fldsinus,y
					bne fldloop
					lda fldnum
					clc
					adc fldadd
					sta fldnum

					//----------------------------------------------------------
					//  set the sprites displayed on screen.
					//----------------------------------------------------------

			        lda #%00000011
			        sta spriteset
			        lda #$00
			        sta spritepr

			        // no multicolour and no x or y expansion
			        
			        lda #$00
			        sta spritemulti
			        sta spriteexpy
			        sta spriteexpx

					//----------------------------------------------------------
					// display little logo sprite
					//----------------------------------------------------------

			        ldx #($2f00/64)
			        stx 2040
			        inx
			        stx 2041
			        lda #$06
			        sta spritecolors
			        lda #$0e
			        sta spritecolors+1
spriteYbyte:		lda #250
			        sta sprite0y
			        sta sprite1y
			        lda #18
			        sta sprite0x
			        clc
			        adc #24
			        sta sprite1x

			        ldx #$f8
			        cpx raster
			        bne *-3

			        jsr music.play

sbyte0:				lda spon1
fxbyte0:			lda fadeon

			        lda #$01
			        sta irqflag
			        jmp $ea31

					//----------------------------------------------------------
					// initialise the text display setting
					//----------------------------------------------------------

textinit: 			ldx #<$d800+(40*8)
			        ldy #>$d800+(40*8)
			        stx tpage
			        sty tpage+1
			        lda #$00
			        sta colorNumber+1
			        sta linenumber
			        rts

					//----------------------------------------------------------

textfx:   			lda delay+2
			        sec
			        sbc #$01
			        and #$07
			        sta delay+5
			        bcc textfx4
			        rts

textfx4:			ldx colorNumber+1
			        cpx #24
			        beq textfx0
			        lda textcolortable,x
			        ldy #$00
textfx1:			sta (tpage),y
			        iny
			        cpy #40
			        bne textfx1
			        inc colorNumber+1
			        rts

textfx0:  			inc linenumber
			        ldx linenumber
			        cpx #$07
			        bne textfx2
			        jsr textinit
			        rts

textfx2:  			lda tpage
			      	clc
			        adc #40
			        sta tpage
			        lda tpage+1
			        adc #$00
			        sta tpage+1
			        lda #$00
			        sta colorNumber+1
			        rts
					//----------------------------------------------------------

spon1:    			ldy spriteYbyte+1
			        cpy #237
			        beq spon1a
			        dec spriteYbyte+1
			        rts
spon1a:			    lda #$ad
			        sta sbyte0
			        sta case0
			        rts

					//----------------------------------------------------------
					// fade on the main logo
					//----------------------------------------------------------
fadeon:				lda delay
			        sec
			        sbc #$04
			        and #$07
			        sta delay
			        bcs fadeon1
			        rts
fadeon1:  			ldy colorNumber
			        cpy #$08
			        beq fadeon2
			        lda coltab0,y
			        sta col21+1
			        sta col22+1
			        sta col23+1
			        inc colorNumber
			        rts
fadeon2:			lda #$06
			        sta col21+1
			        lda #$0f
			        sta col22+1
			        lda #$0e
			        sta col23+1
			        lda #$ad
			        sta fxbyte0
			        sta case1
			        rts

					//----------------------------------------------------------

drawtext: 			ldy #$00
			        lda screentext,y
			        sta $0400+(40*8),y
			        lda screentext+140,y
			        sta $0400+140+(40*8),y
			        lda #BLACK
			        sta $d800+(40*8),y
			        sta $d800+(40*8)+140,y
			        iny
			        cpy #140
			        bne drawtext+2
			        rts

					//----------------------------------------------------------

set4x4:   			ldy #$00
			        lda #$00
			        sta scrollerline-40,y
			        sta scrollerline+120,y
			        iny
			        cpy #160
			        bne set4x4+2
			        rts

					//----------------------------------------------------------

drawlogo: 			ldy #$00
			        lda logodata,y
			        sta $0400,y
			        lda logodata+(40*4),y
			        sta $0400+(40*4),y
			        iny
			        cpy #(40*4)
			        bne drawlogo+2
			        rts

					//----------------------------------------------------------

scroller:			lda scrollDelay
			        beq scrollerNext
			        dec scrollDelay
			        rts
scrollerNext:		lda scrollXpos
			        sec
			        sbc scrollSpeed
			        and #$07
			        sta scrollXpos
			        bcc scrollerMove
			        rts

			        // move scroller 1 physical chacater left

scrollerMove:    	ldx #$00
			        lda scrollerline+1,x
			        sta scrollerline,x
			        lda scrollerline+41,x
			        sta scrollerline+40,x
			        lda scrollerline+81,x
			        sta scrollerline+80,x
			        lda scrollerline+121,x
			        sta scrollerline+120,x
			        inx
			        cpx #39
			        bne scrollerMove+2
			        
			        ldx charWidth			// check current width count
			        cpx widthCheck			// against what it should be
			        bne plotNext			// not complete, then plot next section.

			        jsr nextCharacter

         			ldx #$00
plotNext:    		lda charLine1,x
         			sta scrollerline+39
plotChar01:			lda charLine2,x
         			sta scrollerline+79
plotChar02:			lda charLine3,x
         			sta scrollerline+119
plotChar03:         lda charLine4,x
			        sta scrollerline+159

		         	inc charWidth		// increment width counter
			        rts
					
					//----------------------------------------------------------

nextCharacter:		ldy #$00
			        sty charWidth
			        lda (apage),y
			        cmp #$ff 			// check for end of scroller character
			        bne nextChar
			        jsr scrollerinit

			        jmp nextCharacter

nextChar:			sta tempCharacter	// store the character just loaded

					//----------------------------------------------------------
         			// change speed ?
					//----------------------------------------------------------

checkSpeed:     	ldy #$05
			        cmp speedtable,y
			        beq checkSpeed1
			        dey
			        bpl checkSpeed+2
			        jmp checkPause
checkSpeed1:    	sec
			        sbc #$40
			        sta scrollSpeed
			        lda #$20
			        sta tempCharacter
			        jmp plotCharacter

					//----------------------------------------------------------
         			// pause text ?
					//----------------------------------------------------------

checkPause:     	ldy #$04
			        cmp pausetable,y
			        beq checkPause1
			        dey
			        bpl checkPause+2
			        jmp checkBounce
checkPause1:		lda pauseTable,y
			        sta scrollDelay
			        lda #$20
			        sta tempCharacter
			        jmp plotCharacter

					//----------------------------------------------------------
         			// change bounce
					//----------------------------------------------------------

checkBounce:		ldy #$05
			        cmp bouncetable,y
			        beq checkBounce1
			        dey
			        bpl checkBounce+2
			        jmp checkColor
checkBounce1:    	sec
			        sbc #$b0
			        sta fldadd
			        lda #$20
			        sta tempCharacter
			        jmp plotCharacter
					
					//----------------------------------------------------------
         			// change color
					//----------------------------------------------------------
checkColor:			ldy #$05
			        cmp colortable,y
			        beq checkColor1
			        dey
			        bpl checkColor+2
			        jmp plotCharacter
checkColor1:    	lda colorTable1,y
			        sta sc21+1
			        lda colorTable2,y
			        sta sc22+1
			        lda colorTable3,y
			        sta sc23+1
			        lda #$20
			        sta tempCharacter
			        jmp plotCharacter
					
					//----------------------------------------------------------
					// load the original character back into memory and use it
					// to set all the pointers for the next character in the
					// scroller.
					//----------------------------------------------------------

plotCharacter:		lda tempCharacter
			        and #$3f
			        tay
			        lda charWidths,y
			        sta widthCheck
			        lda asciiLookup,y
			        asl 
			        asl 
			        sta plotNext+1
			        sta plotChar01+1
			        sta plotChar02+1
			        sta plotChar03+1
			        inc apage
			        bne plotCharacterExit
			        inc apage+1
plotCharacterExit:  rts
					//----------------------------------------------------------
scrollerinit:    	ldx #<scrolltext
			        ldy #>scrolltext
			        stx apage
			        sty apage+1
			        ldx #$03
			        stx charWidth
			        inx
			        stx widthCheck
			        rts
					//----------------------------------------------------------

					//----------------------------------------------------------
					// the following are the data bytes used for scrolling, the
					// pause, colour change, scroller speed and delay.  Can be 
					// placed anywhere in memory.
					//----------------------------------------------------------
tempbyte: 			.byte $00
scrollXpos:    		.byte $00
scrollDelay:   		.byte $00
scrollSpeed:   		.byte $03 							// scroll speed.
tempCharacter:    	.byte $00 							// current character.
charWidth:  		.byte $00
widthCheck:  		.byte $00
pauseTable:    		.byte 100,125,150,175,200
					//----------------------------------------------------------
			        
			        // speed codes
speedtable:    		.byte $41,$42,$43,$44,$45,$46
         			
         			// pause codes
pausetable:    		.byte $c1,$c2,$c3,$c4,$c5
					
					// bounce codes
bouncetable:    	.byte $b1,$b2,$b3,$b4,$b5,$b6
         			
         			// color codes
colortable:    		.byte $81,$82,$83,$84,$85,$86
         			
         			// color tables for scroller
colorTable1:     	.byte $06,$02,$0b,$09,$09,$05
colorTable2:     	.byte $0e,$0a,$0c,$08,$05,$0d
colorTable3:     	.byte $0f,$0f,$0f,$0a,$0d,$0f
					
					//----------------------------------------------------------
delay:    			.byte $00,$00,$00
colorNumber:   		.byte $00,$00,$00
linenumber:  		.byte $00,$00,$00
					
					//----------------------------------------------------------
coltab0:  			.byte $00,$06,$02,$04,$0a,$0f,$07,$01
					
					//----------------------------------------------------------
textcolortable:  	.byte $00,$09,$0b,$08,$05,$03,$0d,$07,$01,$01,$01,$01
         			.byte $01,$01,$01,$01,$07,$0d,$03,$05,$08,$0b,$09,$00


					//----------------------------------------------------------
					// data for the fld routine.
					//----------------------------------------------------------

fldnum:   			.byte $00		// current position in sinus table
fldadd:   			.byte $05		// number of postions to jump in sinus table

					//----------------------------------------------------------
					// fld sinus data
					//----------------------------------------------------------

fldsinus:			.byte $15,$15,$14,$14,$13,$13,$12,$12,$11,$11,$10,$10
		        	.byte $0f,$0f,$0e,$0e,$0d,$0d,$0c,$0c,$0c,$0b,$0b,$0a
		        	.byte $0a,$09,$09,$08,$08,$08,$07,$07,$07,$06,$06,$06
			        .byte $05,$05,$05,$04,$04,$04,$03,$03,$03,$03,$02,$02
			        .byte $02,$02,$02,$01,$01,$01,$01,$01,$01,$01,$01,$01
			        .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
			        .byte $01,$01,$01,$01,$01,$02,$02,$02,$02,$02,$03,$03
			        .byte $03,$03,$04,$04,$04,$05,$05,$05,$06,$06,$06,$07
			        .byte $07,$07,$08,$08,$08,$09,$09,$0a,$0a,$0b,$0b,$0c
			        .byte $0c,$0c,$0d,$0d,$0e,$0e,$0f,$0f,$10,$10,$11,$11
			        .byte $12,$12,$13,$13,$14,$14,$15,$16,$16,$16,$15,$14
			        .byte $14,$13,$13,$12,$12,$11,$11,$10,$10,$0f,$0f,$0e
			        .byte $0e,$0d,$0d,$0c,$0c,$0c,$0b,$0b,$0a,$0a,$09,$09
			        .byte $08,$08,$08,$07,$07,$07,$06,$06,$06,$05,$05,$05
			        .byte $04,$04,$04,$03,$03,$03,$03,$02,$02,$02,$02,$02
			        .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
			        .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
			        .byte $01,$01,$02,$02,$02,$02,$02,$03,$03,$03,$03,$04
			        .byte $04,$04,$05,$05,$05,$06,$06,$06,$07,$07,$07,$08
			        .byte $08,$08,$09,$09,$0a,$0a,$0b,$0b,$0c,$0c,$0c,$0d
			        .byte $0d,$0e,$0e,$0f,$0f,$10,$10,$11,$11,$12,$12,$13
			        .byte $13,$14,$14,$15
					//----------------------------------------------------------

					//----------------------------------------------------------
					// macro's
					//----------------------------------------------------------

					.macro SetBorderColor(color) {
							lda #color
							sta border
					}
					.macro SetScreenColor(color) {
							lda #color
							sta screen
					}

					//--------------------------------------------------------------
					// make sure sid file is stored in the correct memory location
					//--------------------------------------------------------------
					*=music.location "Music"
					.fill music.size, music.getData(i)
					//--------------------------------------------------------------
					// import the default character sets.
					//--------------------------------------------------------------
					*=$2000 "1x1 charset"
					.import c64 "1x1.prg"
					*=$2800 "4x4 charset"
					.import c64 "4x4.prg"
					//--------------------------------------------------------------

					*=$3000
asciiLookup:  		.byte $00,$01,$02,$03,$04,$05,$06,$07
			        .byte $08,$09,$0A,$0B,$0C,$1B,$0E,$0F
			        .byte $10,$11,$12,$13,$14,$15,$16,$1D
			        .byte $18,$19,$1A,$00,$00,$00,$00,$00
			        .byte $20,$21,$22,$00,$00,$00,$00,$27
			        .byte $28,$29,$2A,$2B,$2C,$2D,$2E,$2F
			        .byte $30,$31,$32,$33,$34,$35,$36,$37
			        .byte $38,$39,$3A,$3B,$3C,$3D,$3E,$3F

charWidths:		    .byte $04,$04,$04,$04,$04,$04,$04,$04
			        .byte $04,$02,$04,$04,$04,$06,$04,$04
			        .byte $04,$04,$04,$04,$04,$04,$04,$06
			        .byte $04,$04,$04,$04,$04,$04,$04,$04
			        .byte $02,$02,$04,$04,$04,$04,$04,$02
			        .byte $04,$04,$04,$04,$04,$04,$02,$04
			        .byte $04,$02,$04,$04,$04,$04,$04,$04
			        .byte $04,$04,$03,$03,$04,$04,$04,$04

			        .align $0100

charLine1:			.byte $00,$00,$00,$00,$01,$02,$06,$03,$11,$36,$06,$03,$01,$02,$06,$03
			        .byte $11,$36,$06,$03,$11,$35,$1E,$1F,$11,$35,$1E,$1F,$01,$02,$06,$03
			        .byte $11,$24,$11,$24,$11,$24,$00,$00,$00,$26,$25,$24,$11,$24,$11,$24
			        .byte $11,$24,$00,$00,$00,$00,$00,$00,$11,$36,$06,$03,$01,$02,$06,$03
			        .byte $11,$36,$06,$03,$01,$02,$06,$03,$11,$36,$06,$03,$01,$1D,$06,$03
			        .byte $26,$25,$35,$1F,$11,$24,$11,$24,$11,$24,$11,$24,$00,$00,$00,$00
			        .byte $11,$24,$11,$24,$11,$24,$11,$24,$26,$1E,$25,$24,$11,$36,$25,$36
			        .byte $06,$03,$00,$00,$11,$24,$11,$24,$11,$24,$00,$00,$00,$00,$00,$00
			        .byte $00,$00,$00,$00,$11,$24,$00,$00,$11,$24,$11,$24,$00,$00,$00,$00
			        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$11,$24,$00,$00
			        .byte $01,$1D,$1F,$00,$26,$06,$03,$00,$00,$00,$00,$00,$00,$00,$00,$00
			        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
			        .byte $01,$02,$06,$03,$11,$24,$00,$00,$01,$02,$06,$03,$01,$02,$06,$03
			        .byte $11,$24,$11,$24,$11,$35,$1E,$1F,$01,$1D,$06,$03,$26,$1E,$25,$24
			        .byte $01,$1D,$06,$03,$01,$1D,$06,$03,$11,$24,$00,$00,$00,$11,$24,$00
			        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$26,$1E,$06,$03

charLine2:   		.byte $00,$00,$00,$00,$16,$08,$07,$05,$16,$08,$07,$0E,$16,$05,$19,$1A
			        .byte $16,$05,$16,$05,$16,$08,$20,$00,$16,$08,$20,$00,$16,$05,$22,$23
			        .byte $16,$08,$07,$05,$16,$05,$00,$00,$00,$00,$16,$05,$16,$29,$28,$27
			        .byte $16,$05,$00,$00,$00,$00,$00,$00,$16,$05,$16,$05,$16,$05,$16,$05
			        .byte $16,$08,$07,$2A,$16,$05,$16,$05,$16,$08,$07,$0E,$16,$08,$30,$31
			        .byte $00,$16,$05,$00,$16,$05,$16,$05,$16,$05,$16,$05,$00,$00,$00,$00
			        .byte $3D,$40,$07,$0E,$16,$40,$16,$05,$42,$41,$07,$2A,$16,$05,$16,$05
			        .byte $16,$05,$00,$00,$16,$05,$16,$05,$16,$05,$00,$00,$00,$00,$00,$00
			        .byte $00,$00,$00,$00,$16,$05,$00,$00,$19,$1A,$19,$1A,$00,$00,$00,$00
			        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$19,$1A,$00,$00
			        .byte $16,$05,$00,$00,$00,$16,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00
			        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
			        .byte $16,$05,$16,$05,$16,$05,$00,$00,$4B,$4C,$07,$2A,$19,$48,$07,$0E
			        .byte $16,$08,$07,$05,$16,$08,$41,$45,$16,$08,$30,$31,$00,$42,$07,$2A
			        .byte $3D,$40,$07,$0E,$16,$08,$07,$05,$19,$1A,$00,$00,$00,$19,$1A,$00
			        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$42,$07,$2a

charLine3:   		.byte $00,$00,$00,$00,$04,$0B,$0C,$0D,$04,$0B,$0C,$0D,$04,$0D,$17,$18
			        .byte $04,$0D,$04,$0D,$04,$0B,$21,$00,$04,$0B,$21,$00,$04,$0D,$04,$0D
			        .byte $04,$0B,$0C,$0D,$04,$0D,$00,$00,$17,$18,$04,$0D,$04,$0B,$0C,$0D
			        .byte $04,$0D,$00,$00,$00,$00,$00,$00,$04,$0D,$04,$0D,$04,$0D,$04,$0D
			        .byte $04,$0B,$2B,$2C,$04,$0D,$2E,$2D,$04,$0B,$0C,$0D,$32,$33,$0C,$34
			        .byte $00,$04,$0D,$00,$04,$0D,$04,$0D,$04,$3B,$38,$39,$00,$00,$00,$00
			        .byte $3E,$3F,$0C,$0D,$32,$33,$0C,$34,$04,$3F,$2B,$2C,$04,$0D,$04,$0D
			        .byte $04,$0D,$00,$00,$04,$0D,$04,$0D,$04,$0D,$00,$00,$00,$00,$00,$00
			        .byte $00,$00,$00,$00,$2E,$2D,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
			        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
			        .byte $04,$0D,$00,$00,$00,$04,$0D,$00,$00,$00,$00,$00,$00,$00,$00,$00
			        .byte $00,$17,$18,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
			        .byte $04,$0D,$04,$0D,$04,$0D,$00,$00,$04,$3F,$2B,$2C,$47,$46,$0C,$0D
			        .byte $4A,$49,$0C,$0D,$4D,$33,$0C,$34,$04,$3F,$0C,$34,$00,$04,$3F,$2C
			        .byte $3E,$3F,$0C,$0D,$32,$33,$0C,$34,$00,$00,$00,$00,$00,$17,$18,$00
			        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$2E,$4e,$2c

charLine4:   		.byte $00,$00,$00,$00,$09,$0A,$09,$0A,$09,$13,$10,$0F,$15,$13,$10,$0F
			        .byte $09,$13,$10,$0F,$09,$13,$1B,$1C,$09,$0A,$00,$00,$15,$13,$10,$0a
					.byte $09,$0A,$09,$0A,$09,$0A,$00,$00,$15,$13,$10,$0F,$09,$0A,$09,$0A
					.byte $09,$13,$1B,$1C,$00,$00,$00,$00,$09,$0A,$09,$0A,$15,$13,$10,$0F
					.byte $09,$0A,$00,$00,$15,$13,$1B,$1C,$09,$0A,$09,$0A,$15,$13,$10,$0f
					.byte $00,$09,$0A,$00,$15,$13,$10,$0F,$09,$3C,$37,$3A,$00,$00,$00,$00
					.byte $09,$0A,$09,$0A,$15,$13,$10,$0F,$09,$13,$1B,$1C,$09,$0A,$09,$0a
			        .byte $09,$0A,$00,$00,$09,$13,$10,$13,$10,$0F,$00,$00,$00,$00,$00,$00
			        .byte $00,$00,$00,$00,$43,$1C,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
			        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
			        .byte $15,$13,$1C,$00,$43,$10,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00
			        .byte $44,$10,$0F,$00,$00,$00,$00,$00,$43,$1C,$00,$00,$00,$00,$00,$00
			        .byte $15,$13,$10,$0F,$09,$0A,$00,$00,$09,$13,$1B,$1C,$15,$13,$10,$0F
			        .byte $00,$00,$09,$0A,$15,$13,$10,$0F,$15,$13,$10,$0F,$00,$09,$0A,$00
			        .byte $15,$13,$10,$0F,$15,$13,$10,$0F,$43,$1C,$00,$00,$44,$10,$0F,$00
			        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$43,$1C,$00

					//----------------------------------------------------------
					*=$3600 "screen text"
					//----------------------------------------------------------
screentext:			.encoding "screencode_upper"
					.text "       ICC INTRO COMPETITION 2019       "  
					.text "                                        "
					.text "       DESIGNED AND CODED BY CASE       "
					.text "                                        "
					.text "        SEE SCROLLER FOR CREDITS        "
					.text "                                        "
					.text " ITS NICE TO BE BACK CODING CRAP AGAIN! "
					//----------------------------------------------------------



					//----------------------------------------------------------
					*=$3800 "scroll text"
					//----------------------------------------------------------

					//----------------------------------------------------------
					// use the following codes to control the scroller
					//
					// speedtable: $41,$42,$43,$44,$45,$46
					// pausetable: $c1,$c2,$c3,$c4,$c5
					// bouncetable: $b1,$b2,$b3,$b4,$b5,$b6
					// colortable: $81,$82,$83,$84,$85,$86
					//----------------------------------------------------------
scrolltext:			
					.byte $44 // slow speed 
					.byte $b4 // quick bounce
					.byte $82 // set color to 
					.text "  CASE PRESENTS HIS ENTRY FOR THE                 "

					.byte $45 // slow speed 
					.byte $b6 // quick bounce
					.byte $83 // set color to 

					.text " ICC INTRO COMPETITION 2019                 "

					.byte $46 // slow speed 
					.byte $b3 // quick bounce
					.byte $85 // set color to 

					.text "WELL IT HAS BEEN ALONG TIME SINCE I CODED ANYTHING ON THE C64 AND"
					.text " JUST ASLONG SINCE I HAD TO WRITE SCROLL TEXT .......                   "
					.byte $81
					.text " THE CREDITS ALWAYS TAKE UP SOME MEMORY, SO .... ALL CODING, IDEAS "
					.text "1X1 FONT AND LOGO BY                  "

					.byte $46 // slow speed 
					.byte $b2 // quick bounce
					.byte $84 // set color to 
					.text " CASE       "
					.byte $c4
					.text "    LARGE 4X4 FONT BY "
					.byte $46 // slow speed 
					.byte $b2 // quick bounce
					.byte $84 // set color to 
					.text "  UNKNOWN   "
					.byte $c4

					.text "    MUSIC BY "
					.byte $46 // slow speed 
					.byte $b2 // quick bounce
					.byte $84 // set color to 
					.text "  UNKNOWN   "
					.byte $c4




					.text "  IF YOU KNOW WHO MADE THE MUSIC AND GRAPHICS, PLEASE TAG THEM AND I WILL ALTER THE SCROLL TEXT "
					.text "                 "
					.byte $44 // slow speed 
					.byte $b4 // quick bounce
					.byte $82 // set color to 

        			.byte $ff // terminates scroll text
					//----------------------------------------------------------
