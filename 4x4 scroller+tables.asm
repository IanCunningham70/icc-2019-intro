

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




					//----------------------------------------------------------

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
