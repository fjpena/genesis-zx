
;FUNCIONES DEL PLAYER DE FX
;___________________________

;INICIA FX
;REPRODUCE_EFECTO
;FIN_EFECTO


;VARIBLES Y FUNCIONES EXTERNAS
;_____________________________

;TABLA_EFECTOS		DW TABLA DE DIRECCIONES DE LOS EFECTOS
;INTERR			DB
;EXT_WORD		FUNCION DE EXTRACCION DE LA DIRECCION DEL FX POR Nº DE ORDEN


; *** AJUSTAR CANAL DE EFECTOS ANTES DE INICIAR EL REPRODUCTOR
; *** LLAMAR EN CADA INTERRUPCION A REPRODUCE_EFECTO

; VARIABLES
;___________

;INTERR:         DB     00              ;INTERRUPTORES 1=ON 0=OFF
                                        ;BIT 0=CARGA CANCION ON/OFF
                                        ;BIT 1=PLAYER ON/OFF
                                        ;BIT 2=SONIDOS ON/OFF

					;BIT 3=EFECTOS ON/OFF
;EFECTOS

;N_EFECTO	       	DB	0   ;DB : NUMERO DE SONIDO
PUNTERO_EFECTO		DW	0   ;DW : PUNTERO DEL SONIDO QUE SE REPRODUCE;REPRODUCE EFECTOS
CANAL_EFECTOS		DB  	1   ;DB : 1:CANAL A - 2:CANAL B - OTRO:CANAL C

; B: effect number


INICIA_EFECTO:	LD	A,B
		LD      HL,TABLA_EFECTOS
                CALL    EXT_WORD
                LD      [PUNTERO_EFECTO],HL
		LD      HL,INTERR
                SET     3,[HL]
                RET

REPRODUCE_EFECTO:

                LD      HL,INTERR
                BIT     3,[HL]          	;ESTA ACTIVADO EL EFECTO?
                RET     Z
                LD      HL,[PUNTERO_EFECTO]
                LD      A,[HL]
                CP      $FF
                JP      Z,FIN_EFECTO
                LD	B,A			;FRECUENCIA FINO
                INC     HL
                LD	A,[HL]
                RRCA
                RRCA
                RRCA
                RRCA
                AND     00001111B
                LD	C,A			;	FRECUENCIA GRAVE
		LD      A,10111000B		;	ELIMINA RUIDO
       		LD      [PSG_REG_SEC+7],A
                LD      A,[HL]
                DEC	A			;DEC A PARA BAJR VOLUMEN!! O PONER VARIABLE
                ;DEC	A
                AND     00001111B

                LD	D,A			;VOLUMEN
                INC     HL			;INCREMENTA Y GUARDA EL PUNTERO
                LD      [PUNTERO_EFECTO],HL
           	LD	IX,PSG_REG_SEC
                LD	A,[CANAL_EFECTOS]	;SELECCION DE CANAL *********
                CP	1
                JR	Z,RS_CANALA
                CP	2
		JR	Z,RS_CANALB
		
RS_CANALC:  	LD      [IX+4],B
		LD      [IX+5],C
                LD      [IX+10],D
                RET
		
RS_CANALA:	LD      [IX+0],B
		LD      [IX+1],C
                LD      [IX+8],D
                RET
                
RS_CANALB:	LD      [IX+2],B
		LD      [IX+3],C
                LD      [IX+9],D
                RET
                
FIN_EFECTO:     LD      HL,INTERR
                RES     3,[HL]			;DESACTIVA EFECTO
                RET         


;EFECTO ENSAMBLE
ASSEMBLE_EFFECT:
      LD   HL,SFX_ENSAMBLE
      LD   DE,PSG_REG
      LD   BC,14
      LDIR
      CALL   ROUT
      RET
      

SFX_ENSAMBLE:   DB   $FF,$02,$01,$03,0,0,0,10111000B,$10,0,$00,$C0,$00,$0C



TABLA_EFECTOS:	DW	EXPLOSION_NAVE,BLAST,ABRE_CAPSULA,DANO,DISPARO_DOBLE,TECLA_INERCIA,CAPSULA_RECOGIDA,DISPARO_SIMPLE,DISPARO_TRIPLE,JUEGO_START,EXPLOSION_SENCILLA_ORGANICA,LASER,SCORE,DISPARO_MULTI,DISPARO_HOMMING ; .......


EXPLOSION_NAVE:

DB $B0,$0E
DB $DB,$0E
DB $43,$1C
DB $84,$1C
DB $00,$2E
DB $00,$3E
DB $A0,$0D
DB $CB,$0D
DB $33,$1E
DB $74,$1E
DB $00,$2D
DB $00,$3D
DB $05,$4D
DB $90,$0B
DB $BB,$0B
DB $23,$1C
DB $64,$1C
DB $00,$2B
DB $00,$3B
DB $05,$48
DB $50,$58
DB $B0,$07
DB $BB,$07
DB $23,$18
DB $64,$18
DB $00,$27
DB $00,$37
DB $05,$46
DB $50,$56
DB $B0,$05
DB $BB,$05
DB $23,$16
DB $64,$16
DB $00,$25
DB $00,$34
DB $05,$44
DB $50,$53
DB $B0,$04
DB $DB,$04
DB $43,$15
DB $84,$15
DB $00,$24
DB $00,$33
DB $05,$42
DB $50,$51
DB $B0,$03
DB $DB,$03
DB $43,$14
DB $84,$14
DB $00,$23
DB $00,$33
DB $05,$42
DB $50,$51
DB $FF


;blast

BLAST:

DB $10,$0C
DB $15,$0D
DB $00,$0D
DB $25,$0F
DB $30,$0E
DB $35,$0D
DB $80,$0D
DB $00,$81
DB $10,$09
DB $15,$0A
DB $00,$0A
DB $25,$0B
DB $30,$0A
DB $35,$0A
DB $80,$09
DB $00,$81
DB $10,$06
DB $15,$07
DB $00,$07
DB $25,$08
DB $30,$07
DB $35,$07
DB $80,$05
DB $00,$81
DB $B0,$0E
DB $DB,$0E
DB $43,$1C
DB $84,$1C
DB $00,$2E
DB $00,$3E
DB $B0,$0D
DB $DB,$0D
DB $43,$1E
DB $84,$1E
DB $00,$2D
DB $00,$3D
DB $05,$4D
DB $B0,$0B
DB $DB,$0B
DB $43,$1C
DB $84,$1C
DB $00,$2B
DB $00,$3B
DB $05,$48
DB $50,$58
DB $B0,$07
DB $DB,$07
DB $43,$18
DB $84,$18
DB $00,$27
DB $00,$37
DB $05,$46
DB $50,$56
DB $B0,$05
DB $DB,$05
DB $43,$16
DB $84,$16
DB $00,$25
DB $00,$34
DB $05,$44
DB $50,$53
DB $B0,$04
DB $DB,$04
DB $43,$15
DB $84,$15
DB $00,$24
DB $00,$33
DB $05,$42
DB $50,$51
DB $B0,$03
DB $DB,$03
DB $43,$14
DB $84,$14
DB $00,$23
DB $00,$33
DB $05,$42
DB $50,$51
DB $FF

;abre capsula

ABRE_CAPSULA:

DB $C3,$0D
DB $B8,$0A
DB $C6,$0A
DB $7C,$07
DB $0F,$04
DB $09,$04
DB $18,$0B
DB $08,$07
DB $0F,$0B
DB $09,$0C
DB $18,$0A
DB $08,$09
DB $0F,$08
DB $09,$09
DB $18,$06
DB $08,$05
DB $FF

;daño

DANO:

DB $C3,$0D
DB $08,$0A
DB $46,$0A
DB $2C,$07
DB $0F,$04
DB $09,$04
DB $18,$0B
DB $08,$07
DB $FF

;disparo doble

DISPARO_DOBLE:

DB $3A,$0C
DB $29,$0B
DB $22,$0A
DB $2B,$0A
DB $39,$09
DB $52,$09
DB $69,$08
DB $80,$07
DB $A7,$07
DB $D7,$06
DB $FF

;tecla inercia

TECLA_INERCIA:

DB $33,$0D
DB $13,$0D
DB $77,$0B
DB $33,$0C
DB $13,$0B
DB $77,$0B
DB $97,$0A
DB $33,$0A
DB $13,$09
DB $77,$09
DB $97,$08
DB $33,$09
DB $13,$08
DB $77,$07
DB $97,$06
DB $33,$07
DB $13,$06
DB $77,$06
DB $97,$05
DB $33,$06
DB $13,$05
DB $77,$05
DB $97,$04
DB $FF


;capsula recogida

CAPSULA_RECOGIDA:

DB $50,$0B
DB $C0,$0D
DB $50,$0E
DB $40,$0F
DB $40,$0D
DB $20,$0C
DB $40,$0B
DB $20,$0A
DB $A0,$09
DB $20,$0A
DB $A0,$0B
DB $20,$0B
DB $20,$0A
DB $10,$08
DB $20,$06
DB $10,$05
DB $A0,$04
DB $20,$06
DB $A0,$05
DB $20,$04
DB $20,$04
DB $10,$03
DB $20,$03
DB $10,$02
DB $FF

;disparo simple

DISPARO_SIMPLE:

DB $27,$0C
DB $1C,$0B
DB $22,$0A
DB $2B,$0A
DB $39,$09
DB $52,$09
DB $69,$08
DB $80,$07
DB $A7,$07
DB $D7,$06
DB $FF


;start!!!

JUEGO_START:

DB $97,$0A
DB $77,$0C
DB $33,$0D
DB $10,$0C
DB $97,$0B
DB $77,$0C
DB $33,$0C
DB $13,$0A
DB $87,$09
DB $67,$09
DB $23,$08
DB $77,$09
DB $57,$08
DB $25,$07
DB $10,$06
DB $67,$07
DB $47,$06
DB $13,$06
DB $08,$05
DB $67,$06
DB $47,$05
DB $13,$05
DB $08,$04
DB $77,$07
DB $47,$06
DB $13,$05
DB $10,$04
DB $57,$05
DB $37,$04
DB $13,$04
DB $08,$03
DB $47,$04
DB $27,$03
DB $13,$02
DB $08,$02
DB $57,$04
DB $37,$03
DB $13,$03
DB $08,$03
DB $47,$02
DB $27,$02
DB $13,$01
DB $08,$01
DB $FF

;disparo triple

DISPARO_TRIPLE:

DB $4F,$0B
DB $1B,$0B
DB $65,$0C
DB $1A,$0B
DB $74,$0C
DB $60,$0B
DB $85,$0B
DB $A1,$0A
DB $C7,$09
DB $54,$17
DB $FF

;EXPLOSION SENCILLA ORGANICA

EXPLOSION_SENCILLA_ORGANICA:

DB $80,$0E
DB $25,$0D
DB $00,$0D
DB $55,$0D
DB $60,$0D
DB $65,$0C
DB $C0,$0C
DB $00,$81
DB $30,$09
DB $35,$0A
DB $00,$0A
DB $45,$0B
DB $40,$0A
DB $55,$0A
DB $B0,$09
DB $00,$81
DB $20,$06
DB $25,$07
DB $00,$07
DB $FF

LASER:
  
DB $85,$0A
DB $60,$1B
DB $00,$1C
DB $00,$2C
DB $00,$1B
DB $00,$2B
DB $00,$1B
DB $00,$2B
DB $00,$1B
DB $00,$2A
DB $00,$19
  DB $FF

SCORE:
  
DB $25,$0D
DB $18,$0C
DB $36,$0B
DB $1A,$0B
DB $40,$0B
DB $1C,$0A
DB $38,$0D
DB $1E,$0C
DB $20,$0C
DB $28,$0D
DB $27,$0D
DB $28,$0F
DB $27,$0E
DB $38,$0D
DB $37,$0B
DB $38,$0A
DB $37,$09
DB $28,$08
DB $37,$08
DB $38,$07
DB $37,$0B
DB $38,$0A
DB $47,$09
DB $48,$07
DB $37,$06
DB $37,$05
DB $37,$05
DB $37,$07
DB $38,$07
DB $27,$06
DB $28,$06
DB $27,$05
  DB $FF
  
DISPARO_MULTI:

DB $20,$0B
DB $90,$0C
DB $20,$2C
DB $20,$3C
DB $20,$4B
DB $20,$1B
DB $20,$1B
DB $20,$3A
DB $20,$49
DB $20,$17
DB $20,$15
DB $B0,$18
DB $20,$36
DB $FF

DISPARO_HOMMING:

DB $20,$0B
DB $80,$0C
DB $20,$1C
DB $00,$2B
DB $00,$3A
DB $20,$1A
DB $80,$0A
DB $70,$09
DB $60,$08
DB $50,$07
DB $40,$07
DB $30,$06
DB $20,$05
DB $FF

