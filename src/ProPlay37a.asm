org 49152

; SPECTRUM PSG proPLAYER - WYZ 2010

; ENSAMBLAR CON AsMSX

; CARACTERISTICAS
; 6 OCTAVAS:            O[2-6]=60 NOTAS
; 4 LONGITUDES DE NOTA: L[0-3]+PUNTILLO 
; PUNTILLO
; COMANDOS:     T:TEMPO
;               I:INSTRUMENTO
;               S:REPRODUCTOR DE EFECTOS CANAL C


; LOS DATOS QUE HAY QUE VARIAR :

; * Nº DE CANCION. 
; * TABLA DE CANCIONES



; POR INCLUIR
; - ELEGIR CANAL DE EFECTOS

; CARGA UNA CANCION COMPRIMIDA		
; A: numero de cancion
LOAD_SONG:
		CALL    UNCOMP_SONG		; Uncompress song
		LD 	A,0
                CALL    CARGA_CANCION
		ret

;___________________________________________________________

;                DB      "PSG PROPLAYER BY WYZ'10"

;___________________________________________________________

INICIO:      	LD   A, (SOUND_SFX)
		AND  2
		JR   Z, NO_EFECTOS
		CALL    REPRODUCE_EFECTO
NO_EFECTOS:
      		CALL    ROUT
      		LD   HL,PSG_REG
      		LD   DE,PSG_REG_SEC
      		LD   BC,14
	        LDIR            
	  	LD   A, (SOUND_SFX)
		AND  1
		RET   Z
                CALL    REPRODUCE_SONIDO

;                jp	PLAY
;                CALL    PLAY	
;  		RET

;PLAY __________________________________________________


PLAY:          	LD      HL,INTERR       ;PLAY BIT 1 ON?
                BIT     1,[HL]
                RET     Z
;TEMPO          
                LD      HL,TTEMPO       ;CONTADOR TEMPO
                INC     [HL]
                LD      A,[TEMPO]
                CP      [HL]
                JR      NZ,PAUTAS
                LD      [HL],0
                
;INTERPRETA      
                LD      IY,PSG_REG
                LD      IX,PUNTERO_A
                LD      BC,PSG_REG+8
                CALL    LOCALIZA_NOTA
                LD      IY,PSG_REG+2
                LD      IX,PUNTERO_B
                LD      BC,PSG_REG+9
                CALL    LOCALIZA_NOTA
                LD      IY,PSG_REG+4
                LD      IX,PUNTERO_C
                LD      BC,PSG_REG+10
                CALL    LOCALIZA_NOTA
                LD      IX,PUNTERO_P    ;EL CANAL DE EFECTOS ENMASCARA OTRO CANAL
                CALL    LOCALIZA_EFECTO              

;PAUTAS 
                
PAUTAS:         LD      IY,PSG_REG+0
                LD      IX,PUNTERO_P_A
                LD      HL,PSG_REG+8
                CALL    PAUTA           ;PAUTA CANAL A
                LD      IY,PSG_REG+2
                LD      IX,PUNTERO_P_B
                LD      HL,PSG_REG+9
                CALL    PAUTA           ;PAUTA CANAL B
                LD      IY,PSG_REG+4
                LD      IX,PUNTERO_P_C
                LD      HL,PSG_REG+10
;                CALL    PAUTA           ;PAUTA CANAL C                
;                RET
;                jp	PAUTA


; PAUTA DE LOS 3 CANALES
; IN:[IX]:PUNTERO DE LA PAUTA
;    [HL]:REGISTRO DE VOLUMEN
;    [IY]:REGISTROS DE FRECUENCIA

; FORMATO PAUTA	
;	    7    6     5     4   3-0                     3-0  
; BYTE 1 [LOOP|OCT-1|OCT+1|SLIDE|VOL] - BYTE 2 [ | | | |PITCH]

PAUTA:          BIT     4,[HL]        ;SI LA ENVOLVENTE ESTA ACTIVADA NO ACTUA PAUTA
                RET     NZ

		LD	A,[IY+0]
		LD	B,[IY+1]
		OR	B
		RET	Z


                PUSH	HL
                ;LD      L,[IX+0]
                ;LD      H,[IX+1]
                		
                ;LD	A,[HL]		;COMPRUEBA SLIDE BIT 4
		;BIT	4,A
		;JR	Z,PCAJP4
		;LD	L,[IY+0]	;FRECUENCIA FINAL
		;LD	H,[IY+1]
		;SBC	HL,DE
		;JR	Z,PCAJP4
		;JR	C,SLIDE_POS
		;EX	DE,HL
		;RRC	D		;/4
		;RR	E
		;RRC	D
		;RR	E


		;ADC	HL,DE
		;LD	[IY+0],L
		;LD	[IY+1],H
SLIDE_POS:		
		;POP	HL
		;RET
                
PCAJP4:         LD      L,[IX+0]
                LD      H,[IX+1]         
		LD	A,[HL]
		
		BIT     7,A		;LOOP / EL RESTO DE BITS NO AFECTAN
                JR      Z,PCAJP0
                AND     00011111B       ;LOOP PAUTA [0,32]X2!!!-> PARA ORNAMENTOS
                RLCA			;X2
                LD      D,0
                LD      E,A
                SBC     HL,DE
                LD      A,[HL]

PCAJP0:		BIT	6,A		;OCTAVA -1
		JR	Z,PCAJP1
		LD	E,[IY+0]
		LD	D,[IY+1]

;		AND	A
		RRC	D
		RR	E
		LD	[IY+0],E
		LD	[IY+1],D
		JR	PCAJP2
		
PCAJP1:		BIT	5,A		;OCTAVA +1
		JR	Z,PCAJP2
		LD	E,[IY+0]
		LD	D,[IY+1]

;		AND	A
		RLC	E
		RL	D
		LD	[IY+0],E
		LD	[IY+1],D		


PCAJP2:		INC     HL
		PUSH	HL
		LD	E,A
		LD	A,[HL]		;PITCH DE FRECUENCIA
		LD	L,A
		AND	A
		LD	A,E
		JR	Z,ORNMJP1

                LD	A,[IY+0]	;SI LA FRECUENCIA ES 0 NO HAY PITCH
                ADD	A,[IY+1]
                AND	A
                LD	A,E
                JR	Z,ORNMJP1
                

		BIT	7,L
		JR	Z,ORNNEG
		LD	H,$FF
		JR	PCAJP3
ORNNEG:		LD	H,0
		
PCAJP3:		LD	E,[IY+0]
		LD	D,[IY+1]
		ADC	HL,DE
		LD	[IY+0],L
		LD	[IY+1],H
ORNMJP1:	POP	HL
		
		INC	HL
                LD      [IX+0],L
                LD      [IX+1],H
PCAJP5:         POP	HL
                AND	00001111B	;VOLUMEN FINAL
                LD      [HL],A
                RET


;PLAYER OFF

PLAYER_OFF:   LD   A,[INTERR]
      AND   00000100B         ;***** IMPORTANTE SI NO HAY MUSICA ****
      LD   [INTERR],A

      XOR   A
      LD   HL,PSG_REG
      LD   DE,PSG_REG+1
      LD   BC,14
      LD   [HL],A
      LDIR

      LD   HL,PSG_REG_SEC
      LD   DE,PSG_REG_SEC+1
      LD   BC,14
      LD   [HL],A
      LDIR

   
      LD      A,10111000B      ; **** POR SI ACASO ****
      LD      [PSG_REG+7],A
      CALL   ROUT
      CALL   FIN_SONIDO
      RET



;VUELCA BUFFER DE SONIDO AL PSG

ROUT:      	XOR     A
ROUT_A0:        LD      DE,$FFBF
                LD      BC,$FFFD
                LD      HL,PSG_REG_SEC
LOUT:           OUT     [C],A
                LD      B,E
                OUTI 
                LD      B,D
                INC     A
                CP      13
                JR      NZ,LOUT
                OUT     [C],A
                LD      A,[HL]
                AND     A
                RET     Z
                LD      B,E
                OUTI
                XOR     A
                LD      [PSG_REG_SEC+13],A
                LD	[PSG_REG+13],A
                RET


;CARGA UNA CANCION
;IN:[A]=Nº DE CANCION

CARGA_CANCION:  LD      HL,INTERR       ;CARGA CANCION

                SET     1,[HL]          ;REPRODUCE CANCION
                LD      HL,SONG
                LD      [HL],A          ;Nº A

                

;DECODIFICAR
;IN-> INTERR 0 ON
;     SONG

;CARGA CANCION SI/NO

DECODE_SONG:    LD      A,[SONG]

;LEE CABECERA DE LA CANCION
;BYTE 0=TEMPO

                LD      HL,TABLA_SONG
                CALL    EXT_WORD
                LD      A,[HL]
                LD      [TEMPO],A
		XOR	A
		LD	[TTEMPO],A
                
;HEADER BYTE 1
;[-|-|-|-|-|-|-|LOOP]

                INC	HL		;LOOP 1=ON/0=OFF?
;                LD	A,[HL]
;                BIT	0,A
		bit	0,[hl]
                JR	Z,NPTJP0

;                PUSH	HL
;                LD	HL,INTERR
;                SET	4,[HL]
;                POP	HL
                ld	a,[INTERR]
                or	16
                ld	[INTERR],a        

NPTJP0:         INC	HL		;2 BYTES RESERVADOS
                INC	HL
                INC	HL

;BUSCA Y GUARDA INICIO DE LOS CANALES EN EL MODULO MUS

		LD	[PUNTERO_P_DECA],HL
		LD	E,$3F			;CODIGO INTRUMENTO 0
		LD	B,$FF			;EL MODULO DEBE TENER UNA LONGITUD MENOR DE $FF00 ... o_O!
BGICMODBC1:	XOR	A			;BUSCA EL BYTE 0
		CPIR
		DEC	HL
		DEC	HL
		LD	A,E			;ES EL INSTRUMENTO 0??
		CP	[HL]
		INC	HL
		INC	HL
		JR	Z,BGICMODBC1

		LD	[PUNTERO_P_DECB],HL

BGICMODBC2:	XOR	A			;BUSCA EL BYTE 0
		CPIR
		DEC	HL
		DEC	HL
		LD	A,E
		CP	[HL]			;ES EL INSTRUMENTO 0??
		INC	HL
		INC	HL
		JR	Z,BGICMODBC2

		LD	[PUNTERO_P_DECC],HL
		
BGICMODBC3:	XOR	A			;BUSCA EL BYTE 0
		CPIR
		DEC	HL
		DEC	HL
		LD	A,E
		CP	[HL]			;ES EL INSTRUMENTO 0??
		INC	HL
		INC	HL
		JR	Z,BGICMODBC3
		LD	[PUNTERO_P_DECP],HL
		
                
;LEE DATOS DE LAS NOTAS
;[|][|||||] LONGITUD\NOTA

INIT_DECODER:   LD      DE,BUFFER_DEC
                LD      [PUNTERO_A],DE
                LD	HL,[PUNTERO_P_DECA]
                CALL    DECODE_CANAL    ;CANAL A
                LD	[PUNTERO_DECA],HL
                
                LD      DE,BUFFER_DEC+$0010
                LD      [PUNTERO_B],DE
                LD	HL,[PUNTERO_P_DECB]
                CALL    DECODE_CANAL    ;CANAL B
                LD	[PUNTERO_DECB],HL
                
                LD      DE,BUFFER_DEC+$0020
                LD      [PUNTERO_C],DE
                LD	HL,[PUNTERO_P_DECC]
                CALL    DECODE_CANAL    ;CANAL C
                LD	[PUNTERO_DECC],HL
                
                LD      DE,BUFFER_DEC+$0030
                LD      [PUNTERO_P],DE
                LD	HL,[PUNTERO_P_DECP]
                CALL    DECODE_CANAL    ;CANAL P
                LD	[PUNTERO_DECP],HL

                RET


;DECODIFICA NOTAS DE UN CANAL
;IN [DE]=DIRECCION DESTINO
;NOTA=0 FIN CANAL
;NOTA=1 SILENCIO
;NOTA=2 PUNTILLO
;NOTA=3 COMANDO I

DECODE_CANAL:   LD      A,[HL]
                AND     A               ;FIN DEL CANAL?
                JR      Z,FIN_DEC_CANAL
                CALL    GETLEN

                CP      00000001B       ;ES SILENCIO?
                JR      NZ,NO_SILENCIO
;                SET     6,A
                ld	a,01000001B
                JR      NO_MODIFICA
                
NO_SILENCIO:    CP      00111110B       ;ES PUNTILLO?
                JR      NZ,NO_PUNTILLO
;                OR      A
                RRC     B
                XOR     A
                JR      NO_MODIFICA

NO_PUNTILLO:    CP      00111111B       ;ES COMANDO?
                JR      NZ,NO_MODIFICA
                BIT     0,B             ;COMADO=INSTRUMENTO?
                JR      Z,NO_INSTRUMENTO   
                LD      A,11000001B     ;CODIGO DE INSTRUMENTO      
                LD      [DE],A
                INC     HL
                INC     DE
                LD      A,[HL]          ;Nº DE INSTRUMENTO
                LD      [DE],A
                INC     DE
                INC	HL
                JR      DECODE_CANAL
                
NO_INSTRUMENTO: BIT     2,B
                JR      Z,NO_ENVOLVENTE
                LD      A,11000100B     ;CODIGO ENVOLVENTE
                LD      [DE],A
                INC     DE
                INC	HL
                JR      DECODE_CANAL
     
NO_ENVOLVENTE:  BIT     1,B
                JR      Z,NO_MODIFICA           
                LD      A,11000010B     ;CODIGO EFECTO
                LD      [DE],A                  
                INC     HL                      
                INC     DE                      
                LD      A,[HL]                  
                CALL    GETLEN   
                
NO_MODIFICA:    LD      [DE],A
                INC     DE
                XOR     A
                DJNZ    NO_MODIFICA
;		SET     7,A
;		SET 	0,A
		or	129
                LD      [DE],A
                INC     DE
                INC	HL
                RET			;** JR      DECODE_CANAL
                
FIN_DEC_CANAL:  ;SET     7,A
		or	128
                LD      [DE],A
                INC     DE
                RET

GETLEN:         LD      B,A
                AND     00111111B

                PUSH    AF

;                LD      A,B		; 4
;                AND     11000000B	; 7
;                RLCA			; 4
;                RLCA			; 4
;                INC     A		; 4
;                LD      B,A		; 4
;                LD      A,10000000B	; 7 34
;DCBC0:          RLCA			; 4
;                DJNZ    DCBC0		;13/8

		LD 	A,1		; 7
		RL	B		; 8
		JR      NC,GETLEN_NO1	;12/7
		LD	A,4		;   7
GETLEN_NO1:	RL	B		; 8
		JR	NC,GETLEN_NO2	;12/7
		RLCA			;   4
GETLEN_NO2:
                LD      B,A
                POP     AF
                RET
                
                

        
                
;REPRODUCE EFECTOS DE SONIDO 

REPRODUCE_SONIDO:

		LD      HL,INTERR   
                BIT     2,[HL]          ;ESTA ACTIVADO EL EFECTO?
                RET     Z
                LD      HL,[PUNTERO_SONIDO]
                LD      A,[HL]
                CP      $FF
                JR      Z,FIN_SONIDO
                LD      [PSG_REG_SEC+0],A
                INC     HL
                LD      A,[HL]
                RRCA
                RRCA
                RRCA
                RRCA
                AND     00001111B
                LD      [PSG_REG_SEC+1],A
                LD      A,[HL]
                AND     00001111B
                LD      [PSG_REG_SEC+8],A
                INC     HL
                LD      A,[HL]
                AND     A
                JR      Z,NO_RUIDO
                LD      [PSG_REG_SEC+6],A
                LD      A,10110000B
                JR      SI_RUIDO
NO_RUIDO:       LD      A,10111000B
SI_RUIDO:       LD      [PSG_REG_SEC+7],A
       
                INC     HL
                LD      [PUNTERO_SONIDO],HL
                RET
FIN_SONIDO:     LD      HL,INTERR
                RES     2,[HL]

FIN_NOPLAYER:	LD      A,10111000B
       		LD      [PSG_REG+7],A
                RET         
                


;LOCALIZA NOTA CANAL A
;IN [PUNTERO_A]

LOCALIZA_NOTA:  LD      L,[IX+0]       		;HL=[PUNTERO_A_C_B]
                LD      H,[IX+1]
                LD      A,[HL]
                AND     11000000B      		;COMANDO?
                CP      11000000B
                JR      NZ,LNJP0

;BIT[0]=INSTRUMENTO
                
COMANDOS:       LD      A,[HL]
                BIT     0,A             	;INSTRUMENTO
                JR      Z,COM_EFECTO

                INC     HL
                LD      A,[HL]          	;Nº DE PAUTA
                INC     HL
                LD      [IX+00],L
                LD      [IX+01],H
                LD      HL,TABLA_PAUTAS
                CALL    EXT_WORD
                LD      [IX+18],L
                LD      [IX+19],H
                LD      [IX+12],L
                LD      [IX+13],H
                LD      L,C
                LD      H,B
                RES     4,[HL]        		;APAGA EFECTO ENVOLVENTE
                XOR     A
                LD      [PSG_REG_SEC+13],A
                LD	[PSG_REG+13],A
                JR      LOCALIZA_NOTA

COM_EFECTO:     BIT     1,A             	;EFECTO DE SONIDO
                JR      Z,COM_ENVOLVENTE

                INC     HL
                LD      A,[HL]
                INC     HL
                LD      [IX+00],L
                LD      [IX+01],H
                jp	INICIA_SONIDO
;                CALL    INICIA_SONIDO
;                RET

COM_ENVOLVENTE: BIT     2,A
                RET     Z               	;IGNORA - ERROR            
           
                INC     HL
                LD      [IX+00],L
                LD      [IX+01],H
                LD      L,C
                LD      H,B
                LD	[HL],00010000B          ;ENCIENDE EFECTO ENVOLVENTE
                JR      LOCALIZA_NOTA
                
              
LNJP0:          LD      A,[HL]
                INC     HL
                BIT     7,A
                JR      Z,NO_FIN_CANAL_A	;
;                BIT	0,A
;                JR	Z,FIN_CANAL_A
                rrca
                jr	nc,FIN_CANAL_A

FIN_NOTA_A:	LD      E,[IX+6]
		LD	D,[IX+7]	;PUNTERO BUFFER AL INICIO
		LD	[IX+0],E
		LD	[IX+1],D
		LD	L,[IX+30]	;CARGA PUNTERO DECODER
		LD	H,[IX+31]
		PUSH	BC
                CALL    DECODE_CANAL    ;DECODIFICA CANAL
                POP	BC
                LD	[IX+30],L	;GUARDA PUNTERO DECODER
                LD	[IX+31],H
                JP      LOCALIZA_NOTA
                
FIN_CANAL_A:    LD	HL,INTERR	;LOOP?
                BIT	4,[HL]              
;                JR      NZ,FCA_CONT
;                CALL	PLAYER_OFF
;                RET
                jp	z,PLAYER_OFF

FCA_CONT:	LD	L,[IX+24]	;CARGA PUNTERO INICIAL DECODER
		LD	H,[IX+25]
		LD	[IX+30],L
		LD	[IX+31],H
		JR      FIN_NOTA_A
                
NO_FIN_CANAL_A: LD      [IX+0],L        ;[PUNTERO_A_B_C]=HL GUARDA PUNTERO
                LD      [IX+1],H
                AND     A               ;NO REPRODUCE NOTA SI NOTA=0
;                JR      Z,FIN_RUTINA
		ret	z
                BIT     6,A             ;SILENCIO?
                JR      Z,NO_SILENCIO_A
                LD	A,[BC]
                AND	00010000B
                JR	NZ,SILENCIO_ENVOLVENTE
;                XOR     A
                LD	[BC],A		;RESET VOLUMEN
                LD	[IY+0],A
                LD	[IY+1],A
		RET
		
SILENCIO_ENVOLVENTE:
		LD	A,$FF
                LD	[PSG_REG+11],A
                LD	[PSG_REG+12],A               
                XOR	A
                LD	[PSG_REG+13],A                               
                LD	[IY+0],A
                LD	[IY+1],A
                RET

NO_SILENCIO_A:  CALL    NOTA            ;REPRODUCE NOTA
                LD      L,[IX+18]       ;HL=[PUNTERO_P_A0] RESETEA PAUTA 
                LD      H,[IX+19]
                LD      [IX+12],L       ;[PUNTERO_P_A]=HL
                LD      [IX+13],H
FIN_RUTINA:     RET


;LOCALIZA EFECTO
;IN HL=[PUNTERO_P]

LOCALIZA_EFECTO:LD      L,[IX+0]       ;HL=[PUNTERO_P]
                LD      H,[IX+1]
                LD      A,[HL]
                CP      11000010B
                JR      NZ,LEJP0

                INC     HL
                LD      A,[HL]
                INC     HL
                LD      [IX+00],L
                LD      [IX+01],H
;                jp	INICIA_SONIDO
;                CALL    INICIA_SONIDO
;                RET

;INICIA EL SONIDO Nº [A]

INICIA_SONIDO:  LD      HL,TABLA_SONIDOS
                CALL    EXT_WORD
                LD      [PUNTERO_SONIDO],HL
                LD      HL,INTERR
                SET     2,[HL]
                RET

LEJP0:          INC     HL
                BIT     7,A
                JR      Z,NO_FIN_CANAL_P	;
                BIT	0,A
                JR	Z,FIN_CANAL_P
FIN_NOTA_P:	LD      DE,[CANAL_P]
		LD	[IX+0],E
		LD	[IX+1],D
		LD	HL,[PUNTERO_DECP]	;CARGA PUNTERO DECODER
		PUSH	BC
		CALL    DECODE_CANAL    	;DECODIFICA CANAL
		POP	BC
                LD	[PUNTERO_DECP],HL	;GUARDA PUNTERO DECODER
                JP      LOCALIZA_EFECTO
                
FIN_CANAL_P:	LD	HL,[PUNTERO_P_DECP]	;CARGA PUNTERO INICIAL DECODER
		LD	[PUNTERO_DECP],HL
		JR      FIN_NOTA_P
                
NO_FIN_CANAL_P: LD      [IX+0],L        ;[PUNTERO_A_B_C]=HL GUARDA PUNTERO
                LD      [IX+1],H
                RET
                

;NOTA : REPRODUCE UNA NOTA
;IN [A]=CODIGO DE LA NOTA
;   [IY]=REGISTROS DE FRECUENCIA


NOTA:           ;ADD	6		;*************************
		LD      L,C
                LD      H,B
                BIT     4,[HL]
                LD      B,A
                JR	NZ,EVOLVENTES
                LD	A,B
                LD      HL,DATOS_NOTAS
                RLCA                    ;X2
                LD      D,0
                LD      E,A
                ADD     HL,DE
                LD      A,[HL]
                LD      [IY+0],A
                INC     HL
                LD      A,[HL]
                LD      [IY+1],A
                RET

;IN [A]=CODIGO DE LA ENVOLVENTE
;   [IY]=REGISTRO DE FRECUENCIA

EVOLVENTES:     
		PUSH	AF
		CALL	ENV_RUT1
		LD	DE,$0000
		LD      [IY+0],E
                LD      [IY+1],D		
	
		POP	AF	
		ADD	A,36
		CALL	ENV_RUT1
		
	
		LD	A,E
                LD      [PSG_REG+11],A
                LD	A,D
                LD      [PSG_REG+12],A
                LD      A,$0C
                LD      [PSG_REG+13],A
                RET

;IN[A] NOTA
ENV_RUT1:	LD      HL,DATOS_NOTAS
		RLCA                    ;X2
                LD      D,0
                LD      E,A
                ADD     HL,DE
                LD	E,[HL]
		INC	HL
		LD	D,[HL]
                RET



EXT_WORD:       LD      D,0
                SLA     A               ;*2
                LD      E,A
                ADD     HL,DE
                LD      E,[HL]
                INC     HL
                LD      D,[HL]
                EX      DE,HL
                RET


; aPPack decompressor
; original source by dwedit
; very slightly adapted by utopian
; optimized by Metalbrain

;hl = source
;de = dest

depack:		ld	ixl,128
apbranch1:	ldi
aploop0:	ld	ixh,1		;LWM = 0
aploop:		call 	ap_getbit
		jr 	nc,apbranch1
		call 	ap_getbit
		jr 	nc,apbranch2
		ld 	b,0
		call 	ap_getbit
		jr 	nc,apbranch3
		ld	c,16		;get an offset
apget4bits:	call 	ap_getbit
		rl 	c
		jr	nc,apget4bits
		jr 	nz,apbranch4
		ld 	a,b
apwritebyte:	ld 	(de),a		;write a 0
		inc 	de
		jr	aploop0
apbranch4:	and	a
		ex 	de,hl 		;write a previous byte (1-15 away from dest)
		sbc 	hl,bc
		ld 	a,(hl)
		add	hl,bc
		ex 	de,hl
		jr	apwritebyte
apbranch3:	ld 	c,(hl)		;use 7 bit offset, length = 2 or 3
		inc 	hl
		rr 	c
		ret 	z		;if a zero is encountered here, it is EOF
		ld	a,2
		adc	a,b
		push 	hl
		ld	iyh,b
		ld	iyl,c
		ld 	h,d
		ld 	l,e
		sbc 	hl,bc
		ld 	c,a
		jr	ap_finishup2
apbranch2:	call 	ap_getgamma	;use a gamma code * 256 for offset, another gamma code for length
		dec 	c
		ld	a,c
		sub	ixh
		jr 	z,ap_r0_gamma		;if gamma code is 2, use old r0 offset,
		dec 	a
		;do I even need this code?
		;bc=bc*256+(hl), lazy 16bit way
		ld 	b,a
		ld 	c,(hl)
		inc 	hl
		ld	iyh,b
		ld	iyl,c

		push 	bc
		
		call 	ap_getgamma

		ex 	(sp),hl		;bc = len, hl=offs
		push 	de
		ex 	de,hl

		ld	a,4
		cp	d
		jr 	nc,apskip2
		inc 	bc
		or	a
apskip2:	ld 	hl,127
		sbc 	hl,de
		jr 	c,apskip3
		inc 	bc
		inc 	bc
apskip3:	pop 	hl		;bc = len, de = offs, hl=junk
		push 	hl
		or 	a
ap_finishup:	sbc 	hl,de
		pop 	de		;hl=dest-offs, bc=len, de = dest
ap_finishup2:	ldir
		pop 	hl
		ld	ixh,b
		jr 	aploop

ap_r0_gamma:	call 	ap_getgamma		;and a new gamma code for length
		push 	hl
		push 	de
		ex	de,hl

		ld	d,iyh
		ld	e,iyl
		jr 	ap_finishup


ap_getbit:	ld	a,ixl
		add	a,a
		ld	ixl,a
		ret	nz
		ld	a,(hl)
		inc	hl
		rla
		ld	ixl,a
		ret

ap_getgamma:	ld 	bc,1
ap_getgammaloop: call 	ap_getbit
		rl 	c
		rl 	b
		call 	ap_getbit
		jr 	c,ap_getgammaloop
		ret

; Uncompress song on decompression buffer
; A: number of the song to decompress

UNCOMP_SONG:	ld 	hl, TABLA_SONG_CMP
		call	EXT_WORD
		ld 	de, BUFFER_UNCOMP
		call 	depack
		ret

;BANCO DE INSTRUMENTOS 2 BYTES POR INT.

;[0][RET 2 OFFSET]
;[1][+-PITCH]


;BANCO DE INSTRUMENTOS 2 BYTES POR INT.

;[0][RET 2 OFFSET]
;[1][+-PITCH]

TABLA_PAUTAS: DW PAUTA_1,PAUTA_2,PAUTA_3,PAUTA_4,PAUTA_5,PAUTA_6,PAUTA_7;,PAUTA_8,PAUTA_9,PAUTA_10,PAUTA_11,PAUTA_12,PAUTA_13,PAUTA_14,PAUTA_15,PAUTA_16,PAUTA_17,PAUTA_18


INCLUDE	"parasol_a.mus.asm"


;DATOS DE LOS EFECTOS DE SONIDO

;EFECTOS DE SONIDO



TABLA_SONIDOS:  DW      SONIDO1,SONIDO2,SONIDO3,SONIDO4,SONIDO5;,SONIDO6,SONIDO7;,SONIDO8


                

;DATOS MUSICA



TABLA_SONG:     DW      BUFFER_UNCOMP;SONG_0;,SONG_1,SONG_2;,SONG_3          ;******** TABLA DE DIRECCIONES DE ARCHIVOS MUS
TABLA_SONG_CMP: DW	SONG_0,SONG_1,SONG_2,SONG_3,SONG_4,SONG_5,SONG_6,SONG_7,SONG_8,SONG_9,SONG_10,SONG_11,SONG_12,SONG_13,SONG_14;,SONG_15

;DATOS_NOTAS:    .INCBIN "C:/EM/BRMSX/PLAYER/NOTAS.DAT"        ;DATOS DE LAS NOTAS


DATOS_NOTAS:    DW $0000,$0000
;		;DW $077C,$0708
;		DW $06B0,$0640,$05EC,$0594,$0544,$04F8,$04B0,$0470,$042C,$03FD
;		DW $03BE,$0384,$0358,$0320,$02F6,$02CA,$02A2,$027C,$0258,$0238,$0216,$01F8
;		DW $01DF,$01C2,$01AC,$0190,$017B,$0165,$0151,$013E,$012C,$011C,$010A,$00FC
;		DW $00EF,$00E1,$00D6,$00C8,$00BD,$00B2,$00A8,$009F,$0096,$008E,$0085,$007E
;		DW $0077,$0070,$006B,$0064,$005E,$0059,$0054,$004F,$004B,$0047,$0042,$003F
;		DW $003B,$0038,$0035,$0032,$002F,$002C,$002A,$0027,$0025,$0023,$0021,$001F
;		DW $001D,$001C,$001A,$0019,$0017,$0016,$0015,$0013,$0012,$0011,$0010,$000F
		

;TABLA3:	;dw $0CDA,$0C22,$0B73,$0ACF,$0A33,$09A1,$0917,$0894,$0819,$07A4,$0737,$06CF,
;		dw $066D,$0611,$05BA,$0567,$051A,$04D0,$048B,$044A,$040C,$03D2,$039B,$0367
;		dw $0337,$0308,$02DD,$02B4,$028D,$0268,$0246,$0225,$0206,$01E9,$01CE,$01B4
;		dw $019B,$0184,$016E,$015A,$0146,$0134,$0123,$0112,$0103,$00F5,$00E7,$00DA
;		dw $00CE,$00C2,$00B7,$00AD,$00A3,$009A,$0091,$0089,$0082,$007A,$0073,$006D
;		dw $0067,$0061,$005C,$0056,$0052,$004D,$0049,$0045,$0041,$003D,$003A,$0036
;		dw $0033,$0031,$002E,$002B,$0029,$0027,$0024,$0022,$0020,$001F,$001D,$001B
;		dw $001A,$0018,$0017,$0016,$0014,$0013,$0012,$0011,$0010,$000F,$000E,$000D


		DW	$688,$62A,$5D2,$57E,$52F,$4E5,$49E,$45C,$41D,$3E2,$3AA,$376
		DW	$344,$315,$2E9,$2BF,$297,$272,$24F,$22E,$20E,$1F1,$1D5,$1BB
		DW	$1A2,$18A,$174,$15F,$14B,$139,$127,$117,$107,$F8,$EA,$DD
		DW	$D1,$C5,$BA,$AF,$A5,$9C,$93,$8B,$83,$7C,$75,$6E
		DW	$68,$62,$5D,$57,$52,$4E,$49,$45,$41,$3E,$3A,$37
		DW	$34,$31,$2E,$2B,$29,$27,$24,$22,$20,$1F,$1D,$1B
		DW	$1A,$18,$17,$15,$14,$13,$12,$11,$10,$F,$E,$D



SONG_0: INCBIN "parasol.mus.bin" 		; Main menu
SONG_1: INCBIN "genesis_gangway.mus.bin"	; Game over
SONG_2: INCBIN "genesis_fin.mus.bin"		; Happy end
SONG_3: INCBIN "Genesis_warrior.mus.bin"	; Happy end for losers without inertia
SONG_4: INCBIN "genesis_jeff2.mus.bin"		; Level 1
SONG_5: INCBIN "genesis_line.mus.bin"		; Level 2
SONG_6: INCBIN "genesis_compote.mus.bin"	; Level 3
SONG_7: INCBIN "genesis_alice.mus.bin"		; Level 4
SONG_8: INCBIN "genesis_homage.mus.bin"		; Level 5
SONG_9: INCBIN "Genesis_equinox_v2.mus.bin"	; Level 6
SONG_10: INCBIN "genesis_jeff2.mus.bin"		; Level 7
SONG_11: INCBIN "genesis_microint.mus.bin"	; Final boss level 1 and 5
SONG_12: INCBIN "genesis_words.mus.bin"		; Final boss level 2 and 6
SONG_13: INCBIN "genesis_town.mus.bin"		; Final boss level 3 and 7
SONG_14: INCBIN "genesis_hoc.mus.bin"		; Final boss level 4
;SONG_15: INCBIN "iloveretroworks.mus.bin"	; Loading screen... only for the disk version

; Include effects player

INCLUDE	"genesis_sfx.asm"


; VARIABLES__________________________


INTERR:         DB     00               ;INTERRUPTORES 1=ON 0=OFF
                                        ;BIT 0=CARGA CANCION ON/OFF
                                        ;BIT 1=PLAYER ON/OFF
                                        ;BIT 2=SONIDOS ON/OFF
                                        ;BIT 3=EFECTOS ON/OFF

;MUSICA **** EL ORDEN DE LAS VARIABLES ES FIJO ******



SONG:           DB     00               ;DBNº DE CANCION
TEMPO:          DB     00               ;DB TEMPO
TTEMPO:         DB     00               ;DB CONTADOR TEMPO
PUNTERO_A:      DW     00               ;DW PUNTERO DEL CANAL A
PUNTERO_B:      DW     00               ;DW PUNTERO DEL CANAL B
PUNTERO_C:      DW     00               ;DW PUNTERO DEL CANAL C

CANAL_A:        DW     BUFFER_DEC       ;DW DIRECION DE INICIO DE LA MUSICA A
CANAL_B:        DW     BUFFER_DEC+$10   ;DW DIRECION DE INICIO DE LA MUSICA B
CANAL_C:        DW     BUFFER_DEC+$20  ;DW DIRECION DE INICIO DE LA MUSICA C

PUNTERO_P_A:    DW     00               ;DW PUNTERO PAUTA CANAL A
PUNTERO_P_B:    DW     00               ;DW PUNTERO PAUTA CANAL B
PUNTERO_P_C:    DW     00               ;DW PUNTERO PAUTA CANAL C

PUNTERO_P_A0:   DW     00               ;DW INI PUNTERO PAUTA CANAL A
PUNTERO_P_B0:   DW     00               ;DW INI PUNTERO PAUTA CANAL B
PUNTERO_P_C0:   DW     00               ;DW INI PUNTERO PAUTA CANAL C


PUNTERO_P_DECA:	DW     00		;DW PUNTERO DE INICIO DEL DECODER CANAL A
PUNTERO_P_DECB:	DW     00		;DW PUNTERO DE INICIO DEL DECODER CANAL B
PUNTERO_P_DECC:	DW     00		;DW PUNTERO DE INICIO DEL DECODER CANAL C

PUNTERO_DECA:	DW     00		;DW PUNTERO DECODER CANAL A
PUNTERO_DECB:	DW     00		;DW PUNTERO DECODER CANAL B
PUNTERO_DECC:	DW     00		;DW PUNTERO DECODER CANAL C       


;CANAL DE EFECTOS - ENMASCARA OTRO CANAL

PUNTERO_P:      DW     00           	;DW PUNTERO DEL CANAL EFECTOS
CANAL_P:        DW     BUFFER_DEC+$30 	;DW DIRECION DE INICIO DE LOS EFECTOS
PUNTERO_P_DECP:	DW     00		;DW PUNTERO DE INICIO DEL DECODER CANAL P
PUNTERO_DECP:	DW     00	;DW PUNTERO DECODER CANAL P

PSG_REG:        DB      00,00,00,00,00,00,00,10111000B,00,00,00,00,00,00,00    ;DB [11] BUFFER DE REGISTROS DEL PSG
PSG_REG_SEC:    DB      00,00,00,00,00,00,00,10111000B,00,00,00,00,00,00,00    ;DB [11] BUFFER SECUNDARIO DE REGISTROS DEL PSG



;ENVOLVENTE_A    EQU     $D033           ;DB
;ENVOLVENTE_B    EQU     $D034           ;DB
;ENVOLVENTE_C    EQU     $D035           ;DB


;EFECTOS DE SONIDO

N_SONIDO:       DB      0               ;DB : NUMERO DE SONIDO
PUNTERO_SONIDO: DW      0               ;DW : PUNTERO DEL SONIDO QUE SE REPRODUCE
; SOUND AND FX SELECTOR

SOUND_SFX:	DB	3		; 0: Silence; 1: Music only; 2: SFX ony; 3: Musix+SFX
;EFECTOS

;N_EFECTO:       DB      0               ;DB : NUMERO DE SONIDO
;PUNTERO_EFECTO: DW      0               ;DW : PUNTERO DEL SONIDO QUE SE REPRODUCE
                               
BUFFER_UNCOMP:  DS	5410		; Space for the largest MUS file

BUFFER_DEC:     DS      $40		;************************* mucha atencion!!!!
					; aqui se decodifica la cancion hay que dejar suficiente espacio libre.
					;*************************


