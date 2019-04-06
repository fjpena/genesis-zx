; Rutina para pintar el mapa... que Dios me pille confesado

; Entrada:
; 	DE: Mapa + desplazamiento por el mapa en tiles
; 	BC: desplazamiento por el mapa; B: chars (0-2), C: pixels (0,1,2,3)
; 	HL: ancho del mapa en caracteres
;	TablaTiles: tabla con los tiles ya preshifteados (ver create_shifted_tiles.asm), en $B000


; Uso de registros:
;	IYh: contador de altura (16)
;	IYl: contador de anchura en tiles (10)
;	IX: puntero a la Tabla de Tiles. Sigue la sigiente logica:  1111 XXYY YYYZ ZZZZ, donde
;		X: desplazamiento en pixels
;		Y: numero de tile (0-31)
;		Z: bytes dentro del tile (32)
;	HL', DE': se moverán por la pantalla
;	BC': se usa como variable temporal

; IMPORTANTE: ENTRAR CON INTERRUPCIONES DESACTIVADAS!!!!!!!

DrawMap:
	push de
	call ClearMapArea		; limpia la pantalla
	
	ld de, -11
	add hl, de			
	pop de				; calculamos el ancho del mapa - 11, para sumarlo luego
   
	ld a, b
	and a				; si b == 0, llevaremos 3 tiles ya, por lo que se hace uno menos
	jp z, chars_not_zero
	ld a, 10			; si no lo es, hay que hacer un tile m�s (10)
	jr dm_save_sp
chars_not_zero:
	ld a,9
dm_save_sp:
	ld (centertiles),a											
	ld (dm_restoresp+1), SP		; jugamos con SP, así que no puede haber interrupciones activas

	ld iyh, 16			; 16 caracteres de alto
	ld a, c
	rlca
	rlca				; movemos el desplazamiento en pixels a 0000xx00
	or $b0

	ld ixh, a

	exx				; alternate regset
	;ld hl, 16384
	;ld de, 16384
	ld hl, $C000
	ld de, $C000
	ld d,h
	ld e,l
	ld ixl,e
	exx				; cargamos la direccion de la pantalla en HL y DE

draw_loopy:				; Tile mas a la izquierda
	ld a, (de)			; cogemos el primer tile
	inc de
	
	and a				; for empty tiles, we follow a different path
	jp nz, draw_loopy_notzero


	ld a, 3
	sub b				
	exx				; alternate regset
draw_loopy_zero:	
	inc de
	inc hl
	dec a
	jp nz, draw_loopy_zero		; hacemos todo este loop para dejar DE incrementado
	jp go_to_center
			
draw_loopy_notzero:
	exx				; alternate regset
	rrca
	rrca
	rrca				; pasamos el numero de tile a yyy000yy	
	ld b,a
	and 3				; nos quedamos con los dos bits superiores 000000yy
	ld c, a				; y los copiamos a C
	ld a,b

	and $e0				 
	ld ixl, a			; ixl tiene yyy00000, luego IX tiene un puntero al primer byte del tile
	ld a, ixh			
	and $FC				; nos quedamos sólo con los 6 bits más significativos
	or c				
	ld ixh, a			; ixh tiene ahora 1111xxyy



;   	and $e0            
;  	ld b, a            ; en B nos quedamos con los tres bits inferiores yyy00000
;   	ld a, ixh         
;   	and $FC            ; nos quedamos s�lo con los 6 bits más significativos
;   	or c            
;   	ld ixh, a         ; ixh tiene ahora 1111xxyy
;   	ld a,b
;	ld ixl, a         ; ixl tiene yyy00000, luego IX tiene un puntero al primer byte del tile
	exx				; normal regset
			
	ld a, b
	inc a				
	rlca
	rlca
	rlca				; en A tenemos el desplazamiento en chars * 8, para irnos a la primera columna
					; del tile
	or ixl
	ld ixl, a			; y en IXL estamos exactamente en el primero de los bytes
	ld SP, IX			; y ahi tenemos el puntero de la pila

	ld a, 3
	sub b				; en A tenemos el número de columnas a pintar
	; ahora empezamos a pintar el tile en pantalla!
	exx				; alternate regset
leftmost_loop:
	pop bc
	ld (hl), c
	inc h
	ld (hl), b
	inc h
	pop bc
	ld (hl), c
	inc h
	ld (hl), b
	inc h
	pop bc
	ld (hl), c
	inc h
	ld (hl), b
	inc h
	pop bc
	ld (hl), c
	inc h
	ld (hl), b			; los 8 bytes del tile

	inc de				; pasamos al siguiente pixel
	ld h,d
	ld l,e			
	dec a
	jp nz, leftmost_loop 		; dibujamos tantas columnas como hagan falta

go_to_center:
	dec de				; el ultimo tile tendra que hacer OR con el primero del siguiente
	dec hl
	exx				; normal regset

; ahora toca tratar la zona central. Cada tile es de 3 caracteres de ancho, 32/3 = 10 más los picos
	
	ld a, (centertiles)
	ld iyl, a

draw_center_tiles:	
	ld a, (de)			; cogemos el tile
	inc de				; el mapa apunta al siguiente
	
	and a				; for empty tiles, we follow a different path
	jp nz, draw_center_notzero
	exx
	inc de
	inc de
	inc de
	ld h,d
	ld l,e
	jp continue_center_loop		; that is all we do!

draw_center_notzero:
	exx				; alternate regset
	rrca
	rrca
	rrca				; pasamos el numero de tile a yyy000yy	
	ld b,a
	and 3				; nos quedamos con los dos bits superiores 000000yy
	ld c, a				; y los copiamos a C
	ld a,b

	and $e0				 
	ld ixl, a			; ixl tiene yyy00000, luego IX tiene un puntero al primer byte del tile
	ld a, ixh			
	and $FC				; nos quedamos sólo con los 6 bits más significativos
	or c				
	ld ixh, a			; ixh tiene ahora 1111xxyy
;   and $e0            
;   ld b, a            ; en B nos quedamos con los tres bits inferiores yyy00000
;   ld a, ixh         
;   and $FC            ; nos quedamos s�lo con los 6 bits más significativos
;   or c            
;   ld ixh, a         ; ixh tiene ahora 1111xxyy
;   ld a,b
;   ld ixl, a         ; ixl tiene yyy00000, luego IX tiene un puntero al primer byte del tile



	ld SP, IX			; y ahi tenemos el puntero de la pila

; el tile de la izquierda del todo hace OR con lo que haya en pantalla
draw_or_left:
	pop bc
	ld a, (hl)
	or c
	ld (hl),a
	inc h
	ld a, (hl)
	or b
	ld (hl),a
	inc h
	pop bc
	ld a, (hl)
	or c
	ld (hl),a
	inc h
	ld a, (hl)
	or b
	ld (hl),a
	inc h
	pop bc
	ld a, (hl)
	or c
	ld (hl),a
	inc h
	ld a, (hl)
	or b
	ld (hl),a
	inc h
	pop bc
	ld a, (hl)
	or c
	ld (hl),a
	inc h
	ld a, (hl)
	or b
	ld (hl),a

	inc de
	ld h,d
	ld l,e		; siguiente caracter
draw_direct_rest:
	pop bc
	ld (hl),c
	inc h
	ld (hl),b
	inc h
	pop bc
	ld (hl),c
	inc h
	ld (hl),b
	inc h	
	pop bc
	ld (hl),c
	inc h
	ld (hl),b
	inc h
	pop bc
	ld (hl),c
	inc h
	ld (hl),b	; 8 bytes == 1 caracter a lo alto

	inc de
	ld h,d
	ld l,e		; siguiente caracter

	pop bc
	ld (hl),c
	inc h
	ld (hl),b
	inc h
	pop bc
	ld (hl),c
	inc h
	ld (hl),b
	inc h	
	pop bc
	ld (hl),c
	inc h
	ld (hl),b
	inc h
	pop bc
	ld (hl),c
	inc h
	ld (hl),b	; 8 bytes == 1 caracter a lo alto

	inc de
	ld h,d
	ld l,e		; siguiente caracter

	pop bc
	ld (hl),c
	inc h
	ld (hl),b
	inc h
	pop bc
	ld (hl),c
	inc h
	ld (hl),b
	inc h	
	pop bc
	ld (hl),c
	inc h
	ld (hl),b
	inc h
	pop bc
	ld (hl),c
	inc h
	ld (hl),b	; 8 bytes == 1 caracter a lo alto

	ld h,d
	ld l,e		; despues del ultimo no pasamos al siguiente caracter, porque el nuevo hace OR

continue_center_loop:	
	exx		; normal regset
	dec iyl				; continuamos con el loop
	jp nz, draw_center_tiles


; aqui tendria que ir el tile de la derecha del todo. Por no liarlo mas, lo dejamos de momento

draw_loopy_right:				; Tile mas a la derecha

	ld a, (de)			; cogemos el tile
	inc de				; el mapa apunta al siguiente
	and a				; for empty tiles, we follow a different path
	jp nz, draw_right_notzero

	ld a, 4				; si b=0 pintamos 2 columnas, si b=1 0, si b=2 1
	sub b				; la formula es un poco rara, pero funciona
	and 4
	or b				; carry flag=0
	rra				; en A tenemos el número de columnas a pintar				
	exx				; alternate regset
	inc de
	inc hl
	and a
	jp z, draw_next_line
draw_right_zero:	
	inc de
	inc hl
	dec a
	jp nz, draw_right_zero		; hacemos todo este loop para dejar DE incrementado
	jp draw_next_line

draw_right_notzero:
	exx				; alternate regset
	rrca
	rrca
	rrca				; pasamos el numero de tile a yyy000yy	
	ld b,a
	and 3				; nos quedamos con los dos bits superiores 000000yy
	ld c, a				; y los copiamos a C
	ld a,b

	and $e0				 
	ld ixl, a			; ixl tiene yyy00000, luego IX tiene un puntero al primer byte del tile
	ld a, ixh			
	and $FC				; nos quedamos sólo con los 6 bits más significativos
	or c				
	ld ixh, a			; ixh tiene ahora 1111xxyy
 
;  and $e0            
;   ld b, a            ; en B nos quedamos con los tres bits inferiores yyy00000
;   ld a, ixh         
;   and $FC            ; nos quedamos s�lo con los 6 bits más significativos
;   or c            
;   ld ixh, a         ; ixh tiene ahora 1111xxyy
;   ld a,b
;   ld ixl, a         ; ixl tiene yyy00000, luego IX tiene un puntero al primer byte del tile

	ld SP, IX			; y ahi tenemos el puntero de la pila

	
	; ahora empezamos a pintar el tile en pantalla!
; el tile de la izquierda del todo hace OR con lo que haya en pantalla
draw_or_rightmost_left:
	pop bc
	ld a, (hl)
	or c
	ld (hl),a
	inc h
	ld a, (hl)
	or b
	ld (hl),a
	inc h
	pop bc
	ld a, (hl)
	or c
	ld (hl),a
	inc h
	ld a, (hl)
	or b
	ld (hl),a
	inc h
	pop bc
	ld a, (hl)
	or c
	ld (hl),a
	inc h
	ld a, (hl)
	or b
	ld (hl),a
	inc h
	pop bc
	ld a, (hl)
	or c
	ld (hl),a
	inc h
	ld a, (hl)
	or b
	ld (hl),a

	inc de
	ld h,d
	ld l,e		; siguiente caracter


	exx			; normal regset
	ld a, 4				; si b=0 pintamos 2 columnas, si b=1 0, si b=2 1
	sub b				; la formula es un poco rara, pero funciona
	and 4
	or b				; carry flag=0
	rra				; en A tenemos el número de columnas a pintar	
	exx		;alternate regset
	and a
	jp z, draw_next_line	
	
		
		
rightmost_loop:
	pop bc
	ld (hl), c
	inc h
	ld (hl), b
	inc h
	pop bc
	ld (hl), c
	inc h
	ld (hl), b
	inc h
	pop bc
	ld (hl), c
	inc h
	ld (hl), b
	inc h
	pop bc
	ld (hl), c
	inc h
	ld (hl), b			; los 8 bytes del tile

	inc de				; pasamos la siguiente pixel
	ld h,d
	ld l,e			
	dec a
	jp nz, rightmost_loop 		; dibujamos tantas columnas como hagan falta

draw_next_line:	
	ld a, d
	cp $C1		; si DE es $C100 pasamos al siguiente tercio de pantalla
	jr nz, not_second_third
	ld d, $C8
not_second_third:	
	ld h,d
	ld l,e		; pasamos a la siguiente linea
	exx		; normal regset
	ld a, b
	and a				; si b != 0, hicimos un tile de mas
	jp z, b_not_zero
	dec de
b_not_zero:	
	ex de, hl	; hl tiene ahora el punto en el mapa, y de el ancho-11
	add hl, de
	ex de, hl	; ya hemos saltado a la siguiente linea

	dec iyh
	jp nz, draw_loopy


dm_restoresp: ld sp, 0	; este valor es modificado al principio!
	   ret

centertiles db 0


; This little function will clear the map area on screen, via a series of PUSH
; We will assume ints are disabled!!!!!
; Save DE before calling!!!!!

ClearMapArea:
	ld (cma_restoresp+1),sp
	ld sp, $c000+4096	; al final del area de pantalla (segundo tercio)
	ld de, 0		; limpiamos con blancos
	ld a, b	
	ld b, 64		; vamos a hacer series de 32 push (16 pixels por push), 64x2 lineas
cma_loop:
	push de	
	push de	
	push de
	push de	
	push de
	push de
	push de
	push de
	push de
	push de
	push de
	push de
	push de
	push de
	push de
	push de
	
	push de	
	push de	
	push de
	push de	
	push de
	push de
	push de
	push de
	push de
	push de
	push de
	push de
	push de
	push de
	push de
	push de

	djnz cma_loop								
cma_restoresp:
	ld sp,0
	ld b, a
	ret



