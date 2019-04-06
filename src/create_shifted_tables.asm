; Creación de la tabla de tiles shifteados,
; a partir de los 32 tiles normales de un nivel
;
; Entrada:
;	- DE: Puntero a la tabla
;	-  B: Número de tiles (1-32)
;
; Salida:
;	- TablaTiles (alineada en 4K): Tiles shifteados,
;         según el siguiente criterio:
;
;		$1011 XXYY YYYZ ZZZZ
;
;	   XX: shift (0-3)
;	   YYYYY: número de tile (0-31)
;	   ZZZZZ: bytes del tile shifteado (32). Los primeros 8 bytes
;                 son la primera columna, los siguientes la segunda, etc.
;
;	   Los tiles de origen están también organizados por columnas
;
; El algoritmo es sencillo (y no muy rápido), y se basa en hacer rl (hl) a lo bruto,
; después de organizar la tabla con los datos iniciales
;


CreaTablaTiles:	
	push iy
				; en primer lugar, limpia la tabla de tiles con ceros
	push de
	push bc
	ld hl, TablaTiles
	ld de, TablaTiles+1
	ld (hl),0
	ld bc, 4095
	ldir
	pop bc
	pop de
	
	ld hl, TablaTiles
	;ld a, b
	ld ixh, b		; ixh == number of tiles
	push bc
	push de
	ld bc, 8
copiatiles_outerloop:
	ld ixl, 24
	add hl,bc		; skip the first tile
copiatiles_innerloop:
	ld a, (de)
	inc de
	ld (hl), a		; just copy for now
	inc hl
	dec ixl
	jr nz, copiatiles_innerloop
	dec ixh
	jr nz, copiatiles_outerloop ; do it number of tiles times

	pop de
	pop bc
	ld hl, TablaTiles+1024
	;ld a, b
	ld ixh, b		; ixh == number of tiles
	push bc
	push de
	ld bc, 8
copiatiles_outerloop2:
	ld ixl, 24
	add hl,bc		; skip the first tile
copiatiles_innerloop2:
	ld a, (de)
	inc de
	ld (hl), a		; just copy for now
	inc hl
	dec ixl
	jr nz, copiatiles_innerloop2
	dec ixh
	jr nz, copiatiles_outerloop2 ; do it number of tiles times

	pop de
	pop bc
	ld hl, TablaTiles+2048
	;ld a, b
	ld ixh, b		; ixh == number of tiles
	push bc
	push de
	ld bc, 8
copiatiles_outerloop3:
	ld ixl, 24
	add hl,bc		; skip the first tile
copiatiles_innerloop3:
	ld a, (de)
	inc de
	ld (hl), a		; just copy for now
	inc hl
	dec ixl
	jr nz, copiatiles_innerloop3
	dec ixh
	jr nz, copiatiles_outerloop3 ; do it number of tiles times

	pop de
	pop bc
	ld hl, TablaTiles+3072
	;ld a, b
	ld ixh, b		; ixh == number of tiles
	push bc
;	push de			; not needed anymore
	ld bc, 8
copiatiles_outerloop4:
	ld ixl, 24
	add hl,bc		; skip the first tile
copiatiles_innerloop4:
	ld a, (de)
	inc de
	ld (hl), a		; just copy for now
	inc hl
	dec ixl
	jr nz, copiatiles_innerloop4
	dec ixh
	jr nz, copiatiles_outerloop4 ; do it number of tiles times

;	pop de
;	jr endCreaTablaTiles
	; Ahora tenemos los tiles ya copiados, con el primer tile en blanco
	; El siguiente paso es recorrer la tabla de tiles, haciendo rl (hl)


	ld iyl, 0		; un contador simple
	ld hl, TablaTiles
	ld (SaveTablaTiles), hl

newshift:
	ld a, iyl
	add a, 2
	ld iyh, a		; iyh mantiene el contador actual
	ld iyl, a		; iyl mantiene el contador para la siguiente vez
	ld hl, (SaveTablaTiles)
	ld de, 1024
	add hl, de
	ld (SaveTablaTiles), hl ; sumando 1024 pasamos al siguiente set de tiles shifteados
shiftloop:
	ld hl, (SaveTablaTiles)
	pop bc			; cleanup
	;ld a, b
	ld ixh, b		; ixh== numero de tiles
	push bc
	ld bc, 8		; lo usamos luego

rotateouter:
	ld a, 8			; 8 pixels de alto
	ld de, 24

looprotate:
	and a			; limpiar el carry flag
	add hl, de		; pasamos al ultimo
	rl (hl)
	rla			; guardar el carry flag en el bit menos significativo de A
	sbc hl, bc		; pasamos al anterior
	rra			; recuperamos el carry flag
	rl (hl)
	rla
	sbc hl, bc
	rra
	rl (hl)
	rla
	sbc hl, bc
	rra
	rl (hl)			; rotamos los 4 bytes

	inc hl			; siguiente línea
	dec a
	jr nz, looprotate
	
	add hl, de		; pasamos al siguiente megatile

	dec ixh
	jp nz, rotateouter	; seguimos
	dec iyh
	jp nz, shiftloop	; lo hacemos iyh veces
	ld a, iyl
	cp 6
	jr nz, newshift		; calculate new shift and go

endCreaTablaTiles:
	pop bc 		; cleanup
	pop iy
	ret



;-----------------------------------------------------				
;  VARIABLE AREA
;-----------------------------------------------------				

SaveTablaTiles dw 0

