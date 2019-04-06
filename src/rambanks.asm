; Soy muy vago, y me copio + modifico rutinas originales de Metalbrain :)


; B: nueva página RAM a poner en $C000 - $FFFF
; Asumimos interrupciones deshabilitadas, ojito!!!!!!

setrambank:	ld a, (23388)		; Variable del sistema con el valor previo
		and $f8			; conservar bits altos
		or b			; ponemos la página B arriba
		ld bc, $7ffd		; Puerto en el que escribir
     		ld	(23388),a	;Actualizar variable del sistema
     		out	(c),a		;Direccionar
		ret		

; Cambia la pantalla visible, sin hacer nada más
		
switchscreen:	ld	a,(23388)	;Variable del sistema con el valor previo
     		xor	8		;cambiar pantalla
     		ld	bc,32765	;Puerto en el que escribir
;     		di			;Interrupciones no, gracias
     		ld	(23388),a	;Actualizar variable del sistema
     		out	(c),a		;Direccionar
 ;    		ei			;Habilitar interrupciones
		ret			;Volver
