org 24100

load_1:		
		ld hl, loader_table_genesis
		ld (loader_table), hl
		ld hl, genesis_str
		ld (loader_file),hl

start:
       		ld b, 7         ; RAM 7, ROM 3 (+3DOS)
       		call setrambank
deact_ramdisk:
       		ld hl, $0000
       		ld de, $0000
       		call DOS_EST_1346
       		JP NC, 0        ; reset if failed

       		ld hl, (loader_file) ; 
       		ld bc, $0001    	; File handle 0, exclusive read
       		ld de, $0002	; Open, place pointer after  header
       		call DOS_OPEN	; open file
       		jp nc, 0        	; reset if open failed (change into something better!!!)


start_load:    	ld hl, (loader_table)
	        ld b, (hl)	; b==iteratons
		inc hl
load_loop:	push bc
		ld a, (hl)		
		ld ixl, a
		inc hl
		ld a, (hl)
		ld ixh, a
		inc hl			; ix==load address, hl== address of size
		ld e, (hl)		
		inc hl
		ld d, (hl)
		inc hl			; ix==load address, hl== start of store address, de == length
		ld a, (hl)		
		ld (destination_address), a
		inc hl
		ld a, (hl)		; destination_address ==store address
		ld (destination_address+1), a
		inc hl
		ld c, (hl)		; C == RAM BANK & compressed
		
		push hl
				
		ld a,ixh
		ld h, a
		ld a,ixl	
		ld l, a
		push ix	
		push de			; save for later
		push bc
                
		ld bc, $0000	; b=file descriptor 0, load to RAM BANK 0
       		call DOS_READ	; read bytes
       		;jp nc, 0       	; reset if read failed (change into something better!!!)                                                                
                
   		; check for errors!!!!!
   		pop bc
		pop de		
		pop ix		; hl still in the stack
;		pop hl
		
		push bc
		ld a, c
		
		and $07			
                ld b, a
                call setrambank      ; set RAM BANK 
		pop bc
		ld a, c
		and $80		
		jr z, not_compressed	
compressed:	di
		ld de, (destination_address)		; de = destination address
		push ix
		pop hl			; hl = source address
		push iy
                call depack             ; call depacker		
		pop iy
		jr continue_loop_ei

not_compressed:	; block is not compressed
		push ix
		push de
		pop bc		; bc = length
		pop hl		; hl = source address
		ld de, (destination_address)	; de = dest address
		ldir		; just copy!
continue_loop_ei:
		ei
continue_loop:  ld b, $07
                call setrambank         ; set the RAM BANK back to 7 (+3DOS)
		pop hl			; get the HL pointer back!
		inc hl			; and point to the next block
		pop bc			; get the counter back!
		djnz load_loop
		
		ld e, (hl)
		inc hl
		ld d, (hl)		; get the execution address in de
		push de
		
close_file:
       		ld b, 0
       		call DOS_CLOSE	; close file
       		;jp nc, 0        	; reset if close failed (change into something better!!!)       
       		call DOS_MOTOR_OFF	; disconnect drive motor

set_ram_paging:
		ld b, $10		; RAM 0, ROM 2 (48k BASIC)
		call setrambank


launch_program: pop hl			; get the execution address
		LD IY, 5C3Ah		; re-establish the IY pointer (must be done!)
		jp (hl)			; and run!



; INPUT: B: page to set 


setrambank:
       di
       ld A, ($5B5C)
       and $E8
       or b
       ld BC, $7FFD
       ld ($5b5c), a   ; save in the BASIC variable
       out (c), a
       ei
       ret

; DEPACKER

depack:         ld      ixl,128
apbranch1:      ldi
aploop0:        ld      ixh,1           ;LWM = 0
aploop:         call    ap_getbit
               jr      nc,apbranch1
               call    ap_getbit
               jr      nc,apbranch2
               call    ap_getbit
               jr      nc,apbranch3

               ld      bc,16           ;get an offset
apget4bits:     call    ap_getbit
               rl      c
               jr      nc,apget4bits
               jr      nz,apbranch4
               ld      a,b
apwritebyte:    ld      (de),a          ;write a 0
               inc     de
               jr      aploop0
apbranch4:      and     a
               ex      de,hl           ;write a previous byte (1-15 away from dest)
               sbc     hl,bc
               ld      a,(hl)
               add     hl,bc
               ex      de,hl
               jr      apwritebyte
apbranch3:      ld      c,(hl)          ;use 7 bit offset, length = 2 or 3
               inc     hl
               rr      c
               ret     z               ;if a zero is encountered here, it's EOF
               ld      a,2
               ld      b,0
               adc     a,b
               push    hl
               ld      iyh,b
               ld      iyl,c
               ld      h,d
               ld      l,e
               sbc     hl,bc
               ld      c,a
               jr      ap_finishup2
apbranch2:      call    ap_getgamma     ;use a gamma code * 256 for offset, another gamma code for length
               dec     c
               ld      a,c
               sub     ixh
               jr      z,ap_r0_gamma           ;if gamma code is 2, use old r0 offset,
               dec     a
               ;do I even need this code?
               ;bc=bc*256+(hl), lazy 16bit way
               ld      b,a
               ld      c,(hl)
               inc     hl
               ld      iyh,b
               ld      iyl,c

               push    bc

               call    ap_getgamma

               ex      (sp),hl         ;bc = len, hl=offs
               push    de
               ex      de,hl

               ld      a,4
               cp      d
               jr      nc,apskip2
               inc     bc
               or      a
apskip2:        ld      hl,127
               sbc     hl,de
               jr      c,apskip3
               inc     bc
               inc     bc
apskip3:        pop     hl              ;bc = len, de = offs, hl=junk
               push    hl
               or      a
ap_finishup:    sbc     hl,de
               pop     de              ;hl=dest-offs, bc=len, de = dest
ap_finishup2:   ldir
               pop     hl
               ld      ixh,b
               jr      aploop

ap_r0_gamma:    call    ap_getgamma             ;and a new gamma code for length
               push    hl
               push    de
               ex      de,hl

               ld      d,iyh
               ld      e,iyl
               jr      ap_finishup


ap_getbit:      ld      a,ixl
               add     a,a
               ld      ixl,a
               ret     nz
               ld      a,(hl)
               inc     hl
               rla
               ld      ixl,a
               ret

ap_getgamma:    ld      bc,1
ap_getgammaloop:call    ap_getbit
               rl      c
               rl      b
               call    ap_getbit
               jr      c,ap_getgammaloop
               ret



DOS_EST_1346 	equ $13F
DOS_OPEN 	equ $106
DOS_READ 	equ $112
DOS_CLOSE	equ $109
DOS_MOTOR_OFF	equ $19c

loader_table   dw 0
loader_file    dw 0
destination_address dw 0

genesis_str	db "GENESIS.BIN",$ff


loader_table_genesis:
	db 8		; 8 entries
		
	dw 32768	; load at 32768
	dw 5052		; load 3827 bytes (loading.bin)
	dw 16384	; after loading, copy to address 16384 (screen)
	db $80		; RAM Bank 0, compressed
	
	dw 32768
	dw 4194		; load 1853 bytes (sprites.bin)
	dw 51200	; after loading, copy to 51200
	db $81		; RAM Bank 1, not compressed
	
	dw 32768
	dw 11473	; load 9097 bytes (ram3.bin)
	dw 49152	; after loading, copy to 49152
	db $03		; RAM Bank 3, not compressed

	dw 32768
	dw 13531		; load 9097 bytes (ram4.bin)
	dw 49152	; after loading, copy to 49152
	db $04		; RAM Bank 4, not compressed

	dw 32768
	dw 8668		; load 9097 bytes (ram0.bin)
	dw 49152	; after loading, copy to 49152
	db $80		; RAM Bank 0, compressed

	dw 32768
	dw 8474		; load 9097 bytes (ram6.bin)
	dw 49152	; after loading, copy to 49152
	db $86		; RAM Bank 6, compressed

	dw 24600
	dw 8149		; load 9097 bytes (test.bin)
	dw 24600	; after loading, do not copy
	db $00		; RAM Bank 0, not compressed

	dw 33025
	dw 2263		; load 9097 bytes (engine.bin)
	dw 33025	; after loading, do not copy
	db $00		; RAM Bank 0, not compressed
	
	dw 24600	; randomize usr 24600

