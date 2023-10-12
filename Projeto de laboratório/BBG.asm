;***********************************************************
;Projeto de Laboratório
;Bruno Baptista Guerra
;Turma 6.1 
;Sistemas Embarcados I
;***********************************************************
;Inicializacao sistema
;***********************************************************
segment code
	..start:
	; iniciar os registros de segmento DS e SS e o ponteiro de pilha SP.
		MOV 	AX, data					; AX <- data. Carrega o endereço de dados em AX.
		MOV		DS, AX						; DS <- AX. Move o conteudo de AX para DS.
		MOV 	AX, stack					; AX <- stack. Carrega o endereço de stack em AX.
		MOV 	SS, AX						; SS <- AX. Move o conteudo de AX para SS.
		MOV 	SP, stacktop				; SP <- stacktop. Carrega o endereço de stacktop em SP.

		xor 	ax, ax
		mov 	es, ax
		mov     ax, [es:int9*4]				; carregou ax com offset anterior (teclado).
		mov     [offset_dos], ax       		; offset_dos guarda o end. para qual ip de int 9 estava apontando anteriormente.
		mov     ax, [es:int9*4+2]     		; cs_dos guarda o end. anterior de cs (teclado).
		mov     [cs_dos], ax
		cli
		mov     [es:int9*4+2], cs
		mov     WORD [es:int9*4], keyint
		sti

	; salvar modo corrente de video(vendo como está o modo de video da maquina)
		mov  	ah, 0Fh
		int  	10h
		mov  	[modo_anterior], al   

	; alterar modo de video para gráfico 640x480 16 cores
		mov     al, 12h
		mov     ah, 0
		int     10h
	
	; desenhar retangulo
		mov		byte[cor], branco_intenso	
		mov		ax, 0						;inicio da linha em x.
		push	ax
		mov		ax, 0						;inicio da linha em y.
		push	ax
		mov		ax, 639						;fim da linha em x.
		push	ax
		mov		ax, 0						;fim da linha em y.
		push	ax
		call	line
		
		mov		byte[cor], branco_intenso	
		mov		ax, 0						;inicio da linha em x.
		push	ax
		mov		ax, 479 					;inicio da linha em y.
		push	ax
		mov		ax, 639						;fim da linha em x.
		push	ax	
		mov		ax, 479 					;fim da linha em y.
		push	ax
		call	line
		
		mov		byte[cor], branco_intenso	
		mov		ax, 0						;inicio da linha em x.
		push	ax
		mov		ax, 0						;inicio da linha em y.
		push	ax
		mov		ax, 0						;fim da linha em x.
		push	ax
		mov		ax, 479						;fim da linha em y.
		push	ax
		call	line
		
		mov		byte[cor], branco_intenso	
		mov		ax, 639 					;inicio da linha em x.
		push	ax
		mov		ax, 0						;inicio da linha em y.
		push	ax
		mov		ax, 639 					;fim da linha em x.
		push	ax
		mov		ax, 479						;fim da linha em y.
		push	ax
		call	line
		
		mov		byte[cor], branco_intenso	
		mov		ax, 0 						;inicio da linha em x.
		push	ax
		mov		ax, 429						;inicio da linha em y.
		push	ax
		mov		ax, 639 					;fim da linha em x.
		push	ax
		mov		ax, 429						;fim da linha em y.
		push	ax
		call	line
		
		;escrever uma mensagem
		mov     cx, 56						; numero de caracteres
		mov     bx, 0
		mov     dh, 1						;linha 0-29
		mov     dl, 11						;coluna 0-79
		mov		byte[cor], branco_intenso
	l1:
		call	cursor
    	mov     al, [bx+mens1]
		call	caracter
    	inc     bx							;proximo caracter
		inc		dl							;avanca a coluna
    	loop    l1
	
	inicio: ;loop do programa
		; calcula e desenha o circulo
		; verificar colisão com a raquete
		cmp 	[x], word 589 ; [x_raquete-10] 599-10
			jne pula
		cmp 	byte [dirX], 1
			jne pula
		mov 	bx, [yc_raquete]
		sub 	bx, word 45
		cmp 	[y], bx ;yi_raquete
			jb 	pula
		add 	bx, word 90
		cmp 	[y], bx ;yf_raquete
			jg 	pula
		; Bateu na raquete: inc placar do jogador e altera a direção x da bola
		mov 	[dirX], byte 0
		cmp 	byte [mens2+23], 0x39
			je 	incd
		inc 	byte [mens2+23]
			jmp atty
	incd:
		mov 	byte [mens2+23], 0x30
		inc 	byte [mens2+22]
			jmp atty
	pula: ; pega a posição em x e calcula se precisa alterar a direção
		cmp 	[x], word 619
			jg 	dcx
		cmp 	[x], word 20
			jg 	atty
		mov 	[dirX], byte 1
			jmp atty
	dcx: 
		mov 	[dirX], byte 0
		;incrementa placar da maquina
		cmp 	byte[mens2+28], 0x39
			je 	incd1
		inc 	byte[mens2+28]
			jmp atty
	incd1:
		mov 	byte[mens2+28], 0x30
		inc 	byte[mens2+27]
	atty: ; pega a posição em y e calcula se precisa alterar a direção
		cmp 	[y], word 409
			jg 	dcy
		cmp 	[y], word 20
			jg 	calcx
		mov 	[dirY],byte 1
			jmp calcx
	dcy: 
		mov 	[dirY], byte 0
	calcx:						;com base na direção, calcula novo valor de x (incrementa ou decrementa)
		cmp 	[dirX], byte 0
			je 	dccx
		add 	[x], word 10;5
			jmp calcy
	dccx: 
		sub 	[x], word 10;5
	calcy:						;com base na direção, calcula novo valor de y (incrementa ou decrementa)
		cmp 	[dirY], byte 0
			je 	dccy
		add 	[y], word 10;5
			jmp desenha
	dccy: 
		sub 	[y], word 10;5
	desenha: ;utiliza posição x, y e rotina do "linec.asm" para desenhar circulo vermelho
		mov		byte[cor], vermelho		;circulo vermelho
		mov		ax, [x]
		push	ax
		mov		ax, [y]
		push	ax
		mov		ax, 10
		push	ax
		call	full_circle

		mov		byte[cor], branco_intenso	;
		call d_raq

	; Atualiza a mensagem que tem o placar e velocidade atual do jogo

    	mov     cx, 60					;numero de caracteres
    	mov     bx, 0
    	mov     dh, 2					;linha 0-29
    	mov     dl, 10					;coluna 0-79
		mov		byte[cor], branco_intenso
	l2:
		call	cursor
    	mov     al,[bx+mens2]
		call	caracter
    	inc     bx						;proximo caracter
		inc		dl						;avanca a coluna
    	loop    l2
		mov 	ah, 86H ; Delay (interrupção)
		mov 	cx, [T1]
		mov 	dx, [T2]
		int 	15H
		;utiliza posição x,y e rotina do "linec.asm" para desenhar circulo preto
		mov		byte[cor], preto		;circulo preto
		mov		ax,[x]
		push	ax
		mov		ax,[y]
		push	ax
		mov		ax,10
		push	ax
		call	full_circle
		
		mov		byte[cor], preto	
		call d_raq

		; tratamento do teclado
		mov     ax,[p_i]
		cmp     ax,[p_t]
		je      inicio1
		inc     word[p_t]
		and     word[p_t],7
		mov     bx,[p_t]
		mov     al, [bx+tecla]

		cmp     al, byte [esc]
			je  sai
		cmp 	al, byte [setaup] 		;sobe
			je 	incraq  
		cmp 	al, byte [setadown] 	;desce
			je 	decraq
		cmp 	al, byte[plus] 			;+
			je 	aumenta
		cmp 	al, byte[minus] 		;-
			je 	diminui
			jmp inicio1
	incraq: 
		cmp 	[yc_raquete], word 378 	; (yc_raquete+25+inc) < linha de cima-1 (429-1)
			jge max 
		add 	[yc_raquete], word 25 	; incremento da raquete
			jmp inicio
	max: 
		mov 	[yc_raquete], word 403
			jmp inicio
	decraq: 
		cmp 	[yc_raquete], word 52 	;(yc_raquete-25-inc) > linha de baixo+1 (1+1)
			jbe min
		sub 	[yc_raquete], word 25
			jmp inicio
	min:
		mov 	[yc_raquete], word 26
			jmp inicio
	inicio1:					
			jmp inicio					; volta para o inicio do loop do programa
	aumenta:
		; aumenta velocidade = diminui delay (tempo de delay = T1:T2)
		cmp 	word [T2], 0x5240
			je 	inicio1 ;minimo
		sub 	word [T2], 0x5000
		inc 	byte [mens2+59]
			jmp inicio
	diminui:
		; diminui velocidade = aumenta delay (tempo de delay = T1:T2)
		cmp 	word [T2], 0xF240
			je 	inicio1 ;maximo
		add 	word [T2], 0x5000
		dec 	byte [mens2+59]
			jmp inicio

;***************************************************************
; MODO SAIDA PROGRAMA
;***************************************************************
	sai:						
		cli		; Retirando a keyint e colocando a ISR original do sistema no seu lugar
		xor     ax, ax
		mov     es, ax

		mov     ax, [cs_dos]
		mov     [es:int9*4+2], ax
		mov     ax, [offset_dos]
		mov     [es:int9*4], ax

		mov 	ah, 0 						; set video mode
		mov 	al, [modo_anterior] 		; recupera o modo anterior
		int 	10h						
		mov 	ax,4c00h				
		int 	21h						
;*****************************************************************

	d_raq:
		push	bp
		mov		bp,sp
		pushf
		push 	ax
		push 	bx
		push	cx
		push	dx
		push	si
		push	di
	  ; Raquete
		mov		ax, 599						;inicio da linha em x.
		push	ax
		mov		ax, [yc_raquete]			;inicio da linha em y.
		sub 	ax, word 25
		push	ax
		mov		ax, 599 					;fim da linha em x.
		push	ax
		mov		ax,[yc_raquete]				;fim da linha em y.
		add 	ax, word 25
		push	ax
		call	line
		
		pop		di
		pop		si
		pop		dx
		pop		cx
		pop		bx
		pop		ax
		popf
		pop		bp
		ret	
;*****************************************************************
	keyint:
		push    ax
		push    bx
		push    ds
		mov     ax,data
		mov     ds,ax
		in      al, kb_data
		inc     WORD [p_i]
		and     WORD [p_i],7
		mov     bx,[p_i]
		mov     [bx+tecla],al
		in      al, kb_ctl
		or      al, 80h
		out     kb_ctl, al
		and     al, 7Fh
		out     kb_ctl, al
		mov     al, eoi
		out     pictrl, al
		pop     ds
		pop     bx
		POP     ax
	iret

;*****************************************************************
	;Funçãoo cursor
	;dh = linha (0-29) e  dl=coluna  (0-79)
	cursor:
		pushf
		push 	ax
		push 	bx
		push	cx
		push	dx
		push	si
		push	di
		push	bp
		mov     ah,2
		mov     bh,0
		int     10h
		pop		bp
		pop		di
		pop		si
		pop		dx
		pop		cx
		pop		bx
		pop		ax
		popf
	ret

;*****************************************************************
	; Função caracter escrito na posição do cursor
	; al= caracter a ser escrito
	; cor definida na variavel cor
	caracter:
		pushf
		push 	ax
		push 	bx
		push	cx
		push	dx
		push	si
		push	di
		push	bp
    	mov     ah,9
    	mov     bh,0
    	mov     cx,1
   		mov     bl,[cor]
    	int     10h
		pop		bp
		pop		di
		pop		si
		pop		dx
		pop		cx
		pop		bx
		pop		ax
		popf
	ret

;*****************************************************************
	; push x; push y; call plot_xy;  (x<639, y<479)
	; cor definida na variavel co
	plot_xy:
		push	bp
		mov		bp,sp
		pushf
		push 	ax
		push 	bx
		push	cx
		push	dx
		push	si
		push	di
	    mov     ah,0ch
	    mov     al,[cor]
	    mov     bh,0
	    mov     dx,479
		sub		dx,[bp+4]
	    mov     cx,[bp+6]
	    int     10h
		pop		di
		pop		si
		pop		dx
		pop		cx
		pop		bx
		pop		ax
		popf
		pop		bp
	ret		4
;*****************************************************************
	;função line do linec.asm
	; push x1; push y1; push x2; push y2; call line;  (x<639, y<479)
	line:
		push	bp
		mov		bp,sp
		pushf				;coloca os flags na pilha
		push 	ax
		push 	bx
		push	cx
		push	dx
		push	si
		push	di
		mov		ax,[bp+10]   		; resgata os valores das coordenadas
		mov		bx,[bp+8]    		; resgata os valores das coordenadas
		mov		cx,[bp+6]    		; resgata os valores das coordenadas
		mov		dx,[bp+4]    		; resgata os valores das coordenadas
		cmp		ax,cx
		je		line2
		jb		line1
		xchg	ax,cx
		xchg	bx,dx
		jmp		line1
	line2:		; deltax=0
		cmp		bx,dx  				;subtrai dx de bx
		jb		line3
		xchg	bx,dx        		;troca os valores de bx e dx entre eles
	line3:	; dx > bx
		push	ax
		push	bx
		call 	plot_xy
		cmp		bx,dx
		jne		line31
		jmp		fim_line
	line31:		
		inc		bx
		jmp		line3
	;deltax <>0
	line1:
		; comparar módulos de deltax e deltay sabendo que cx>ax
		; cx > ax
		push	cx
		sub		cx,ax
		mov		[deltax],cx
		pop		cx
		push	dx
		sub		dx,bx
		ja		line32
		neg		dx
	line32:		
		mov		[deltay],dx
		pop		dx

		push	ax
		mov		ax,[deltax]
		cmp		ax,[deltay]
		pop		ax
		jb		line5

	; cx > ax e deltax>deltay
		push	cx
		sub		cx,ax
		mov		[deltax],cx
		pop		cx
		push	dx
		sub		dx,bx
		mov		[deltay],dx
		pop		dx

		mov		si,ax
	line4:
		push	ax
		push	dx
		push	si
		sub		si,ax	;(x-x1)
		mov		ax,[deltay]
		imul	si
		mov		si,[deltax]		;arredondar
		shr		si,1
; se numerador (DX)>0 soma se <0 subtrai
		cmp		dx,0
		jl		ar1
		add		ax,si
		adc		dx,0
		jmp		arc1
	ar1:		
		sub		ax,si
		sbb		dx,0
	arc1:
		idiv	word [deltax]
		add		ax,bx
		pop		si
		push	si
		push	ax
		call	plot_xy
		pop		dx
		pop		ax
		cmp		si,cx
		je		fim_line
		inc		si
		jmp		line4

	line5:		
		cmp		bx,dx
		jb 		line7
		xchg	ax,cx
		xchg	bx,dx
	line7:
		push	cx
		sub		cx,ax
		mov		[deltax],cx
		pop		cx
		push	dx
		sub		dx,bx
		mov		[deltay],dx
		pop		dx
		mov		si,bx
	line6:
		push	dx
		push	si
		push	ax
		sub		si,bx	;(y-y1)
		mov		ax,[deltax]
		imul	si
		mov		si,[deltay]		;arredondar
		shr		si,1
		; se numerador (DX)>0 soma se <0 subtrai
		cmp		dx,0
		jl		ar2
		add		ax,si
		adc		dx,0
		jmp		arc2
	ar2:		
		sub		ax,si
		sbb		dx,0
	arc2:
		idiv	word [deltay]
		mov		di,ax
		pop		ax
		add		di,ax
		pop		si
		push	di
		push	si
		call	plot_xy
		pop		dx
		cmp		si,dx
		je		fim_line
		inc		si
		jmp		line6
	fim_line:
		pop		di
		pop		si
		pop		dx
		pop		cx
		pop		bx
		pop		ax
		popf
		pop		bp
	ret		8

;*****************************************************************
	; Função full_circle
	; push xc; push yc; push r; call full_circle;  (xc+r<639,yc+r<479)e(xc-r>0,yc-r>0)
	; cor definida na variavel cor					  
	full_circle:
		push 	bp
		mov	 	bp,sp
		pushf                        ;coloca os flags na pilha
		push 	ax
		push 	bx
		push	cx
		push	dx
		push	si
		push	di

		mov		ax,[bp+8]    ; resgata xc
		mov		bx,[bp+6]    ; resgata yc
		mov		cx,[bp+4]    ; resgata r
		
		mov		si,bx
		sub		si,cx
		push    ax			;coloca xc na pilha			
		push	si			;coloca yc-r na pilha
		mov		si,bx
		add		si,cx
		push	ax		;coloca xc na pilha
		push	si		;coloca yc+r na pilha
		call line
		
			
		mov		di,cx
		sub		di,1	 ;di=r-1
		mov		dx,0  	;dx será a variável x. cx é a variavel y
	
	;aqui em cima a lógica foi invertida, 1-r => r-1
	;e as comparações passaram a ser jl => jg, assim garante 
	;valores positivos para d
	stay_full:				;loop
		mov		si,di
		cmp		si,0
		jg		inf_full       ;caso d for menor que 0, seleciona pixel superior (não  salta)
		mov		si,dx		;o jl é importante porque trata-se de conta com sinal
		sal		si,1		;multiplica por doi (shift arithmetic left)
		add		si,3
		add		di,si     ;nesse ponto d=d+2*dx+3
		inc		dx		;incrementa dx
		jmp		plotar_full
	inf_full:	
		mov		si,dx
		sub		si,cx  		;faz x - y (dx-cx), e salva em di 
		sal		si,1
		add		si,5
		add		di,si		;nesse ponto d=d+2*(dx-cx)+5
		inc		dx		;incrementa x (dx)
		dec		cx		;decrementa y (cx)
			
	plotar_full:	
		mov		si,ax
		add		si,cx
		push	si		;coloca a abcisa y+xc na pilha			
		mov		si,bx
		sub		si,dx
		push    si		;coloca a ordenada yc-x na pilha
		mov		si,ax
		add		si,cx
		push	si		;coloca a abcisa y+xc na pilha	
		mov		si,bx
		add		si,dx
		push    si		;coloca a ordenada yc+x na pilha	
		call 	line
		
		mov		si,ax
		add		si,dx
		push	si		;coloca a abcisa xc+x na pilha			
		mov		si,bx
		sub		si,cx
		push    si		;coloca a ordenada yc-y na pilha
		mov		si,ax
		add		si,dx
		push	si		;coloca a abcisa xc+x na pilha	
		mov		si,bx
		add		si,cx
		push    si		;coloca a ordenada yc+y na pilha	
		call	line
		
		mov		si,ax
		sub		si,dx
		push	si		;coloca a abcisa xc-x na pilha			
		mov		si,bx
		sub		si,cx
		push    si		;coloca a ordenada yc-y na pilha
		mov		si,ax
		sub		si,dx
		push	si		;coloca a abcisa xc-x na pilha	
		mov		si,bx
		add		si,cx
		push    si		;coloca a ordenada yc+y na pilha	
		call	line
		
		mov		si,ax
		sub		si,cx
		push	si		;coloca a abcisa xc-y na pilha			
		mov		si,bx
		sub		si,dx
		push    si		;coloca a ordenada yc-x na pilha
		mov		si,ax
		sub		si,cx
		push	si		;coloca a abcisa xc-y na pilha	
		mov		si,bx
		add		si,dx
		push    si		;coloca a ordenada yc+x na pilha	
		call	line
		
		cmp		cx,dx
		jb		fim_full_circle  ;se cx (y) está abaixo de dx (x), termina     
		jmp		stay_full		;se cx (y) está acima de dx (x), continua no loop
		
	fim_full_circle:
		pop		di
		pop		si
		pop		dx
		pop		cx
		pop		bx
		pop		ax
		popf
		pop		bp
	ret		6

;***************************************************************
; SEGMENTO DE DADOS
;***************************************************************  
segment data 			;segmento de dados inicializados

	cor				db		branco_intenso
	preto			equ		0
	azul			equ		1
	verde			equ		2
	cyan			equ		3
	vermelho		equ		4
	magenta			equ		5
	marrom			equ		6
	branco			equ		7
	cinza			equ		8
	azul_claro		equ		9
	verde_claro		equ		10
	cyan_claro		equ		11
	rosa			equ		12
	magenta_claro	equ		13
	amarelo			equ		14
	branco_intenso	equ		15

	modo_anterior	db		0
	deltax			dw		0
	deltay			dw		0
	mens1    		db  	'Projeto de Laboratorio de Sistemas Embarcados 1 - 2023/1'
	mens2    		db  	'Bruno Baptista Guerra 00 x 00 Computador Velocidade atual: 1'

	x 				dw 		319
	y 				dw 		220
	yc_raquete 		dw 		215
	dirX			db		1
	dirY			db		1
	;velocidade dw 10

	T1 				dw 		0x0
	T2 				dw 		0xF240

	kb_data 		equ 	60h  ;PORTA DE LEITURA DE TECLADO
	kb_ctl  		equ 	61h  ;PORTA DE ResET PARA PEDIR NOVA INTERRUPCAO
	pictrl  		equ 	20h
	eoi     		equ 	20h
	int9    		equ 	9h
	cs_dos  		dw  	1
	offset_dos  	dw 		1
	tecla   		resb 	8
	p_i     		dw  	0   ;ponteiro p/ interrupcao (qnd pressiona tecla)
	p_t     		dw  	0   ;ponterio p/ interrupcao ( qnd solta tecla)

	esc 			db 		1
	setaup 			db 		0xE048
	setadown 		db 		0xE050
	plus 			db		0x4E
	minus 			db 		0x4A

segment stack stack
	RESB 256 			; reserva 256 bytes para formar a pilha
stacktop: 				