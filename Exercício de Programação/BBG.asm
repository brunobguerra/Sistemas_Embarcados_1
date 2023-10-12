;***********************************************************
;Exercício de Programação
;Bruno Baptista Guerra
;Turma 6.1 
;Sistemas Embarcados I
;***********************************************************
;Inicializacao sistema
;***********************************************************
segment code
    ..start:
    ; setup do programa
        mov     ax, data
        mov     ds, ax
        mov     ax, stack
        mov     ss, ax
        mov     sp, stacktop

    ; salvar modo corrente de video (vendo como está o modo de video da maquina)
        mov     ah, 0Fh                            
        int     10h
        mov     [modo_anterior], al   

    ; Altera modo de vídeo para gráfico 640x480 16 cores
        mov     al, 12h                           
        mov     ah, 0
        int     10h

;***********************************************************
;CHAMAR INTERFACE
;***********************************************************
        call    faz_interface ;CHAMAR INTERFACE
        jmp     inicializa_mouse

;***********************************************************
;FUNÇÕES RELATIVAS AO MOUSE (PELA INT 33H)
;***********************************************************
    inicializa_mouse: ;LIGAR MOUSE
        mov     ax, 0                       ;reset mouse
        int     33h
        mov     ax, 1                       ;display mouse
        int     33h 

    checa_clique: ;CHECAR SE HOUVE O CLIQUE DO MOUSE
        mov     ax, 5              			;Get Mouse Button Press Information
        mov     bx, 0						;status botao esquerdo
        int     33h               
        cmp     bx, 0              			;se bx for = 0, nao houve clique, caso contrario, ha o clique
            jne trata_clique            ;tratamento do clique do mouse
        jmp     checa_clique	            ;se nao ha o clique a rotina voltara

    trata_clique: ;Tratamento mouse
        cmp     dx, 115                   ;compara dx com 115        
            jb  localiza_clique		;salta se dx (y do mouse) for menor 115
        jmp     checa_clique				;caso nao haja o clique no espaco delimitado, voltaremos para checa_clique

	apaga_mouse: ;Apaga mouse
		push	ax
		mov		ax, 2
		int 	33h
		pop		ax
	ret

	aparece_mouse: ;Reaparecer mouse
		push	ax
		mov		ax, 1
		int		33h
		pop		ax
	ret

;***************************************************************
; LOCALIZAÇÃO CLIQUE DO MOUSE NO MENU
;***************************************************************
; Essa funcao indicara aonde o clique fora feito
    localiza_clique:
        mov     al, [aberto]
        cmp     al, 1					;compara ver se o arquivo está aberto ou não
            je  localiza_clique_2		;[JUMP IF EQUAL] se tiver aberta vai para o localiza 2 caso contrario pode ficar aqui dentro do localiza 1
        cmp     cx, 64                  ;para x<64 o clique sera abrir
            jb  botao_abrir				
        cmp     cx, 128                 ;para 64<x<128 faz nada
            jb  faz_nada				
        cmp     cx, 192                 ;para 128<x<192 faz nada 
            jb  faz_nada				
        cmp     cx, 256                 ;para 128<x<256 faz nada 
            jb  faz_nada				
        cmp     cx, 320                 ;para 256<x<320, o clique será fechar o programa 
            jb botao_sair				

    faz_nada:
        jmp checa_clique

    localiza_clique_2:
        cmp     cx, 64                  ;para x<64 o clique sera abrir
            jb  botao_abrir_rec
        cmp     cx, 128                 ;para 64<x<128 o clique sera LBP
            jb  botao_LBP
        cmp     cx, 192                 ;para 128<x<192, o clique sera histograma
            jb  botao_hist
        cmp     cx, 256                 ;para 128<x<256, o clique sera histograma LBP 
            jb  botao_hist_LBP
        cmp     cx, 320                 ;para 256<x<320, o clique sera fechar 
            jb  botao_sair
        jmp     checa_clique

; lógica dos botões - aumentando o tamanho dos jumps pois ao compilar no DOSBOX dizia que era muito curto, aí fiz assim e deu certo
    botao_abrir:
        jmp botao_abrir2
    botao_sair:
        jmp botao_sair2
    botao_hist:
        jmp botao_hist_2
    botao_abrir_rec:
        jmp botao_abrir_rec2   
    botao_LBP:
        jmp botao_LBP_2
    botao_hist_LBP:
        jmp botao_hist_LBP_2

    botao_abrir2:
        call	letra_branco_intenso
        mov     byte[cor], amarelo			;muda cor para amarelo como solicitado
        call    msg_abrir					;Reescreve 'Abrir' em amarelo
        call	apaga_mouse	                ;Apaga mouse
        mov     al, byte[aberto]     		;Confere se o arquivo está aberta/abrindo
        cmp     al, 0						;caso al=0 o arquivo nao foi aberto e sera aberto
            je  vai_abrir       		    ;se a imagem não tiver aberta salta para vai abrir
        mov     bx, [arquivo_img_handle]	
        mov     ah, 3eh						
        mov     al, 00h						
        int     21h							    

    vai_abrir:
        call    abre_arquivo				;Abre o arquivo
        call    aparece_mouse               ;reaparece o mouse
        jmp     checa_clique				;volta para checa click

    botao_abrir_rec2:
        ;Inicializa o programa em relação aos parâmetros
        mov     byte[aberto], 0
        mov     word[y_anterior], 364
        mov     word[x_anterior_2], 320
        mov     word[y_anterior_2],	0
        mov     byte[x_anterior], 0 
        mov     word[x_anterior_3], 0
        mov     word[y_anterior_3], 0
        mov     word[incremento], 0
        mov     byte[grava_vec], 0
        mov     word[coluna_grafico], 0
        mov     byte[reinicia], 1
        mov     byte[eh_lbp], 0
        mov     byte[valordacor], 0
        mov     word[x_after_4], 0
        mov     word[y_after_4], 0
        mov     ah, 0   
        mov     al,[modo_anterior] 
        int     10h
        jmp     ..start                         ;Caso dê problema volta lá pro inicio do programa
	
    botao_sair2:
        call	letra_branco_intenso
        mov     byte[cor], amarelo          ;muda cor para amarelo como solicitado
        call    msg_sair                    ;Reescreve 'Sair' em amarelo
        jmp     sair						;Sai do programa

    botao_hist_2:
        call	letra_branco_intenso
        mov     byte[cor], amarelo			;muda cor para amarelo como solicitado
        call    msg_Hist					;Reescreve 'Hist' em amarelo 
        call	apaga_mouse                 ;Apaga mouse enquanto plota o histograma
        call    plota_hist				    ;plota o histograma
        call	aparece_mouse               ;Reaparece mouse       
        jmp     checa_clique				
        
    botao_LBP_2:
        cmp     byte[eh_lbp], 1             ;Avalia se está em LBP
            jne continua
        mov     byte[eh_lbp], 0
            jmp botao_abrir_rec2
        continua:
            call	letra_branco_intenso
            mov     byte[cor], amarelo		;muda cor para amarelo como solicitado
            call    msg_LBP                 ;Reescreve 'LBP' em amarelo 
            call	apaga_mouse             ;Apaga mouse	
            call    muda_bigvetor
            call    printa_lbp
            mov     byte[esperaprocesso], 1
            call	aparece_mouse           ;Reaparece mouse 
            jmp     checa_clique			;volta para checa click

    botao_hist_LBP_2:
        call	letra_branco_intenso
        cmp     byte[esperaprocesso], 1
        mov     byte[cor],amarelo			;muda cor para amarelo como solicitado
        call    msg_HistLBP				    ;Reescreve 'HistLBP' em amarelo  
        call	apaga_mouse	                ;Apaga mouse
        call    plota_hist_lbp				;plota o histograma da imagem LBP
        mov     byte[pula_vec_hist], 1
        call	aparece_mouse		        ;Reaparece mouse
        jmp     checa_clique                ;volta para checa click

    abre_arquivo:
        pushf							
        push ax
        push bx
        push cx
        push dx
        push si
        push di
        push bp
        mov	    word[num_count], 0			;zera contador de v_decimal
        call    realocate
        lahf                			    ;carry que é disponibilizado depois da interrupção, confere abertura arquivo
        and     ah, 01						;zera todos menos o ultimo bit, que é mantido, se for 1 ou 0           
        cmp     ah, 01           			;compara ah com 01, e se for zero pula pra 'abriu_corretamente'
            jne abriu_corretamente          
        pop	bp
        pop	di
        pop	si
        pop	dx
        pop	cx
        pop	bx
        pop	ax
        popf
    ret                                     ;se não abriu corretamente, temos a volta ao clique

    abriu_corretamente:
        mov byte[aberto], 1				    ;sinaliza que o arquivo abriu corretamente.
    
    proximo_byte:			
        mov     bx, [arquivo_img_handle]	;carrega bx com o identificador do arquivo
        mov     dx, buffer					;carrega dx com o ponteiro par o buffer
        mov     cx, 1						;é lido 1 char por vez
        call	int_file	                ;chamada para leitura em ah
        cmp     ax, cx						;checa ax recebe numero de byte lido
            jne final_arquivo			    ;se ax é diferente de 1 salta para o final do arquivo
        mov     al, byte[buffer]			
        mov     byte[ascii], al  			;copia o conteudo do reg para a posição de memoria ascii
        cmp     al, '0'						;compara o conteudo com o caracter ascii '0'
            jae continua_lendo			    ;salta se al>= '0' (ascii) 
        call    junta_digitos					
        call    plota_foto				    
        mov	    byte[count], 0				;copia 0 para a posição de memoria ascii
        jmp proximo_byte				    ;volta para função proximo_byte para garantir se ainda tem numero a ser lido ou era espaço
        continua_lendo:
            call 	ascii2decimal		    ;traduz ascii para decimal          
            inc		byte[count]			    ;incrementa o contador de digitos do numero
            jmp 	proximo_byte    	    ;volta para função proximo byte
        final_arquivo:
            call    reset_pointer
        pop		bp
        pop		di
        pop		si
        pop		dx
        pop		cx
        pop		bx
        pop		ax
        popf
    ret

    ascii2decimal:                          ;traduz ascii para decimal   
        pushf
        push 	ax
        push 	bx
        push	cx
        push	dx
        push	si
        push	di
        push	bp
        xor 	cx, cx				        ;zerando registrador cx
        mov 	al, [ascii]				    
        sub 	al, 30h					    ;subtrindo 30 de al e encontrando o numero, e armazenando em al
        mov 	cl, byte[unidade] 		    ;copia o conteudo da posição de memoria unidade no registrador cl
        mov 	ch, byte[dezena]			;copia o conteudo da posição de memoria dezena no registrador cl
        mov 	byte[unidade], al		    ;copia o conteudo do registrador al para posição de memoria unidade
        mov 	byte[dezena], cl			;copia o conteudo do registrador cl para posição de memoria dezena
        mov 	byte[centena], ch		    ;copia o conteudo do registrador ch para posição de memoria centena
        pop		bp
        pop		di
        pop		si
        pop		dx
        pop		cx
        pop		bx
        pop		ax
        popf
    ret

    junta_digitos:  
        pushf
        push 	ax						
        push 	bx
        push	cx
        push	dx
        push	si
        push	di
        push	bp
        xor		ax, ax					    ;zera registradores
        xor		bx, bx
        xor		cx, cx
        xor		dx, dx	
        xor 	ah, ah
        xor 	ch, ch
        mov 	bl, byte[count]			    ;variavel contadora do tamanho do número ascii, [un,dz,ct], carrega valor 1,2 ou 3 no reg bl
        cmp     bl, 3						
            je numero_3					    ;se bl = 3 ,numero centena. >=100
        cmp     bl, 2						
            je numero_2					    ;se bl = 2 ,numero dezena  >=10 e <100
        jmp	numero_1					    ;se bl sobrar com 1, numero unidade >=0 e <10
        numero_3:
            mov 	al, byte[centena]	    ;colaca centena em al
            mov 	bl, 100				    ;coloca 100 em bl
            mul 	bl					    
            mov 	cx, ax				    
            xor 	ah, ah				    ;zera registrador AH
            mov 	al, byte[dezena]		;colaca dezena em al
            mov 	bl, 10				    ;coloca 10 em bl
            mul 	bl					    
            add 	cx, ax				    
            xor 	ah, ah				    ;zera registrador ah
            mov 	al, [unidade]		    ;coloca unidade em al
            add 	cx, ax				     
            jmp final_juncao

        numero_2:						   
            mov 	al, byte[dezena]        ;colaca dezena em al
            mov 	bl, 10                  ;coloca 10 em bl
            mul 	bl
            mov 	cx, ax	                
            xor 	ah, ah                  ;zera registrador AH
            mov 	al, byte[unidade]       ;colaca unidade em al
            add 	cx, ax	
            jmp final_juncao

        numero_1:						
            mov 	al, byte[unidade]       ;colaca unidade em al
            xor 	ah, ah                  ;zera registrador AH
            mov 	cx, ax	                

        final_juncao:	
            mov		bx, word[num_count]		
            mov 	byte[v_decimal+bx], cl	
            mov 	byte[decimal], cl 		
            cmp 	byte[reinicia], 1		
                je reiniciando 				;compara conteudo da posição de memoria apontada por [reinicia] e sefor igual a 1, salta para 'reiniciando'
            push    bx						
            mov     bx, cx					;numero traduzido em bx
            add     bx, bx					
            add		word[hist_vetor+bx], 1	;soma 1, a posição de memoria apontada por hist_vetor, deslocado de bx [bx=2*numero traduzido]
            pop     bx						 
        reiniciando:
            inc 	bx						;incrementa contador
            mov		word[num_count], bx		;armazena na variavel 'num_count'
            mov 	byte[unidade], 0		;zera unidade antes de retornar para 'ascii2decimal'
            mov 	byte[dezena], 0         ;zera dezena antes de retornar para 'ascii2decimal'
            mov 	byte[centena], 0        ;zera centena antes de retornar para 'ascii2decimal'
    pop     bp								
    pop		di
    pop		si
    pop		dx
    pop		cx
    pop		bx
    pop		ax
    popf
    ret
 
;***************************************************************
;FUNÇÃO PARA IMPRIMIR A IMAGEM ORIGEM
;****************************************************************
    plota_foto:    
        push    cx     					
        push    ax
        push    dx
        push    bx
        mov     bx, word[x_anterior]		;copia o conteudo da posição de memoria [x_anterior] no reg bx 
        cmp     bx, 250						
            jne n_reseta_x				    ;compara x_anterior com 250 e se não for igual, então salta para 'n_reseta_x'
        mov     word[x_anterior], 0			;zera x_anterior se na comparação for igual a 250
        mov     bx, word[x_anterior]		;zera bx
        dec     word[y_anterior]			
        n_reseta_x:
            add     bx, 35                  ;adiciona por conta do layout e não ficar encostado nas linhas plotadas
            push    bx						
            inc     word[x_anterior]		
            mov     ax, word[y_anterior]	
            add     ax, 35                  ;adiciona 35 por conta do layout e não ficar encostado nas linhas plotadas
            push    ax						
            xor     ax, ax					;zera registrador ax
            mov     al, byte[decimal]		;byte decimal tem o número traduzido, armazena em al
            ;Fazendo fatiamento de níveis de intensidade na Imagem em 4 níveis usando os canais de 16 cores para plotar a imagem de um jeito mais interessante
            cmp     ax, 60					;se ax (intensidade) é igual ou menor que 60, pinta de preto
                jbe plot_preto
            cmp     ax, 120					;se ax (intensidade) é maior que 60 e igual ou menor que 120, pinta de cinza
                jbe plot_cinza
            cmp     ax, 180					;se ax (intensidade) é maior que 120 e igual ou menor que 180, pinta de branco
                jbe plot_branco
            cmp     ax, 255					;se ax (intensidade) é maior que 0 e igual ou menor que 255, pinta de branco intenso
                jbe plot_branco_intenso	
            plot_preto:
                mov byte[cor], preto
                call plot_xy				;>> call, os parametros x,y são empilhados para função plot_xy, a cor foi passada no jump		
                jmp plota_no_xy
            plot_cinza:
                mov byte[cor], cinza
                call plot_xy				;>> call, os parametros x,y são empilhados para função plot_xy, a cor foi passada no jump		
                jmp plota_no_xy
            plot_branco:
                mov byte[cor], branco
                call plot_xy				;>> call, os parametros x,y são empilhados para função plot_xy, a cor foi passada no jump		
                jmp plota_no_xy
            plot_branco_intenso:
                mov byte[cor], branco_intenso
                call plot_xy				;>> call, os parametros x,y são empilhados para função plot_xy, a cor foi passada no jump	
                jmp plota_no_xy
            plota_no_xy:
        pop   bx
        pop   dx
        pop   ax    
        pop   cx
    ret

    muda_bigvetor:
        pushf
        push 	ax
        push 	bx
        push	cx
        push 	dx
        push 	si
        push 	di
        push	bp
        mov     cx, 61504
        loop_1:
            call    muda_vetor
            mov     bx, 61504					    ;conta para bx ser incrementdor de zero até o final, não o inverso
            sub     bx, cx
            mov     al, byte[grava_vec]			    ;al recebe a nova cor, já processada
            mov     byte[v_decimal+bx], al		
            cmp     byte[pula_vec_hist], 1
            je pula_vec_histograma
                push    bx							
                xor 	bx, bx						;zera registrador bx
                mov     bl, byte[grava_vec]			;numero traduzido em bl
                add     bx, bx						;duplica numero traduzido
                add		word[hist_lbp+bx], 1		;soma 1, a posição de memoria apontada por hist_vetor, deslocado de bx [bx=2*numero traduzido]
                pop     bx							;restaura numero do contador  
            pula_vec_histograma:
                push    ax
                push    bx
                push    dx
                xor     ax, ax                      ;zera registradores
                xor     bx, bx
                xor     dx, dx
                mov     bl, 250
                mov     ax, word[incremento]
                div     bl
                cmp     ah, 247
                    jne     inc_2
                mov     ax,word[incremento]
                inc     ax
                inc     ax
                mov     word[incremento], ax
                inc_2:	
                    mov     ax, word[incremento]
                    inc     ax
                    mov     word[incremento], ax
                    pop     dx
                    pop     bx
                    pop     ax
        loop loop_1
        pop		bp
        pop		di
        pop		si
        pop		dx
        pop		cx
        pop		bx
        pop		ax
        popf
	ret

    muda_vetor:
        pushf
        push 	ax
        push 	bx
        push	cx
        push 	dx
        push 	si
        push 	di
        push	bp
        mov     cx, word[incremento]
        mov     bx, 251
        add     bx, cx
        mov     ah, byte[v_decimal+bx]
        mov     bx, 0
        add     bx, cx
        mov     al, byte[v_decimal+bx]
        cmp     ah, al
            jb  zero1
        add     dl, 00000001b
        zero1:
            mov     bx, 1
            add     bx, cx
            mov     al, byte[v_decimal+bx]
            cmp     ah, al
                jb  zero2
            add     dl, 00000010b
        zero2:
            mov     bx, 2
            add     bx, cx
            mov     al,byte[v_decimal+bx]
            cmp     ah, al
                jb  zero3
            add     dl, 00000100b
        zero3:
            mov     bx, 252
            add     bx, cx
            mov     al, byte[v_decimal+bx]
            cmp     ah, al
                jb  zero4
            add     dl, 00001000b
        zero4:
            mov     bx, 502
            add     bx, cx
            mov     al, byte[v_decimal+bx]
            cmp     ah, al
                jb  zero5
            add     dl, 00010000b
        zero5:
            mov     bx, 501
            add     bx, cx
            mov     al, byte[v_decimal+bx]
            cmp     ah, al
                jb  zero6
            add     dl, 00100000b
        zero6:
            mov     bx, 500
            add     bx, cx
            mov     al, byte[v_decimal+bx]
            cmp     ah, al
                jb  zero7
            add     dl, 01000000b
        zero7:
            mov     bx, 250
            add     bx, cx
            mov     al, byte[v_decimal+bx]
            cmp     ah, al
                jb  zero8
            add     dl, 10000000b
        zero8:	
            mov byte[grava_vec], dl
        pop		bp
        pop		di
        pop		si
        pop		dx
        pop		cx
        pop		bx
        pop		ax
        popf
	ret	
	
    printa_lbp:
        push    cx     					
        push    ax
        push    dx
        push    bx
        mov     cx, 61504
        loop_print_lbp:
            mov     bx, 61504
            sub     bx, cx						
            mov     dl, byte[v_decimal+bx]		;contem o valor da cor na posição bx
            mov     byte[valordacor], dl		;guarda cor na variavel
            mov     ax, bx						;contem o incremento
            mov     bl, 248						
            div     bl							
            xor     bx, bx                      ;zera registrador bx
            mov     bl, ah	
            add     bx, 355
            push bx
            mov     word[x_anterior_3], bx
            xor     bx, bx
            mov     bl, al
            xor     ax, ax
            mov     ax, 469
            sub     ax, bx
            push ax
            mov     word[y_anterior_3], ax
            xor     ax, ax					    ;zera registrador ax
            mov     al, byte[valordacor]		;byte decimal tem o número traduzido, armazena em al
            ;Fazendo fatiamento de níveis de intensidade na Imagem em 4 níveis usando os canais de 16 cores para plotar a imagem em escala de cinza. 
            ;Para o LBP, tentei fazer assim por conta dos critérios do LBP. Mas acabava só printando uma tela toda em amarelo
            ;call	plot_xy
            cmp ax, 60					        ;se ax (intensidade) é igual ou menor que 60, pinta de preto
                jbe plot_preto_LBP
            cmp ax, 120					        ;se ax (intensidade) é maior que 60 e igual ou menor que 120, pinta de cinza
                jbe plot_cinza_LBP
            cmp ax, 180					        ;se ax (intensidade) é maior que 120 e igual ou menor que 180, pinta de branco
                jbe plot_branco_LBP
            cmp ax, 255					        ;se ax (intensidade) é maior que 0 e igual ou menor que 255, pinta de branco intenso
                jbe plot_branco_intenso_LBP     
            plot_preto_LBP:
                mov byte[cor],preto
                call plot_xy
                jmp continua_plot
            plot_cinza_LBP:
                mov byte[cor],cinza	
                call plot_xy
                jmp continua_plot
            plot_branco_LBP:
                mov byte[cor],branco
                call plot_xy
                jmp continua_plot
            plot_branco_intenso_LBP:
                mov byte[cor],branco_intenso
                call plot_xy
                jmp continua_plot
            continua_plot:	
                loop loop_print_lbp				
	mov     byte[eh_lbp], 1
	pop     bx
    pop     dx
    pop     ax    
    pop     cx
	ret

    plota_hist:
        call    limpa_histograma                ;Pinta de preto caso já esteja algum histograma plotado na região antes
        pushf
        push    ax
        push    bx
        push    cx
        push    dx
        push    si
        push    di
        push    bp
        mov     byte[cor], branco_intenso		
        mov     word[coluna_grafico], 0			;garantindo que o primeiro valor do histograma vai ser o primeiro valor do vetor
        mov     dx, word[hist_vetor]			;vetor de word com o histograma, de tamanho 255, transferindo primeiro valor para dx
        mov     ax, dx							;transferindo primeiro valor do vetor histograma para ax
        div     byte[escala]  					;divide ax pelo valor do byte escala [5] que na verdade é uma word
        xor     ah, ah							;zera o resto deixando apenas o quociente em al, tornando ax = al
        mov     word[y_anterior_2], ax			;move o quociente para y_anterior_2
        coluna_valida:	
            mov     bx, 355						;coloca bx em 355 para respeitar o layout
            mov     cx, 256						;contador do loop para printar o histograma em todos os niveis de intesidade de 0 a 255
        printar:
            mov     ax, bx
            add     ax, 1						;355 + 1 = 356	
            push    ax							;empilha x1
            mov     ax, word[y_anterior_2]			
            push    ax							
            inc     bx							
            mov     ax, bx						
            add     ax, 1						
            mov     word[x_anterior_2], bx		
            push    ax							
            ;movendo a parte do auxiliar que anda no hist_vetor
            push    bx			
            mov     bx, word[coluna_grafico]		
            mov     dx, word[hist_vetor+bx]			
            add     bx, 2							
            mov     word[coluna_grafico], bx			
            pop     bx
            mov     ax, dx							
            xor     dx, dx					    ;zera registrador dx
            div     word[escala] 					
            mov     word[y_anterior_2], ax			
            push    ax								 
            call line    						
        loop printar                            ;roda a roda até cx chegar a 0
    pop		bp
    pop		di
    pop		si
    pop		dx
    pop		cx
    pop		bx
    pop		ax
    popf
    ret

    plota_hist_lbp:
        call limpa_histograma                   ;Pinta de preto caso já esteja algum histograma plotado na região antes
        pushf
        push ax
        push bx
        push cx
        push dx
        push si
        push di
        push bp
        mov     byte[cor], branco_intenso	
        mov     cx, 256					        ;contador do loop para printar o histograma coluna por coluna
        printar2:
            xor     bx, bx					    ;zero registrador bx
            mov     bx, 256					    ;movo 256 para diminuir de cx
            sub     bx, cx					    ;contador de 0-255
            mov     ax, bx					    ;ax recebe o contador incremental
            add     ax, 355					    ;desloco ax pra imprimir da metade pra la
            push    ax						    
            add     ax, 1
            mov     word[x_after_4], ax		    ;armazena x2 para empilhar
            add     bx, bx					    ;duplico bx pra andar no vetor word
            mov     word[y_after_4], bx		    ;guardando posiçao pro y1, somar 2 e encontrar posição y2
            xor     dx, dx					    ;zerando dx pra nao ter nada na word superior apos a divisao
            mov     ax, word[hist_lbp+bx]	    ;ponho numero em quantidade de repetição da cor no vetor hist lbp
            push    cx
            xor     cx, cx
            mov     cx, 40					    
            div     cx						    ;ax fica com o quociente 
            pop     cx
            push    ax
            mov     bx, word[x_after_4]
            push    bx						    
            mov     bx, word[y_after_4]		    
            add     bx, 2
            xor     dx, dx					    ;zerando dx pra nao ter nada na word superior apos a divisao
            mov     ax, word[hist_lbp+bx]	    ;ponho numero em quantidade de repetição da cor no vetor hist lbp
            push    cx
            xor     cx, cx
            mov     cx, 40					    ;escalando y2 pq ta muito grande
            div     cx						    ;ax fica com o quociente 
            pop     cx
            push    ax
            call line
        loop printar2 				            ;roda a roda até cx chegar a 0 	
        pop		bp
        pop		di
        pop		si
        pop		dx
        pop		cx
        pop		bx
        pop		ax
        popf
    ret

; Função para pintar de preto o espaço do histograma para alternar entre Hist e HistLBP
    limpa_histograma: 
        pushf
        push    ax
        push    bx
        push    cx
        push    dx
        push    si
        push    di
        push    bp
        mov     byte[cor], preto            ;Pintar o espaço do histograma de preto
        mov     cx, 638	
        loopx:
            push    cx
            mov     bx, 320
            mov     bx, cx
            mov     cx, 218
        loopy:
            push    bx
            mov     ax, -1
            add     ax, cx
            push    ax
            call    plot_xy
            loop    loopy
            pop     cx
            cmp     cx, 321
                je  pula_loop
        loop loopx
        pula_loop:
            pop		bp
            pop		di
            pop		si
            pop		dx
            pop		cx
            pop		bx
            pop		ax
            popf
    ret

;***************************************************************
; FUNÇÕES PARA O MENU
;***************************************************************
; Criação das linhas do menu seguindo o layout
;***************************************************************
; CHAMADA INTERFACE GRAFICA
    faz_interface:
        mov		byte[cor], branco_intenso     
        call    cria_divisorias
        call    msg_abrir
        call    msg_LBP
        call    msg_Hist
        call    msg_HistLBP
        call    msg_sair
        call    msg_identificacao
    ret 

; CRIA O LAYOUT SOLICITADO NO EP
    cria_divisorias:
        push ax       
		push bx       
		push cx       
		push dx       
		push si       
		push di
           
		mov     ax, 319                        
		push    ax
		mov     ax, 0
		push    ax
		mov     ax, 319
		push    ax
		mov     ax, 479
		push    ax
		mov	    byte[cor], branco_intenso 
		call    line

		mov     ax, 0                       
		push    ax
		mov     ax, 415
		push    ax
		mov     ax, 319
		push    ax
		mov     ax, 415
		push    ax
		mov	    byte[cor], branco_intenso 
		call    line

		mov     ax, 63                        
		push    ax
		mov     ax, 479
		push    ax
		mov     ax, 63
		push    ax
		mov     ax, 415
		push    ax
		mov	    byte[cor], branco_intenso 
		call    line
		
		mov     ax, 127                        
		push    ax
		mov     ax, 479
		push    ax
		mov     ax, 127
		push    ax
		mov     ax, 415
		push    ax
		mov	    byte[cor], branco_intenso 
		call    line

		mov     ax, 191                        
		push    ax
		mov     ax, 479
		push    ax
		mov     ax, 191
		push    ax
		mov     ax, 415
		push    ax
		mov	    byte[cor], branco_intenso 
		call    line
		
		mov     ax, 259                        
		push    ax
		mov     ax, 479
		push    ax
		mov     ax, 259
		push    ax
		mov     ax, 415
		push    ax
		mov	    byte[cor], branco_intenso 
		call    line

		mov     ax, 0                        
		push    ax
		mov     ax, 120
		push    ax
		mov     ax, 319
		push    ax
		mov     ax, 120
		push    ax
		mov	    byte[cor], branco_intenso 
		call    line

		mov     ax, 319                        
		push    ax
		mov     ax, 219
		push    ax
		mov     ax, 639
		push    ax
		mov     ax, 219
		push    ax
		mov	    byte[cor],branco_intenso 
		call    line

		mov     ax, 0                        
		push    ax
		mov     ax, 479
		push    ax
		mov     ax, 639
		push    ax
		mov     ax, 479
		push    ax
		mov	    byte[cor],branco_intenso 
		call    line

		mov     ax, 0                        
		push    ax
		mov     ax, 0
		push    ax
		mov     ax, 639
		push    ax
		mov     ax, 0
		push    ax
		mov	    byte[cor],branco_intenso
		call    line

		mov     ax, 0                        
		push    ax
		mov     ax, 0
		push    ax
		mov     ax, 0
		push    ax
		mov     ax, 479
		push    ax
		mov	    byte[cor],branco_intenso 
		call    line

		mov     ax, 639                        
		push    ax
		mov     ax, 0
		push    ax
		mov     ax, 639
		push    ax
		mov     ax, 479
		push    ax
		mov	    byte[cor],branco_intenso 
		call    line

		pop     di
		pop     si
		pop     dx
		pop     cx
		pop     bx
		pop     ax
    ret   

;***************************************************************
; Escrita das mensagens na tela 
;***************************************************************
    msg_abrir:
        push ax
        push bx
        push cx
        push dx
        mov cx, 5     
        mov bx, 0
        mov dh, 2     
        mov dl, 2      
        loop_abrir:
            call cursor
            mov al, [bx+mens_1]     ;Imprimir 'Abrir'
            call  caracter
            inc bx      
            inc dl     
            loop loop_abrir
        pop dx 
        pop cx
        pop bx
        pop ax
    ret
	
    msg_LBP:
        push ax
        push bx
        push cx
        push dx
        mov cx, 3      
        mov bx, 0
        mov dh, 2     
        mov dl, 10      
        loop_lbp:
            call cursor
            mov al, [bx+mens_2]     ;Imprimir 'LBP'
            call  caracter
            inc bx      
            inc dl     
            loop loop_lbp
        pop dx 
        pop cx
        pop bx
        pop ax
    ret

    msg_Hist:
	    push ax
		push bx
		push cx
		push dx
		mov 	cx, 4               ;numero de caracteres
		mov 	bx, 0
		mov 	dh, 2               ;linha 0-29
		mov 	dl, 18              ;coluna 0-79
        ret_Hist:
            call 	cursor
            mov 	al, [bx+mens_3] ;Imprimir 'Hist'
            call 	caracter
            inc 	bx              ;proximo caracter
            inc 	dl              ;avanca a coluna
            loop ret_Hist
        pop dx 
        pop cx
        pop bx
        pop ax
	ret 
	  
	msg_HistLBP:
		push ax
		push bx
		push cx
		push dx
		mov 	cx, 7               ;numero de caracteres
		mov 	bx, 0
		mov 	dh, 2               ;linha 0-29
		mov 	dl, 25              ;coluna 0-79
        ret_HistLBP:
            call 	cursor
            mov 	al,[bx+mens_4]  ;Imprimir 'HistLBP'
            call	caracter
            inc 	bx              ;proximo caracter
            inc 	dl              ;avanca a coluna
            loop 	ret_HistLBP
        pop dx 
        pop cx
        pop bx
        pop ax
	ret 

    msg_sair:
        push ax
        push bx
        push cx
        push dx
        mov cx,4      
        mov bx,0
        mov dh,2      
        mov dl,34      
        loop_sair:
            call cursor
            mov al,[bx+mens_5]      ;Imprimir 'Sair'
            call caracter
            inc bx       
            inc dl       
        loop loop_sair
        pop dx 
        pop cx
        pop bx
        pop ax
    ret

    msg_identificacao:
        push ax
        push bx
        push cx
        push dx
        mov 	cx, 22                  ;numero de caracteres
        mov 	bx, 0
        mov 	dh, 24                  ;linha 0-29
        mov 	dl, 4                   ;coluna 0-79
        
        loop_identificacao:
            call    cursor
            mov     al, [bx+mens_6]     ; Imprimir 'Bruno Baptista Guerra,'
            call    caracter
            inc 	bx                  ;proximo caracter
            inc 	dl                  ;avanca a coluna
            loop    loop_identificacao
            mov 	cx, 23		        ;numero de caracteres
            mov 	dh, 25		        ;linha 0-29
            mov 	dl, 4		        ;coluna 0-79
        
        loop_identificacao2:
            call    cursor
            mov     al, [bx+mens_6]     ;Imprimir 'Sistemas Embarcados I'
            call    caracter
            inc     bx		            ;proximo caracter
            inc     dl		            ;avanca a coluna
            loop    loop_identificacao2
            mov     cx, 20		        ;numero de caracteres
            mov     dh, 26		        ;linha 0-29
            mov     dl, 4		        ;coluna 0-79
        
        loop_identificacao3:
            call    cursor
            mov     al, [bx+mens_6]     ;Imprimir '2023/1 e Turma 06.1'
            call    caracter
            inc     bx		            ;proximo caracter
            inc     dl		            ;avanca a coluna
            loop    loop_identificacao3 
        pop dx 
        pop cx
        pop bx
        pop ax
    ret

;Funçãoo cursor
;dh = linha (0-29) e  dl=coluna  (0-79)
    cursor:
        pushf
        push    ax
        push 	bx
        push	cx
        push	dx
        push	si
        push	di
        push	bp
        mov     ah, 2
        mov     bh, 0
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

;_____________________________________________________________________________
; Função caracter escrito na posição do cursor
; al= caracter a ser escrito
; cor definida na variavel cor
    caracter:
        pushf
        push    ax
        push 	bx
        push	cx
        push	dx
        push	si
        push	di
        push	bp
        mov     ah, 9
        mov     bh, 0
        mov     cx, 1
        mov     bl, [cor]
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

;-----------------------------------------------------------------------------
; push x; push y; call plot_xy;  (x<639, y<479)
; cor definida na variavel cor
    plot_xy:
        push	bp
        mov     bp, sp
        pushf
        push 	ax
        push 	bx
        push	cx
        push	dx
        push	si
        push	di
        mov     ah, 0ch
        mov     al, [cor]
        mov     bh, 0
        mov     dx, 479
        sub     dx, [bp+4]
        mov     cx, [bp+6]
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

;-----------------------------------------------------------------------------
;função line do linec.asm
; push x1; push y1; push x2; push y2; call line;  (x<639, y<479)
    line:
        push    bp
        mov		bp, sp
        pushf                        
        push 	ax
        push 	bx
        push	cx
        push	dx
        push	si
        push	di
        mov		ax, [bp+10]   
        mov		bx, [bp+8]    
        mov		cx, [bp+6]    
        mov		dx, [bp+4]    
        cmp		ax, cx
        je		line2
        jb		line1
        xchg	ax, cx
        xchg	bx, dx
        jmp		line1
    line2:		
        cmp		bx, dx  
        jb		line3
        xchg	bx, dx        
    line3:	
        push	ax
        push	bx
        call 	plot_xy
        cmp		bx, dx
        jne		line31
        jmp		fim_line
    line31:		
        inc		bx
        jmp		line3
    line1:
        push	cx
        sub		cx, ax
        mov		[deltax], cx
        pop		cx
        push	dx
        sub		dx, bx
        ja		line32
        neg		dx
    line32:		
        mov		[deltay], dx
        pop		dx
        push	ax
        mov		ax, [deltax]
        cmp		ax, [deltay]
        pop		ax
        jb		line5
        push	cx
        sub		cx, ax
        mov		[deltax], cx
        pop		cx
        push	dx
        sub		dx, bx
        mov		[deltay], dx
        pop		dx
        mov		si, ax
    line4:
        push	ax
        push	dx
        push	si
        sub		si, ax
        mov		ax, [deltay]
        imul	si
        mov		si, [deltax]		
        shr		si, 1
        cmp		dx, 0
        jl		ar1
        add		ax, si
        adc		dx, 0
        jmp		arc1
    ar1:		
        sub		ax, si
        sbb		dx, 0
    arc1:
        idiv	word [deltax]
        add		ax, bx
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
        cmp		bx, dx
        jb 		line7
        xchg	ax, cx
        xchg	bx, dx
    line7:
        push	cx
        sub		cx, ax
        mov		[deltax], cx
        pop		cx
        push	dx
        sub		dx, bx
        mov		[deltay], dx
        pop		dx
        mov		si, bx
    line6:
        push	dx
        push	si
        push	ax
        sub		si, bx	
        mov		ax, [deltax]
        imul	si
        mov		si, [deltay]		
        shr		si, 1
        cmp		dx, 0
        jl		ar2
        add		ax, si
        adc		dx, 0
        jmp		arc2
    ar2:		
        sub		ax, si
        sbb		dx, 0
    arc2:
        idiv	word [deltay]
        mov		di, ax
        pop		ax
        add		di, ax
        pop		si
        push	di
        push	si
        call	plot_xy
        pop		dx
        cmp		si, dx
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

;***************************************************************
;FUNCAO RESETAR PONTEIRO(interrupcao)
;***************************************************************
	reset_pointer:
		mov     bx, [arquivo_img_handle]	;carrega o conteudo da posição de memoria em BX
        mov     ah, 3eh					    ;Fecha arquivo
        mov     al, 00h					    ;carrega al com 0, aqui era lido o digito
        int     21h						    
	ret

;***************************************************************
;FUNÇÃO PARA LEITURA DE ARQUIVO (interrupcao)
;**************************************************************
	int_file:
		mov     ah, 3Fh						
        int     21h							     
	ret

;***************************************************************
;Realocacao imagem (interrupcao)
;***************************************************************
	realocate: ;grava endereco para poder usar o arquivo
		mov     ah, 3dh						;abrir arquivo na int21h       
        mov     al, 00h						;0 = read access
        mov     dx, arquivo_img				;carrega com o nome [ASCII] do arquivo, definido nas variáveis do programa
        int     21h							;chama a interrupção
        mov     [arquivo_img_handle], ax	;carrega a variavel com o identificador do arquivo, depois ax copia para variavel [arquivo_img_handle] 
	ret
 
    letra_branco_intenso:
		mov		byte[cor], branco_intenso
		call 	msg_abrir
	  	call 	msg_LBP
	  	call 	msg_Hist
	  	call 	msg_HistLBP
		call 	msg_sair
	  	call 	msg_identificacao
	ret 

;***************************************************************
; MODO SAIDA PROGRAMA
;***************************************************************

    sair:
        mov     ah, 0                  ; set video mode
        mov     al, [modo_anterior]    ; modo anterior
        int     10h
        mov     ax, 4c00h
        int     21h

;***************************************************************
; SEGMENTO DE DADOS
;***************************************************************  
segment data

    ; Constantes de cores utilizadas
    cor           db    branco_intenso	  
    ; I R G B COR
    ; 0 0 0 0 preto
    ; 0 0 0 1 azul
    ; 0 0 1 0 verde
    ; 0 0 1 1 cyan
    ; 0 1 0 0 vermelho
    ; 0 1 0 1 magenta
    ; 0 1 1 0 marrom
    ; 0 1 1 1 branco
    ; 1 0 0 0 cinza
    ; 1 0 0 1 azul claro
    ; 1 0 1 0 verde claro
    ; 1 0 1 1 cyan claro
    ; 1 1 0 0 rosa
    ; 1 1 0 1 magenta claro
    ; 1 1 1 0 amarelo
    ; 1 1 1 1 branco intenso

    preto			equ   0
    azul			equ   1
    verde			equ   2
    cyan      		equ   3
    vermelho    	equ   4
    magenta     	equ   5
    marrom      	equ   6
    branco      	equ   7
    cinza     		equ   8
    azul_claro    	equ   9
    verde_claro   	equ   10
    cyan_claro    	equ   11
    rosa      		equ   12
    magenta_claro 	equ   13
    amarelo     	equ   14
    branco_intenso  equ   15

    modo_anterior 	db    0

    deltax      	dw    0
    deltay      	dw    0

   ; Mensagens que aparecerao no Menu
	mens_1			    db    	'Abrir'
	mens_2			    db      'LBP'
	mens_3         	    db      'Hist'
	mens_4         	    db      'HistLBP'
	mens_5         	    db      'Sair'
	mens_6         	    db      'Bruno Baptista Guerra, Sistemas Embarcados I, 2023/1 e Turma 06.1'
	  
	arquivo_img			db		'imagem.txt',0
    arquivo_img_handle  dw      0
    aberto        		db    	0
    ascii				db		0
    buffer        		resb  	10	;reserva 10 bytes em buffer
    unidade				db    	0
    dezena				db    	0
    centena				db    	0
    count				dw		0
    num_count			dw		0
    decimal				db		0
    v_decimal			resb 	62500	;reserva 62500 bytes
    coluna_grafico		dw      0
    x_anterior			dw		0
    y_anterior			dw		364
    x_anterior_2		dw		320
    y_anterior_2		dw		1
    hist_vetor			resw	255
    escala				dw		10
    reinicia 			db 		0
	x_anterior_3		dw		0
    y_anterior_3		dw		0
	incremento			dw		0
	grava_vec			db		0
	valordacor			db		0
	eh_lbp				db		0
	hist_lbp			resw	255
	x_after_4			dw		0
    y_after_4			dw		0
	pula_vec_hist		db 		0
	esperaprocesso		db		0

segment stack stack
resb    512
stacktop: