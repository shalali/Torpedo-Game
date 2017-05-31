//TORPEDO START

;Stack and Stack Pointer Addresses 
.equ     SPH    =	$3E              ;High Byte Stack Pointer Address 
.equ     SPL    =	$3D              ;Low Byte Stack Pointer Address 
.equ     RAMEND =	$25F             ;Stack Address 

;Port Addresses:

//BUZZER
.equ    PORTA =$1B                 ;Port A Output Address 
.equ    DDRA  =$1A                 ;Port A Data Direction Register Address 


//DUAL SEVEN SEGEMENT DISPLAY (DSSD)
.equ    PORTB  =	$18               ;Port B Output Address 
.equ    DDRB   =	$17               ;Port B Data Direction Register Address 

//SWITCHBOX
.equ    PORTC  =	$15              ;Port C Output Address 
.equ    DDRC   =	$14              ;Port C Data Direction Register Address 

//ONBOARD PUSH BUTTONS
.equ	PORTD	= 	$12
.equ	DDRD 	= 	$11	                ;Port D Output Address 
.equ    PIND  =		$10					;Port D Input Address 

;Register Definitions 
.def     leds   =	r23              ;Register to store data for LEDs 
.def     tempB	=	r26  			 ;Varible used to locate the postion of the leds for comparison
.def     temp   =	r16              ;Temporary storage register 
.def	 randomNo = r22			  ;where my random no. is stored
.def	 value = 	r20			  ;value gotten from RNG to determine output using comparison
.def	 counter = 	r27			  ;for shift loop
.def	 directionVal = r28
.def	 count 	= 	r29	
;.def     ZL     =	r30              ;Define low byte of Z 
.;def     ZH     =	r31              ;Define high byte of Z 

.def	 lives = r6
.def	 ledsDisplay = r0			 ;leds for segment display
.def	 leftDigit = r15				; left no of segment display
.def	 rightDigit = r14				; right no of segment display
.def	 scoreDelay= r7
.def	 points		=r8
.def	 killedShip	= r9	;the ship that is killed if user scores	
.def     count   =r10	;for segment display
.def     flash   =r11  ;to make the number flash on DSSD
;Program Initialisation `
;Set stack pointer to end of memory 
		ldi    temp,high(RAMEND) 
        out    SPH,temp          ;Load high byte of end of memory address 
        ldi    temp,low(RAMEND) 
        out    SPL,temp          ;Load low byte of end of memory address 

;Initialise output ports 
		
		ldi    temp,$ff	
        out    DDRB,temp         ;Set Port B for output by sending $FF to direction register 
		
      	ldi		temp,$ff	
        out     DDRC,temp         ;Set Port C for output by sending $FF to direction register 
		
		ldi		temp,$00
		out		DDRD,temp
		
		ldi		temp,$ff
		out		DDRA,temp
				
;Initialise Main Program
		
		;load seed value in random number generator	
		ldi 	randomNo,$66		;load pre-seeded no.		 
		ldi		r17,$66 			
		ldi 	r18,$66
		ldi     r27,$00
		
		rjmp	beginGame

//DISPLAY 00 AT START OF THE GAME SUB
displayZero:			
//SEGEMENT DISPLAY
		ldi    temp,$ff	
        out    DDRB,temp

Zero:
			ldi		temp,$03
			mov 	flash,temp		
			ldi		temp,$ff ;$0f
			mov		count,temp
			rcall 	outputZeros
			rcall 	play
			rjmp	Zero

outputZeros:					
//LEFT digit
		ldi 	temp,$BF			;0_
		out    	PORTB,temp
       	rcall  	SmallDelay 
												;rcall  	delay   
         	
//RIGHT digit		
		ldi 	temp,$3F			;_0	
		out    	PORTB,temp			; display data on port B 
		rcall  	SmallDelay
		
		dec		count
		ldi		temp,$00
		cp		count,temp
		brne	outputZeros
		
		dec		flash
		clr		temp
		out    	PORTB,temp
		ldi		temp,$00
		cp		flash,temp
		brne	delayU

		rjmp leave
		
SmallDelayZ:   
			ldi    r24,$FF    		;Initialise decrement counter 
DelayLoopZ:    
			dec         r24        	;Decrement counter 
            brne   DelayLoopZ      	;and continue to decrement until counter = 0 
            ret		
leave:
	
		clr		temp
		out    	PORTB,temp
					 
delayU:   
	   	ldi    r24,$ff          ;Initialise 2nd loop counter 
loopS:   ldi    r25,$ff          ;Initialise 1st loop counter 
loopT:   dec    r25              ;Decrement the 1st loop counter 
         brne   loopT            ;and continue to decrement until 1st loop counter = 0 
         dec    r24              ;Decrement the 2nd loop counter 
         brne   loopS            ;If the 2nd loop counter is not equal to zero repeat the 1st loop, else continue 
         ret
		 

play:
		in		r13,PIND		;get the button pressed
		com		r13				;get the inverted eqivalent 
		ldi		temp,$00			
		cp		r13,temp		;ensure that a buttons pressed by comparing to 0
		
		brne 	infinite		;if any button is pressed
		ret
start:
		in		r13,PIND		;get the button pressed
		com		r13				;get the inverted eqivalent 
		ldi		temp,$00			
		cp		r13,temp		;ensure that a buttons pressed by comparing to 0		
		brne 	displayZero		;if any button is pressed
		ret

;**********MAIN PROGRAM************ 		
;___________________________________
infinite:							
		rcall	direction							
		rjmp  	infinite 			
;___________________________________
;**********************************


;delay section of code (25.348 ms @ 1MHz) - utilises r25,r24 

delay:   
	   	ldi		temp,$00
		ldi    r24,$ff          ;Initialise 2nd loop counter 

loop2:   ldi    r25,$FF          ;Initialise 1st loop counter 
			
loop1:   dec    r25              ;Decrement the 1st loop counter 
         brne   loop1            ;and continue to decrement until 1st loop counter = 0 
         dec    r24              ;Decrement the 2nd loop counter 
         brne   loop2            ;If the 2nd loop counter is not equal to zero repeat the 1st loop, else continue 
         
		 ret                   ;Return



//PSEUDO RANDOM NO. GENERATOR (RNG)
randomNoGen:
		ldi 	counter,$08		  ;counter for shifting leds 7 bits to the left
	 		
;load r17 and r18 with latest values of r16
		ldi r18,$00
		add r18,randomNo
		ldi r17,$00
		add r17,randomNo
		andi 	r18, $02	;and it with $02 to extract 2nd bit
		lsr     r18			;ensures bit is one the lsb
		andi	r17, $01	;and it with $01 to extract 1st bit
		eor 	r18,r17
;put r18 as msb so following 7 0s are required 
		ldi 	r19, $07
loopA:	lsl 	r18
		dec 	r19
		brne 	loopA
		lsr		randomNo	;shift right to remove lsb
		or 		randomNo,r18 ;or to replace the msb with the xor value
		rjmp	shipToDisplay 

//DISPLAY SHIPS
shipToDisplay:		;determines the ship to display according to the random no. generated
		ldi 	value,$00
		add 	value,randomNo
		
		cpi 	value, 85
		brlo	AC
		
		cpi 	value, 170
		brlo	B
		
		cpi 	value, 255
		brlo	D

		rjmp 	direction
//GET DIRECTION VALUE and place on correct side
		
AC:
		cpi 	directionVal, 127
		brlo	ACL		
		cpi 	directionVal, 254
		brlo	ACR
		ret
		
B:
		cpi 	directionVal, 127
		brlo	BL
		cpi 	directionVal, 254
		brlo	BR
		ret
		
D:
		cpi 	directionVal, 127
		brlo	DL
		cpi 	directionVal, 254
		brlo	DR
		ret
	
		
//right to LEFT
ACL:	;Aircraft carrier moving left
		ldi 	leds,$07	;Aircraft Carrier 3leds
		mov		killedShip,leds
		ret

BL:							
		ldi 	leds,$03	;Battleship 2leds
		mov		killedShip,leds
		ret

DL:							
		ldi 	leds,$01	;Destroyer 1led
		mov		killedShip,leds
		ret
//left to RIGHT
ACR:		;Aircraft carrier moving right
		ldi 	leds,$E0	;Aircraft Carrier 3leds
		mov		killedShip,leds
		ret

BR:							
		ldi 	leds,$C0	;Battleship 2leds
		mov		killedShip,leds
		ret

DR:							
		ldi 	leds,$80	;Destroyer 1led
		mov		killedShip,leds
		ret

//DIRECTION 254/2 = 127; below 127 left, above right
direction:
		
		rcall 	randomNoGen
		ldi		directionVal,$00
		add		directionVal,randomNo
		
		//MOVE LEFT
		cpi 	directionVal, 127
/*the following ensures left bits dont go further left i.e. sends them 
in the opposite direction so that leds stay on display only:*/			
		cpi 	leds, 0b10000000
		breq	foreverRight
		
		cpi 	leds, 0b11000000
		breq	foreverRight

		cpi 	leds, 0b11100000
		breq	foreverRight
//when console reaches here only the right bits will be moving left	
		brlo	foreverLeft
		//MOVE RIGHT

		cpi 	directionVal, 254
/*Ensures right bits dont go further right i.e. sends them 
in the opposite direction so that leds stay on display only:*/	
		cpi 	leds, 0b00000001
		breq	foreverLeft
		
		cpi 	leds, 0b00000011
		breq	foreverLeft

		cpi 	leds, 0b00000111
		breq	foreverLeft
//when console reaches here only the left bits will be moving right		
		brlo	foreverRight 


		
foreverLeft:
			
			out    	PORTC,leds      	;Display leds to port C 
   			rcall  	delay
		 	rcall	Hit
			lsl 	leds			  	;shift the leds by 1 bit to the left	 	
			dec		counter			  	;do this 7 times 
		 	cpi		counter,0
			brne 	foreverLeft
  		 	rcall	randomNoGen			;counter is reinitalised here 
			rjmp	direction
		 	

foreverRight:
			
			out    	PORTC,leds      	;Display leds to port C 
   			rcall  	delay
		 	rcall	Hit
			lsr 	leds			  	;shift the leds by 1 bit to the left
			dec		counter			  	;do this 7 times 
		 	cpi		counter,0
			brne 	foreverRight
  			rcall	randomNoGen			;counter is reinitalised here 
			rjmp	direction

//DetectPushButtons	
;READ ONBOARD PUSH BUTTONS
Hit:
		in		r13,PIND		;get the button pressed
		com		r13				;get the inverted eqivalent 
		ldi		temp,$00			
		cp		r13,temp		;ensure that a buttons pressed by comparing to 0
		brne 	pressed			
		ret
pressed:
		mov		r12,r13			;copy the pressed operand to r12 fron original r13
		and		r12,leds		;and the values with the leds displayed
		cp		r13,r12					
		breq	score			;if there is equality (i.e. a common one is extracted)		
		ldi		temp,$01
		add		lives,temp
		rcall	lostLives
		ret	

score:

		ldi		leds,0b11111111
		out    	PORTC,leds      	;Display leds to port C 
		rcall	delay
		ldi	temp,$09 ;??
		rcall	buzz
		rcall	buzz
		rcall	buzz
		rcall	buzz
		rcall	buzz
		rcall	buzz
		rcall	buzz
		rcall	buzz
		rcall	buzz
		rcall	buzz
		rcall	GetScore
		ldi		leds,$00
		out    	PORTC,leds      	;Display leds to port C 
		rcall  	delay
		rjmp	infinite
buzz:
		
		ldi		temp,0b00001000
		out 	PORTA,temp
		//delay
		rcall	buzzerDelay
		ldi		temp,0b00000000
		out 	PORTA,temp
		rcall	buzzerDelay
		ret


buzzerDelay: 
			
		   	ldi    r24,$10          ;Initialise 2nd loop counter 
loopbuzz2:  ldi    r25,$05;$ff          ;Initialise 1st loop counter 
loopbuzz1:  dec    r25              ;Decrement the 1st loop counter 
         	brne   loopbuzz1            ;and continue to decrement until 1st loop counter = 0 
         	dec    r24              ;Decrement the 2nd loop counter 
         	brne   loopbuzz2            ;If the 2nd loop counter is not equal to zero repeat the 1st loop, else continue 	
			ret 

beginGame:
//SEGEMENT DISPLAY
		ldi    temp,$ff	
        out    DDRB,temp

Flashing:
			ldi		temp,$03
			mov 	flash,temp		
			ldi		temp,$ff ;$0f
			mov		count,temp
			rcall 	resetScore //displays lives
			rcall 	start
			rjmp	Flashing


resetScore://DisplaysLives					
//RIGHT digit
			ldi 	temp,$CF			;_3
			out    	PORTB,temp
       		rcall  	SmallDelay 
												;rcall  	delay   
//LEFT digit
			ldi 	temp,$3F			;0_	
			out    	PORTB,temp			; display data on port B 
			rcall  	SmallDelay
			dec		count
			ldi		temp,$00
			cp		count,temp
			brne	resetScore
		
			dec		flash
			clr		temp
			out    	PORTB,temp
			ldi		temp,$00
			cp		flash,temp
			brne	delayA
		
			rjmp exit
		
SmallDelay:   
			ldi    r24,$FF    		;Initialise decrement counter 
DelayLoop:    
			dec         r24        	;Decrement counter 
            brne   DelayLoop      	;and continue to decrement until counter = 0 
            ret
exit:
			clr		temp
			out    	PORTB,temp
	
delayA:   
	   		ldi    r24,$ff          ;Initialise 2nd loop counter 
loopQ:   	ldi    r25,$ff          ;Initialise 1st loop counter 
loopR:   	dec    r25              ;Decrement the 1st loop counter 
         	brne   loopR            ;and continue to decrement until 1st loop counter = 0 
         	dec    r24              ;Decrement the 2nd loop counter 
         	brne   loopQ            ;If the 2nd loop counter is not equal to zero repeat the 1st loop, else continue 
         	ret
		 
		 
GetScore:
	
		ldi 	temp,$07	
		cp		temp,killedShip		;Aircraft Carrier 3leds
		breq	ACScore		;	Aircraft Score	
		
							
		ldi 	temp,$03	;Battleship 2leds
		cp		temp,killedShip
		breq	BScore
				
		ldi 	temp,$01	;Destroyer 1led
		cp		temp,killedShip
		breq	DScore
		
		ldi 	temp,$E0
		cp		leds,killedShip
		breq	ACScore	
		
		ldi 	temp,$C0	;Battleship 2leds
		mov		killedShip,leds
		breq	BScore
				
		ldi 	temp,$80	;Destroyer 1led
		mov		killedShip,leds
		breq	DScore
		rjmp infinite
	

ACScore:	//worth 4 points
		ldi    temp,$FF    		;Initialise decrement counter 
		mov		scoreDelay,temp
		rcall 	showScoreAC	
		rjmp	infinite

showScoreAC:
		ldi	temp,$E6 ;_4			;load temp with score to display on DSSD
		mov rightDigit,temp
			
		out    PORTB,rightDigit
       	rcall  	SmallDelay 
		
		ldi	temp,$3F ;0_
		mov	leftDigit,temp
		
		out    PORTB,leftDigit
       	rcall  	SmallDelay 
		   
		dec    scoreDelay        	;Decrement counter 
        brne   showScoreAC      	;and continue to decrement until counter = 0 
        clr 	temp
		out		PORTB, temp
		ret  

BScore:	//worth 8 points
		ldi    temp,$FF    		;Initialise decrement counter 
		mov		scoreDelay,temp
		rcall 	showScoreB	
		rjmp	infinite

showScoreB:
		ldi	temp,$FF ;_8
		mov	rightDigit,temp

		out    PORTB,rightDigit
       	rcall  	SmallDelay 
		
		ldi	temp,$3F	;0_
		mov	leftDigit,temp

		out    PORTB,leftDigit
       	rcall  	SmallDelay 

		dec    scoreDelay        	;Decrement counter 
        brne   showScoreB      	;and continue to decrement until counter = 0 
        clr 	temp
		out		PORTB, temp
		ret  

DScore:	//worth 12 points
		ldi    temp,$FF    		;Initialise decrement counter 
		mov		scoreDelay,temp
		rcall 	showScoreD	
		rjmp	infinite

showScoreD:		
		
		ldi	temp,$DB ;_2
		mov	rightDigit,temp
		
		out    PORTB,rightDigit
       	rcall  	SmallDelay 

		ldi	temp,$06 ;1_
		mov	leftDigit,temp

		out    PORTB,leftDigit
       	rcall  	SmallDelay 

		dec    scoreDelay        	;Decrement counter 
        brne   showScoreD      	;and continue to decrement until counter = 0 
        clr 	temp
		out		PORTB, temp
		ret

lostLives:	
		ldi		temp,$01
		cp		lives,temp
		breq	twoLives
		
		ldi		temp,$02
		cp		lives,temp
		breq	oneLife
		
		ldi		temp,$03
		cp		lives,temp
		breq	lastLife
		
		rcall	endGame
		ret
		
		
twoLives:
		ldi    temp,$FF    		;Initialise decrement counter 
		mov		scoreDelay,temp
		rcall 	lifeTwo	
		rjmp	infinite

lifeTwo:
		ldi	temp,$DB ;_2			;load temp with score to display on DSSD
		mov rightDigit,temp
			
		out    PORTB,rightDigit
       	rcall  	SmallDelay 
		
		ldi	temp,$3F ;0_
		mov	leftDigit,temp
		
		out    PORTB,leftDigit
       	rcall  	SmallDelay 
		   
		dec    scoreDelay        	;Decrement counter 
        brne   lifeTwo      	;and continue to decrement until counter = 0 
        ;rjmp 	infinite  
		clr 	temp
		out		PORTB, temp
		ret

oneLife:
		ldi    temp,$FF    		;Initialise decrement counter 
		mov		scoreDelay,temp
		rcall 	lifeOne	
		rjmp	infinite
		
		
lifeOne:		
		ldi	temp,$86 ;_1			;load temp with score to display on DSSD
		mov rightDigit,temp
			
		out    PORTB,rightDigit
       	rcall  	SmallDelay 
		
		ldi	temp,$3F ;0_
		mov	leftDigit,temp
		
		out    PORTB,leftDigit
       	rcall  	SmallDelay 
		   
		dec    scoreDelay        	;Decrement counter 
        brne   lifeOne      	;and continue to decrement until counter = 0 
		clr 	temp
		out		PORTB, temp
		ret  


lastLife:
		ldi    temp,$FF    		;Initialise decrement counter 
		mov		scoreDelay,temp
		rcall 	last	
		rjmp	infinite

last:		
		ldi	temp,$BF ;_0			;load temp with score to display on DSSD
		mov rightDigit,temp
			
		out    PORTB,rightDigit
       	rcall  	SmallDelay 
		
		ldi	temp,$3F ;0_
		mov	leftDigit,temp
		
		out    PORTB,leftDigit
       	rcall  	SmallDelay 
		   
		dec    scoreDelay        	;Decrement counter 
        brne   last      	;and continue to decrement until counter = 0 
	   	clr 	temp
		out		PORTB, temp
	   	ret 

endGame:
		rjmp	beginGame
		ret

//TORPEDO END
