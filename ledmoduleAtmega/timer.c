#include <avr/io.h>
#include <avr/interrupt.h>

#include "timer.h"
#include "leds.h"


volatile uint8_t initialized = 0;


ISR(TIMER1_COMPA_vect)
{
	if(initialized==1)	PORTD |= (1<<PORTD7);//blanc on

	if(newdata == 1)
	{
		PORTB |= (1<<PORTB1); // latch on
		PORTB &= ~(1<<PORTB1); // latch off
	                
		newdata=0;
	}

	if(initialized==1)	PORTD &= ~(1<<PORTD7);//blanc off

}

    


//*****************************************************************************
// 
void TIMER1_Init (void)
{
	// this will generate an TIMER1_COMPA_vect every 4096 clocks, exacly the duration of a tlc cycle
	TCCR1B |= (1<<WGM12)|(1<<CS10);
	OCR1A = 0xFFF;
	TIMSK1 |= (1<<OCIE1A);
}



