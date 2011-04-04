#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/wdt.h>
#include <util/delay.h>
#include <stdarg.h>

#include "main.h"
#include "usart.h"


#define UART_RX0BUFSIZE 1500

volatile static uint8_t rxbuf[UART_RX0BUFSIZE];
volatile static uint8_t *volatile rxhead, *volatile rxtail;
volatile static uint8_t xon = 0;

uint8_t uart_getc_nb(uint8_t *c)
{
	if (rxhead==rxtail) return 0;
	*c = *rxtail;
	if (++rxtail == (rxbuf + UART_RX0BUFSIZE)) rxtail = rxbuf;
	
	uint8_t diff = rxhead - rxtail;
	if((diff < 10)&&(xon==1))
	{
		PORTC |= (1<<PORTC3);
		xon=0;
		USART_putc(0x11);
	}
                                                                                    	
	return 1;
}


ISR (USART_RXC_vect)
{
	uint8_t diff;
	uint8_t c;
	c=UDR;
	diff = rxhead - rxtail;
	if (diff < 0) diff += UART_RX0BUFSIZE;
	if (diff < UART_RX0BUFSIZE -1) 
	{
		*rxhead = c;
		++rxhead;
		if (rxhead == (rxbuf + UART_RX0BUFSIZE)) rxhead = rxbuf;
		if((diff > 500)&&(xon==0))
		{
			PORTC &= ~(1<<PORTC3);
			xon=1;
			USART_putc(0x13);
		}
	}
}

//*****************************************************************************
// 
void USART_Init (void)
{
	// set clock divider
	#undef BAUD
	#define BAUD USART_BAUD
	#include <util/setbaud.h>
	UBRRH = UBRRH_VALUE;
	UBRRL = UBRRL_VALUE;
	
#if USE_2X
	UCSRA |= (1 << U2X);	// enable double speed operation
#else
	UCSRA &= ~(1 << U2X);	// disable double speed operation
#endif
	
	// set 8N1
	UCSRC = (1 << UCSZ1) | (1 << UCSZ0);
	UCSRB &= ~(1 << UCSZ2);

	// flush receive buffer
	while ( UCSRA & (1 << RXC) ) UDR;

	UCSRB |= (1 << RXEN) | (1 << TXEN);
	UCSRB |= (1 << RXCIE);

	rxhead = rxtail = rxbuf;

}



void USART_putc (char c)
{
	loop_until_bit_is_set(UCSRA, UDRE);
	UDR = c;
}

