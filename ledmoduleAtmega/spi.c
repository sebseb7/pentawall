#include <avr/io.h>

#include "spi.h"


void SPI_Init(void)
{
	SPCR = (1<<SPE)|(1<<MSTR)|(1<<SPR0);
}
		
void SPI_send(uint8_t cData) 
{
	SPDR = cData; 
	while(!(SPSR & (1<<SPIF)));
}
