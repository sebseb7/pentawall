
#include <inttypes.h>
#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/eeprom.h>
#include <util/delay.h>

#include "main.h"
#include "timer.h"
#include "TWI_Slave.h"
#include "leds.h"
#include "spi.h"


// BLANC == PB2
// XLAT  == PB1
// SCLK == SCK/PB5
// SIN == MOSI/PB3
// MODE == PD5
// XERR == PD2
// SOUT == MISO/PB4
// GSCLK == PB0


int main (void)
{

	// set mosi/sck output
	DDRB |= (1<<DDB5)|(1<<DDB3)|(1<<DDB2);
	
	//latch output
	DDRB |= (1<<DDB1);
	//blank output
	DDRD |= (1<<DDD7);
	//mode output
	DDRD |= (1<<DDD5);

	// latch off
	PORTB &= ~(1<<PORTB1);
	// blank = high (all off)
	PORTD |= (1<<PORTD7);
	// mode = ground
	PORTD &= ~(1<<PORTD5);


	SPI_Init();
	TIMER1_Init();
	sei ();
	


	SetDC(0,63,63,63);
	SetLed(0,0,0,0);
	
	// address
	uint8_t addr = ADDR;
	
	//inital strobe for address identifiction
	for(uint8_t j=0;j < (addr-1);j++)
	{
		for(uint8_t i = 0;i<5;i++)
		{
			if(i!=0) SetLed(i,0,0,0);
			SetLed(i+1,0,0,0);
			_delay_ms(10);
		}
	}
	for(uint8_t i = 0;i<5;i++)
	{
		if(i!=0) SetLed(i,0,0,0);
		SetLed(i+1,255,255,255);
		_delay_ms(10);
	}
	SetLed(0,0,0,0);
	
	
	TWIS_Init(addr, 210526);
	

	uint8_t pixel = 0;
	uint8_t	TWIS_ResonseType;
			
						
	while(1)
	{						
		if (TWIS_ResonseRequired (&TWIS_ResonseType))
		{
			switch (TWIS_ResonseType)
			{
				case TWIS_ReadBytes:
	
					pixel = TWIS_ReadAck();
	
					if(pixel < 6)
					{
						SetLed(pixel,TWIS_ReadAck(),TWIS_ReadAck(),TWIS_ReadNack());
						TWIS_Stop();
					}
					else if(pixel < 11)
					{
						SetDC(pixel,TWIS_ReadAck(),TWIS_ReadAck(),TWIS_ReadNack());
						TWIS_Stop();
					}
					break;
			}
		}
	}
}
