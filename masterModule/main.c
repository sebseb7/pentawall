#include <inttypes.h>
#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/pgmspace.h>
#include <avr/eeprom.h>
#include <util/delay.h>

#include "main.h"
#include "usart.h"
#include "TWI_Master.h"


/*

ideas to optimize the code:

- not more uart buffer, directly write to two full-frame bufferd and 2 single LED buffers
- do the parsing in the Uarts ISR (keep in mind that the ISR should be as short as possible)
- set xoff if one fullframe/led buffer is full and the second one is 50% filled
- the while loop then only polls buffer status 


think about: can eleminate the full-frame buffer by directly writing to i2c?

*/


uint8_t framebuffer[720];
uint8_t ledbuffer[5];


void SetLed(void)
{
	if (!TWIM_Start (2, TWIM_WRITE))
	{
		TWIM_Stop ();
	}
	else
	{
		TWIM_Write(ledbuffer[0]);
		TWIM_Write(ledbuffer[1]);
		TWIM_Write(ledbuffer[2]);
		TWIM_Write(ledbuffer[3]);
		TWIM_Write(ledbuffer[4]);
		TWIM_Stop ();
	}
}


void SetFrame(void)
{
	if (!TWIM_Start (2, TWIM_WRITE))
	{
		TWIM_Stop ();
	}
	else
	{
		TWIM_Write (255);
					
		for(uint8_t m = 0;m<48;m++)
		{
			uint16_t midx = m*15;
			
			for(uint8_t n = 0;n<15;n++)
			{
				TWIM_Write(framebuffer[midx+n]);
			}			
		}
		TWIM_Write (255);
		TWIM_Stop ();
	}
}



int main (void)
{

	// overflow led
	DDRC |= (1<<DDC3);

	USART_Init ();
	sei ();


	TWIM_Init (200000);

	_delay_ms(500);


	// fill wall with green 
	ledbuffer[2]=55;
	ledbuffer[3]=105;
	ledbuffer[4]=0;
	for(uint8_t p = 1;p<49;p++)
	{
		ledbuffer[0]=p;
		
		ledbuffer[1]=1;
		SetLed();
		_delay_ms(20);
		ledbuffer[1]=2;
		SetLed();
		_delay_ms(20);
		ledbuffer[1]=3;
		SetLed();
		_delay_ms(20);
		ledbuffer[1]=4;
		SetLed();
		_delay_ms(20);
		ledbuffer[1]=5;
		SetLed();
		_delay_ms(20);
	}

	
	uint8_t mode = 0;
	
	uint8_t data = 0;
	uint8_t escape = 0;
	uint16_t idx = 0;
	
	while(1)
	{
		while (uart_getc_nb(&data))
		{

			
			if(data == 0x42)
			{
				mode = 1;
				continue;
			}
			if(data == 0x23)
			{
				mode = 2;
				idx = 0;
				continue;
			}
			if(data == 0x65)
			{
				escape = 1;
				continue;
			}


			if(escape == 1)
			{
				escape = 0;
				if(data == 0x01)
				{
					data = 0x23;
				}
				else if(data == 0x02)
				{
					data = 0x42;
				}
				else if(data == 0x03)
				{
					data = 0x65;
				}
				else if(data == 0x04)
				{
					data = 0x66;
				}
			}
			
		
			if(mode == 2)
			{
				ledbuffer[idx]=data;
				idx++;

				if(idx == 5)
				{
					SetLed();
					mode = 0;
					idx = 0;
				}
			}
			else if(mode == 2)
			{
				framebuffer[idx]=data;
				idx++;	
				

				if(idx == 720)
				{
					SetFrame();
					mode = 0;
					idx = 0;
				}
			}
		}
	}
}
