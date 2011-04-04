#include <inttypes.h>
#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/pgmspace.h>
#include <avr/eeprom.h>
#include <util/delay.h>

#include "main.h"
#include "usart.h"
#include "TWI_Master.h"


uint8_t bytemap[720];


void SetLed(uint8_t x,uint8_t y,uint8_t r,uint8_t g, uint8_t b)
{
	if (!TWIM_Start (2, TWIM_WRITE))
	{
		TWIM_Stop ();
	}
	else
	{
		TWIM_Write (x);
		TWIM_Write (y);
		TWIM_Write (r);
		TWIM_Write (g);
		TWIM_Write (b);
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
					
		for(uint8_t m = 1;m<48;m++)
		{
			uint16_t midx = (m-1)*15;
						
			TWIM_Write(bytemap[midx]);
			TWIM_Write(bytemap[midx+1]);
			TWIM_Write(bytemap[midx+2]);
						
			TWIM_Write(bytemap[midx+3]);
			TWIM_Write(bytemap[midx+4]);
			TWIM_Write(bytemap[midx+5]);
						
			TWIM_Write(bytemap[midx+6]);
			TWIM_Write(bytemap[midx+7]);
			TWIM_Write(bytemap[midx+8]);
						
			TWIM_Write(bytemap[midx+9]);
			TWIM_Write(bytemap[midx+10]);
			TWIM_Write(bytemap[midx+11]);
						
			TWIM_Write(bytemap[midx+12]);
			TWIM_Write(bytemap[midx+13]);
			TWIM_Write(bytemap[midx+14]);
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

	for(uint8_t p = 1;p<50;p++)
	{
		SetLed(p,1,55,105,0);
		_delay_ms(20);
		SetLed(p,2,55,105,0);
		_delay_ms(20);
		SetLed(p,3,55,105,0);
		_delay_ms(20);
		SetLed(p,4,55,105,0);
		_delay_ms(20);
		SetLed(p,5,55,105,0);
		_delay_ms(20);
		
	}

	
	uint8_t mode = 0;
	
	uint8_t b0 = 0;
	uint8_t b1 = 0;
	uint8_t b2 = 0;
	uint8_t b3 = 0;
	uint8_t b4 = 0;
	
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
				mode = 6;
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
			
		
			if(mode == 1)
			{
				b0 = data;
				mode = 2;
			}
			else if(mode == 2)
			{
				b1 = data;
				mode = 3;
			}
			else if(mode == 3)
			{
				b2 = data;
				mode = 4;
			}
			else if(mode == 4)
			{
				b3 = data;
				mode = 5;
			}
			else if(mode == 5)
			{
				b4 = data;
				mode = 0;
				if(b0 < 50)
				{
					if(b1 < 6)
					{
						SetLed(b0,b1,b2,b3,b4);
					}
				}
			}
			else if(mode == 6)
			{
				bytemap[idx]=data;
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





