
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
	
	
	// all slaves listen to addr 2
	TWIS_Init(2, 210526);
	
	uint8_t pixel = 0;
	uint8_t module  = 0;

	uint8_t	TWIS_ResonseType;

	uint8_t temp_r = 0;
	uint8_t temp_g = 0;
	uint8_t temp_b = 0;

	uint8_t led_1_r = 0;
	uint8_t led_1_g = 0;
	uint8_t led_1_b = 0;
	uint8_t led_2_r = 0;
	uint8_t led_2_g = 0;
	uint8_t led_2_b = 0;
	uint8_t led_3_r = 0;
	uint8_t led_3_g = 0;
	uint8_t led_3_b = 0;
	uint8_t led_4_r = 0;
	uint8_t led_4_g = 0;
	uint8_t led_4_b = 0;
	uint8_t led_5_r = 0;
	uint8_t led_5_g = 0;
	uint8_t led_5_b = 0;

						
	while(1)
	{						
		if (TWIS_ResonseRequired (&TWIS_ResonseType))
		{
			switch (TWIS_ResonseType)
			{
				case TWIS_ReadBytes:
	
					module = TWIS_ReadAck();

					if(module >= 254)
					{
						for(uint8_t n=0;n<3;n++)
						{
							for(uint8_t i=0;i<5;i++)
							{
								for(uint8_t m=0;m<16;m++)
								{
									temp_r = TWIS_ReadAck();
									temp_g = TWIS_ReadAck();
									temp_b = TWIS_ReadAck();

			
									if( (ADDR-1) == n*16+m )
									{

										if(i == 0)
										{
											led_1_r = temp_r;
											led_1_g = temp_g;
											led_1_b = temp_b;
										}
										if(i == 1)
										{
											led_2_r = temp_r;
											led_2_g = temp_g;
											led_2_b = temp_b;
										}
										if(i == 2)
										{
											led_3_r = temp_r;
											led_3_g = temp_g;
											led_3_b = temp_b;
										}
										if(i == 3)
										{
											led_4_r = temp_r;
											led_4_g = temp_g;
											led_4_b = temp_b;
										}
										if(i == 4)
										{
											led_5_r = temp_r;
											led_5_g = temp_g;
											led_5_b = temp_b;
										}
									}
								}
							}	
						}

						TWIS_ReadNack();
						TWIS_Stop();

						if(module == 255)
						{
							SetLed(1,led_1_r,led_1_g,led_1_b);
							SetLed(2,led_2_r,led_2_g,led_2_b);
							SetLed(3,led_3_r,led_3_g,led_3_b);
							SetLed(4,led_4_r,led_4_g,led_4_b);
							SetLed(5,led_5_r,led_5_g,led_5_b);
						}

						if(module == 254)
						{
							SetDC(1,led_1_r,led_1_g,led_1_b);
							SetDC(2,led_2_r,led_2_g,led_2_b);
							SetDC(3,led_3_r,led_3_g,led_3_b);
							SetDC(4,led_4_r,led_4_g,led_4_b);
							SetDC(5,led_5_r,led_5_g,led_5_b);
						}

					}
					else
					{
						pixel = TWIS_ReadAck();
						uint8_t red = TWIS_ReadAck();
						uint8_t green = TWIS_ReadAck();
						uint8_t blue = TWIS_ReadNack();
						TWIS_Stop();

						if((module == ADDR)||(module == 0))
						{
								SetLed(pixel,red,green,blue);
						}
					}					
					
					break;
			}
		}
	}
}