/*****************************************************************************
 *   Copyright (C) 2010 seb@exse.net										 *
 *                                                                           *
 *   This program is free software; you can redistribute it and/or modify    *
 *   it under the terms of the GNU General Public License as published by    *
 *   the Free Software Foundation; either version 2 of the License.          *
 *                                                                           *
 *   This program is distributed in the hope that it will be useful,         *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *
 *   GNU General Public License for more details.                            *
 *                                                                           *
 *   You should have received a copy of the GNU General Public License       *
 *   along with this program; if not, write to the                           *
 *   Free Software Foundation, Inc.,                                         *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.               *
 *                                                                           *
 *****************************************************************************/

#include <avr/io.h>
#include <avr/eeprom.h>

#include "eeprom.h"

//*****************************************************************************
// 
uint8_t  i2c_address EEMEM = 0;
//uint8_t  EEMEM dc[96];

//*****************************************************************************
// 
void ReadParameter (void)
{
	CONF_i2c_address   = eeprom_read_byte(&i2c_address);
//	eeprom_read_block(&CONF_dc,&dc,96);

	if(CONF_i2c_address 	   == 0xFF  ) CONF_i2c_address = 0;
}

//*****************************************************************************
// 
void WriteParameter (void)
{
	eeprom_write_byte(&i2c_address,  CONF_i2c_address);
//	eeprom_write_block(&CONF_dc,&dc,sizeof(CONF_dc));
}
