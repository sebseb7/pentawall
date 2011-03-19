#ifndef _TWI_Slave
#define _TWI_Slave

#define TRUE 1
#define FALSE 0

#define	TWIS_ReadBytes		0x60
#define	TWIS_WriteBytes		0xA8

uint8_t	TWIS_Init (uint8_t Address, uint32_t Bitrate);
void	TWIS_Stop (void);
void	TWIS_Write (uint8_t byte);
uint8_t	TWIS_ReadAck (void);
uint8_t	TWIS_ReadNack (void);
uint8_t	TWIS_ResonseRequired (uint8_t *TWI_ResonseType);

#endif
