/*
 * ============================================================================
 *
 *       Filename:  pentawall.h
 *
 *    Description:  
 *
 *        Version:  1.0
 *        Created:  07.05.2011 15:45:12
 *       Revision:  none
 *       Compiler:  gcc
 *
 *     john at tuxcode org   || <<</>>
 * ============================================================================
 */

#include <stdint.h>

typedef   struct pentawall_ctx *pw_ctx_t;


typedef struct {
	uint16_t x ;
	uint16_t y ;
	uint8_t r;
	uint8_t g;
	uint8_t b;
	uint8_t a;
} wallpixel;


