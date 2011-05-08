/*
 * ============================================================================
 *
 *       Filename:  libpentawall-1.0.h
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
#pragma once

#include <stdint.h>

/*opaque declarations*/
typedef   struct pentawall_ctx *pw_ctx_t;
struct pw_config ;

typedef struct {
	uint16_t x ;
	uint16_t y ;
	uint8_t r;
	uint8_t g;
	uint8_t b;
	uint8_t a;
} wallpixel;

/*lib init + connection*/
struct pentawall_ctx * pentaw_init(struct pw_config *c);
/*destruction*/
void pentaw_deinit(pw_ctx_t wall);

/*set one pixel*/
void pantaw_setpixel(pw_ctx_t wall, wallpixel *p);



