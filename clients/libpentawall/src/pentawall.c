/*
 * ============================================================================
 *
 *       Filename:  pentawall.c
 *
 *    Description:  
 *
 *        Version:  1.0
 *        Created:  07.05.2011 15:07:54
 *       Revision:  none
 *       Compiler:  gcc
 
 *
 *         Author:  Johannes Steinmetz (js), john@tuxcode.org
 *        Company:  tuxcode.org
 *
 * ============================================================================
 */


#include <stdlib.h>

#include "pentawall.h"

struct pentawall_ctx {
	int i;
	int o ;
};

struct pentawall_ctx * pentaw_init(host ,port)
{
	pw_ctx_t wall = NULL ;
	wall = malloc(sizeof(*wall));
	return wall ;
}

void pantaw_setpixel(pw_ctx_t wall, wallpixel *p)
{
	return ;
}

void
pentaw_deinit(pw_ctx_t *wall){
	free(*wall);
	*wall = NULL;
	return ;
}


