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
 *     john at tuxcode org ||  <<</>> c3d2.de
 * ============================================================================
 */


#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <string.h>

#include "libpentawall-1.0.h"

#if 1
#include <stdio.h>
#define debugf(format, args...) do{printf("%s:%d: " format "\n" , __FILE__,__LINE__ , ##args );}while(0)
#else
#define debugf(format, args...) do{}while(0)
#endif

struct pw_config {
	char * node;
	char * service;
	struct addrinfo hints;
};

	


void hardcoded_config(struct pw_config *c){
	memset(&c->hints, 0, sizeof(c->hints));
	c->hints.ai_family = AF_UNSPEC;
	c->hints.ai_socktype = SOCK_STREAM;
	c->node = "ledwall";
	c->service = "1234";
	}


struct pentawall_ctx {
	int sfd;
	struct addrinfo *walladdr;
};



static int connect_wall(struct pentawall_ctx *wall , struct pw_config *c)
{

	struct addrinfo *res0 , *res;
	int err;

	if ((err = getaddrinfo(c->node, c->service, &(c->hints), &res)) != 0) {
		debugf("error %d : %s", err, gai_strerror(err));
		return 1;
	}

	for (res=res0; res!=NULL; res=res->ai_next) {
		wall->sfd = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
		if (wall->sfd < 0) {
			continue;
		}

		if (connect(wall->sfd, res->ai_addr, res->ai_addrlen) != 0) {
			close(wall->sfd);
			continue;
		}

		break;
	}

	if (res == NULL) {
		/* could not create a valid connection */
		debugf("mhh cons invalid ");
		return 1;
	}

	freeaddrinfo(res0);
	return 0;

}

struct pentawall_ctx * 
pentaw_init(struct pw_config *c)
{
	pw_ctx_t wall = NULL ;

	wall = malloc(sizeof(*wall));
	if (!wall){
		debugf("couldnt alloc mem for wall");
		return 0;
	}
	wall->walladdr = NULL;

	if (connect_wall(wall,c)){
		debugf("connection failed");
		free(wall);
		return NULL;
	}

	
	return wall;

}



void pantaw_setpixel(pw_ctx_t wall, wallpixel *p)
{
	return ;
}

void
pentaw_deinit(pw_ctx_t wall){
	
	close(wall->sfd);
	free(wall);
	return ;
}


