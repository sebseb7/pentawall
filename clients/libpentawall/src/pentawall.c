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
#include <unistd.h>
#include <stdio.h>

#include "libpentawall-1.0.h"

#if 0
#define debugf(format, args...) do{printf("%s:%d: " format "\n" , __FILE__,__LINE__ , ##args );}while(0)
#else
#define debugf(format, args...) do{}while(0)
#endif

#define FAULHEIT 10
#define SCHLAF 0

struct pw_config {
	char * node;
	char * service;
	struct addrinfo hints;
};

void hardcoded_config(struct pw_config *c){
	memset(&c->hints, 0, sizeof(c->hints));
	c->hints.ai_family = AF_UNSPEC;  /*IPv6 in Ur home ? */
	c->hints.ai_socktype = SOCK_STREAM;
	c->node = "ledwall";
	c->service = "1338";
}

struct pentawall_ctx {
	int sfd;
	struct addrinfo *walladdr;
	int priority; 
};

static int connect_wall(struct pentawall_ctx *wall , struct pw_config *c);
static void send_command(struct pentawall_ctx *wall , char command, size_t datalen ,char * data);

static int connect_wall(struct pentawall_ctx *wall , struct pw_config *c)
{

	struct addrinfo *res0 , *res;
	int err;
	
	debugf("hints node %s service %s",c->node,c->service);
	if ((err = getaddrinfo(c->node, c->service, &(c->hints), &res0)) != 0) {
		debugf("error %d : %s", err, gai_strerror(err));
		return 1;
	}

	for (res=res0; res!=NULL; res=res->ai_next) {
		debugf("res is %p",res);
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

static void 
send_command(struct pentawall_ctx *wall , char command,
		size_t datalen ,char * data)
{
	int i;
	char * buf = NULL;
	static unsigned int lazyrv = 0;
	char resp[256];
	buf = malloc(datalen + 5);
	if (! buf){
		debugf("alloc fail! scheiße");
		return;
	}
	sprintf(buf,"%02x",command);
	if (datalen && data){
		memcpy(buf+2,data,datalen);
	}
	strcpy(buf+2+datalen,"\r\n");
	debugf("ja hallo %s",buf);
	i = send (wall->sfd , buf , datalen +4 ,0);
	if (i < (datalen+4)) {
		debugf("huh i send only %d bytes (-1 error)",i);
	}
	free(buf);
	if (++lazyrv >= FAULHEIT){
		i = recv(wall->sfd , resp , 256,0 /*flags*/);
		debugf ("gu %i",i);
		lazyrv ^= lazyrv;
	}
	return;
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


void 
pentaw_setpixel(pw_ctx_t wall, wallpixel *p)
{
	char buf[4+6+1];
	sprintf(buf,"%02x" "%02x""%02x""%02x""%02x" ,1+ p->x , p->y +1 , p->r , p->g ,p->b ); 
	send_command(wall , 2 , sizeof(buf) -1 ,buf) ;
	return ;
}

void
pentaw_deinit(pw_ctx_t wall){
	
	close(wall->sfd);
	free(wall);
	return ;
}



#ifdef TEST_FREESTANDING

int 
main(void)
{
	unsigned int row,col ;
	struct pw_config cfg;
	pw_ctx_t wall= NULL;
	wallpixel p;
	hardcoded_config(&cfg);
	wall = pentaw_init(&cfg);
	if (!wall) return 1;
	p.r = p.g = p.b = 0xCC;
	p.a = 0; /*unused anyway*/
	for (row=0;row<15;row++){
		for(col=0;col<16;col++){
			p.y= row ; p.x = col;
			p.r= col * row % 0xf0;
			p.b= ((col & 1) ^ (row &1)) * 0xef;
			pentaw_setpixel(wall , &p);
		}
	}
	sleep(SCHLAF);
	pentaw_deinit(wall);
	return 0;
}

#endif
