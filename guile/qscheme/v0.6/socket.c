/* -*- tab-width:4; -*- */
/*
 * Socket interface
 *
 */
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <memory.h>
#include <errno.h>

#include "s.h"

int SOBJ_T_SOCKET;				/* socket type */

typedef struct {
  SOBJ hostname, hostip;
  int port;
  int fd;
  SOBJ input, output;
  SOBJ ready_event;
} SCM_SocketAux;

#define SCM_SOCKETP(x)	(SCM_OBJTYPE(x) == SOBJ_T_SOCKET)
#define SCM_SOCKET(x)	((SCM_SocketAux*)SCM_AUX(x))

/* report system error */
static void system_error(char *msg)
{
  char buf[1024];
  sprintf(buf, "%s: %s", msg, strerror(errno));
  SCM_ERR(buf, NULL);
}


static SOBJ scm_socket_new()
{
  SOBJ new;
  new = scm_newcell(SOBJ_T_SOCKET);
  SCM_SOCKET(new) = scm_must_alloc(sizeof(SCM_SocketAux));
  memset(SCM_SOCKET(new), 0, sizeof(SCM_SocketAux));
  return(new);
}

static void set_socket_io_ports(int s, SOBJ sock, char *who)
{
  int t;

  if ((t = dup(s)) == -1) SCM_ERR("cannot dup io port", NULL);

  SCM_SOCKET(sock)->input = scm_mk_fn_port(s, TRUE);
  SCM_SOCKET(sock)->output= scm_mk_fn_port(t, FALSE);
}

/*E* (make-client-socket HOST PORT) => SOCKET */
static SOBJ scm_make_client_socket(SOBJ hostname, SOBJ port)
{
  char str[] = "make-client-socket";
  struct hostent *hp;
  struct sockaddr_in server;
  int s;
  SOBJ z;

  if (!SCM_STRINGP(hostname))	SCM_ERR("bad hostname", hostname);
  if (!SCM_INUMP(port))			SCM_ERR("bad port number", port);

  /* Locate the host IP address */
  if ((hp=gethostbyname(SCM_STR_VALUE(hostname))) == NULL)
	SCM_ERR("unknow hostname", hostname);

  /* Get a socket */
  if ((s=socket(AF_INET,SOCK_STREAM,0)) < 0)
	SCM_ERR("cannot create socket",NULL);

  /* Setup a connect address */
  memset(&server, 0, sizeof(server));
  memcpy((char*)&server.sin_addr, hp->h_addr, hp->h_length);
  server.sin_family = AF_INET;
  server.sin_port   = htons(SCM_INUM(port));

  /* try to connect */
  if (connect(s, (struct sockaddr *) &server, sizeof(server)) < 0) {
    close(s);
    system_error(str);
  }
  
  z = scm_socket_new();
  SCM_SOCKET(z)->port		= ntohs(server.sin_port);
  SCM_SOCKET(z)->hostname	= scm_mkstring(hp->h_name);
  SCM_SOCKET(z)->hostip		= scm_mkstring(inet_ntoa(server.sin_addr));
  SCM_SOCKET(z)->fd			= s;
  SCM_SOCKET(z)->input		= scm_false;
  SCM_SOCKET(z)->output		= scm_false;
  SCM_SOCKET(z)->ready_event= scm_false;
  set_socket_io_ports(s,z,str);
  return(z);
}  

/*E* (make-server-socket [PORT]) => SOCKET */
/*D* Create a new socket listening on the specified port. If no port
 * argument is given, the system will choose a port number automaticaly
 */
static SOBJ scm_make_server_socket(int argc, SOBJ *arg)
{
  char msg[] = "make-server-socket";
  struct sockaddr_in sin;
  int s, portnum, len;
  SOBJ z;
  int opt;

  switch(argc) {
  case 0:
	portnum = 0;
	break;
  case 1:
  	if (!SCM_INUMP(arg[0]))	SCM_ERR("bad port number", arg[0]);
	portnum = SCM_INUM(arg[0]);
	break;
  default:
	portnum = 0;
	SCM_ERR("make-server-socket bad number of arguments",NULL);
  }

  if ((s=socket(AF_INET, SOCK_STREAM, 0)) < 0)
	SCM_ERR("cannot create socket", NULL);

  opt = 1;
  setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));

  /* Bind the socket to a name */
  sin.sin_family      = AF_INET;
  sin.sin_port 	      = htons(portnum);
  sin.sin_addr.s_addr = INADDR_ANY;

  if (bind(s, (struct sockaddr *) &sin, sizeof(sin)) < 0) {
    close(s);
    system_error(msg);
  }

  /* Query the socket name (obtain the true socket number if 0 was given) */
  len = sizeof(sin);
  if (getsockname(s, (struct sockaddr *) &sin, (int *) &len) < 0) {
    close(s);
    system_error(msg);
  }

  /* Indicate that we are ready to listen */
  if (listen(s, 5) < 0) {
    close(s);
    system_error(msg);
  }
  z = scm_socket_new();
  
  SCM_SOCKET(z)->port		= ntohs(sin.sin_port);
  SCM_SOCKET(z)->hostname	= scm_false;
  SCM_SOCKET(z)->hostip		= scm_false;
  SCM_SOCKET(z)->fd			= s;
  SCM_SOCKET(z)->input 		= scm_false;
  SCM_SOCKET(z)->output		= scm_false;
  SCM_SOCKET(z)->ready_event= scm_false;
  return(z);
}

/*E* (socket-accept-connection SOCKET) => #undefined */
static SOBJ scm_socket_accept_connection(SOBJ sock)
{
  char *s;
  char str[]= "socket-accept-connection";
  struct sockaddr_in sin;
  struct hostent *host;
  int len = sizeof(sin);
  int new_s;

  if (!SCM_SOCKETP(sock))	SCM_ERR("bad socket", sock);

  if ((new_s=accept(SCM_SOCKET(sock)->fd,(struct sockaddr *) &sin, &len)) < 0)
	system_error(str);
  
  /* Set the client info (if possible its name, otherwise its IP number) */
  host = gethostbyaddr((char *) &sin.sin_addr, sizeof(sin.sin_addr), AF_INET);
  s    = (char *) inet_ntoa(sin.sin_addr);
  
  SCM_SOCKET(sock)->hostip   = scm_mkstring(s);
  SCM_SOCKET(sock)->hostname = scm_mkstring(host? (char*) (host->h_name): s);
  set_socket_io_ports(new_s, sock, str);
  return(scm_undefined);
}

/*E* (socket-shutdown SOCKET CLOSE) => #undefined */
/*D* Shutdown the connection associated to the socket. CLOSE indicates
 * if the socket should be close or not */

static SOBJ scm_socket_shutdown2(SOBJ sock, int close_it)
{
  if (!SCM_SOCKETP(sock))	SCM_ERR("bad socket", sock);

  if (close_it && SCM_SOCKET(sock)->fd >= 0) {
	/*	scm_puts("shutdown of socket: ");  scm_cprint(sock);
		scm_puts("fd="); scm_putn(SCM_SOCKET(sock)->fd); scm_puts("\n"); */
	close(SCM_SOCKET(sock)->fd);
	/* shutdown(SCM_SOCKET(sock)->fd, 2); */
	SCM_SOCKET(sock)->fd = -1;
  }
  if (scm_input_portp(SCM_SOCKET(sock)->input) != scm_false) {
	scm_close_port(SCM_SOCKET(sock)->input);
	SCM_SOCKET(sock)->input = scm_false;
  }

  if (scm_output_portp(SCM_SOCKET(sock)->output) != scm_false) {
	scm_close_port(SCM_SOCKET(sock)->output);
	SCM_SOCKET(sock)->output = scm_false;
  }
  return(scm_undefined);
}

static SOBJ scm_socket_shutdown(int nargs, SOBJ *arg)
{
  SOBJ sock;
  int close_it;

  if (nargs < 1 || nargs > 2) 	SCM_ERR("bad number of args", NULL);
  sock = arg[0];
  close_it = (nargs == 2 && arg[1] != scm_false);
  return(scm_socket_shutdown2(sock, close_it));
}

/*** other sockets functions */
/*E* (socket? OBJ) => BOOLEAN */
static SOBJ scm_socketp(SOBJ s)
{
  return(SCM_MKBOOL(SCM_SOCKETP(s)));
}

/*E* (socket-port-number SOCKET) => INT */
static SOBJ scm_socket_port_number(SOBJ s)
{
  if (!SCM_SOCKETP(s))	SCM_ERR("bad socket", s);
  return(SCM_MKINUM(SCM_SOCKET(s)->port));
}

/*E* (socket-host-name SOCK) => STR */
static SOBJ scm_socket_host_name(SOBJ s)
{
  if (!SCM_SOCKETP(s))	SCM_ERR("bad socket", s);
  return(SCM_SOCKET(s)->hostname);
}

/*E* (socket-host-address SOCK) => STR */
static SOBJ scm_socket_host_address(SOBJ s)
{
  if (!SCM_SOCKETP(s))	SCM_ERR("bad socket", s);
  return(SCM_SOCKET(s)->hostip);
}

/*E* (socket-local-address SOCK) => STR */
static SOBJ scm_socket_local_address(SOBJ s)
{
  struct sockaddr_in sin;
  int len = sizeof(sin);

  if (!SCM_SOCKETP(s))	SCM_ERR("bad socket", s);

  if (getsockname(SCM_SOCKET(s)->fd,  (struct sockaddr *) &sin, &len))
	SCM_ERR("cannot get socket name", s);

  return(scm_mkstring((char *) inet_ntoa(sin.sin_addr)));
}

/*E* (socket-input SOCKET) => PORT */
static SOBJ scm_socket_input(SOBJ s)
{
  if (!SCM_SOCKETP(s))	SCM_ERR("bad socket", s);
  return(SCM_SOCKET(s)->input);
}

/*E* (socket-output SOCKET) => PORT */
static SOBJ scm_socket_output(SOBJ s)
{
  if (!SCM_SOCKETP(s))	SCM_ERR("bad socket", s);
  return(SCM_SOCKET(s)->output);
}

/*E* (socket-down? SOCKET) => BOOLEAN */
static SOBJ scm_socket_downp(SOBJ s)
{
  if (!SCM_SOCKETP(s))	SCM_ERR("bad socket", s);
  return(SCM_MKBOOL(SCM_SOCKET(s)->fd < 0));
}

static SOBJ scm_socket_dup(SOBJ s)
{
  SOBJ x;
  int newfd;

  if (!SCM_SOCKETP(s))	SCM_ERR("bad socket", s);

  if ((newfd = dup(SCM_SOCKET(s)->fd)) < 0)
	SCM_ERR("cannot dup socket", s);

  x = scm_socket_new();
  memcpy(SCM_SOCKET(x), SCM_SOCKET(s), sizeof(SCM_SocketAux));
  SCM_SOCKET(x)->fd = newfd;
  return(x);
}

/* forward declaration for type descriptor functions */
static void scm_socket_mark(SOBJ x)
{
  if (SCM_SOCKET(x)) {
	scm_gc_mark(SCM_SOCKET(x)->hostname);
	scm_gc_mark(SCM_SOCKET(x)->hostip);
	scm_gc_mark(SCM_SOCKET(x)->input);
	scm_gc_mark(SCM_SOCKET(x)->output);
	scm_gc_mark(SCM_SOCKET(x)->ready_event);
  }
}

static void scm_socket_sweep(SOBJ x)
{
  if (SCM_SOCKET(x)) {
	/*	scm_puts("; sweeping socket "); scm_cprint(x); */
	scm_socket_shutdown2(x, 1);
	scm_free(SCM_SOCKET(x));
	SCM_SOCKET(x) = NULL;
  }
}

static void scm_socket_print(SOBJ x, PORT *p)
{
  port_puts(p, "#<socket ");  scm_write_obj(SCM_SOCKET(x)->hostname, p, 1);
  port_puts(p, " ");		  port_putn(p, SCM_SOCKET(x)->port);
  port_puts(p, ">");
}

static void scm_socket_write(SOBJ x, PORT *p)
{
  scm_socket_print(x,p);
}

char *scm_socket2str(SOBJ obj, int raw)
{
  char *s;
  s = scm_asprintf("#<socket %s %d>",
				   SCM_STR_VALUE(SCM_SOCKET(obj)->hostname),
				   SCM_SOCKET(obj)->port);
  return(s);
}


/*
static SOBJ scm_socket2obj(void *x);
static void *scm_obj2socket(SOBJ x);
*/

/* the type descriptor */

SOBJ_TYPE_DESCR scm_socket_type = {
  0,											/* nothing here */
  "socket",										/* type name */
  scm_socket_mark, 		scm_socket_sweep, 		/* mark / sweep */
  scm_socket_print,		scm_socket_write,		/* print / write */
  NULL,					NULL,					/* creconize / cparse */
  NULL,					NULL,					/* wreconize / wparse */
  NULL,											/* compare */
  NULL,					NULL					/* ->obj obj-> */
};


void scm_init_socket()
{
  SOBJ_T_SOCKET = scm_add_type(&scm_socket_type);

  scm_add_cprim("make-client-socket",	scm_make_client_socket,		2);
  scm_add_cprim("make-server-socket",  	scm_make_server_socket,		-1);

  scm_add_cprim("socket-accept-connection",
				scm_socket_accept_connection,						1);

  scm_add_cprim("socket-shutdown",		scm_socket_shutdown,		-1);

  scm_add_cprim("socket?",				scm_socketp,				1);
  scm_add_cprim("socket-port-number",	scm_socket_port_number,		1);
  scm_add_cprim("socket-host-name",		scm_socket_host_name,		1);
  scm_add_cprim("socket-host-address",	scm_socket_host_address,	1);
  scm_add_cprim("socket-local-address",	scm_socket_local_address,	1);
  scm_add_cprim("socket-input",			scm_socket_input,			1);
  scm_add_cprim("socket-output",		scm_socket_output,			1);
  scm_add_cprim("socket-down?",			scm_socket_downp,			1);
  scm_add_cprim("socket-dup",			scm_socket_dup,				1);

  scm_puts("; socket extension loaded\n");
}

