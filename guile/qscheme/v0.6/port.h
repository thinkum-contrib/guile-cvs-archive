/*
 * Includes for port.c
 *
 * $Id$
 */

typedef struct PORT {
  short type;
  short io_flag;
  short	open_mode;
  int line;						/* line number starting from 0 */
  union {
	FILE *f;
	struct { int alloced; int index; int length; char *data;  } s;
  } descr;
} PORT;

/* open mode for files
 *
 * r	PORT_READ
 * w	PORT_WRITE
 * a	PORT_APPEND
 * r+	PORT_UPDATE
 * w+	PORT_TRUNCATE_UPDATE
 * a+	PORT_APPEND_UPDATE
 */

/*-- used to determine wether read or write are permitted on the file */

#define PORT_IO_R		1
#define PORT_IO_W		2
#define PORT_IO_RW		(PORT_IO_R|PORT_IO_W)

#define PORT_UNDEFINED		0
#define PORT_READ			1	/* "r"  */
#define PORT_CREATE			2	/* "w"  */
#define PORT_APPEND			3	/* "a"  */
#define PORT_UPDATE			4	/* "r+" */
#define PORT_UPDATE_CREATE	5	/* "w+" */
#define PORT_UPDATE_APPEND	6	/* "a+" */

#define PORT_MAX_MODES		7

/*-- port type: must be kept sync with port_driver[] */
#define PORT_T_FILE   0
#define PORT_T_STRING 1

#define PORT_MAX_TYPES	32		/* max number of port types */

/*-- special symbols */
#define PORT_EOF		EOF
#define PORT_STR_QTUM	128

/* driver depending on the port type */

typedef struct {
  char 	*name;					/* type of this port */
  void 	(*close)	(struct PORT *p);
  int 	(*getc)		(struct PORT *p);
  int 	(*peekc)	(struct PORT *p);
  void	(*putc)		(struct PORT *p, char c);
  void	(*seek)		(struct PORT *p, int pos);

  int	(*read)		(struct PORT *p, SOBJ str, int len);
  int	(*write)	(struct PORT *p, SOBJ str, int len);

  int 	(*getline)	(struct PORT *p, SOBJ str);
  int	(*putline)	(struct PORT *p, SOBJ str);

} PORT_DESCR;
