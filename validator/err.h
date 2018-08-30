#ifndef _ERR_
#define _ERR_

/* write error info about system function and terminate program */
extern void syserr(const char *fmt, ...);

/* write error info and terminate program */
extern void fatal(const char *fmt, ...);

#endif