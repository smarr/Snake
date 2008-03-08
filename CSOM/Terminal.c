/*
 *  Terminal.c
 *  CSOM
 *
 *
 */

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>


/*************************************************/
#pragma mark * Included Headers                  *
/*************************************************/

#include <termios.h>
#include <unistd.h>
#include <vmobjects/VMObject.h>

/*************************************************/
#pragma mark * Primitive Foreward Declaration    *
/*************************************************/

;
void _Terminal_get(pVMObject object, pVMFrame frame);
void _Terminal_uninit(pVMObject object, pVMFrame frame);
void _Terminal_init(pVMObject object, pVMFrame frame);


/*************************************************/
#pragma mark * Internal functions and init.      *
/*************************************************/

/*** Lib initialization **/
#ifdef __GNUC__
void init(void) __attribute__((constructor));
void fini(void) __attribute__((destructor));
#elif
void _init(void);
void _fini(void);
#pragma init _init
#pragma fini _fini
#endif

#ifdef __GNUC__
void init(void)
#elif
void _init(void)
#endif
{
	// Call init funcions.
	

}

#ifdef __GNUC__
void fini(void)
#elif
void _fini(void)
#endif
{
	
}

// Classes supported by this lib.
static char *supported_classes[] = {
    "Terminal",
    NULL
};


/*************************************************/
#pragma mark * Exported functions starting here  *
/*************************************************/

// returns, whether this lib is responsible for a specific class
bool		supports_class(const char* name) {
	
	char **iter=supported_classes;
	while(*iter)
		if (strcmp(name,*iter++)==0)
			return true;
	return false;
	
}



/*************************************************/
/*************************************************/
/*************************************************/
#pragma mark * Primitive Implementatition here   *
/*************************************************/
/*************************************************/
/*************************************************/

/******* initialize ******************************/





void Terminal_get(pVMObject object, pVMFrame frame){
  FILE *fp_tty;
  int c;

printf("ok");return;

  fp_tty = fopen( "/dev/tty", "r" ); /* NEW: cmd stream */
  if ( fp_tty == NULL ) /* if open fails */
    exit(1); /* no use in running */

  while( (c=getc(fp_tty)) != EOF ) // NEW: reads from tty 
  {
    printf("%s", c);
    if ( c == 'q' ) // q -> N 
      return;// 0;
    if ( c == ' ' ) // ' ' => next p
      return;// PAGELEN; // how many to show 
    if ( c == '\n' ) // Enter key => 1 line 
      return;// 1; 
  }
}



void Terminal_uninit(pVMObject object, pVMFrame frame){
}


void Terminal_init(pVMObject object, pVMFrame frame){
	    struct termios tty, old_tty;
printf("TErminal_init");
	    tcgetattr(0, &old_tty);
	    tty = old_tty;
	    tty.c_lflag = tty.c_lflag & ~(ECHO | ECHOK | ICANON);
	    tty.c_cc[VTIME] = 1;
	    tcsetattr(0, TCSANOW, &tty);
}

/*************************************************/
/*************************************************/
/*************************************************/
#pragma mark * EOF                               *
/*************************************************/
/*************************************************/
/*************************************************/

