/*
 *   Copyright (c) 2003 Nikodemus Siivola
 *   
 *   Permission is hereby granted, free of charge, to any person obtaining
 *   a copy of this software and associated documentation files (the
 *   "Software"), to deal in the Software without restriction, including
 *   without limitation the rights to use, copy, modify, merge, publish,
 *   distribute, sublicense, and/or sell copies of the Software, and to
 *   permit persons to whom the Software is furnished to do so, subject to
 *   the following conditions:
 *   
 *   The above copyright notice and this permission notice shall be included
 *   in all copies or substantial portions of the Software.
 *   
 *   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 *   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 *   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 *   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 *   CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 *   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 *   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#include <unistd.h>
#include <termios.h>
#include <stdlib.h>
#include <sys/ioctl.h>

const int linedit_OK              = 0;
const int linedit_NOT_ATTY        = 1;
const int linedit_MEMORY_ERROR    = 2;
const int linedit_TCGETATTR_ERROR = 3;
const int linedit_TCSETATTR_ERROR = 4;
const int linedit_ATTR_ERROR      = 5;
const int linedit_NO_ATTR_ERROR   = 6;

static struct termios * attr = NULL;

int
linedit_terminal_init (void)
{
  struct termios tmp;  

  if (! isatty (STDIN_FILENO)) 
    return linedit_NOT_ATTY;

  /* Save current terminal state in attr. */
  if (attr) 
    return linedit_ATTR_ERROR;

  attr = malloc (sizeof (struct termios));

  if (! attr) 
    return linedit_MEMORY_ERROR;

  if (0 > tcgetattr (STDIN_FILENO, attr)) 
    return linedit_TCGETATTR_ERROR;

  /* Enter keyboard input mode. */
  if (0 > tcgetattr (STDIN_FILENO, &tmp))
    return linedit_TCGETATTR_ERROR;

  cfmakeraw (&tmp);
  tmp.c_oflag |= OPOST;

  if (0 > tcsetattr (STDIN_FILENO, TCSAFLUSH, &tmp))
    return linedit_TCSETATTR_ERROR;

  return linedit_OK;
}

int
linedit_terminal_close (void)
{
  /* Restore saved terminal state from attr. */
  if (! attr)
    return linedit_NO_ATTR_ERROR;
  
  if (! isatty (STDIN_FILENO)) 
    return linedit_NOT_ATTY;

  if (0 > tcsetattr (STDIN_FILENO, TCSANOW, attr)) 
    return linedit_TCSETATTR_ERROR;

  free (attr);

  attr = NULL;

  return linedit_OK;
}

int
linedit_terminal_columns (int def)
{
  char * col;

  /* For Linux at least. See ioctl_list(2). */
# if defined TIOCGWINSZ
  struct winsize size;

  if (0 == ioctl (STDIN_FILENO, TIOCGWINSZ, &size))
    return size.ws_col;
# endif

  if ((col = getenv ("COLUMNS")))
    return atoi (col);
  else
    return def;
}

int
linedit_terminal_lines (int def)
{
  char * lin;

  /* For Linux at least. See ioctl_list(2). */
# if defined TIOCGWINSZ
  struct winsize size;

  if (0 == ioctl (STDIN_FILENO, TIOCGWINSZ, &size))
    return size.ws_row;
# endif

  if ((lin = getenv ("LINES")))
    return atoi (lin);
  else
    return def;
}
