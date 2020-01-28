#ifndef KILO_H
#define KILO_H

#include <termios.h>
#include <time.h>

struct editorSyntax {
  char *filetype;
  char **filematch;
  char **keywords;
  char *singleline_comment_start;
  char *multiline_comment_start;
  char *multiline_comment_end;
  int flags;
};

typedef struct erow {
  int idx;     // which row in the buffer it represents
  int size;    // the row length
  char *chars; // the characters in the line
  int rsize; // the length of the "rendered" line, where eg. \t will expand to n
             // spaces
  char *render;        // the "rendered" characters in the line
  unsigned char *hl;   // the highlight property of a character
  int hl_open_comment; // whether this line begins or is part of a multiline
                       // comment
} erow;

enum editorMode { MODE_NORMAL = 0, MODE_INSERT = 1 };

typedef struct editorConfig {
  int cx, cy;         // cursor
  int rx;             // render index, as some chars are multi-width (eg. tabs)
  int rowoff;         // file offset
  int coloff;         // same as above
  int screenrows;     // size of the terminal
  int screencols;     // size of the terminal
  int numrows;        // size of the buffer
  erow *row;          // current row
  int dirty;          // is modified?
  char *filename;     // name of file linked to the buffer
  char statusmsg[80]; // status message displayed on at bottom of buffer
  time_t statusmsg_time;       // how long ago status message was written
  struct editorSyntax *syntax; // the syntax rules that apply to the buffer
  struct termios orig_termios; // the terminal state taken at startup; used to
                               // restore on exit
  int mode;
  struct editorConfig *undo; // pointer to the previous state
  struct editorConfig *redo; // pointer to the next state
} editorConfig;

void initEditor(editorConfig *e);

#endif
