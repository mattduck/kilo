# define _DEFAULT_SOURCE
# define _BSD_SOURCE
# define _GNU_SOURCE

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <time.h>
#include <termios.h>
#include <unistd.h>

#define KILO_VERSION "0.0.1"
#define KILO_TAB_STOP 4

#define CTRL_KEY(k) ((k) & 0x1F)


int isPrefix (char c) {
  return c == CTRL_KEY('x');
}


enum editorKey {
                BACKSPACE = 127,
                ARROW_LEFT = 1000,
                ARROW_RIGHT,
                ARROW_UP,
                ARROW_DOWN,
                DEL_KEY,
                HOME_KEY,
                END_KEY,
                PAGE_UP,
                PAGE_DOWN
};

enum editorHighlight {
                      HL_NORMAL = 0,
                      HL_COMMENT,
                      HL_MLCOMMENT,
                      HL_KEYWORD1,
                      HL_KEYWORD2,
                      HL_STRING,
                      HL_NUMBER,
                      HL_MATCH
};

#define HL_HIGHLIGHT_NUMBERS (1<<0)
#define HL_HIGHLIGHT_STRINGS (1<<1)

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
  int idx;  // which row in the buffer it represents
  int size;  // the row length
  char *chars;  // the characters in the line
  int rsize; // the length of the "rendered" line, where eg. \t will expand to n spaces
  char *render;  // the "rendered" characters in the line
  unsigned char *hl;  // the highlight property of a character
  int hl_open_comment;  // whether this line begins or is part of a multiline comment
} erow;

enum editorMode {
           MODE_NORMAL = 0,
           MODE_INSERT = 1
};

struct editorConfig {
  int cx, cy;  // cursor
  int rx;  // render index, as some chars are multi-width (eg. tabs)
  int rowoff; // file offset
  int coloff; // same as above
  int screenrows; // size of the terminal
  int screencols; // size of the terminal
  int numrows;  // size of the buffer
  erow *row;  // current row
  int dirty;  // is modified?
  char *filename;  // name of file linked to the buffer
  char statusmsg[80];  // status message displayed on at bottom of buffer
  time_t statusmsg_time;  // how long ago status message was written
  struct editorSyntax *syntax;  // the syntax rules that apply to the buffer
  struct termios orig_termios;  // the terminal state taken at startup; used to restore on exit
  int mode;
};

struct editorConfig E;  // the global state

char *C_HL_extensions[] = { ".c", ".h", ".cpp", NULL };
char *C_HL_keywords[] = {
  "switch", "if", "while", "for", "break", "continue", "return", "else",
  "struct", "union", "typedef", "static", "enum", "class", "case",
  "int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|",
  "void|", NULL
};

struct editorSyntax HLDB[] = {
                              {"c",
                               C_HL_extensions,
                               C_HL_keywords,
                               "//", "/*", "*/",
                               HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
                              },
};

#define HLDB_ENTRIES (sizeof(HLDB) / sizeof(HLDB[0]))

void die(const char *s) {
  write(STDOUT_FILENO, "\x1b[2J", 4);  // clear screen
  write(STDOUT_FILENO, "\x1b[H", 3);  // reposition cursor
  perror(s);
  exit(1);
}

void message(const char *fmt, ...);
void editorRefreshScreen();
char *editorPrompt(char *prompt, void (*callback)(char *, int));

struct abuf {
  char *b;
  int len;
};

#define ABUF_INIT {NULL, 0}  // Represents an empty buffer

void abAppend(struct abuf *ab, const char *s, int len) {
  // Get a block of memory that is the size of the current string, plus the
  // string we're appending.
  char *new = realloc(ab->b, ab->len + len);

  if (new == NULL) return;
  memcpy(&new[ab->len], s, len);  // copy "s" after the current data
  ab->b = new;
  ab->len += len;
}

void abFree(struct abuf *ab) {
  free(ab->b);
}

int editorReadKey() {
  int nread;
  char c;
  // read() returns the number of bytes read
  while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
    if (nread == -1 && errno != EAGAIN) die("read");
  }

  if (c == '\x1b') {
    char seq[3];
    if (read(STDIN_FILENO, &seq[0], 1) != 1) return '\x1b';
    if (read(STDIN_FILENO, &seq[1], 1) != 1) return '\x1b';
    if (seq[0] == '[') {

      // Page up / down, which are represented by \x1b[5~ and \x1b[6~
      if (seq[1] >= '0' && seq[1] <= '9') {
        if (read(STDIN_FILENO, &seq[2], 1) != 1) return '\x1b';
        if (seq[2] == '~') {
          switch (seq[1]) {
          case '1': return HOME_KEY;
          case '3': return DEL_KEY;
          case '4': return END_KEY;
          case '5': return PAGE_UP;
          case '6': return PAGE_DOWN;
          case '7': return HOME_KEY;
          case '8': return END_KEY;
          }
        }
      } else {

        // Arrows
        switch (seq[1]) {
        case 'A': return ARROW_UP;
        case 'B': return ARROW_DOWN;
        case 'C': return ARROW_RIGHT;
        case 'D': return ARROW_LEFT;
        case 'H': return HOME_KEY;
        case 'F': return END_KEY;
        }
      }
    } else if (seq[0] == '0') {
      switch (seq[1]) {
      case 'H': return HOME_KEY;
      case 'F': return END_KEY;
      }
    }
    return '\x1b';
  } else {
    return c;
  }
}

int getCursorPosition (int *rows, int *cols) {
  char buf[32];
  unsigned int i = 0;
  // 6n (in the line below) asks for the cursor position. 6 is a function that
  // queries for terminal status info.
  if (write(STDOUT_FILENO, "\x1b[6n", 4) != 4) return -1;
  while (i < sizeof(buf) -1){
    if (read(STDIN_FILENO, &buf[i], 1) != 1) break;
    if (buf[i] == 'R') break;
    i++;
  }
  buf[i] = '\0';  // printf expects strings to end with a 0 byte

  if (buf[0] != '\x1b' || buf[1] != '[') return -1;

  // sscanf will parse out two integers ("%d;%d") and put them into rows/cols.
  if (sscanf(&buf[2], "%d;%d", rows, cols) != 2) return -1;

  printf("\r\n&buf[1]: '%s'\r\n", &buf[1]);
  editorReadKey();
  return -1;
}

int getWindowSize(int *rows, int *cols) {
  struct winsize ws;
  if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
  // ~C~ is cursor forward, and ~B~ is cursor down. We assume that 999 is a large
  // enough value to position to the bottom right.
    if (write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12) return -1;
    return getCursorPosition(rows, cols);
  } else {
    *cols = ws.ws_col;
    *rows = ws.ws_row;
    return 0;
  }
}

struct termios orig_termios;

void disableRawMode() {
  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) == -1) die("tcsetattr");
}

void enableRawMode() {
  if (tcgetattr(STDIN_FILENO, &E.orig_termios) == -1) die("tcgetatr");
  atexit(disableRawMode);

  struct termios raw = E.orig_termios;
  raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
  raw.c_oflag &= ~(OPOST);
  raw.c_cflag |= ~(CS8);
  raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);

  raw.c_cc[VMIN] = 0;
  raw.c_cc[VTIME] = 1;  // 100ms
  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) die("tcsetattr");
}

int is_separator(int c) {
  return isspace(c) || c == '\0' || strchr(",.()+-/*=~%<>[];", c) != NULL;
}

void editorUpdateSyntax(erow *row) {
  // The hl array is the same size as the render array
  row->hl = realloc(row->hl, row->rsize);
  memset(row->hl, HL_NORMAL, row->rsize);

  if (E.syntax == NULL) return;

  char **keywords = E.syntax->keywords;

  char *scs = E.syntax->singleline_comment_start;
  char *mcs = E.syntax->multiline_comment_start;
  char *mce = E.syntax->multiline_comment_end;

  int scs_len = scs ? strlen(scs) : 0;
  int mcs_len = mcs ? strlen(mcs) : 0;
  int mce_len = mce ? strlen(mce) : 0;

  int prev_sep = 1; // beginning of line can be considered a separator
  int in_string = 0;  // we store the string char in here so we know when it closes
  int in_comment = (row->idx > 0 && E.row[row->idx - 1].hl_open_comment);

  int i = 0;
  while (i < row->size) {
    char c = row->render[i];
    unsigned char prev_hl = (i > 0) ? row->hl[i - 1] : HL_NORMAL;

    // single line comments
    if (scs_len && !in_string && !in_comment) {
      if (!strncmp(&row->render[i], scs, scs_len)) {
          memset(&row->hl[i], HL_COMMENT, row->rsize - i);
          break;
      }
    }

    // multiline comments
    if (mcs_len && mce_len && !in_string){
      if (in_comment) {
        row->hl[i] = HL_MLCOMMENT; // highlight
        if (!strncmp(&row->render[i], mce, mce_len)) { // match end?
          memset(&row->hl[i], HL_MLCOMMENT, mce_len);  // highlight end token
          i += mce_len;
          in_comment = 0;
          prev_sep = 1;
          continue;
        } else {
          i++;
          continue;
        }
      } else if (!strncmp(&row->render[i], mcs, mcs_len)) { // match multiline start?
        memset(&row->hl[i], HL_MLCOMMENT, mcs_len);  // highlight the start token
        i += mcs_len;
        in_comment = 1;
        continue;
      }
    }

    if (E.syntax->flags & HL_HIGHLIGHT_STRINGS) {
      if (in_string) {
        row->hl[i] = HL_STRING;
        // backslashes should keep this as a string
        if (c == '\\' && i + 1 < row->rsize) {
          row->hl[i+1] = HL_STRING;
          i += 2;
          continue;
        }

        if (c == in_string) in_string = 0;  // this is the closing quote
        i ++;
        prev_sep = 1;
        continue;
      } else {
        if (c == '"' || c == '\''){
          in_string = c;
          row->hl[i] = HL_STRING;
          i++;
          continue;
        }
      }
    }

    if (E.syntax->flags & HL_HIGHLIGHT_NUMBERS) {
      if ((isdigit(c) && (prev_sep || prev_hl == HL_NUMBER)) ||
          (c == '.' && prev_hl == HL_NUMBER)) {  // support if number is a decimal
        row->hl[i] = HL_NUMBER;
        i ++;
        prev_sep = 0;  // it wasn't a separator because we know it was number
        continue;
      }
    }

    if (prev_sep) {
      int j;
      for (j = 0; keywords[j]; j++) {
        int klen = strlen(keywords[j]);
        int kw2 = keywords[j][klen - 1] == '|';
        if (kw2) klen--;

        if (!strncmp(&row->render[i], keywords[j], klen) &&
            is_separator(row->render[i + klen])) {
          memset(&row->hl[i], kw2 ? HL_KEYWORD2 : HL_KEYWORD1, klen);
          i += klen;
          break;
        }
      }
      if (keywords[j] != NULL) {
        prev_sep = 0;
        continue;
      }
    }

    prev_sep = is_separator(c);
    i++;
  }

  // set hl_open_comment appropriately
  int changed = (row->hl_open_comment != in_comment);
  row->hl_open_comment = in_comment;
  if (changed && row->idx + 1 < E.numrows)
    // Recursive iteration over the rest of the file as the highlighting may
    // have changed.
    editorUpdateSyntax(&E.row[row->idx + 1]);
}

int editorSyntaxToColor(int hl) {
  switch (hl) {
  case HL_COMMENT:
  case HL_MLCOMMENT: return 36;
  case HL_KEYWORD1: return 33;
  case HL_KEYWORD2: return 32;
  case HL_STRING: return 35;
  case HL_NUMBER: return 31;
  case HL_MATCH: return 34;
  default: return 37;
  }
}

void editorSelectSyntaxHighlight() {
  /*Sets E.syntax based on E.filename */
  E.syntax = NULL;
  if (E.filename == NULL) return;
  char *ext = strchr(E.filename, '.');
  for (unsigned int j = 0; j < HLDB_ENTRIES; j++) {
    struct editorSyntax *s = &HLDB[j];
    unsigned int i = 0;
    while (s->filematch[i]){
      int is_ext = (s->filematch[i][0] == '.');
      if ((is_ext && !strcmp(ext, s->filematch[i])) ||
          (!is_ext && strstr(E.filename, s->filematch[i]))) {
        E.syntax = s;

        int filerow;
        for (filerow = 0; filerow < E.numrows; filerow++) {
          editorUpdateSyntax(&E.row[filerow]);
        }

      }
      i++;
    }
  }
}

int editorRowCxToRx(erow *row, int cx) {
  int rx = 0;
  int j;
  for (j=0; j<cx; j++) {
    if (row->chars[j] == '\t')
      rx += (KILO_TAB_STOP - 1) - (rx % KILO_TAB_STOP);
    rx++;
  }
  return rx;
}

int editorRowRxToCx(erow *row, int rx) {
  // For a given row, converts the given rx value to the corresponding cx
  int cur_rx = 0;
  int cx;
  for (cx = 0; cx < row->size; cx++) {
    if (row->chars[cx] == '\t')
      cur_rx += (KILO_TAB_STOP - 1) - (cur_rx % KILO_TAB_STOP);
    cur_rx++;
    if (cur_rx > rx) return cx;
  }
  return cx;
}

void editorUpdateRow(erow *row) {
  int tabs = 0;
  int j;
  for (j = 0; j < row->size; j++) {
    if (row->chars[j] == '\t') tabs++;
  }

  free(row->render);
  row->render = malloc(row->size + tabs*(KILO_TAB_STOP - 1) + 1);

  int idx =0;
  for (j = 0; j < row->size; j++) {
    if (row->chars[j] == '\t') {
      // insert spaces until the next % 8 is hit.
      row->render[idx++] = ' ';
      while (idx % KILO_TAB_STOP != 0) row->render[idx++] = ' ';
    } else {
      // Print the character
      row->render[idx++] = row->chars[j];
    }
  }
  row->render[idx] = '\0';
  row->rsize = idx; // idx contains the number of characters we copied into row->render

  editorUpdateSyntax(row);
}

void editorInsertRow(int at, char *s, size_t len) {
  if (at < 0 || at > E.numrows) return;

  E.row = realloc(E.row, sizeof(erow) * (E.numrows + 1));
  memmove(&E.row[at + 1], &E.row[at], sizeof(erow) * (E.numrows - at));
  for (int j = at + 1; j <= E.numrows; j++) E.row[j].idx++;

  E.row[at].idx = at;

  E.row[at].size = len;
  E.row[at].chars = malloc(len + 1);
  memcpy(E.row[at].chars, s, len);
  E.row[at].chars[len] = '\0';

  E.row[at].rsize = 0;
  E.row[at].render = NULL;
  E.row[at].hl = NULL;
  E.row[at].hl_open_comment = 0;
  editorUpdateRow(&E.row[at]);

  E.numrows++;
  E.dirty++;
}

void editorFreeRow(erow *row) {
  free(row->render);
  free(row->chars);
  free(row->hl);
}

void editorDelRow(int at) {
  if (at < 0 || at >= E.numrows) return;
  editorFreeRow(&E.row[at]);
  memmove(&E.row[at], &E.row[at + 1], sizeof(erow) * (E.numrows - at - 1));
  for (int j = at; j < E.numrows - 1; j++) E.row[j].idx--;
  E.numrows--;
  E.dirty++;
}

void editorRowInsertChar(erow *row, int at, int c) {
  if (at < 0 || at > row->size) at = row->size; // bounds
  row->chars = realloc(row->chars, row->size + 2); // the new character + null byte
  // shift later chars along
  memmove(&row->chars[at + 1], &row->chars[at], row->size - at + 1);
  row->size++;
  row->chars[at] = c;
  editorUpdateRow(row);
  E.dirty++;
}

void editorRowAppendString(erow *row, char *s, size_t len) {
  row->chars = realloc(row->chars, row->size + len + 1);
  memcpy(&row->chars[row->size], s, len);
  row->size += len;
  row->chars[row->size] = '\0';
  editorUpdateRow(row);
  E.dirty++;
}

void editorRowDelChar(erow *row, int at) {
  if (at < 0 || at >= row->size) return;
  memmove(&row->chars[at], &row->chars[at + 1], row->size - at);
  row->size--;
  editorUpdateRow(row);
  E.dirty++;
}

void editorJoinLines() {
  if (E.cy == E.numrows - 1)
    return;
  erow *row = &E.row[E.cy];
  erow *rowBelow = &E.row[E.cy + 1];
  editorRowAppendString(row, " ", 1);
  editorRowAppendString(row, rowBelow->chars, rowBelow->size);
  editorDelRow(E.cy + 1);
}

void editorInsertChar(int c){
  if (isPrefix(c)) return;  // Don't insert control chars
  if (E.cy == E.numrows) { // the cursor is on the tilde after the last line
    editorInsertRow(E.numrows, "", 0);
  }
  editorRowInsertChar(&E.row[E.cy], E.cx, c);
  E.cx++;
}

void editorInsertNewline() {
  if (E.cx == 0) {
    editorInsertRow(E.cy, "", 0);
  } else {
    erow *row = &E.row[E.cy];
    editorInsertRow(E.cy + 1, &row->chars[E.cx], row->size - E.cx);
    row = &E.row[E.cy];
    row->size = E.cx;
    row->chars[row->size] = '\0';
    editorUpdateRow(row);
  }
  E.cy++;
  E.cx=0;
}

void editorDelChar() {
  if (E.cy == E.numrows) return;
  if (E.cx == 0 && E.cy == 0) return;

  erow *row = &E.row[E.cy];
  if (E.cx > 0) {
    editorRowDelChar(row, E.cx -1);
    E.cx--;
  } else {
    E.cx = E.row[E.cy - 1].size;
    editorRowAppendString(&E.row[E.cy - 1], row->chars, row->size);
    editorDelRow(E.cy);
    E.cy--;
  }
}

char *editorRowsToString(int *buflen) {
  int totlen = 0;
  int j;
  for (j=0; j < E.numrows; j++)
    totlen += E.row[j].size + 1; // + 1 for newline
  *buflen = totlen; // so the caller can inspect how long the string is

  char *buf = malloc(totlen);
  char *p = buf;
  for (j=0; j<E.numrows; j++) {
    memcpy(p, E.row[j].chars, E.row[j].size);
    p += E.row[j].size;
    *p = '\n';
    p++;
  }

  return buf;
}

void editorOpen(char *filename) {
  free(E.filename);
  E.filename = strdup(filename); // copies the given string to new memory loc.

  editorSelectSyntaxHighlight();

  FILE *fp = fopen(filename, "r");
  if (!fp) die("fopen");

  char *line = NULL;
  size_t linecap = 0;
  ssize_t linelen;
  while ((linelen = getline(&line, &linecap, fp)) != -1) { // iterate over lines
    while (linelen > 0 && (line[linelen -1] == '\n' || line[linelen -1] == '\r'))
      linelen--;
    editorInsertRow(E.numrows, line, linelen);
  }
  free(line);
  fclose(fp);
  E.dirty = 0;
}

void editorSave() {
  if (E.filename == NULL) {
    E.filename = editorPrompt("Save as: %s (ESC to cancel)", NULL);
    if (E.filename == NULL) {
      message("Save aborted");
      return;
    }
    editorSelectSyntaxHighlight();
  }

  int len;
  char *buf = editorRowsToString(&len);

  int fd = open(E.filename, O_RDWR | O_CREAT, 0644);
  if (fd != -1) {
    if (ftruncate(fd, len) != -1) {
      if (write(fd, buf, len) == len) {
        close(fd);
        free(buf);
        E.dirty = 0;
        message("%d bytes written to disk", len);
        return;
      }
    }
    close(fd);
  }
  free(buf);
  message("Can't save! I/O error: %s", strerror(errno));
}

void editorFindCallback(char *query, int key) {
  static int last_match = -1;
  static int direction = 1;

  static int saved_hl_line;
  static char *saved_hl = NULL;

  if (saved_hl) {
    memcpy(E.row[saved_hl_line].hl, saved_hl, E.row[saved_hl_line].rsize);
    free(saved_hl);
    saved_hl = NULL;
  }

  if (key == '\r' || key == '\x1b') {
    last_match = -1;
    direction = 1;
    return;
  } else if (key == ARROW_RIGHT || key == ARROW_DOWN) {
    direction = 1;
  } else if (key == ARROW_LEFT || key == ARROW_UP) {
    direction = -1;
  } else {
    last_match = -1;
    direction = 1;
  }

  if (last_match == -1) direction = 1;
  int current = last_match;
  int i;
  for (i = 0; i < E.numrows; i++) {
    current += direction;

    // loops around the file
    if (current == -1) current = E.numrows - 1;
    else if (current == E.numrows) current = 0;

    erow *row = &E.row[current];
    char *match = strstr(row->render, query);
    if (match) {
      last_match = current;
      E.cy = current;
      E.cx = editorRowRxToCx(row, match - row->render);
      E.rowoff = E.numrows;

      saved_hl_line = current;
      saved_hl = malloc(row->rsize);
      memcpy(saved_hl, row->hl, row->rsize);
      memset(&row->hl[match - row->render], HL_MATCH, strlen(query));
      break;
    }
  }
}

void editorFind(){
  int saved_cx = E.cx;
  int saved_cy = E.cy;
  int saved_coloff = E.coloff;
  int saved_rowoff = E.rowoff;

  char *query = editorPrompt("Search: %s (ESC/Arrows/Enter)", editorFindCallback);
  if (query) {
    free(query);
  } else { // NULL query means they pressed ESC.
    E.cx = saved_cx;
    E.cy = saved_cy;
    E.coloff = saved_coloff;
    E.rowoff = saved_rowoff;
  }
}


void editorQuit() {
  write(STDOUT_FILENO, "\x1b[2J", 4);  // clear screen
  write(STDOUT_FILENO, "\x1b[H", 3);  // reposition cursor
  exit(0);
}


void editorColon(){
  char *query = editorPrompt(":%s", NULL);
  if (query) {
    if (strcmp(query, "q!") == 0) {
      editorQuit();
    } else if (strcmp(query, "wq") == 0) {
      editorSave();
      editorQuit();
    }
    free (query);
  }
}

void editorScroll() {
  E.rx = 0;
  if (E.cy < E.numrows) {
    E.rx = editorRowCxToRx(&E.row[E.cy], E.cx);
  }
  if (E.cy < E.rowoff) { // is the cursor above the visible window?
    E.rowoff = E.cy;
  }
  if (E.cy >= E.rowoff + E.screenrows) {
    E.rowoff = E.cy - E.screenrows + 1;
  }
  if (E.rx < E.coloff) {
    E.coloff = E.rx;
  }
  if (E.rx >= E.coloff + E.screencols) {
    E.coloff = E.rx - E.screencols + 1;
  }
}

void editorDrawRows(struct abuf *ab) {
  int y;
  for (y = 0; y < E.screenrows; y++) {
    int filerow = y + E.rowoff;
    if (filerow >= E.numrows) {
      // Draw things that come after the rows
      if (E.numrows == 0 && y == E.screenrows / 3) {
        char welcome[80];
        int welcomelen = snprintf(welcome, sizeof(welcome),
                                  "Kilo editor -- version %s", KILO_VERSION);
        if (welcomelen > E.screencols) welcomelen = E.screencols;
        // Add spaces for padding to center the welcome message
        int padding = (E.screencols - welcomelen) / 2;
        if (padding) {
          abAppend(ab, "~", 1);
          padding--;
        }
        while (padding--) abAppend(ab, " ", 1);
        abAppend(ab, welcome, welcomelen);
      } else {
        abAppend(ab, "~", 1);
      }
    } else {
      // Draw the row
      int len = E.row[filerow].rsize - E.coloff;
      if (len < 0) len = 0;
      if (len > E.screencols) len = E.screencols;  // Truncate the len
      char *c = &E.row[filerow].render[E.coloff];
      unsigned char *hl = &E.row[filerow].hl[E.coloff];
      int j;
      int current_color = -1; // keep track of colour to keep number of resets down
      for (j=0; j<len; j++){
        // control characters
        if (iscntrl(c[j])) {
          char sym = (c[j] <= 26) ? '@' + c[j] : '?';
          abAppend(ab, "\x1b[7m", 4); // invert colours
          abAppend(ab, &sym, 1);
          abAppend(ab, "\x1b[m", 3);  // reset
          if (current_color != -1) {
            char buf[16];
            int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", current_color);
            abAppend(ab, buf, clen);
          }

        } else if (hl[j] == HL_NORMAL) {
          if (current_color != -1) {
            abAppend(ab, "\x1b[39m", 5);
            current_color = -1;
          }
          abAppend(ab, &c[j], 1);
        } else {
          int color = editorSyntaxToColor(hl[j]);
          if (color != current_color) {
            current_color = color;
            char buf[16];
            int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", color);
            abAppend(ab, buf, clen);
          }
          abAppend(ab, &c[j], 1);
        }
      }
      abAppend(ab, "\x1b[39m", 5); // reset at end of line
    }
    abAppend(ab, "\x1b[K", 3);  // clear the rest of the row before drawing
    abAppend(ab, "\r\n", 2);  // this means there's always an empty row at the
                              // bottom of the screen
  }
}

void editorDrawStatusBar(struct abuf *ab) {
  abAppend(ab, "\x1b[7m", 4);
  char status[80], rstatus[80], statusmode[4];

  switch (E.mode) {
  case MODE_NORMAL:
    strcpy(statusmode, "<N>");
    break;
  case MODE_INSERT:
    strcpy(statusmode, "<I>");
    break;
  default:
    strcpy(statusmode, "???");
  }

  int len = snprintf(status, sizeof(status), "%s | %.20s - %d lines %s",
                     statusmode,
                     E.filename ? E.filename : "[No Name]", E.numrows,
                     E.dirty ? "(modified)" : "");
  int rlen = snprintf(rstatus, sizeof(rstatus), "%s | %d/%d",
                      E.syntax ? E.syntax->filetype : "no ft", E.cy + 1, E.numrows);
  if (len > E.screencols) len = E.screencols; // bounds
  abAppend(ab, status, len);
  while (len < E.screencols) {
    if (E.screencols - len == rlen) { // The starting column index to start
                                      // printing rstatus
      abAppend(ab, rstatus, rlen);
      break;
    } else {
      abAppend(ab, " ", 1);
      len++;
    }
  }
  abAppend(ab, "\x1b[m", 3);
  abAppend(ab, "\r\n", 2);
}

void editorDrawMessageBar(struct abuf *ab) {
  abAppend(ab, "\x1b[K", 3);
  int msglen = strlen(E.statusmsg);
  if (msglen > E.screencols) msglen = E.screencols; // bounds
  if (msglen && time(NULL) - E.statusmsg_time < 5)
    abAppend(ab, E.statusmsg, msglen);
}

void editorRefreshScreen() {
  editorScroll();

  struct abuf ab = ABUF_INIT;
  abAppend(&ab, "\x1b[?25l", 6);  // hide cursor
  abAppend(&ab, "\x1b[H", 3);  // reposition cursor
  editorDrawRows(&ab);
  editorDrawStatusBar(&ab);
  editorDrawMessageBar(&ab);

  // Move the cursor
  char buf[32];
  // The ~[H~ escape sequence moves the cursor to the position given by the
  // coordinates. The +1 is to convert because the terminal uses 1-indexed values.
  snprintf(buf, sizeof(buf), "\x1b[%d;%dH", (E.cy - E.rowoff) + 1, (E.rx - E.coloff) + 1);
  abAppend(&ab, buf, strlen(buf));

  abAppend(&ab, "\x1b[?25h", 6);  // show cursor
  write(STDOUT_FILENO, ab.b, ab.len);
  abFree(&ab);
}

void message(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap);
  va_end(ap);
  E.statusmsg_time = time(NULL);
}

char *editorPrompt(char *prompt, void (*callback)(char *, int)) {
  size_t bufsize = 128;
  char *buf = malloc(bufsize);

  size_t buflen = 0;
  buf[0] = '\0';

  while (1) {
    message(prompt, buf);
    editorRefreshScreen();

    int c = editorReadKey();
    if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE) {
      if (buflen !=0) buf[--buflen] = '\0';
    } else if (c == '\x1b') {
      message("");
      if (callback) callback(buf, c);
      free(buf);
      return NULL;
    } else if (c == '\r') {
      if (buflen != 0) {
        // clear status message, return the user input
        message("");
        if (callback) callback(buf, c);
        return buf;
      }
    } else if (!iscntrl(c) && c < 128) {
      if (buflen == bufsize - 1) {
        bufsize *= 2; // dynamically increase memory as user input grows
        buf = realloc(buf, bufsize);
      }
      buf[buflen++] = c;
      buf[buflen] = '\0';
    }
    if (callback) callback(buf, c);
  }
}

void editorMoveCursor(int key) {
  erow *row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy]; // get current row

  switch (key) {
  case 'h':
  case CTRL_KEY('b'):
  case ARROW_LEFT:
    if (E.cx != 0) {
      E.cx--;
    } else if (E.cy > 0) {
        // Move to the row above
        E.cy--;
        E.cx = E.row[E.cy].size;
    }
    break;
  case 'l':
  case CTRL_KEY('f'):
  case ARROW_RIGHT:
    if (row && E.cx < row->size) { // limit horizontal scrolling by column width
      E.cx++;
    } else if (row && E.cx == row->size) {
      // Move to the row below
      E.cy++;
      E.cx = 0;
    }
    break;
  case 'k':
  case CTRL_KEY('p'):
  case ARROW_UP:
    if (E.cy != 0) {
      E.cy--;
    }
    break;
  case 'j':
  case CTRL_KEY('n'):
  case ARROW_DOWN:
    if (E.cy != E.numrows - 1) {  // Allow advancing past the screen, but not the file.
      E.cy++;
    }
    break;
  }

  // Limit the cursor to the end of the row. Fixes the case where
  // different rows have different widths and you move to the row above/below.
  row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];
  int rowlen = row ? row->size : 0;
  if (E.cx > rowlen) {
    E.cx = rowlen;
  }

}


enum charType {
               CHAR_ALPHANUM = 1,
               CHAR_SYMBOL,
               CHAR_SPACE,
};


int getCharType(char c){
  if (isspace(c)) {
    return CHAR_SPACE;
  } else if (is_separator(c)) {
    return CHAR_SYMBOL;
  }
  return CHAR_ALPHANUM;
}


/* Vim-like word movement. In vim it seems to move the cursor using any
   transition from word/symbol/whitespace.*/
void editorMoveCursorWordForward() {
  erow *row = &E.row[E.cy];
  int cursor = -1;
  int previous_cursor = -1;
  while (E.cy < E.numrows) {
    if (E.cx >= row->size) {  // move down a row
      if (E.cy == E.numrows - 1) { // if this is last row keep in bounds
        E.cx = row->size;
        return;
      }
      E.cy ++;
      row = &E.row[E.cy];
      E.cx = 0;
    }
    cursor = getCharType(row->chars[E.cx]);
    if ((cursor != CHAR_SPACE) && (previous_cursor > 0) &&
        ((previous_cursor != cursor) || E.cx == 0)) {
      // The E.cx == 0 check above will mean that we're on a new word in a
      // different row.
      break;
    }
    E.cx ++;
    previous_cursor = cursor;
  }
}

/* Vim-like word movement.
   If the cursor is at the start of a word, go to the start of the preceding
   word. Otherwise, go to the start of _this_ word.

   TODO: these could be rewritten to not mutate cx/cy, but instead to return the
         cursor position for the start of the word.
 */
void editorMoveCursorWordBackward() {
  erow *row = &E.row[E.cy];
  erow *previous_row;
  int cursor_type = -1;
  int lookbehind_type = -1;
  int num_type_changes = 0;
  int was_on_first_letter = -1;
  while (E.cy >=0) {
    row = &E.row[E.cy];
    cursor_type = getCharType(row->chars[E.cx]);
    if (cursor_type != CHAR_SPACE) {

      // lookup the lookbehind
      if (E.cx >= 1) {
        lookbehind_type = getCharType(row->chars[E.cx - 1]);
      } else if (E.cx == 0) {
        lookbehind_type = CHAR_SPACE;
      }

      // set was_on_first_letter
      if (was_on_first_letter == -1 && lookbehind_type >= 0) {
        if (cursor_type == lookbehind_type) {
          was_on_first_letter = 0;
        } else {
          was_on_first_letter = 1;
        }
      }

      // set num changes
      if (cursor_type != lookbehind_type)
        num_type_changes ++;

      // check if should break. If we were on the first letter of an existing
      // word/symbol then we want to move to the start of the NEXT word,
      // otherwise this one.
      if (was_on_first_letter == 0 && num_type_changes == 1)  {
        break;
      } else if (was_on_first_letter == 1 && num_type_changes == 2) {
        break;
      }
    }

    // Move cursor backwards
    // if E.cx < 0, then set it to end of the above row.
    E.cx --;
    if (E.cx < 0) {
      if (E.cy > 0) {
        E.cy --;
        previous_row = &E.row[E.cy];
        E.cx = previous_row->size - 1;
      } else {
        E.cx = 0;
      }
    }
  }
}


void editorProcessKeypressNormalMode() {
  int c = editorReadKey();
  switch (c) {
  case 'i':
    E.mode = MODE_INSERT;
    break;
  case 'a':
    E.cx ++; // TODO: bounds check
    E.mode = MODE_INSERT;
    break;
  case 'A':
    if (E.cy < E.numrows)
      E.cx = E.row[E.cy].size;  // move to end of the line
    E.mode = MODE_INSERT;
    break;
  case 'I':
    E.cx = 0;
    E.mode = MODE_INSERT;
    break;
  case 'o':
    E.cx = E.row[E.cy].size;  // move to end of the line
    editorInsertNewline();
    E.mode = MODE_INSERT;
    break;
  case ':':
    editorColon();
    break;
  case 'k':
  case 'j':
  case 'h':
  case 'l':
    editorMoveCursor(c);
    break;
  case 'w':
    editorMoveCursorWordForward();
    break;
  case 'b':
    editorMoveCursorWordBackward();
    break;
    break;
  case 'J':
    editorJoinLines();
    break;
  case 'x':
    editorMoveCursor(ARROW_RIGHT);
    editorDelChar();
    break;
  case '$':
    if (E.cy < E.numrows)
      E.cx = E.row[E.cy].size;  // move to end of the line
    break;
  case '^':
    E.cx = 0;
    break;
  case '/':
    editorFind();
    break;
  case 'W':
    editorSave();
    break;
  }
}

void editorProcessKeypressInsertMode() {
  static int previous_key = -1;
  int c = editorReadKey();
  switch (c) {
  case '\x1b':
    E.mode = MODE_NORMAL;
    break;
  case '\r':
    editorInsertNewline();
    break;
  case CTRL_KEY('c'):
    if (previous_key == CTRL_KEY('x')) {
      editorQuit();
    }
    break;
  case CTRL_KEY('s'):
    if (previous_key == CTRL_KEY('x')) {
      editorSave();
      break;
    }
    editorFind();
    break;
  case CTRL_KEY('a'):
  case HOME_KEY:
    E.cx = 0;
    break;
  case CTRL_KEY('e'):
  case END_KEY:
    if (E.cy < E.numrows)
      E.cx = E.row[E.cy].size;  // move to end of the line
    break;
  case BACKSPACE:
  case CTRL_KEY('h'): // legacy - C-h produces "8", which used to represent backspace
  case DEL_KEY:
    if (c == DEL_KEY) editorMoveCursor(ARROW_RIGHT);
    editorDelChar();
    break;
  case PAGE_UP:
  case PAGE_DOWN:
    {

      // Set cursor y position to simulate scrolling the page
      if (c == PAGE_UP) {
        E.cy = E.rowoff;
      } else if (c == PAGE_DOWN) {
        E.cy = E.rowoff + E.screenrows - 1;
        if (E.cy > E.numrows) E.cy = E.numrows; // cap to end of file
      }

      // move the cursor
      int times = E.screenrows;
      while (times--)
        editorMoveCursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
    }
    break;
  case CTRL_KEY('f'):
  case CTRL_KEY('b'):
  case CTRL_KEY('n'):
  case CTRL_KEY('p'):
  case ARROW_UP:
  case ARROW_DOWN:
  case ARROW_LEFT:
  case ARROW_RIGHT:
    editorMoveCursor(c);
    break;

  // C-l traditionally refreshes the screen. don't do anything as we refresh by
  // default after each keypress.
  case CTRL_KEY('l'):
    break;

  default:
    editorInsertChar(c);
    break;
  }

  previous_key = c;
}

void initEditor () {
  E.cx = 0;  // horizontal cursor
  E.cy = 0;  // vertical cursor
  E.rx = 0;  // cursor index
  E.rowoff = 0;
  E.coloff = 0;
  E.numrows = 0;
  E.row = NULL;
  E.dirty = 0;
  E.filename = NULL;
  E.statusmsg[0] = '\0';
  E.statusmsg_time = 0;
  E.syntax = NULL;
  E.mode = MODE_NORMAL;
  if (getWindowSize(&E.screenrows, &E.screencols) == -1) die("getWindowSize");
  E.screenrows -= 2;  // For the status bar and message bar
}

int main(int argc, char *argv[]) {
  enableRawMode();
  initEditor();

  if (argc >= 2) {
    editorOpen(argv[1]);
  }

  while (1) {
    editorRefreshScreen();

    switch (E.mode) {
    case MODE_NORMAL:
      editorProcessKeypressNormalMode();
      break;
    case MODE_INSERT:
      editorProcessKeypressInsertMode();
      break;
    }
  }
  return 0;
}
