* Kilo

This is my implementation of the Kilo text editor, written by following [[https://viewsourcecode.org/snaptoken/kilo/index.html][Build
your own text editor]].

It was initially written as an org-mode file, as an exercise for me to learn a
bit more about writing terminal applications with C, and to see whether the
literate programming approach with org-mode is useful.

Overall I think embedding the code in this file actually made it harder to keep
the overall structure in my head as I went, because I was only operating on
individuals parts at a time. Next time I will just write notes separately.

I've now renamed the org-mode version to ~kilo-org.c~. For future edits I'll work
on ~kilo.c~ directly.

* New features

I've extended ~kilo.c~ with a few things that I'm used to from vim/emacs:

- Splitting user input into ~normal~ and ~insert~ modes.
- Word-based cursor movement that is normally found with ~w/W/b/B~
- A new prompt to simulate ~:wq~ and ~:q!~.
- Standard cursor movement with ~hjkl~, ~^/$~, ~C-f/C-b~, ~gg~ and ~G~.
- Using ~dd~ to remove lines, and ~J~ to join lines.
- Adding the ~jj~ and ~jk~ bindings that I use in ~insert~ mode to exit to ~normal~ mode
  (which means waiting for a follow-up key to ~j~, and inserting it into the row
  if it doesn't come after a set timeout).

* Compile with org-mode

This just concatenates all the C snippets to ~kilo.c~, and then runs ~make~.

#+begin_src emacs-lisp :results silent
  (interactive)
  (setq-local org-confirm-babel-evaluate nil)
  (org-babel-tangle nil "kilo-org.c" "c")
  (compile "make")
#+end_src

* Code
** Feature test macros

There are various macros that you can define that control what features are
available to the compiler. There is more info in the [[https://www.gnu.org/software/libc/manual/html_node/Feature-Test-Macros.html][GNU libc
documentation]]. Some are added in step 59, to remove a warning about implicit
declaration of ~getline()~.

#+begin_src c :results silent
# define _DEFAULT_SOURCE
# define _BSD_SOURCE
# define _GNU_SOURCE
#+end_src

** Includes

#+begin_src c
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
#+end_src

** Constants

#+begin_src c
  #define KILO_VERSION "0.0.1"
  #define KILO_TAB_STOP 4
  #define KILO_QUIT_TIMES 2
#+end_src

Some of these macros (like ~CTRL_KEY~ below) take a parameter, similar to
functions. The main advantage of doing this is that the preprocessor replaces
the template so there's no stack or function call needed. There are downsides
too: if you have a lot of macros it can increase the binary size, and they're
limited because they're not functions - you can't return a parameter, you can't
do recursion, etc.

In ASCII, the CTRL character strips bits 5 and 6 from whatever key you
press. For example, ~h~ is 01101000, and ~C-h~ is 00001000. We define this below:

#+begin_src c
  #define CTRL_KEY(k) ((k) & 0x1F)
#+end_src

#+begin_src c
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
#+end_src

#+begin_src c
  #define HL_HIGHLIGHT_NUMBERS (1<<0)
  #define HL_HIGHLIGHT_STRINGS (1<<1)
#+end_src

** State

The global editor state is stored in ~editorConfig~. This stores data like the
cursor position, screen offset, size of the terminal, whether the buffer has
been modified, the associated filename, etc. It also contains some setup and
teardown data (like the properties of the user's terminal),

~erow~ represents a single line of text. User input results in a lot of mutation
of ~editorConfig~, particularly the rows.

~editorSyntax~ OTOH just contains information associated with a particular
filetype, and is not affected by user input. The buffer can be associated with a
single ~editorSyntax~ struct.

#+begin_src c
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
    int size;  // the row length, excluding the null byte at the end.
    char *chars;  // the characters in the line
    int rsize; // the length of the "rendered" line, where eg. \t will expand to n spaces
    char *render;  // the "rendered" characters in the line
    unsigned char *hl;  // the highlight property of a character
    int hl_open_comment;  // whether this line begins or is part of a multiline comment
  } erow;

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
  };

  struct editorConfig E;  // the global state
#+end_src

** Filetypes

The tutorial specifies an entry for C:

#+begin_src c
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
#+end_src

** Exiting

Most C library functions that fail set the global ~errno~. ~perror()~ looks at this
and prints a descriptive message for it - for example, "inappropriate ioctl for
device".

#+begin_src c
  void die(const char *s) {
    write(STDOUT_FILENO, "\x1b[2J", 4);  // clear screen
    write(STDOUT_FILENO, "\x1b[H", 3);  // reposition cursor
    perror(s);
    exit(1);
  }
#+end_src

** Prototypes

C compiles in a single pass, so you can't always call functions that aren't
defined yet. We can define the signature though. These are the few functions
that are required:

#+begin_src c
  void editorSetStatusMessage(const char *fmt, ...);
  void editorRefreshScreen();
  char *editorPrompt(char *prompt, void (*callback)(char *, int));
#+end_src

** Append buffer

Rather than calling ~write()~ regularly to modify the terminal output, we instead
buffer everything in ~abuf~, and only write to the terminal once our update is
complete. This reduces the number of updates, can prevent screen flickering,
etc.

#+begin_src c
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
#+end_src

** Terminal

There are a few functions here that just get information from the
terminal. ~editorReadKey()~ translates ANSI codes into an ~editorKey()~ enum:

#+begin_src c
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
#+end_src

Control characters are prefixed by ESC. If we read ESC, immediately read two
more bytes into ~seq~. If the reads timeout, then assume the user just pressed
escape.

~getCursorPosition~ below doesn't really need to exist for me. It is only used in
~getWindowSize~ if ~TIOCGWINSZ~ isn't supported by the terminal.

#+begin_src c
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
#+end_src

#+begin_src c
  int getWindowSize(int *rows, int *cols) {
    struct winsize ws;
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
    // ~C~ is cursor forward, and ~B~ is cursor down. We assume that 999 is a large
    // enough value to position to the bottom right.
      if (write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12) return -1;
      return getCursorPosition(rows, cols);
    } else {
      ,*cols = ws.ws_col;
      ,*rows = ws.ws_row;
      return 0;
    }
  }
#+end_src

TIOCGWINSZ tells the terminal to return the window size. We check for 0 in the
column value because "apparently" that's a possible outcome.

*** Raw mode

#+begin_src c
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
#+end_src

- TCSAFLUSH specifies when to apply the ~setattr~ change.

- ECHO is a bitflag - ~&= ~~(ECHO)~ flips the echo bit off
  (00000000000000000000000000001000). We also do this to the ICANON flag, which
  disables canonical mode, making us read one byte at a time rather than reading
  the whole line when enter is pressed.

  IEXTEN controls ~C-v~, and ISIG controls the ~C-c~ and ~C-z~ signals.

  IXON controls ~C-s~ and ~C-q~, and ICRNL controls a feature where ~\r~
  (character 13) is turned into a newline (character 10).

  OPOST controls some output processing. The main thing we want to disable here
  (and possibly the only thing enabled by default) is the output translation of
  ~\n~ into ~\r\n~. The terminal requires these as distinct characters to begin a
  new line.

- The CS8 line is not a flag, it's a bit mask with multiple bits. Here we set
  the character size (CS) to 8 bits per byte. This is often a default.

- ~c_lflag~ stores "local" flags, which is apparently a dumping ground for a few
  miscellaneous things. There are also ~iflag~ (input), ~oflag~ (output) and ~clfag~
  (control flags).

- ~c_cc~ stands for "control characters". VMIN sets the minimum number of bytes of
  input needed before ~read()~ can return - we use 0 so that ~read()~ will return as
  soon as there's any input to read. VTIME is the timeout value in 10ths of a
  second.
** Syntax highlighting

This is one of the bigger features. ~editorUpdateSyntax~ operates on a single row,
setting each column of the ~hl~ array according to that column's syntax
property. When following the steps, we initially only supported syntax state
within a single line. Afterwards the multi-line feature was added.

This implementation could easily get unwieldy if you wanted to add support for
more syntax features, because there's a lot of state to keep track of in the
main loop.

#+begin_src c
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
#+end_src

** Row operations

These functions operate on rows - eg. to insert a row in the buffer, or insert a
character into a row. They do /not/ operate on the cursor position or the file
offset.

Translation between Cx<->Rx below is quite simple because there is only one character
supported (tab). Having to hard-code every translation isn't ideal though.

#+begin_src c
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
#+end_src

#+begin_src c

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
#+end_src

** Editor operations

These are more user-focused operations that can perform row operations but also
managed the cursor at the same time. They do /not/ manage the file offset though.

#+begin_src c
  void editorInsertChar(int c){
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
#+end_src

** File I/O

#+begin_src c
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
      ,*p = '\n';
      p++;
    }

    return buf;
  }
#+end_src

#+begin_src c
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
        editorSetStatusMessage("Save aborted");
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
          editorSetStatusMessage("%d bytes written to disk", len);
          return;
        }
      }
      close(fd);
    }
    free(buf);
    editorSetStatusMessage("Can't save! I/O error: %s", strerror(errno));
  }
#+end_src

- ~getline()~ can be used to read lines from a file when we don't know how much
  memory to allocate for each line. It allocates memory for the next line it
  reads, and sets the second argument to point to that memory. You can then feed
  it the pointer back, to try to reuse the memory next time you use ~getline()~.

- We strip out the newline and CR before copying it into erow - we know that
  every erow represents a single line of text, so we don't need to actually
  store those characters at the end.

** Search

Search is implemented using the prompt. It loops through all the rows in the
file, uses ~strstr()~ to see if there is a substring match, and then if so scrolls
and moves the cursor to the row.

#+begin_src c
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
#+end_src
** Output

There are a few functions here that handle drawing the terminal output,
scrolling,  refreshing the screen, drawing the status bar, etc.

#+begin_src c
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
#+end_src

#+begin_src c
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
#+end_src

~filerow~ above represents the offset row, whereas ~y~ represents the absolute
row.

#+begin_src c
  void editorDrawStatusBar(struct abuf *ab) {
    abAppend(ab, "\x1b[7m", 4);
    char status[80], rstatus[80];
    int len = snprintf(status, sizeof(status), "%.20s - %d lines %s",
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
#+end_src

#+begin_src c
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
#+end_src

Below, the ~...~ takes a varying number of arguments. Between ~va_start()~ and
~va_end()~ you can use ~va_arg()~ to get the next argument. ~va_start()~ needs to know
the last argument before the variable arguments list starts, so it can know the
address of the next arguments. In our case we don't use ~va_arg()~, but instead
just pass ~ap~ to ~vsnprintf~, which can format the string with a varying number of
arguments.

#+begin_src c
  void editorSetStatusMessage(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap);
    va_end(ap);
    E.statusmsg_time = time(NULL);
  }
#+end_src

** Input

These are the main user input functions. ~editorPrompt~ is similar to the main
loop - it waits for user input and then runs a callback function on
RET. ~editorProcessKeypress~ is basically a big case statement that checks the key
enum and performs appropriate operations.

#+begin_src c
  char *editorPrompt(char *prompt, void (*callback)(char *, int)) {
    size_t bufsize = 128;
    char *buf = malloc(bufsize);

    size_t buflen = 0;
    buf[0] = '\0';

    while (1) {
      editorSetStatusMessage(prompt, buf);
      editorRefreshScreen();

      int c = editorReadKey();
      if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE) {
        if (buflen !=0) buf[--buflen] = '\0';
      } else if (c == '\x1b') {
        editorSetStatusMessage("");
        if (callback) callback(buf, c);
        free(buf);
        return NULL;
      } else if (c == '\r') {
        if (buflen != 0) {
          // clear status message, return the user input
          editorSetStatusMessage("");
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
    case ARROW_LEFT:
      if (E.cx != 0) {
        E.cx--;
      } else if (E.cy > 0) {
          // Move to the row above
          E.cy--;
          E.cx = E.row[E.cy].size;
      }
      break;
    case ARROW_RIGHT:
      if (row && E.cx < row->size) { // limit horizontal scrolling by column width
        E.cx++;
      } else if (row && E.cx == row->size) {
        // Move to the row below
        E.cy++;
        E.cx = 0;
      }
      break;
    case ARROW_UP:
      if (E.cy != 0) {
        E.cy--;
      }
      break;
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
#+end_src

#+begin_src c
  void editorProcessKeypress() {
    static int quit_times = KILO_QUIT_TIMES;

    int c = editorReadKey();
    switch (c) {
    case '\r':
      editorInsertNewline();
      break;
    case CTRL_KEY('q'):
      if (E.dirty && quit_times > 0){
        editorSetStatusMessage("Warning! File has unsaved changes. "
                               "Press C-q %d more times to quit.", quit_times);
        quit_times --;
        return;
      }
      write(STDOUT_FILENO, "\x1b[2J", 4);  // clear screen
      write(STDOUT_FILENO, "\x1b[H", 3);  // reposition cursor
      exit(0);
      break;
    case CTRL_KEY('s'):
      editorSave();
      break;
    case HOME_KEY:
      E.cx = 0;
      break;
    case END_KEY:
      if (E.cy < E.numrows)
        E.cx = E.row[E.cy].size;  // move to end of the line
      break;
    case CTRL_KEY('f'):
      editorFind();
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
    case ARROW_UP:
    case ARROW_DOWN:
    case ARROW_LEFT:
    case ARROW_RIGHT:
      editorMoveCursor(c);
      break;

    // C-l traditionally refreshes the screen. don't do anything as we refresh by
    // default after each keypress.
    case CTRL_KEY('l'):
    case '\x1b':
      break;

    default:
      editorInsertChar(c);
      break;
    }

    quit_times = KILO_QUIT_TIMES;  // reset to 3
  }
#+end_src
** Main

The entry point. ~initEditor()~ initialises all the fields in the E struct. ~main()~
handles arguments and enters the main loop.

#+begin_src c
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
    if (getWindowSize(&E.screenrows, &E.screencols) == -1) die("getWindowSize");
    E.screenrows -= 2;  // For the status bar and message bar
  }
#+end_src

#+begin_src c
  int main(int argc, char *argv[]) {
    enableRawMode();
    initEditor();

    if (argc >= 2) {
      editorOpen(argv[1]);
    }

    editorSetStatusMessage("HELP: C-Q: quit | C-S: save | C-f: find");

    while (1) {
      editorRefreshScreen();
      editorProcessKeypress();
    }
    return 0;
  }
#+end_src



* Log

Notes that I'm writing as I go.

** Raw mode

By default the terminal starts in canonical/cooked mode, which captures a lot of
user input rather than passing it straight to the program. Input is only sent to
the program when you hit enter, and various keys have special terminal
behaviour, like ~C-c~ and ~C-z~.

Interestingly you can "break" your terminal by running Step 5, which sets some
termios flags, and it has to be reset by the ~reset~ trick.

Step 15 disables various flags that nowadays are usually disabled by default
(but it's still good practice to disable them to enable "raw mode").

** C-s and C-q

~C-s~ stops data from being transmitted to the terminal, and ~C-q~ resumes it. I
haven't used these before. Then can be disabled with the IXON termios flag.

** EAGAIN

EAGAIN is returned by ~read()~ on timeout in Cygwin, instead of just
returning 0. I'm not using Cygwin so I suspect it's safe to remove that part.

** VT100 escape sequences

In an escape sequence like ~\x1b[2J~, ~J~ is the function and ~2~ is an argument to
it. I hadn't thought about this before - I think I had just treated "2J" as a
whole.

The ~m~ command controls text attributes like bold (~1~), underscore (~4~), blink (~5~)
and inverted colours (~7~).

~ncurses~ uses the ~terminfo~ database to figure out the capabilities of a terminal
and what the escape sequences for that terminal are. In our case we're just
hardcoding the VT100 sequences.

*** Home and End

Home and End can have multiple representations depending on the OS, which is why
they're added in multiple places in ~editorReadyKey()~ in step 52.

** Hide the cursor when drawing

This is standard practice - the cursor might jump around the screen if we're
writing to it. This can be controlled with ~?25h~ and ~?25l~, at least in later VT
models.

** Enums

If you set the first constant in an enum (as we do in step 48), then the
remaining constants are incremented automatically.

** Saving the file

A safer way to write the file would be to write it to a temporary file, ensure
it succeeds safely, and then rename it to the desired location. This is
mentioned in step 106.
** openemacs

There's a [[https://github.com/practicalswift/openemacs/blob/master/openemacs.c][fork of the project]] that implements some emacs-like features (eg. the
movement bindings).
