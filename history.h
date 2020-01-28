#ifndef UNDO_H
#define UNDO_H

#include "kilo.h"

typedef struct history {
  editorConfig *buffer[5];
  int size;
} history;

editorConfig *history_push(editorConfig *e);
editorConfig *history_undo(editorConfig *e);
editorConfig *history_redo(editorConfig *e);

#endif
