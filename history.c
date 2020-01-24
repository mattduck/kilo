#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "history.h"

history H;

editorConfig *copyEditorConfig(editorConfig *old) {
  editorConfig *new = malloc(sizeof(editorConfig));

  new->cx = old->cx;
  new->cy = old->cy;
  new->rx = old->rx;
  new->rowoff = old->rowoff;
  new->coloff = old->coloff;
  new->screenrows = old->screenrows;
  new->screencols = old->screencols;
  new->dirty = old->dirty;
  new->numrows = old->numrows;
  new->filename = old->filename;
  new->statusmsg_time = old->statusmsg_time;
  new->syntax = old->syntax;  // pointer but that's fine.
  new->orig_termios = old->orig_termios;
  new->mode = old->mode;
  new->statusmsg[0] = *old->statusmsg;

  // copy row
  new->row = malloc(sizeof(erow) * (old->numrows));
  int i;
  for (i = 0; i < new->numrows; i++){
    new->row[i].idx = old->row[i].idx;
    new->row[i].size = old->row[i].size;
    new->row[i].rsize = old->row[i].rsize;
    new->row[i].hl_open_comment = old->row[i].hl_open_comment;

    new->row[i].chars = malloc(old->row[i].size);
    memcpy(new->row[i].chars, old->row[i].chars, old->row[i].size);

    new->row[i].render = malloc(old->row[i].rsize);
    memcpy(new->row[i].render, old->row[i].render, old->row[i].rsize);

    new->row[i].hl = malloc(old->row[i].rsize);
    memcpy(new->row[i].hl, old->row[i].hl, old->row[i].rsize);
    /* memset(new->row[i].hl, 0, old->row[i].rsize); // HL_NORMAL */
  }
  return new;
}

int history_has_changed(struct editorConfig *a, struct editorConfig *b) {
  if (a->cx != b->cx)
    return 1;
  if (a->cy != b->cy)
    return 1;
  if (a->rx != b->rx)
    return 1;
  if (a->numrows != b->numrows)
    return 1;
  if (a->rowoff != b->rowoff)
    return 1;
  if (a->coloff != b->coloff)
    return 1;
  for (int i=0; i < a->numrows; i++) {
    if (!strcmp(a->row[i].render, b->row[i].render)){
      return 1;
    }
  }
  return 0;
}

struct editorConfig *history_push (struct editorConfig *snapshot) {
  editorConfig *new_e = copyEditorConfig(snapshot);
  snapshot->redo = new_e;
  new_e->undo = snapshot;
  return new_e;
}

struct editorConfig *history_undo (struct editorConfig *e) {
  if (e->undo) {
    if (history_has_changed(e, e->undo))
        return e->undo;
  };
  return e;
}

struct editorConfig *history_redo (struct editorConfig *e) {
  if (e->redo) {
    return e->redo;
  };
  return e;
}
