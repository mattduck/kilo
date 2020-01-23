/* Functions to manage cursor movement logic */

#include <ctype.h> // isspace

#include "kilo.h"
#include "point.h"

/* Advance point by one space */
point point_inc(point co, editorConfig e) {
  erow *row = &e.row[co.y];
  co.x ++;
  if (co.x >= row->size) { // last column
    if (co.y == e.numrows - 1) {  // last row in file
      co.x = row->size;
      return co;
    }
    co.y ++;
    co.x = 0;
  }
  return co;
}

/* Decrement point by one space */
point point_dec(point co, editorConfig e) {
  co.x --;
  if (co.x < 0) {
    if (co.y == 0) {
      co.x = 0;
      return co;
    }
    co.y --;
    co.x = e.row[co.y].size;
  }
  return co;
}


/* return the last point in the buffer */
point point_max(editorConfig e) {
  erow *row = &e.row[e.numrows - 1];
  point co = {row->idx, row->size -1};
  return co;
}

/* return the first point in the buffer */
point point_min() {
  point co = {0, 0};
  return co;
}


/* Equality functions */
int point_gt(point a, point b){
  if (a.y > b.y)
    return 1;
  if ((a.y == b.y) && (a.x == b.x))
    return 1;
  return 0;
}

int point_eq(point a, point b){
  return ((a.y == b.y) && (a.x == b.x));
}

int point_lt(point b, point a){
  return (!point_gt(b, a) && !point_eq(b, a));
}

int point_gte(point a, point b){
  return (point_gt(a, b) || point_eq(a, b));
}

int point_lte(point a, point b){
  return (point_lt(a, b) || point_eq(a, b));
}


/*
 Perform W and return the new point.

 TODO: if a row is empty it doesn't skips the first word on the next row.
*/
point point_W(editorConfig e) {
  point co = {e.cy, e.cx};
  point lookahead_co;
  erow *row = &e.row[co.y];
  int lookahead_is_space = -1;
  while (1) {
    row = &e.row[co.y];
    lookahead_co = point_inc(co, e);

    if (point_gte(lookahead_co, point_max(e)))
      return point_max(e);

    lookahead_is_space = isspace(e.row[lookahead_co.y].chars[lookahead_co.x]);
    if ((co.x == row->size -1) && (lookahead_is_space == 0)
        && (!isspace(row->chars[co.x]))) {
      return lookahead_co;
    } else {  // If transitioning out of a space, return the lookahead
      if (isspace(row->chars[co.x]) && lookahead_is_space == 0) {
        return lookahead_co;
      }
    }

    // Move cursor forwards and check bounds
    co = point_inc(co, e);
  }
}
