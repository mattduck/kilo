#!/bin/bash
# Snapshot the results of pmap to a file, then diff it on
# subsequent calls.
ARG="$1"
if [ -z "$ARG" ]; then exit 1; fi
pidof "$ARG" || exit 1;
PROCESSPID="$(pidof $ARG)"
F="/tmp/pmap-$ARG.$PROCESSPID"

if [ ! -f "$F" ]; then
    pmap "$(pidof $ARG)" -x > "$F";
    cat "$F";
    exit 0;
else
    mv "$F" "$F".previous;
    pmap "$(pidof $ARG)" -x > "$F";
    head -n 2 "$F";
    diff "$F".previous "$F" && echo "no change";
    rm "$F".previous;
fi
