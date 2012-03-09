#! /bin/bash -i

LOCAL="$1"
REMOTE="$2"

 
emacs --eval="(progn (compare-dirs \"$LOCAL\" \"$REMOTE\"))"
