#! /bin/bash

LOCAL="$1"
REMOTE="$2"

rootdir="$(dirname "$BASH_SOURCE")"
emacs --eval "(progn (setq debug-on-error t) (load-file \"$rootdir/dircmp.el\") (compare-directories \"$LOCAL\" \"$REMOTE\"))"
