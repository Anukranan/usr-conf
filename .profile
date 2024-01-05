#!/bin/sh

PATH=$PATH:$HOME/.local/bin:$HOME/.local/bin/void-packages/
export PATH

ENV=$HOME/.config/ksh/kshrc
export ENV=$ENV

(cat ~/.cache/wal/sequences &)
. ~/.cache/wal/colors-tty.sh

[[ -s $ENV ]] && . $ENV
