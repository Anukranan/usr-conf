#-----------#
# Anukranan #
# bashrc    #
#-----------#

#----------------------------------------#
# System
#----------------------------------------#

[[ $- != *i* ]] && return
PS1="\[\e[1;32m\][\u \W]\[\e[0m\]> " # It just works.

export PATH=$PATH:$HOME/.local/bin:$HOME/.local/bin/void-packages
export POSIXLY_CORRECT=yes


#----------------------------------------#
# Aesthetics
#----------------------------------------#

(cat $HOME/.cache/wal/sequences &)
. $HOME/.cache/wal/colors-tty.sh


#----------------------------------------#
# Aliases
#----------------------------------------#

# Normal
alias cdp='cd $HOME/Documents/Programming'
alias ls='ls --color=auto -A -T 2 -1 -X -r -h --group-directories-first'

# XBPS
alias xq='xbps-query'
alias xqe='xbps-query -Rs'
alias xqi='xbps-query -l'

alias xn='doas xbps-install'
alias xns='doas xbps-install -Su'

alias xr='doas xbps-remove'
alias xrr='doas xbps-remove -Ro'


#----------------------------------------#
# Environment variables
#----------------------------------------#

# Env
export EDITOR='jmacs'

# Bemenu (fix this...)
export BEMENU_BACKEND=wayland
export BEMENU_OPTS="-i -l 20 -P '>' -p '[run]' -c -w --fixed-height -B 2 --bdr=$c3 --fn=terminus -W 0.5 --tb=$cb --tf=$c3 --fb=$cb --ff=$c3 --cb=$c7 --cf=$c3 --nb=$cb --nf=$cf --hb=$c3 --hf=$cb --fbb=$cb --fbf=$cf --sb=$c6 --sf=$c3 --ab=$cb --af=$cf --scb=$cb --scf=$c3"

# Wayland
export MOZ_ENABLE_WAYLAND=1

export QT_QPA_PLATFORM=wayland-egl
export QT_WAYLAND_FORCE_DPI=physical
export QT_WAYLAND_DISABLE_WINDOWDECORATION=1

export ELM_ENGINE=wayland_egl
export ELM_DISPLAY=wl
export ECORE_EVAS_ENGINE=wayland_egl

export SDL_VIDEODRIVER=wayland

export _JAVA_AWT_WM_NONREPARENTING=1