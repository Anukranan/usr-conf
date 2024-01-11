#---------------#
# Anukranan     #
# shell profile #
#---------------#

# System.
export ENV=$XDG_CONFIG_HOME/ksh/kshrc
export PS1="\[\e[1;32m\][\u \W]\[\e[0m\]> "
export PATH=$PATH:$HOME/.local/bin:$HOME/.local/bin/void-packages
export POSIXLY_CORRECT=yes

# Default directories.
export XDG_DESKTOP_DIR="$HOME/usr/env"
export XDG_PUBLICSHARE_DIR="$HOME/usr/pub"
export XDG_TEMPLATES_DIR="$HOME/usr/template"
export XDG_DOCUMENTS_DIR="$HOME/usr/doc"
export XDG_DOWNLOAD_DIR="$HOME/usr/tmp"
export XDG_MUSIC_DIR="$HOME/usr/media/aud"
export XDG_PICTURES_DIR="$HOME/usr/media/img"
export XDG_SCREENSHOTS_DIR="$HOME/usr/media/img/scr"
export XDG_VIDEOS_DIR="$HOME/usr/media/vid"

export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

# Source rc.
. $ENV
