#---------------#
# Anukranan     #
# shell profile #
#---------------#

#----------------------------------------#
# System.
#----------------------------------------#

# Directories.
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

export XDG_DOCUMENTS_DIR="$HOME/doc"
export XDG_DOWNLOAD_DIR="$HOME/tmp"
export XDG_MUSIC_DIR="$HOME/media/aud/music"
export XDG_PICTURES_DIR="$HOME/media/img"
export XDG_SCREENSHOTS_DIR="$HOME/media/img/scr"
export XDG_VIDEOS_DIR="$HOME/media/vid"

# Application-specific.
export GNUPGHOME="$XDG_DATA_HOME/gnupg"
export GTK_RC_FILES="$XDG_CONFIG_HOME/gtk-1.0/gtkrc"
export GTK2_RC_FILES="$XDG_CONFIG_HOME"/gtk-2.0/gtkrc
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"
export PASSWORD_STORE_DIR="$XDG_DATA_HOME/pass"
export W3M_DIR="$XDG_STATE_HOME/w3m"

export ENV="$HOME/.config/sh/shrc"
. $ENV
