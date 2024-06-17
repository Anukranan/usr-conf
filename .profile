#------------#
# sh profile #
#------------#

#---------------
# Initialization.
#---------------

if test -z "${XDG_RUNTIME_DIR}"; then
	export XDG_RUNTIME_DIR=$(mktemp -d /tmp/$(id -u)-runtime-dir.XXX)
fi

#---------------
# Environment.
#---------------

# Wayland.
if [ "${XDG_SESSION_TYPE}" = "wayland" ]; then
	export BEMENU_BACKEND=wayland

	export MOZ_ENABLE_WAYLAND=1

	export QT_QPA_PLATFORM=wayland-egl
	export QT_WAYLAND_FORCE_DPI=physical
	export QT_WAYLAND_DISABLE_WINDOWDECORATION=1

	export ELM_ENGINE=wayland_egl
	export ELM_DISPLAY=wl
	export ECORE_EVAS_ENGINE=wayland_egl

	export SDL_VIDEODRIVER=wayland   

	export _JAVA_AWT_WM_NONREPARENTING=1
fi

# XDG (fake).
export XDG_LOCAL_HOME="${HOME}/.local"
export XDG_BIN_HOME="${XDG_LOCAL_HOME}/bin"

# XDG.
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_DATA_HOME="${XDG_LOCAL_HOME}/share"
export XDG_STATE_HOME="${XDG_LOCAL_HOME}/state"

export XDG_DOCUMENTS_DIR="${HOME}/doc"
export XDG_DOWNLOAD_DIR="${HOME}/tmp"
export XDG_MUSIC_DIR="${HOME}/media/aud/mus"
export XDG_PICTURES_DIR="${HOME}/media/img"
export XDG_SCREENSHOTS_DIR="${HOME}/media/img/scr"
export XDG_VIDEOS_DIR="${HOME}/media/vid"

# XDG (application-specific).
export GTK_RC_FILES="${XDG_CONFIG_HOME}/gtk-1.0/gtkrc"
export GTK2_RC_FILES="${XDG_CONFIG_HOME}/gtk-2.0/gtkrc"
export GNUPGHOME="${XDG_DATA_HOME}/gnupg"
export RUSTUP_HOME="${XDG_DATA_HOME}/rustup"
export PASSWORD_STORE_DIR="${XDG_DATA_HOME}/pass"
export W3M_DIR="${XDG_STATE_HOME}/w3m"
export XBPS_SRC_DIR="${XDG_LOCAL_HOME}/ports"

# System.
export PATH="${PATH}":"${XDG_BIN_HOME}":"${XBPS_SRC_DIR}"
export POSIXLY_CORRECT=yes
export PS1="\[\e[1;32m\][\u \W]\[\e[0m\]> "
export HISTFILE="${XDG_CACHE_HOME}/sh_history"
export ENV="${XDG_CONFIG_HOME}/sh/shrc"
