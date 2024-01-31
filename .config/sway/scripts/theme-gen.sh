#!/bin/sh

# Script to unify applying wal and GTK themes (wpg), light or dark, and with
# internal or generated wal color schemes.

#--------------------
# Variables.
#--------------------

ERROR_MSG=$'Error: invalid input\n'
HELP_MSG=$'Usage: theme-gen.sh [options] [wallpaper/theme] \n    \
           \t options: \n                                        \
           \t\t arg 1: dark, light \n                            \
           \t\t arg 2: wallpaper file or built-in pywal theme \n'


THEME_COLOR=$1
THEME_FILE=$2

#--------------------
# Functions.
#--------------------

# Use an internal pywal theme.
theme_init() {
  wal $1 --theme $3
  wpg-install.sh -g -i
  wpg $2 --theme $3
}

# Use a generated wallpaper theme.
gen_init() {
  wal $1 -i $3
  wpg-install.sh -g -i
  wpg $2 -a $3
  wpg $2 -ns $3
}

die() {
  echo $1
  echo $2
  exit 1
}


#--------------------
# Main.
#--------------------

if [ ! $THEME_COLOR ] || [ ! $THEME_FILE ]; then
  die "$ERROR_MSG" "$HELP_MSG"
fi

if [ $THEME_COLOR = "dark" ]; then
  ARGS_WAL=""
  ARGS_WPG=""
  gsettings set org.gnome.desktop.interface color-scheme prefer-dark
elif [ $THEME_COLOR = "light" ]; then
  ARGS_WAL="-l"
  ARGS_WPG="-L"
  gsettings set org.gnome.desktop.interface color-scheme prefer-light
else
  die "$ERROR_MSG" "$HELP_MSG"
fi

if [ -r $THEME_FILE ]; then
  gen_init "$ARGS_WAL" "$ARGS_WPG" "$THEME_FILE"
else
  theme_init "$ARGS_WAL" "$ARGS_WPG" "$THEME_FILE"
fi
