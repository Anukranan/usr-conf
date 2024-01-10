#!/bin/sh

# Script to unify generating pywal colors and applying them to GTK using wpgtk.

# Configs using pywal cached colors instead of the GTK theme:
#  - .bashrc
#  - .config/sway/config
#  - .config/waybar/style.css

# Generate colorscheme
wal -i $1

# Ensure that FlatColor is installed.
wpg-install.sh -g -i

# Apply colorscheme to GTK.
wpg -a $1
wpg -ns $1

# Ensure proper GTK settings are in place.
gsettings set org.gnome.desktop.interface color-scheme prefer-dark
gsettings set org.gnome.desktop.interface gtk-theme FlatColor
gsettings set org.gnome.desktop.interface icon-theme FlattrColor
gsettings set org.gnome.desktop.interface font-name "Terminus 10"

exit 0
