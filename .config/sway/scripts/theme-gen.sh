#!/bin/sh

# Script to unify generating pywal colors and applying them to GTK using wpgtk.
# I have chosen to keep it in sway/scripts since it is only called in Sway.

# Configs using pywal cached colors instead of the GTK theme:
#  - Shell rc.
#  - Sway configuration.
#  - Waybar styling.

# Generate colorscheme.
wal -i $1

# Ensure that FlatColor is installed.
wpg-install.sh -g -i

# Apply colorscheme to GTK.
wpg -a $1
wpg -ns $1

# Ensure proper GTK settings are in place.
gsettings set org.gnome.desktop.interface color-scheme prefer-dark
gsettings set org.gnome.desktop.interface font-name "Terminus 10"
gsettings set org.gnome.desktop.interface gtk-theme FlatColor
gsettings set org.gnome.desktop.interface icon-theme FlattrColor
gsettings set org.gnome.desktop.interface gtk-key-theme Emacs

gsettings set org.gtk.Settings.FileChooser show-hidden true
gsettings set org.gtk.Settings.FileChooser sort-column name
gsettings set org.gtk.Settings.FileChooser sort-directories-first true
gsettings set org.gtk.Settings.FileChooser sort-order ascending
gsettings set org.gtk.Settings.FileChooser type-format category
