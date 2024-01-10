#!/bin/sh

# Generate colorscheme.
wal -i $1

# Apply colorscheme to GTK.
wpg -a $1
wpg -ns $1

# Ensure proper GTK settings are in place.
gsettings set org.gnome.desktop.interface gtk-theme FlatColor
gsettings set org.gnome.desktop.interface font-name "Terminus 10"

exit 0
