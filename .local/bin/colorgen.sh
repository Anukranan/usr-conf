#!/bin/sh

# This small script runs wal on the given input and then generates a GTK
# theme using wpg, finally ensuring that the proper theme and font is applied as well.

# If you run it in exec_always, it completely lags the system out.

wal -i $1 && wpg -a $1 && wpg -ns $1 && \
gsettings set org.gnome.desktop.interface gtk-theme FlatColor && \
gsettings set org.gnome.desktop.interface font-name "Terminus 10"
exit 0
