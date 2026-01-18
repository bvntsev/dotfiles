#!/bin/sh

syncthing &
goldendict &
dunst &
brightnessctl s 100%
wlr-randr   --output DP-1 --mode 2560x1440@154.964005 --pos 1920,0 \
            --output eDP-1 --mode 1920x1080@60.000999 --pos 0,0
