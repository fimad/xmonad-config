#!/bin/bash

#Set up background
hsetroot -solid "#002b36" -center ~/.xmonad/lambda.png

#disable the tap to click
synclient TapButton1=0

killall stalonetray
stalonetray &
#xcompmgr &
