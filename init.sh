#!/bin/bash

#Set up background
xsetroot -solid "#002b36"

#disable the tap to click
synclient TapButton1=0

killall stalonetray
stalonetray &
