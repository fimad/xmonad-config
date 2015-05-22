#!/bin/bash

XMONAD_BIN=$HOME/.xmonad/bin
PULSE_CONTROL=$XMONAD_BIN/pulse_control.pl

WHITE="#fdf6e3"
BLACK="#002b36"
BG="#2aa198"

color=$WHITE
if [[ "$($PULSE_CONTROL -is-muted)" -eq "1" ]]
then
  color=$BLACK
fi

volume="$($PULSE_CONTROL -volume)"

echo "<fc=${WHITE},${BG}><fn=1>🔈</fn> </fc><fc=${color},${BG}>${volume}</fc>"
