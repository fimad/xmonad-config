#!/bin/bash

#Set up background
#hsetroot -solid "#002b36" -center ~/.xmonad/lambda.png
hsetroot -solid "#002b36"

#disable the tap to click
synclient TapButton1=0

killall stalonetray
stalonetray &

killall redshift
redshift -l 37.7749:-122.4194 &

killall skype
skype &

killall dzen2
$HOME/.xmonad/bin/irc-status-bar.hs \
  '#greatestguys' \
  "$HOME/.weechat/logs/irc.halfling.#greatestguys.weechatlog" \
  'will:' '@will' 'fimad' 2> ~/.xmonad/irc-status-bar.errors

#killall xflux
#xflux -z 94110 &
killall compton
#compton -c -f -o 1.0 -I 0.1 -O 0.1 -C -z --vsync opengl-swc \
#  --paint-on-overlay --backend glx --shadow-exclude "name~='.*'" -b \
#  --use-ewmh-active-win --glx-no-stencil

# -c  enable shadows
# -f  fade windows in and out
# -o  shadow opacity
# -I  opacity step for fading in
# -I  opacity step for fading out
# -C  don't draw shadows for docks
# -z  zero the shadow behind windows
# -b  run in background
# -D  time between fade steps
compton \
  -c \
  -b \
  --inactive-dim 0.2 \
  --focus-exclude "name~='dzen title'" \
  --shadow-exclude "name~='.*'" \
  --vsync opengl-swc  --paint-on-overlay --backend glx \
  --use-ewmh-active-win --glx-no-stencil
