#!/bin/sh

#Set up background
#hsetroot -solid "#002b36" -center ~/.xmonad/lambda.png
hsetroot -solid "#002b36"

IBUS_ENABLE_SYNC_MODE=1 ibus-daemon -xrd

#disable the tap to click
synclient TapButton1=0

killall nm-applet
nm-applet &

killall stalonetray
stalonetray &

killall redshift
redshift -l 37.7833:122.4167 &

killall dzen2
ghc \
  $HOME/.xmonad/bin/irc-status-bar.hs \
  -o $HOME/.xmonad/bin/irc-status-bar 2> ~/.xmonad/irc-status-bar.errors
$HOME/.xmonad/bin/irc-status-bar \
  '#greatestguys' \
  "$HOME/.weechat/logs/irc.halfling.#greatestguys.weechatlog" \
  'will:' '@will' 'fimad' 2>> ~/.xmonad/irc-status-bar.errors

killall compton

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
