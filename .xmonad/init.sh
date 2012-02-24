#!/bin/bash

#set up wallpaper
#Esetroot ~/Pictures/wallpapers/hypnotoad.jpg &
#pick a random wallpaper from $WALL_DIR
WALL_DIR="$HOME/Pictures/wallpapers/active"
PAPER=`ls $WALL_DIR | perl -e 'while(<STDIN>){chomp; push(@f,$_);} print $f[int(rand(scalar(@f)))]'`
FLAG=`echo $PAPER | sed -rn 's/.+\.(.+)\.[^\.]+/-\1/p'`
echo $FLAG
#Esetroot $FLAG "$WALL_DIR/$PAPER" &
#Esetroot $HOME/Pictures/space/Sun_small.png 
#Esetroot -scale $HOME/Pictures/space/Earth.jpg 
#Esetroot -center $HOME/Pictures/space/as08-13-2329.jpg
Esetroot -center $HOME/Pictures/space/o-HIGH-DEFINITION-EARTH-PICTURE-900.jpg

#disable the tap to click
synclient TapButton1=0

killall stalonetray
stalonetray &

killall nm-applet
nm-applet &

#only start pidgin and skype if they aren't already running
if [ "`ps -A | egrep ' instantbird-bin$'`" == "" ]; then
#	pidgin &
#	~/bin/instantbird/instantbird &
fi
if [ "`ps -A | egrep ' skype$'`" == "" ]; then
	skype &
fi

#start your alarm clock
alarm-clock-applet &

#adesklets
