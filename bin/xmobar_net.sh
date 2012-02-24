#!/bin/bash

ethIP=`ifconfig eth0 | sed -rn 's/.*inet addr:([^ ]*).*/\1/gp'`
pppIP=`ifconfig ppp0 | sed -rn 's/.*inet addr:([^ ]*).*/\1/gp'`
wlanIP=`ifconfig wlan0 | sed -rn 's/.*inet addr:([^ ]*).*/\1/gp'`
wlanAP=`iwconfig wlan0 | sed -rn 's/.*ESSID:"([^"]*).*/\1/gp'`
wlanQ=`iwconfig wlan0 | sed -rn 's/.*Link Quality=([^ ]*).*/\1*100/gp' | bc -l | sed 's/\..*/%/g'`

output=""
if [ "$pppIP" != "" ]; then
	output="ppp0: <fc=#00aaff>$pppIP</fc>"
fi

if [ "$ethIP" != "" ]; then
	if [ "$output" != "" ]; then
		output=$output", "
	fi
	output="$output eth0: <fc=#00aaff>$ethIP</fc>"
fi

if [ "$wlanIP" != "" ]; then
	if [ "$output" != "" ]; then
		output=$output", "
	fi
	output=$output"wlan0: <fc=#00aaff>$wlanIP</fc> ($wlanAP @ $wlanQ)"
fi

if [ "$output" == "" ]; then
	output="Not Connected"
fi

echo $output
