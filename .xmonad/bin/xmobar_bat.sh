#!/bin/bash
cur_chg=`cat /sys/class/power_supply/BAT0/charge_now`
max_chg=`cat /sys/class/power_supply/BAT0/charge_full`
prc_chg=`echo "($cur_chg/$max_chg) * 100 + .5" | bc -l | sed -r 's/([0-9]*).([0-9]).*/\1/g'`
sts_chg=`cat /sys/class/power_supply/BAT0/status`
grn_amt=`echo "($prc_chg/50) * 255"| bc -l | sed -r 's/([0-9]*).([0-9]).*/\1/g'`
red_amt=`echo "(1-(($prc_chg-50)/50)) * 255"| bc -l | sed -r 's/([0-9]*).([0-9]).*/\1/g'`

if [ "$sts_chg" == "Discharging" ]; then
	if [ "$prc_chg" -ge "50" ]; then
		color="#"`printf "%02x" $red_amt`"ff00"
	else
		color="#ff"`printf "%02x" $grn_amt`"00"
	fi
else
	color="#55ccff"
fi

echo "Batt: <fc=$color>$prc_chg%</fc>"
