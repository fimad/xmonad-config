#!/usr/bin/perl

$dmenu="dmenu -nb '#002b36' -nf '#839496' -sb '#073642' -sf '#93a1a1' -fn '-*-fixed-*-*-*-*-*-*-*-*-*-*-*-*' -l 16 -i";
$number=`mpc playlist | cat -n | sed 's/\t/    /g' | $dmenu | sed -r 's/ {2,}/\t/g' | cut -f 2`;
`mpc play $number` if( length $number > 0 );
