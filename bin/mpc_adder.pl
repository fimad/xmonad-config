#!/usr/bin/perl

$dmenu="dmenu -nb '#002b36' -nf '#839496' -sb '#073642' -sf '#93a1a1' -fn '-*-fixed-*-*-*-*-*-*-*-*-*-*-*-*' -l 16 -i";
$artist=`mpc ls | sed -r 's/^/        /g' | $dmenu | sed -r 's/^ *//g'`;
if( length $artist > 0 ){
  if($ARGV[0] eq "artist"){
    `mpc add "$artist"`;
  }else{
    $album=`mpc ls "$artist" | sed -r 's/^/        /g' | $dmenu | sed -r 's/^ *//g'`;
    `mpc add "$album"` if( length $album > 0 );
  }
}
