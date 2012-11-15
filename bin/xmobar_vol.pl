#!/usr/bin/perl

$info = `amixer -c 0 get Master | grep 'Mono:'`;
chomp $info;
if( $info =~ m/\[([0-9]+)%\]/ ){
  if( `pulse_is_muted.pl` eq "1\n" ){
    print "Vol: [$1]\n";
  }else{
    print "Vol: $1\n";
  }
}else{
  print "BAD VOL\n";
}
