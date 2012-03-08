#!/usr/bin/perl

my $ismuted = `pulse_is_muted.pl`;
chomp $ismuted;

if( $ismuted eq "0" ){
	`echo "set-sink-mute 0 1" | pacmd`;
}else{
	`echo "set-sink-mute 0 0" | pacmd`;
}
