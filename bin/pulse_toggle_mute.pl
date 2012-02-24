#!/usr/bin/perl

my $ismuted = `echo "list-sinks" | pacmd | grep "muted: no"`;
chomp $ismuted;

if( $ismuted ne "" ){
	`echo "set-sink-mute 0 1" | pacmd`;
}else{
	`echo "set-sink-mute 0 0" | pacmd`;
}
