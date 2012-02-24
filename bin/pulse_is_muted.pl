#!/usr/bin/perl

my $ismuted = `echo "list-sinks" | pacmd | grep "muted: no"`;
chomp $ismuted;

if( $ismuted ne "" ){
	print "0\n";
}else{
	print "1\n";
}
