#!/usr/bin/perl

my $ismuted = `echo "list-sinks" | pacmd | grep "muted: yes"`;
chomp $ismuted;

if( $ismuted ne "" ){
	print "1\n";
}else{
	print "0\n";
}
