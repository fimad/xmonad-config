#!/usr/bin/perl
use List::Util qw[min max];
use Getopt::Long;

my $_IS_MUTED;
my $_TOGGLE_MUTE;
my $_VOL;
my $_INC_VOL;
my $_DEC_VOL;

GetOptions(
		"is-muted" => \$_IS_MUTED
	,	"toggle-mute" => \$_TOGGLE_MUTE
	,	"volume" => \$_VOL
	,	"inc-vol" => \$_INC_VOL
	,	"dec-vol" => \$_DEC_VOL
);

my $sink = `pactl info | sed -rn 's/^Default Sink: (.+)\$/\\1/gp'`;
chomp $sink;

sub ismuted{
	my $ismuted = `pacmd dump | grep "$sink" | grep sink-mute | grep yes`;
	chomp $ismuted;
	if( $ismuted ne "" ){
		return 1;
	}else{
		0;
	}
}

sub curvol{
	my $volline = `pacmd dump | grep "$sink" | grep sink-vol`;
  if( $volline =~ m/(0x[0-9a-f]+)/ ){
    return hex $1;
  }else{
    return 0;
  }
}

if( $_IS_MUTED ){
	print ismuted(), "\n";
}
elsif( $_TOGGLE_MUTE ){
	if( ismuted() ){
		`pactl set-sink-mute $sink 0`;
	}else{
		`pactl set-sink-mute $sink 1`;
	}
}elsif( $_VOL ){
  print int(curvol()*100.0/hex("0x10000")), "%\n";
}elsif( $_INC_VOL ){
  my $newvol = min(hex("0x10000"), curvol() + int((hex("0x10000")*.04)));
  `pactl set-sink-volume $sink $newvol`;
}elsif( $_DEC_VOL ){
  my $newvol = max(0, curvol() - int((hex("0x10000")*.04)));
  `pactl set-sink-volume $sink $newvol`;
}
