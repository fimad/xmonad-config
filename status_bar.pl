#!/usr/bin/perl
use warnings;
use strict;
use FileHandle; # needed for the autoflush call on DZEN
use POSIX qw/strftime/;
use Date::Parse;
use Time::HiRes qw( usleep nanosleep );


################################################################################
# Settings
################################################################################

my $Xres = `xrandr 2>&1 | sed -r 's/[\sx]+/ /g' | grep '*' | cut -d " " -f 4 -`;
chomp $Xres;
my $StatusBarWidth = $Xres - 50;
my $StatusBarSections = [.40, .20, .40];

my $Separator = " | ";
my $SeparatorFG = "#444444";
my $SeparatorBG = "#000000";

my $StatusBarBG = "#000000";
my $StatusBarFG = "#AAAAAA";

my $CurrentSpaceFG = "#F5CC16";
my $CurrentSpaceBG = $StatusBarBG;
my $SpaceFG = $StatusBarFG;
my $SpaceBG = $StatusBarBG;

my $SpaceLayoutFG = "#CF5519";
my $SpaceLayoutBG = $StatusBarBG;

my $WindowTitleFG = "#59CF59";
my $WindowTitleBG = $StatusBarBG;

my %LayoutReplacements = (
    "Hinted Tall" => "|||"
  , "Hinted Mirror Tall" => "==="
  , "IM ReflectX IM Grid" => "IM"
  , "IM ReflectX IM Full" => "Gimp"
  , "Full" => "[ ]"
);


################################################################################
# Formatting Methods
################################################################################

sub color{
  my( $color, $text ) = @_;
  return "^fg($color)$text^fg()"
}

sub textWidth{
  return int($StatusBarWidth / 7) - 2;
}

#calculates length ignoring command sequences
sub realLength{
  my( $text ) = @_;
  $text =~ s/\^[a-z]+\([^\)]*\)//g;
  return (length $text);
}

#removes commands from the end of a string
sub popCommands{
  my ( $text ) = @_;
  $text =~ s/(\^[a-z]+\([^\)]*\))+$//g;
  return $text;
}

#takes 2 3 element arrays for each section of the bar (left, center, right)
#The first is the percent of the bar to a lot to each secttion
#The second is the text to put in each section
sub formatText{
  my($divisions,$texts) = @_;
  
  my $output = "";

  for my $i (0 .. 2){
    my $maxLen = int(textWidth() * $divisions->[$i])+1;

#truncate text if needed
    my $text = $texts->[$i];
    $text =~ s/\^bg\(\)//g;
    while( realLength($text) > $maxLen ){
      $text = popCommands($text);
      chop $text;
    }

#add padding
    if( $i == 0 ){ #left justify
      $text = $text . " "x($maxLen - realLength($text));
    }
    elsif( $i == 1 ){
      my $padding = ($maxLen - realLength($text)) / 2.0;
      $text = (" "x int($padding)) . $text . (" "x int($padding+.999));
    }
    elsif( $i == 2 ){ #right justify
      $text = " "x($maxLen - realLength($text)) . $text;
    }

    $output = "${output}^fg()^bg($StatusBarBG)$text";
  }

  return "$output"." "x 0 . "\n";
}

sub separate{
  return join("^fg($SeparatorFG)^bg($SeparatorBG)$Separator^fg()^bg($StatusBarBG)", @_);
}


################################################################################
# Widgets
################################################################################

sub daysTillJess{
  my $pomona = strftime("%s",localtime (str2time "9/1/2012"));
  my $days = int( ($pomona - time) / (60*60*24) + .99);
  return "$days DAYS!";
}

sub battery{
  my $cur_chg = `cat /sys/class/power_supply/BAT0/charge_now`;
  my $max_chg = `cat /sys/class/power_supply/BAT0/charge_full`;
  my $prc_chg = int(($cur_chg/$max_chg) * 100 + .5);
  my $sts_chg = `cat /sys/class/power_supply/BAT0/status`;
  my $grn_amt = int(($prc_chg/50) * 255);
  my $red_amt = int((1 - (($prc_chg-50)/50)) * 255);

	my $color="#55ccff"; #blue for charging

  if( $sts_chg =~ m/Discharging/ ){
    if( $prc_chg > 50 ){
      $color = sprintf("#%02xff00", $red_amt);
    }else{
      $color = sprintf("#ff%02x00", $grn_amt);
    }
  }

  return "^fg($color)$prc_chg%^fg()";
}

sub volume{
  my $info = `~/.xmonad/bin/pulse_control.pl -vol`;
  chomp $info;
  if( $info =~ m/([0-9]+)%/ ){
    if( `~/.xmonad/bin/pulse_control.pl -is-muted` eq "1\n" ){
      return "[[$1]]";
    }else{
      return "((^fg(white)$1^fg()))";
    }
  }else{
    return "([~~])";
  }
}

sub internet_ether{
  my( $dev ) = @_;
  my $ip=`ifconfig $dev | sed -rn 's/.*inet addr:([^ ]*).*/\\1/gp'`;
  my $color = "#BA2929";
  if( $ip =~ m/.{3,}/ ){
#$color = "#24C943";
    $color = "#00aaff";
  }
  return "[^fg($color)=^fg()]" ;
}

sub internet_ether_verbose{
  my( $dev ) = @_;
  my $color = "#BA2929";
  my $ip=`ifconfig $dev | sed -rn 's/.*inet addr:([^ ]*).*/\\1/gp'`;
  chomp $ip;
  $color = "#00aaff" if $ip != "";
  $ip = "Not Connected" if $ip == "";
  return "$dev: ^fg($color)$ip^fg()";
}

sub internet_wifi{
  my( $dev ) = @_;
  my $ip=`ifconfig $dev | sed -rn 's/.*inet addr:([^ ]*).*/\\1/gp'`;
  my $color = "#BA2929";
  if( $ip =~ m/.{3,}/ ){
#$color = "#24C943";
    $color = "#00aaff";
  }
  return
      "^fg($color)\\|/^fg()" ;
}

sub internet_wifi_verbose{
  my( $dev ) = @_;
  my $color = "#BA2929";
  my $ip=`ifconfig $dev | sed -rn 's/.*inet addr:([^ ]*).*/\\1/gp'`;
  my $wlanAP=`iwconfig $dev | sed -rn 's/.*ESSID:"([^"]*).*/\\1/gp'`;
  my $wlanQ=`iwconfig $dev | sed -rn 's/.*Link Quality=([^ ]*).*/\\1*100/gp' | bc -l | sed 's/\\..*/%/g'`;
  my $wifi_info = "";
  chomp $wlanAP;
  chomp $wlanQ;
  chomp $ip;
  $color = "#00aaff" if $ip != "";
  $wifi_info = " ($wlanAP @ $wlanQ)" if $ip != "";
  $ip = "Not Connected" if $ip == "";
  return "$dev: ^fg($color)$ip^fg()$wifi_info";
}


################################################################################
# XMonad Status Reading
################################################################################

my $_xmonadStatus = "";
sub getXmonadStatus{
  my $rid = '';
  vec ($rid, fileno(STDIN), 1) = 1;
  if( select($rid,undef,undef,0.25) >= 0 && vec($rid, fileno(STDIN), 1)){
    $_xmonadStatus= <STDIN>;
    chomp $_xmonadStatus;

#perform any layout replacements that apply
    for my $from (keys %LayoutReplacements){
      my $to = $LayoutReplacements{$from};
      $_xmonadStatus =~ s/(\^fg\(black\)\^bg\(#cccccc\)) $from (\^fg\(\)\^bg\(\)\^bg\(#324c80\))/$1 $to $2/g;
    }
#format the space layout
    $_xmonadStatus =~ s/\^fg\(black\)\^bg\(#cccccc\) ([^\^]+) \^fg\(\)\^bg\(\)\^bg\(#324c80\)/^fg($SpaceLayoutFG)^bg($SpaceLayoutBG)$1^fg()^bg($StatusBarBG)^bg(#324c80)/g;
#format non selected spaces
    $_xmonadStatus =~ s/\^fg\(black\)\^bg\(#cccccc\) ([^\^]+) \^fg\(\)\^bg\(\)/^fg($SpaceFG)^bg($SpaceBG)$1 ^fg()^bg($StatusBarBG)/g;
#format the selected space
    $_xmonadStatus =~ s/\^fg\(white\)\^bg\(#2b4f98\) ([^\^]+) \^fg\(\)\^bg\(\)/^fg($CurrentSpaceFG)^bg($CurrentSpaceBG)[$1] ^fg()^bg($StatusBarBG)/g;
#format the title text
    $_xmonadStatus =~ s/\^bg\(#324c80\)/^fg($WindowTitleFG)^bg($WindowTitleBG)/g;
  }
  return " $_xmonadStatus";
}


################################################################################
# Update Loop
################################################################################

`killall -9 dzen2 2> /dev/null`; #THERE CAN BE ONLY ONE
open(DZEN, "|-", "dzen2 -e 'entertitle=grabmouse;leavetitle=ungrabmouse;button3=togglecollapse' -l 2 -ta l -bg $StatusBarBG -fg $StatusBarFG -tw $StatusBarWidth") or die ("Unable to start dzen");
DZEN->autoflush(1);

my $i = 0;
while( 1 ){
  my $xmonad_status = getXmonadStatus();
  
  my $time = "^fg(#ee9a00)" . strftime('%a %b %_d %Y %I:%M:%S %p',localtime);

#title window
  print DZEN "^tw()" . formatText($StatusBarSections, [$xmonad_status, daysTillJess, separate(internet_ether("eth0"), internet_wifi("wlan0"),volume,battery,$time)]);
#slave window
  if( $i % 1000 == 0 ){ #only update the slave every 1000
#  print DZEN "^cs()\n";
    print DZEN formatText($StatusBarSections, ["", "", internet_ether_verbose("eth0")]);
    print DZEN formatText($StatusBarSections, ["", "", internet_wifi_verbose("wlan0")]);
  }


  usleep(500);
  $i++;
}

close(DZEN);

