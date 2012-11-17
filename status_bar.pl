#!/usr/bin/perl
use warnings;
use strict;
use FileHandle; # needed for the autoflush call on DZEN
use POSIX qw/strftime/;
use Date::Parse;
use Time::HiRes qw( usleep nanosleep );
use POSIX qw(floor ceil);


################################################################################
# Settings
################################################################################

my $Xres = `xrandr 2>&1 | sed -r 's/[\\sx]+/ /g' | grep '*' | cut -d " " -f 4 -`;
chomp $Xres;
$Xres /=2 if( $Xres > 2000 ); #hack for multimonitors
my $StatusBarWidth = $Xres - 50;
my $StatusBarSections = [.45, .10, .45];

my $StatusBarBG = "#002b36";
my $StatusBarFG = "#839496";

my $Separator = " | ";
my $SeparatorFG = "#073642";
my $SeparatorBG = "$StatusBarBG";

my $CurrentSpaceFG = "#b58900";
my $CurrentSpaceBG = $StatusBarBG;
my $OtherSpaceFG = "#6c71c4";
my $OtherSpaceBG = "$StatusBarBG";
my $SpaceFG = $StatusBarFG;
my $SpaceBG = $StatusBarBG;

my $SpaceLayoutFG = "#cb4b16";
my $SpaceLayoutBG = $StatusBarBG;

my $WindowTitleFG = "#859900";
my $WindowTitleBG = $StatusBarBG;

my $TimeFG = "#cb4b16";

my $BatteryFull = '#859900';
my $BatteryHalf = '#b58900';
my $BatteryEmpty = '#dc322f';
my $BatteryCharging = '#268bd2';

my $NetworkConnected = '#268bd2';
my $NetworkDisconnected = '#dc322f';

my $VolumeUnmuted = "#eee8d5";

my %LayoutReplacements = (
    "Hinted Tall" => "|||"
  , "Hinted Mirror Tall" => "==="
  , "IM ReflectX IM Grid" => "IM"
  , "IM ReflectX IM Full" => "Gimp"
  , "Hinted Full" => "[ ]"
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
  return int($StatusBarWidth / 7)-3;
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

#takes 2 3-element arrays for each section of the bar (left, center, right)
#The first is the percent of the bar to a lot to each secttion
#The second is the text to put in each section
sub formatText{
  my($divisions,$texts) = @_;
  
  my $output = "";

  for my $i (0 .. 2){
    my $maxLen = floor(textWidth() * $divisions->[$i] + .5);

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
      $text = (" "x floor($padding)) . $text . (" "x floor($padding+.5));
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

	my $color="$BatteryCharging"; #blue for charging

  if( $sts_chg =~ m/Discharging/ ){
    if( $prc_chg > 66 ){
      $color = $BatteryFull;
    }elsif( $prc_chg > 33 ){
      $color = $BatteryFull;
    }else{
      $color = $BatteryEmpty;
    }
  }

  return color($color, "$prc_chg%");
}

sub volume{
  my $info = `~/.xmonad/bin/pulse_control.pl -vol`;
  chomp $info;
  if( $info =~ m/([0-9]+)%/ ){
    if( `~/.xmonad/bin/pulse_control.pl -is-muted` eq "1\n" ){
      return "[[$1]]";
    }else{
      return "((^fg($VolumeUnmuted)$1^fg()))";
    }
  }else{
    return "([~~])";
  }
}

sub internet_ether{
  my( $dev ) = @_;
  my $ip=`ifconfig $dev | sed -rn 's/.*inet addr:([^ ]*).*/\\1/gp'`;
  my $color = "$NetworkDisconnected";
  if( $ip =~ m/.{3,}/ ){
    $color = "$NetworkConnected";
  }
  return "[^fg($color)=^fg()]" ;
}

sub internet_ether_verbose{
  my( $dev ) = @_;
  my $color = "$NetworkDisconnected";
  my $ip=`ifconfig $dev | sed -rn 's/.*inet addr:([^ ]*).*/\\1/gp'`;
  chomp $ip;
  $color = "$NetworkConnected" if $ip ne "";
  $ip = "Not Connected" if $ip eq "";
  return "$dev: ^fg($color)$ip^fg()";
}

sub internet_wifi{
  my( $dev ) = @_;
  my $ip=`ifconfig $dev | sed -rn 's/.*inet addr:([^ ]*).*/\\1/gp'`;
  my $color = "$NetworkDisconnected";
  if( $ip =~ m/.{3,}/ ){
    $color = "$NetworkConnected";
  }
  return
      "^fg($color)\\|/^fg()" ;
}

sub internet_wifi_verbose{
  my( $dev ) = @_;
  my $color = "$NetworkDisconnected";
  my $ip=`ifconfig $dev | sed -rn 's/.*inet addr:([^ ]*).*/\\1/gp'`;
  my $wlanAP=`iwconfig $dev | sed -rn 's/.*ESSID:"([^"]*).*/\\1/gp'`;
  my $wlanQ=`iwconfig $dev | sed -rn 's/.*Link Quality=([^ ]*).*/\\1*100/gp' | bc -l | sed 's/\\..*/%/g'`;
  my $wifi_info = "";
  chomp $wlanAP;
  chomp $wlanQ;
  chomp $ip;
  $color = "$NetworkConnected" if $ip ne "";
  $wifi_info = " ($wlanAP @ $wlanQ)" if $ip ne "";
  $ip = "Not Connected" if $ip eq"";
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
      $_xmonadStatus =~ s/(<LAYOUT>)$from(<\/LAYOUT>)/$1$to$2/g;
    }
#format the space layout
    $_xmonadStatus =~
      s/<LAYOUT>([^\<]+)<\/LAYOUT>/^fg($SpaceLayoutFG)^bg($SpaceLayoutBG)$1^fg()^bg()/g;
    $_xmonadStatus =~
      s/<CURRENT>([^\<]+)<\/CURRENT>/^fg($CurrentSpaceFG)^bg($CurrentSpaceBG)$1^fg()^bg()/g;
    $_xmonadStatus =~
      s/ <VISIBLE>([^\<]+)<\/VISIBLE>/^fg($OtherSpaceFG)^bg($OtherSpaceBG) $1^fg()^bg()/g;
    $_xmonadStatus =~
      s/<TITLE>([^\<]+)<\/TITLE>/^fg($WindowTitleFG)^bg($WindowTitleBG)$1^fg()^bg()/g;
  }
  return " $_xmonadStatus";
}


################################################################################
# Update Loop
################################################################################

`killall -9 dzen2 2> /dev/null`; #THERE CAN BE ONLY ONE
open(DZEN, "|-", "dzen2 -e 'entertitle=grabmouse;leavetitle=ungrabmouse;button3=togglecollapse' -l 2 -ta l -bg '$StatusBarBG' -fg '$StatusBarFG' -tw '$StatusBarWidth'") or die ("Unable to start dzen");
DZEN->autoflush(1);

my $i = 0;
while( 1 ){
  my $xmonad_status = getXmonadStatus();
  
  my $time = "^fg($TimeFG)" . strftime('%a %b %_d %Y %I:%M:%S %p',localtime);

#title window
  print DZEN "^tw()" . formatText(
    $StatusBarSections
    , [
          $xmonad_status
#        , daysTillJess
        , ""
        , separate(
            internet_ether("eth0")
          , internet_wifi("eth1")
          , volume
          , $time
          )
      ]
  );
#slave window
  if( $i % 1000 == 0 ){ #only update the slave every 1000
#  print DZEN "^cs()\n";
    print DZEN formatText($StatusBarSections, ["", "", internet_ether_verbose("eth0")]);
    print DZEN formatText($StatusBarSections, ["", "", internet_wifi_verbose("eth1")]);
  }


  usleep(500);
  $i++;
}

close(DZEN);

