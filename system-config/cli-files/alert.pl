use strict;
use Irssi;

my $moan = 0;

sub notify {
    my ($summary, $body) = @_;
    if ($moan) {
        my $notify_sound = "/home/cdchawthorne/media/sounds/irene_adler_moan_ringtone.mp3";
        system("mplayer " . $notify_sound . " -af volume=15 &> /dev/null &");
    }
    system("notify-send", "-t", "5000", $summary, $body);
}

sub priv_msg {
    my ($server,$msg,$nick,$address) = @_;
    if ($nick !~ /^\*/) {
        notify($nick, $msg);
    }
}

sub public_msg {
    my ($server,$msg,$nick,$address,$target) = @_;
    if ($msg =~ /$server->{nick}/ && $nick != "root") {
        notify($target . ": " . $nick, $msg);
    }
}

sub list_buddies {
    my ($data, $server, $witem) = @_;
    $server->command("clear");
    $server->send_message("&bitlbee", "blist", 0);
}

sub moan_toggle {
    $moan = 1-$moan;
    if ($moan) {
        print("Moan on");
    } else {
        print("Moan off");
    }
}

Irssi::signal_add_last("message private", "priv_msg");
Irssi::signal_add_last("message public", "public_msg");
Irssi::command_bind("blist", "list_buddies");
Irssi::command_bind("moan", "moan_toggle");
