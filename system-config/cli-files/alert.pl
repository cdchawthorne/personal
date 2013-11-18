use strict;
use Irssi;

sub notify {
    my ($summary, $body) = @_;
    my $notify_sound = "/home/cdchawthorne/media/music/irene_adler_moan_ringtone.mp3";
    system("mplayer " . $notify_sound . " -af volume=15 &> /dev/null &");
    system("notify-send", "-t", "5000", $summary, $body);
}

sub priv_msg {
    my ($server,$msg,$nick,$address) = @_;
    notify($nick, $msg);
}

sub public_msg {
    my ($server,$msg,$nick,$address,$target) = @_;
    if ($msg =~ /$server->{nick}/) {
        notify($target . ": " . $nick, $msg);
    }
}

sub list_buddies {
    my ($data, $server, $witem) = @_;
    $server->command("clear");
    $server->send_message("&bitlbee", "blist", 0);
}

Irssi::signal_add_last("message private", "priv_msg");
Irssi::signal_add_last("message public", "public_msg");
Irssi::command_bind("blist", "list_buddies");
