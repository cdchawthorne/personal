#!/usr/bin/perl

use feature ':5.10';
use strict;

say "Yo dog.";
print "It's ", "a ", "trap!\n";
my $i = "foo";
print "$i\n";
my @arr = (5 .. 10);
for my $i (@arr) {
    say $i;
}
