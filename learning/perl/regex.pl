#!/usr/bin/perl

use strict;
use feature ':5.10';
use 5.010;

my $pet = "I love my cat; my cat is awesome.";
$pet =~ s{cat}{dog}g;
say $pet;
