#!/usr/bin/perl
#Eduardo Zamora ezamora9@ucsc.edu

use strict;
use warnings;

use Getopt::Std;
use Switch;


print "Hello World!\n";


open(my $fh, "<", "score/test0/Makefile")
    or die "Can't open < score/test0/Makefile";

print $ARGV[1];
print "\n";

my $opt_x = getopts('d:');

# my %hash;
# while (my $row = <$fh>) {
#     my @chars = split //, $row;
#     my $len = split //, $row;
#     my $first_char = $chars[0];
#     if ($first_char ne '#' and $first_char ne "\n") {
#         print "$row";
        
#     }

#     # my @words = split / /, $row;
#     # foreach my $word (@words) {
#     #     if ($word ne ':' and $word ne '-') {
#     #         print ":$word ";
#     #     }
#     # }

#     # my @words = split /\t/, $row;
#     # foreach my $word (@words) {
#     #     if ($word ne ':' and $word ne '-') {
#     #         print ":$word ";
#     #     }
#     #     if ($first_char eq "\t") {
#     #         print "!!!!!!!!!!";
#     #     }
#     # }
# }

my @makefile_array;
my $array_count = 0;
while (my $row = <$fh>) {
    my @chars = split //, $row;
    my $first_char = $chars[0];
    if ($first_char ne '#' and $first_char ne "\n") {
        $makefile_array[$array_count] = $row;
        $array_count++;
    }
}

my $new_command = 0;
foreach my $row (@makefile_array) {
    print $row;

    my @chars = split //, $row;
    my $first_char = $chars[0];
    switch($first_char) {
        case "\t"   { $new_command = 0 }
        else        { $new_command = 1 }
    }

    if ($new_command == 1) {
        print "NEW\n";
    }
}
