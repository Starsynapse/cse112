#!/usr/bin/perl
#Eduardo Zamora ezamora9@ucsc.edu

use strict;
use warnings;

use Getopt::Std;
use Switch;


# print "Hello World!\n";


open(my $fh, "<", "score/test0/Makefile")
    or die "Can't open < score/test0/Makefile";

# print $ARGV[1];
# print "\n";

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

#Load Makefile into array
my @makefile_array;
my $array_count = 0;
my @command_list; # list of commands
my %target_number; # map of target to location in @command_list
my $command_count = 0;

while (my $row = <$fh>) {
    my @chars = split //, $row;
    my $first_char = $chars[0];

    if ($first_char ne '#' and $first_char ne "\n") {
        $makefile_array[$array_count] = $row;
        $array_count++;

        if ($first_char ne "\t") {
            $row =~ /\W+(?=:)/;
            $target_number{$`}=$command_count;
            $command_count++;
        }
    }

    if ($row =~ /(?<=\t)/) {
        push(@command_list, $');
    }
}


#Parse through Makefile
my $new_command = 0;
my %make_instructions;
my $count = 0;
foreach my $row (@makefile_array) {
    print $row;

    my @chars = split //, $row;
    my $first_char = $chars[0];
    if ($first_char eq "\t") {
        $new_command = 0;
    }
    else {
        $new_command = 1;
    }

    # my @words = split / /, $row;
    # foreach my $word (@words) {
    #     if ($word =~ /[a-z]/i) {
    #         print "$word ";
    #     }
    # }

    if ($new_command == 1) {
        # print "NEW\n";
        %make_instructions;
    }
}

print %make_instructions;
