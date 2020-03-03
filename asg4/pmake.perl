#!/usr/bin/perl
#Eduardo Zamora ezamora9@ucsc.edu

use strict;
use warnings;

use Getopt::Std;
use Switch;


# print "Hello World!\n";


open(my $fh, "<", "Makefile")
    or die "Can't open < Makefile";

my @args = @ARGV;
my %options=();
getopts("d", \%options);

# print scalar(%options);

my $target_argument;
if (scalar(@args) == 2) {
    $target_argument = $args[1];
    print "$target_argument\n";
}
if (scalar(@args) == 1) {
    $target_argument = $args[0];
    print "$target_argument\n";
}

#Load Makefile into array
my @makefile_array;
my $array_count = 0;
my @command_list; # list of commands
my $command_count = 0;

while (my $row = <$fh>) {
    my @chars = split //, $row;
    my $first_char = $chars[0];

    if ($first_char ne '#' and $first_char ne "\n") {
        $makefile_array[$array_count] = $row;
        $array_count++;
    }

    if ($row =~ /(?<=\t)/) {
        push(@command_list, $');
    }
}

my %target_numbers; # map of target to locations in @command_list
my $dependent;
my $targ;
foreach my $row (@makefile_array) {
    my @chars = split //, $row;
    my $first_char = $chars[0];
    my @commands;
    
    if ($first_char ne "\t") {
            $row =~ /\W+(?=:)/;
            $targ = $`;
        }
    else {
        $row =~ /(?<=\t)/;
        @commands = split /;/, $';

        my $temp_c = 0;
        my @c_numbers;
        foreach my $c (@command_list) {
            if ($c eq $') {
                foreach my $split_command (@commands) {
                    push(@c_numbers, $temp_c);
                    $temp_c++;
                }
            }
            else {
                $temp_c++;
            }
        }
        $target_numbers{$targ} = \@c_numbers;
    }
}

while(my($key,$val)=each(%target_numbers)) {
    my @array = @{$val};
    print "key='$key', val='@array'";
}

print "\n";
print "\n";
foreach my $row (@makefile_array) {
    print $row;
}
print "\n";

#Parse through Makefile
my $new_command = 0;
my %target_instructions;
my %target_dependencies;
my $count = 0;
my $current_target;
my $current_dependencies;
my $current_command;
foreach my $row (@makefile_array) {
    my @dependency_list;

    my @chars = split //, $row;
    my $first_char = $chars[0];
    if ($first_char eq "\t") {
        $new_command = 0;
    }
    else {
        $new_command = 1;
    }


    if ($new_command == 1) {
        $row =~ /\W+(?=:)/;
        $current_target = $`;
        $row =~ /(?<=:)\W+/;
        $current_dependencies = $';
        chomp($current_dependencies);
    }
    my @dependencies;
    if ($new_command == 0) {

        # print "$current_target,$current_dependencies\n";
        @dependencies = split / /, $current_dependencies;
        $target_dependencies{$current_target} = \@dependencies;

        if (scalar(@dependencies) > 0) {
            foreach my $dependency (@dependencies) {
                my $d = $target_numbers{$dependency};
                my @dd = @{$d};
                # print "$d, @dd\n";
                $target_instructions{$dependency} = $d;
            }
        }
    }
    # print @dependency_list;
}

# foreach my $i (%target_instructions) {
#     my @temp = 
#     print "$i\n";
# }
# print %target_instructions;

while(my($key,$val)=each(%target_instructions)) {
    my @array = @{$val};
    print "key='$key', val='@array'\n";
}
print "\n";

while(my($key,$val)=each(%target_dependencies)) {
    my @array = @{$val};
    print "key='$key', val='@array'\n";
}
print "\n";

foreach my $row (@makefile_array) {
    if ($row =~ /\W+(?=:)/) {
        $current_target = $`;
        # print "$current_target\n";

        if ($current_target eq $target_argument) {
            my $cn = $target_numbers{$current_target};
            my @command_numbers = @{$cn};
            my $dt = $target_dependencies{$current_target};
            my @dependent_targets = @{$dt};
            # print "t:@command_numbers\n";
            # print "d:@dependent_targets\n";

            # Could probably be converted into subroutine
            foreach my $t (@dependent_targets) {
                # print "dep: $t\n";
                my @t_nums = @{$target_numbers{$t}};
                # print "@t_nums\n";
                foreach my $num (@t_nums) {
                    my $command_to_execute = $command_list[$num];
                    # print $command_to_execute;

                    if ($command_to_execute =~ /(?<=@)/) {
                        $' =~ /(?<=\s)/;
                        $command_to_execute = $';
                    }
                    else {
                        print $command_to_execute;
                    }

                    system($command_to_execute);
                }
            }

            foreach my $num (@command_numbers) {
                my $command_to_execute = $command_list[$num];
                # print $command_to_execute;

                if ($command_to_execute =~ /(?<=@)/) {
                    $' =~ /(?<=\s)/;
                    $command_to_execute = $';
                }
                else {
                    print $command_to_execute;
                }

                system($command_to_execute);
            }
        }
    }
}
