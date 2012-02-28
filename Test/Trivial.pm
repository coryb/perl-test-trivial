# Copyright (c) 2008 Yahoo! Inc. All rights reserved.
# The copyrights to the contents of this file are licensed
# under the Perl Artistic License (ver. 15 Aug 1997)
##########################################################
package Test::Trivial;
##########################################################
use strict;
use warnings;
use IO::Handle;
use POSIX qw(strftime);
use Regexp::Common qw(balanced);
use Text::Diff;
use Filter::Simple;
use File::Basename;
use constant IFS => $/;

FILTER {
#     warn <<EOM;
# ====================> IN <====================
# $_
# ==============================================
# EOM
    my @grps;
    my $marker = '****Test::Trivial::Group****';
    while( s/$RE{balanced}{-parens=>'(){}[]'}{-keep}/$marker/s ) {
        push @grps, $1;
    }

    s/TODO\s+(.*?);/do { local \$Test::Trivial::TODO = "Test Know to fail"; $1; };/gs;

    while( my $grp = shift @grps ) {
        s/\Q$marker\E/$grp/;
    }
#     warn <<EOM;
# ===================> OUT <====================
# $_
# ==============================================
# EOM
};

use Getopt::Long;
Getopt::Long::Configure(
    "pass_through"
);

our $FATAL   = 0;
our $VERBOSE = 0;
our $LEVEL   = 0;
our $DIFF    = "Unified";
our $TODO    = "";
our $LOG     = $ENV{TEST_TRIVIAL_LOG};

GetOptions(
    'fatal'   => \$FATAL,
    'verbose' => \$VERBOSE,
    'diff=s'  => \$DIFF,
    'log:s'   => \$LOG,
);

# rebless the singleton so we can intercept
# the _is_diag function
BEGIN {
    require Test::More;

    # forgive me, for I have sinned ...
    no warnings qw(redefine);

    # replace Test::More _format_stack so 
    # we can call Text::Diff when needed
    *Test::More::_format_stack = \&format_stack;
}

bless Test::More->builder, 'Test::Trivial::Builder';

sub import {
    my $package = shift;

    if ( !@_ ) {
        eval "use Test::More qw( no_plan )";
        if ( $@ ) {
            die "Failed to load Test::More: $@";
        }
    }        
    elsif ( @_ == 1 ) {
        eval "use Test::More qw( $_[0] )";
        if ( $@ ) {
            die "Failed to load Test::More: $@";
        }
    }
    else {
        my %args = @_;
        if( my $tests = delete $args{tests} ) {
            eval "use Test::More tests => \"$tests\"";
        }
        elsif( my $skip = delete $args{skip_all} ) {
            eval "use Test::More skip_all => \"$skip\"";
        }
        if ( $@ ) {
            die "Failed to load Test::More: $@";
        }
        if ( $args{diff} ) {
            $DIFF = $args{diff};
        }
    }

    # crude Exporter
    my ($pkg) = caller();
    for my $func ( qw(ERR OK NOK EQ ID ISA IS ISNT LIKE UNLIKE) ) {
        no strict 'refs';
        *{"${pkg}::$func"} = \&{$func};
    }

    if ( defined $LOG ) {
        my $logfile = $LOG;
        if( !$logfile ) {
            my ($name, $dir) = File::Basename::fileparse($0);
            $logfile = "$dir/$name.log";
        }
        open my $log, ">>$logfile" or die "Could not open $logfile: $!";
        my $tee = tie( *STDOUT, "Test::Trivial::IO::Tee", $log, \*STDOUT);
        tie( *STDERR, "Test::Trivial::IO::Tee", $log, \*STDERR);
        if( $VERBOSE ) {
            $SIG{__WARN__} = sub { print STDERR @_ };
        }
        else {
            $VERBOSE++;
            $SIG{__WARN__} = sub { $tee->log(@_) }
        }
        $SIG{__DIE__} = sub { print STDOUT @_ };
        my $tb = Test::Builder->new();
        $tb->output(\*STDOUT);
        $tb->failure_output(\*STDERR);
        warn "#"x50, "\n";
        warn "#\n";
        warn "# Test: $0\n";
        warn "# Time: ", POSIX::strftime("%Y-%m-%d %X", localtime()), "\n";
        warn "#\n";
        warn "#"x50, "\n";
    }
}

sub ERR (&) {
    my $code = shift;
    local $@;
    my $ret = eval {
        &$code;
    };
    return $@ if $@;
    return $ret;
}

sub OK ($;$) {
    my ($test, $msg) = @_;
    $msg ||= line_to_text();
    if( $VERBOSE ) {
        require Data::Dumper;
        warn "--------------------------------------------------------\n";
        warn Data::Dumper->Dump([$test], ["OK"]);
        warn "--------------------------------------------------------\n";
    }
    check($test) || warn_line_failure();
    ok($test, $msg) || ($FATAL && die "All errors Fatal\n");
    
}

sub NOK ($;$) {
    my ($test, $msg) = @_;
    $msg ||= line_to_text();
    if( $VERBOSE ) {
        require Data::Dumper;
        warn "--------------------------------------------------------\n";
        warn Data::Dumper->Dump([$test], ["NOK"]);
        warn "--------------------------------------------------------\n";
    }
    check(!$test) || warn_line_failure();
    ok(!$test, "not [$msg]") || ($FATAL && die "All errors Fatal\n");
    
}

sub EQ ($$;$) {
    my ($lhs, $rhs, $msg) = @_;
    $msg ||= line_to_text();
    if( $VERBOSE ) {
        require Data::Dumper;
        warn "--------------------------------------------------------\n";
        warn Data::Dumper->Dump([[$lhs, $rhs]], ["EQ"]);
        warn "--------------------------------------------------------\n";
    }
    check_is($lhs,$rhs) || warn_line_failure();
    is($lhs,$rhs, $msg) || ($FATAL && die "All errors Fatal\n");
}

sub ID ($$;$) {
    my ($lhs, $rhs, $msg) = @_;
    $msg ||= line_to_text();
    if( $VERBOSE ) {
        require Data::Dumper;
        warn "--------------------------------------------------------\n";
        warn Data::Dumper->Dump([[$lhs,$rhs]], ["ID"]);
        warn "--------------------------------------------------------\n";
    }
    check_is($lhs,$rhs) || warn_line_failure();
    is($lhs,$rhs, $msg) || ($FATAL && die "All errors Fatal\n");
}

my ($OFH, $FFH, $TFH);
sub capture_io {
    my $data = shift;
    my $io = IO::Scalar->new($data); 
    my $tb = Test::Builder->new();
    ($OFH, $FFH, $TFH) = (
        $tb->output(),
        $tb->failure_output,
        $tb->todo_output,
    );
    $tb->output($io);
    $tb->failure_output($io);
    $tb->todo_output($io);
}

sub reset_io {
    my $tb = Test::Builder->new();
    $tb->output($OFH) if defined $OFH;
    $tb->failure_output($FFH) if defined $FFH;
    $tb->todo_output($TFH) if defined $TFH;
}    

sub ISA ($$;$) {
    local $LEVEL += 1;
    return OK(UNIVERSAL::isa($_[0],$_[1]),$_[2]);
}

sub IS ($$;$) {
    my ($lhs, $rhs, $msg) = @_;
    $msg ||= line_to_text();
    use IO::Scalar;
    my $output = "";
    if( $VERBOSE ) {
        require Data::Dumper;
        warn "--------------------------------------------------------\n";
        warn Data::Dumper->Dump([[$lhs, $rhs]], ["IS"]);
        warn "--------------------------------------------------------\n";
    }
    capture_io(\$output);
    my $ok = is_deeply($lhs, $rhs, $msg);
    reset_io();
    warn_line_failure() unless $ok;
    print $output;
    $ok || ($FATAL && die "All errors Fatal\n");
}

# Test::More does not have an isnt_deeply
# so hacking one in here.
sub isnt_deeply {
    my $tb = Test::More->builder;
    my($got, $expected, $name) = @_;

    $tb->_unoverload_str(\$expected, \$got);

    my $ok;
    if ( !ref $got and !ref $expected ) {
        # no references, simple comparison
        $ok = $tb->isnt_eq($got, $expected, $name);
    } elsif ( !ref $got xor !ref $expected ) {
        # not same type, so they are definately different
        $ok = $tb->ok(1, $name);
    } else {                    # both references
        local @Test::More::Data_Stack = ();
        if ( Test::More::_deep_check($got, $expected) ) {
            # deep check passed, so they are the same
            $ok = $tb->ok(0, $name);
        } else {
            $ok = $tb->ok(1, $name);
        }
    }

    return $ok;
}

sub ISNT ($$;$) {
    my ($lhs, $rhs, $msg) = @_;
    $msg ||= line_to_text();
    if( $VERBOSE ) {
        require Data::Dumper;
        warn "--------------------------------------------------------\n";
        warn Data::Dumper->Dump([[$lhs, $rhs]], ["ISNT"]);
        warn "--------------------------------------------------------\n";
    }
    check_is($lhs,$rhs) && warn_line_failure();
    isnt_deeply($lhs, $rhs, $msg) || ($FATAL && die "All errors Fatal\n");
}

sub LIKE ($$;$) {
    my ($lhs, $rhs, $msg) = @_;
    $msg ||= line_to_text();
    if( $VERBOSE ) {
        require Data::Dumper;
        warn "--------------------------------------------------------\n";
        warn Data::Dumper->Dump([[$lhs, $rhs]], ["LIKE"]);
        warn "--------------------------------------------------------\n";
    }
    check_like($lhs,$rhs) || warn_line_failure();
    like($lhs, $rhs, $msg) || ($FATAL && die "All errors Fatal\n");
}

sub UNLIKE ($$;$) {
    my ($lhs, $rhs, $msg) = @_;
    $msg ||= line_to_text();
    if( $VERBOSE ) {
        require Data::Dumper;
        warn "--------------------------------------------------------\n";
        warn Data::Dumper->Dump([[$lhs, $rhs]], ["UNLIKE"]);
        warn "--------------------------------------------------------\n";
    }
    check_like($lhs,$rhs) && warn_line_failure();
    unlike($lhs, $rhs, $msg) || ($FATAL && die "All errors Fatal\n");
}

sub check {
    if( !$_[0] ) {
        return 0;
    }
    return 1;
}

sub check_is {
    my $data = shift;
    my $expected = shift;
    return 1 if (not defined $data) && (not defined $expected);
    return 0 if (not defined $data) && (defined $expected);
    return 0 if (defined $data) && (not defined $expected);
    return $data eq $expected;
}

sub check_like {
    my $data = shift;
    my $match = shift;
    return 0 unless defined $match;
    
    if ( ((not defined $data) && (defined $match))
             || ($data !~ $match) ) {
        return 0;
    }
    return 1;
}

my %file_cache = ();

sub warn_line_failure {
    my ($pkg, $file, $line, $sub) = caller($LEVEL + 1);
    print STDERR POSIX::strftime("# Time: %Y-%m-%d %X\n", localtime())
        unless $ENV{HARNESS_ACTIVE};
    $sub =~ s/^.*?::(\w+)$/$1/;
    my $source = $file_cache{$file}->[$line-1];
    my $col = index($source,$sub);
    # index -1 on error, else add 1 (editors start at 1, not 0)
    $col = $col == -1 ? 0 : $col + 1;
    my $tb = Test::Builder->new();
    print "$file:$line:$col: Test ", $tb->current_test()+1, " Failed\n"
        unless $ENV{HARNESS_ACTIVE};
}


my %OPS = (
    'OK'     => "",
    'NOK'    => "",
    'EQ'     => "==",
    'ID'     => "==",
    'IS'     => "==",
    'ISA'    => "ISA",
    'ISNT'   => "!=",
    'LIKE'   => "=~",
    'UNLIKE' => "!~",
);

sub line_to_text {
    my ($pkg, $file, $line, $sub) = caller($LEVEL + 1);

    $sub =~ s/^.*::(\w+)$/$1/;

    my $source;
    unless( $file_cache{$file} && @{$file_cache{$file}}) {
        # reset input line seperator in case some
        # is trying to screw with us
        local $/ = IFS;
        my $io = IO::Handle->new();
        open($io, "$file") or die "Could not open $file: $!";
        my @source = <$io>;
        $file_cache{$file} = \@source;
    }

    # sometimes caller returns the line number of the end
    # of the statement insted of the beginning, so backtrack
    # to find the calling sub if the current line does not 
    # have sub in it.
    $line-- while defined $file_cache{$file}->[$line-1] && $file_cache{$file}->[$line-1] !~ /$sub/;
    my $offset = $line-1;
    $source = $file_cache{$file}->[$offset];
    while ($source !~ /;/ && $offset+1 != @{$file_cache{$file}} ){ 
        $offset++;
        $source .= $file_cache{$file}->[$offset];
    }

    my $msg = "Unknown";
    if( $source =~ /$sub$RE{balanced}{-parens=>'()'}{-keep}/s ) {
        $msg = substr($1,1,-1);
    }
    elsif( $source =~ /$sub(.*?)\s(or|and)\b/s ) {
        $msg = $1;
    }
    elsif( $source =~ /$sub(.*?)(;|$)/s ) {
        $msg = $1;
    }

    $msg =~ s/^\s+//;
    $msg =~ s/\s+$//;

    if( my $op = $OPS{$sub} ) {
        # multiple args
        my @parens;
        while( $msg =~ s/$RE{balanced}{-parens=>'(){}[]'}{-keep}/#####GRP#####/s ) {
            push @parens, $1;
        }
        my @parts = split /\s*(?:,|=>)\s*/s, $msg;
        s/^\s+// || s/\s+$// for @parts;
        $msg = "$parts[0] $op $parts[1]";

        while( my $paren = shift @parens ) {
            $msg =~ s/#####GRP#####/$paren/;
        }
        
    }
    return $msg;
}

#
# this routing is basically copied from 
#
# Test::More::_format_stack.
# Original Author: Michael G Schwern <schwern@pobox.com>
# Copyright: Copyright 2001-2008 by Michael G Schwern <schwern@pobox.com>
#
# It has been modified to wedge in the Text::Diff call
#

sub format_stack {
    my(@Stack) = @_;
        
    my $var = '$FOO';
    my $did_arrow = 0;
    foreach my $entry (@Stack) {
        my $type = $entry->{type} || '';
        my $idx  = $entry->{'idx'};
        if ( $type eq 'HASH' ) {
            $var .= "->" unless $did_arrow++;
            $var .= "{$idx}";
        } elsif ( $type eq 'ARRAY' ) {
            $var .= "->" unless $did_arrow++;
            $var .= "[$idx]";
        } elsif ( $type eq 'REF' ) {
            $var = "\${$var}";
        }
    }

    my @vals = @{$Stack[-1]{vals}}[0,1];
    my @vars = ();

    my $out = "Structures begin differing at:\n";
    if ( $vals[0] =~ /\n/ || $vals[1] =~ /\n/ ) {
        ($vars[0] = $var) =~ s/\$FOO/\$got/;
        ($vars[1] = $var) =~ s/\$FOO/\$expected/;
        $out .= Text::Diff::diff(\$vals[0], \$vals[1], { 
            STYLE => $DIFF,
            FILENAME_A => $vars[0],
            FILENAME_B => $vars[1],
        })
    } else {
        foreach my $idx (0..$#vals) {
            my $val = $vals[$idx];
            $vals[$idx] = !defined $val ? 'undef'          :
                Test::More::_dne($val)    ? "Does not exist" :
                      ref $val      ? "$val"           :
                          "'$val'";
        }
        ($vars[0] = $var) =~ s/\$FOO/     \$got/;
        ($vars[1] = $var) =~ s/\$FOO/\$expected/;
        $out .= "$vars[0] = $vals[0]\n";
        $out .= "$vars[1] = $vals[1]\n";
        $out =~ s/^/    /msg;
    }
    return $out;
}

package Test::Trivial::Builder;
use base qw(Test::Builder);

#
# Overload the base Test::Builder _is_diag function
# so we can call Text::Diff on multiline statements.
#
sub _is_diag {
    my($self, $got, $type, $expect) = @_;
    return $self->SUPER::_is_diag($got,$type,$expect)
        unless defined $got && defined $expect;

    if( $got =~ /\n/ || $expect =~ /\n/ ) {
        return $self->diag(
            Text::Diff::diff(\$got, \$expect, { 
                STYLE => $DIFF,
                FILENAME_A => "got",
                FILENAME_B => "expected",
            })
          );
    }
    return $self->SUPER::_is_diag($got,$type,$expect);
}

#
# chop out the "at tests.t line 32" stuff since
# we add that above with warn_line_failure().
# I prefer ours since it prints out before
# the test header so emacs next-error will
# let me see what just ran
#
sub diag{ 
    my ($self, @msgs) = @_;
    $self->SUPER::diag(
        grep { !/\s+at\s+\S+\s+line\s+\d+[.]\n/ } @msgs
    );
}

package Test::Trivial::IO::Tee;
use base qw(IO::Tee);

sub TIEHANDLE {
    my $class = shift;
    my @handles = ();
    for my $handle ( @_ ) {
        unless( UNIVERSAL::isa($handle, "IO::Handle") ) {
            my $io = IO::Handle->new();
            $io->fdopen($handle->fileno(), "w");
            $io->autoflush(1);
            push @handles, $io;
        }
        else {
            $handle->autoflush(1);
            push @handles, $handle;
        }
    }
    return bless [@handles], $class;
}

sub log {
    shift->[0]->print(@_);
}

1;
