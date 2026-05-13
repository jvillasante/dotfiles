#!/usr/bin/perl
# rlwrap filter: when C-R is pressed inside an `nntp` session, replace
# readline's incremental search with an fzf picker over the session's
# rlwrap history buffer. Paired with an INPUTRC that contains:
#   "\C-r": rlwrap-hotkey
#
# Newest history line first, dedup keeping most recent, current input
# line prefilled as fzf query. Selection replaces the line buffer
# without auto-executing -- press Enter to send, edit first, or C-G to
# discard. FZF_DEFAULT_OPTS is honored for colors/height/layout.

use strict;
use warnings;
use lib '/usr/share/rlwrap/filters';
use RlwrapFilter;
use IPC::Open2;

my $filter = RlwrapFilter->new;
$filter->help_text("nntp-fzf-filter: bind C-R (via INPUTRC) to an fzf history picker.");
$filter->hotkey_handler(\&fzf_history);
$filter->run;

sub fzf_history {
    my ($keyseq, $prefix, $postfix, $history, $histpos) = @_;

    # \x12 == C-R. Pass anything else through unchanged so other filters
    # downstream can have a shot at it.
    return ($keyseq, $prefix, $postfix, $history, $histpos)
        unless $keyseq eq "\x12";

    # Newest first, drop empties, keep most-recent occurrence of dupes.
    my @lines = reverse split /\n/, ($history // '');
    my %seen;
    @lines = grep { length && !$seen{$_}++ } @lines;
    return ("", $prefix, $postfix, $history, $histpos) unless @lines;

    my $selected;
    my $rc = -1;
    eval {
        my $pid = open2(
            my $out, my $in,
            'fzf',
            '--no-sort',
            '--prompt=nntp> ',
            '--query=' . ($prefix // ''),
        );
        print $in $_, "\n" for @lines;
        close $in;
        local $/;
        $selected = <$out>;
        waitpid $pid, 0;
        $rc = $? >> 8;
    };

    # Cancelled (ESC/C-G), empty selection, or fzf missing -> keep current line.
    return ("", $prefix, $postfix, $history, $histpos)
        if $@ || $rc != 0 || !defined $selected;

    chomp $selected;
    return ("", $prefix, $postfix, $history, $histpos) if $selected eq '';

    # Replace the input line with the selection, cursor at end, no auto-submit.
    return ("", $selected, "", $history, $histpos);
}
