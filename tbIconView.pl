#!perl -w

use strict;
use Tk;

my $mw = new MainWindow;

my $file = Tk->findINC('ToolBar/tkIcons');
die "ERROR: Can't find tkIcons!\n" unless defined $file;

open my $fh, $file or die $!;

my %icons;

while (<$fh>) {
    chomp;
    my ($n, $d) = (split /:/)[0, 4];

    $icons{$n} = $mw->Photo($n, -data => $d);
}

close $fh;

my $selected = 'None';
$mw->Label(-textvariable => \$selected)->pack(qw/-side top/);

my $f = $mw->Frame->pack(qw/-fill both -expand 1/);

my $r = my $c = 0;
my %labels;

for my $n (sort keys %icons) {
    my $l = $f->Label(
		      -image       => $n,#$icons{$n},
		      )->grid(-column => $c,
			      -row    => $r,
			      );

    $labels{$n} = $l;

    $l->bind('<1>' => sub {
	if ($selected ne 'None') {
	    $labels{$selected}->configure(-borderwidth => 0, -bg => Tk::ACTIVE_BG);#defaultthing);
	}

	$selected = $n;
	$l->configure(-borderwidth => 2, -bg => 'white');
    });

    $c++;
    if ($c == 10) {
	$r++;
	$c = 0;
    }
}

MainLoop;
