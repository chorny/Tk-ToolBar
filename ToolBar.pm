package Tk::ToolBar;

use strict;
use base qw/Tk::Frame/;
use Tk::widgets qw(Frame);

use Carp;

Construct Tk::Widget 'ToolBar';

use vars qw/$VERSION/;
$VERSION = 0.03;

my $edgeH = 32;
my $edgeW = 3;

my %sideToSticky = qw(
		      bottom s
		      right  e
		      left   w
		      top    n
		      );

my $packIn     = '';
my @allWidgets = ();
my %packIn;
my %containers;

1;

sub ClassInit {
    my ($class, $mw) = @_;
    $class->SUPER::ClassInit($mw);
}

sub Populate {
    my ($self, $args) = @_;

    $self->SUPER::Populate($args);
    $self->{MW}     = $self->parent;
    $self->{SIDE}   = exists $args->{-side}   ? delete $args->{-side}   : 'top';
    $self->{STICKY} = exists $args->{-sticky} ? delete $args->{-sticky} : 'nsew';

    unless ($self->{STICKY} =~ /$sideToSticky{$self->{SIDE}}/) {
	croak "can't place '$self->{STICKY}' toolbar on '$self->{SIDE}' side";
    }

    $self->{CONTAINER} = $self->{MW}->Frame;
    $self->_packSelf;

    my $edge = $self->{CONTAINER}->Frame(qw/
					 -borderwidth 5
					 -relief ridge
					 /);

    $self->{EDGE} = $edge;

    $self->_packEdge;

    $self->ConfigSpecs(
		       -movable => [qw/METHOD movable Movable 1/],
		       -close   => [qw/METHOD close   Close   15/],
		      );

    push @allWidgets => $self;

    $containers{$self->{CONTAINER}} = $self;
}

sub _packSelf {
    my $self = shift;

    my $side = $self->{SIDE};
    my $fill = 'y';
    if ($side eq 'top' or $side eq 'bottom') { $fill = 'x' }

    if ($packIn && $packIn != $self) {
	my $side = $packIn->{SIDE} =~ /top|bottom/ ? 'left' : 'top';

	$self->{CONTAINER}->pack(-in => $packIn->{CONTAINER},
				 -side => $side,
				 -anchor => ($fill eq 'x' ? 'w' : 'n'),
				 -expand => 0);
	$self->{CONTAINER}->raise;
	$packIn{$self->{CONTAINER}} = $packIn->{CONTAINER};
    } else {
	# force a certain look! for now.
	my $slave = ($self->{MW}->packSlaves)[0];

	$self->configure(qw/-relief raised -borderwidth 1/);
	$self->pack(-side => $side, -fill => $fill,
		    $slave ? (-before => $slave) : ()
		    );

	$self->{CONTAINER}->pack(-in => $self,
				 -anchor => ($fill eq 'x' ? 'w' : 'n'),
				 -expand => 0);

	$packIn{$self->{CONTAINER}} = $self;
    }
}

sub _packEdge {
    my $self = shift;

    my $e    = $self->_edge;
    my $s    = $self->{SIDE};

    my ($pack, $pad, $nopad);

    if ($s eq 'top' or $s eq 'bottom') {
	$e->configure(-height => $edgeH, -width => $edgeW);
	$pack  = 'left';
	$pad   = '-padx';
	$nopad = '-pady';
    } else {
	$e->configure(-height => $edgeW, -width => $edgeH);
	$pack  = 'top';
	$pad   = '-pady';
	$nopad = '-padx';
    }

    $e->pack(-side => $pack, $pad => 5, $nopad => 0, -expand => 0);
}

sub close {
    my ($self, $value) = @_;

    $self->{CLOSE} = $value if defined($value) && $value > 0;

    return $self->{CLOSE};
}

sub movable {
    my ($self, $value) = @_;

    if (defined $value) {
	$self->{ISMOVABLE} = $value;
	my $e = $self->_edge;

	if ($value) {
	    $e->configure(qw/-cursor fleur/);
	    $self->_enableEdge($e);
	} else {
	    $e->configure(-cursor => undef);
	    $self->_disableEdge($e);
	}
    }

    return $self->{ISMOVABLE};
}

sub _enableEdge {
    my ($self, $e) = @_;

    $e->bind('<B1-Motion>' => sub {
		 $self->{ISMOVED} = 1;

		 my $p       = $e->parent;
		 my ($x, $y) = ($self->pointerx - $self->{MW}->rootx,
				$self->pointery - $self->{MW}->rooty);

		 if (defined $self->{DUMMY}) {
		     $self->{DUMMY}->place('-x' => $x,
					   '-y' => $y,
					  );
		 } else {
		     $self->{DUMMY} = $self->{MW}->Frame(
							 qw/-height 30
							 -width 50
							 -borderwidth 2
							 -relief ridge
							 /,
							   )->place('-x' => $x,
								    '-y' => $y,
								    );
		     $self->{DUMMY}->raise;
		 }
	     });

    $e->bind('<ButtonRelease-1>' => sub {
	return unless $self->{ISMOVED};

	$self->{ISMOVED} = 0;

	$self->{DUMMY}->destroy;
	$self->{DUMMY} = undef;

	my $p       = $e->parent;
	my ($x, $y) = ($self->pointerx - $self->{MW}->rootx,
		       $self->pointery - $self->{MW}->rooty);

	my $w  = $self->{MW}->Width;
	my $h  = $self->{MW}->Height;

	# bound check
	$x     = 1      if $x < 0;
	$y     = 1      if $y < 0;
	$x     = $w - 1 if $x > $w;
	$y     = $h - 1 if $y > $h;

	my $dx = 0;
	my $dy = 0;

	my $close = $self->{CLOSE};
	
	if    ($x      < $close) { $dx = $x }
	elsif ($w - $x < $close) { $dx = $x - $w }

	if    ($y      < $close) { $dy = $y }
	elsif ($h - $y < $close) { $dy = $y - $h }

	$packIn       = '';
	if ($dx || $dy) {
	    my $newSide;
	    if ($dx && $dy) {
		# which is closer?
		if (abs($dx) < abs($dy)) {
		    $newSide = $dx > 0 ? 'left' : 'right';
		} else {
		    $newSide = $dy > 0 ? 'top' : 'bottom';
		}
	    } elsif ($dx) {
		$newSide = $dx > 0 ? 'left' : 'right';
	    } else {
		$newSide = $dy > 0 ? 'top' : 'bottom';
	    }

	    # make sure we're stickable on that side.
	    return unless $self->{STICKY} =~ /$sideToSticky{$newSide}/;

	    $self->{SIDE} = $newSide;
	} else {
	    # check for overlaps.
	    for my $w (@allWidgets) {
		next if $w == $self;

		my $x1 = $w->x;
		my $y1 = $w->y;
		my $x2 = $x1 + $w->width;
		my $y2 = $y1 + $w->height;
		
		if ($x > $x1 and $y > $y1 and $x < $x2 and $y < $y2) {
		    $packIn = $w;
		    last;
		}
	    }

	    return unless $packIn;
	    $self->{SIDE} = $packIn->{SIDE};
	}
	# repack everything now.
	my @allSlaves = grep {$_ ne $e} $self->{CONTAINER}->packSlaves;
	$_   ->packForget for $self, @allSlaves, $self->{CONTAINER};

	$self->_packSelf;
	$self->_packEdge;
	$self->_packWidget($_) for @allSlaves;
    });
}

sub _disableEdge {
    my ($self, $e) = @_;

    $e->bind('<B1-Motion>'       => undef);
    $e->bind('<ButtonRelease-1>' => undef);
}

sub _edge {
    $_[0]->{EDGE};
}

sub ToolButton {
    my $self = shift;
    my %args = @_;

    my $type = delete $args{-type} || 'Button';

    unless ($type eq 'Button' or
	    $type eq 'Checkbutton' or
	    $type eq 'Menubutton' or
	    $type eq 'Radiobutton') {

	croak "toolbutton can be only 'Button', 'Menubutton', 'Checkbutton', or 'Radiobutton'";
    }

    my $b = $self->{CONTAINER}->$type(%args,
				      -relief  => 'flat',
				      );

    $self->_createButtonBindings($b);

    push @{$self->{WIDGETS}} => $b;

    $self->_packWidget($b);

    return $b;
}

sub ToolLabel {
    my $self = shift;

    my $l = $self->{CONTAINER}->Label(@_);

    push @{$self->{WIDGETS}} => $l;

    $self->_packWidget($l);

    return $l;
}

sub ToolEntry {
    my $self = shift;

    if ($self->{STICKY} =~ /[ew]/) {
	croak "can create toolentry only on 'ns', 'n' or 's' toolbar";
    }

    my $l = $self->{CONTAINER}->Entry(@_, -width => 5);

    push @{$self->{WIDGETS}} => $l;

    $self->_packWidget($l);

    return $l;
}

sub ToolLabEntry {
    my $self = shift;

    require Tk::LabEntry;
    my $l = $self->{CONTAINER}->LabEntry(@_, -width => 5);

    push @{$self->{WIDGETS}} => $l;

    $self->_packWidget($l);

    return $l;
}

sub _packWidget {
    my ($self, $b) = @_;

    my ($side, $pad, $nopad) = $self->{SIDE} =~ /^top$|^bottom$/ ? 
	qw/left -padx -pady/ : qw/top -pady -padx/;

    if (ref($b) eq 'Tk::LabEntry') {
	$b->configure(-labelPack => [-side => $side]);
    }

    my @extra;
    if (exists $packIn{$b}) {
	@extra = (-in => $packIn{$b});

	# repack everything now.
	my $top = $containers{$b};
	$top->{SIDE} = $self->{SIDE};

	my $e = $top->_edge;
	my @allSlaves = grep {$_ ne $e} $b->packSlaves;
	$_   ->packForget for @allSlaves;

	$top->_packEdge;
	$top->_packWidget($_) for @allSlaves;
    }

    $b->pack(-side => $side, $pad => 4, $nopad => 0, @extra);
}

sub _createButtonBindings {
    my ($self, $b) = @_;

    $b->bind('<Enter>' => sub { $b->configure(qw/-relief raised/) });
    $b->bind('<Leave>' => sub { $b->configure(qw/-relief flat/) });
}

__END__

=pod

=head1 NAME

Tk::ToolBar - A toolbar widget for Perl/Tk

=for category Tk Widget Classes

=head1 SYNOPSIS

        use Tk;
        use Tk::ToolBar;

        my $mw = new MainWindow;
        my $tb = $mw->ToolBar(qw/-movable 1 -side top/);

        $tb->ToolButton(-text  => 'Button',
                        -command => sub { print "hi\n" });
        $tb->Label     (-text  => 'A Label');
        $tb->LabEntry  (-label => 'A LabEntry',
                        -labelPack => [-side => "left", -anchor => "w"]);

=head1 DESCRIPTION

This module implements a dockable toolbar. It is in the same spirit as the
"short-cut" toolbars found in most major applications, such as most web browsers
and text editors (where you find the "back" or "save" and other shortcut buttons).

Buuttons of any type (regular, menu, check, radio) can be created inside this widget.
You can also create Label, Entry and LabEntry widgets.
Moreover, the ToolBar itself can be made dockable, such that it can be dragged to
any edge of your window. Multiple ToolBars are embeddable inside each other.

If you drag a ToolBar to within 15 pixels of an edge, it will stick to that
edge. If, the ToolBar is further than 15 pixels away from an edge, but you
release it over another ToolBar widget, then it will be embedded inside the
second ToolBar. You can "un-embed" an embedded ToolBar simply by dragging it
out. You can change the 15 pixel limit using the B<-close> option.

The ToolBar is supposed to be created as a child of a Toplevel (MainWindow is
a Toplevel widget) or a Frame. You are free to experiment otherwise,
but expect the unexpected :-)

=head1 WIDGET-SPECIFIC OPTIONS

The ToolBar widget takes the following arguments:

=over 4

=item B<-side>

This option tells the ToolBar what edge to I<initially> stick to. Can be one of 'top', 'bottom',
'left' or 'right'. Defaults to 'top'. This option can be set only during object
creation. Default is 'top'.

=item B<-movable>

This option specifies whether the ToolBar is dockable or not. A dockable ToolBar
can be dragged around with the mouse to any edge of the window, subject to the
sticky constraints defined by I<-sticky>. Default is 1.

=item B<-close>

This option specifies, in pixels, how close we have to drag the ToolBar an edge for the
ToolBar to stick to it. Default is 15.

=item B<-sticky>

This option specifies which sides the toolbar is allowed to stick to. The value
must be a string of the following characters 'nsew'. A string of 'ns' means that
the ToolBar can only stick to the north (top) or south (bottom) sides. Defaults to
'nsew'. This option can be set only during object creation.

=back

=head1 WIDGET METHODS

The following methods are used to create widgets that are placed inside
the ToolBar. Widgets are ordered in the same order they are created.

=over 4

=item I<$ToolBar>-E<gt>B<ToolButton>(?-type => I<buttonType>,? I<options>)

This method creates a new Button inside the ToolBar.
The I<-type> option can be used to specify
what kind of button to create. Can be on of 'Button', 'Checkbutton', 'Menubutton', or
'Radiobutton'. Any other options will be passed directly to the constructor
of the button. The Button object is returned.

=item I<$ToolBar>-E<gt>B<ToolLabel>(I<options>)

This method creates a new Label inside the ToolBar.
Any options will be passed directly to the constructor
of the label. The Label object is returned.

=item I<$ToolBar>-E<gt>B<ToolEntry>(I<options>)

This method creates a new Entry inside the ToolBar.
Any options will be passed directly to the constructor
of the entry. The Entry object is returned.

=item I<$ToolBar>-E<gt>B<ToolLabEntry>(I<options>)

This method creates a new LabEntry inside the ToolBar.
Any options will be passed directly to the constructor
of the labentry. The LabEntry object is returned.
In horizontal ToolBars, the label of the LabEntry widget
will be packed to the left of the entry. On vertical
ToolBars, the label will be packed on top of the entry.

=back

=head1 BUGS

Not really a bug, but a feature ;-)
The ToolBar widget assumes that you use I<pack> in its parent.
Actually, it will I<pack()> itself inside its parent. If you are using
another geometry manager, then you I<MIGHT> get some weird behaviour.
I have tested it very quickly, and found no surprises, but let me know
if you do.


=head1 TODO

=over 4

=item o

I've implemented everything I wanted to implement. If you want more, send me requests.

=back


=head1 INSTALLATION

Either the usual:

	perl Makefile.PL
	make
	make install

or just stick it somewhere in @INC where perl can find it. It's in pure Perl.

=head1 ACKNOWLEDGEMENTS

Many thanks go to Chris Whiting who has tested this widget extensively and
gave valuable comments and suggestions. Thanks are also due to Jack
Dunnigan for showing me the light; Jack, you da man!

=head1 AUTHOR

Ala Qumsieh I<aqumsieh@cpan.org>

=head1 COPYRIGHTS

This module is distributed under the same terms as Perl itself.

=cut

