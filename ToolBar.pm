package Tk::ToolBar;

use strict;
use base qw/Tk::Frame/;

Construct Tk::Widget 'ToolBar';

our $VERSION = 0.01;

my $edgeH = 32;
my $edgeW = 3;

1;

sub ClassInit {
    my ($class, $mw) = @_;
    $class->SUPER::ClassInit($mw);
}

sub Populate {
    my ($self, $args) = @_;

    $self->SUPER::Populate($args);

    $self->{SIDE}  = exists $args->{-side} ? delete $args->{-side} : 'top';

    $self->_packSelf;

    my $edge = $self->Frame(qw/
			    -borderwidth 5
			    -relief ridge
			    /);
			    #/)->pack(qw/-side left -expand 0 -padx 5/);

    $self->{EDGE} = $edge;

    $self->_packEdge;

    $self->ConfigSpecs(
		       -movable => [qw/METHOD movable Movable 1/],
		       -close   => [qw/METHOD close   Close   20/],
		      );
}

sub _packSelf {
    my $self = shift;

    my $side = $self->{SIDE};
    my $fill = 'y';
    if ($side eq 'top' or $side eq 'bottom') { $fill = 'x' }

    # force a certain look! for now.
    my $slave = ($self->parent->packSlaves)[0];

    $self->configure(qw/-relief raised -borderwidth 1/);
    $self->pack(-side => $side, -fill => $fill,
		$slave ? (-before => $slave) : ()
		);
}

sub _packEdge {
    my $self = shift;

    my $e    = $self->_edge;
    my $s    = $self->{SIDE};

    my ($pack, $pad);

    if ($s eq 'top' or $s eq 'bottom') {
	$e->configure(-height => $edgeH, -width => $edgeW);
	$pack = 'left';
	$pad  = '-padx';
    } else {
	$e->configure(-height => $edgeW, -width => $edgeH);
	$pack = 'top';
	$pad  = '-pady';
    }

    $e->pack(-side => $pack, $pad, 5, -expand => 0);
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
		 my ($x, $y) = ($self->pointerx - $p->parent->rootx,
				$self->pointery - $p->parent->rooty);

		 if (defined $self->{DUMMY}) {
		     $self->{DUMMY}->place('-x' => $x,
					   '-y' => $y,
					  );
		 } else {
		     $self->{DUMMY} = $self->parent->Frame(
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
	my ($x, $y) = ($self->pointerx - $p->parent->rootx,
		       $self->pointery - $p->parent->rooty);

	my $w = $p->parent->Width;
	my $h = $p->parent->Height;

	my $dx = 0;
	my $dy = 0;

	my $close = $self->{CLOSE};

	if    ($x      < $close) { $dx = $x }
	elsif ($w - $x < $close) { $dx = $x - $w }

	if    ($y      < $close) { $dy = $y }
	elsif ($h - $y < $close) { $dy = $y - $h }

	return unless $dx || $dy;

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

	$self->{SIDE} = $newSide;

	# repack everything now.
	$_->packForget for $self->packSlaves, $self;
	$self->_packSelf;

	$self->_packEdge;
	$self->_packButton($_) for @{$self->{BUTTONS}};
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

    my $b = $self->$type(%args,
			 -relief  => 'flat',
			 -state   => 'active',
			 );

    $self->_createButtonBindings($b);

    push @{$self->{BUTTONS}} => $b;

    $self->_packButton($b);

    return $b;
}

sub _packButton {
    my ($self, $b) = @_;

    my ($side, $fill) = $self->{SIDE} =~ /^top$|^bottom$/ ? 
	qw/left y/ : qw/top x/;

    $b->pack(-side => $side, -fill => $fill);
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

        $tb->ToolButton(-text => 'test');
        $tb->ToolButton(-text => 'Me');

=head1 DESCRIPTION

This module implements a dockable toolbar. It is in the same spirit as the
"short-cut" toolbars found in most major applications, such as most web browsers
and text editors (where you find the "back" or "save" buttons).

Buuttons of any type (regular, check, radio) can be packed inside this widget.
Moreover, the ToolBar itself can be made dockable, such that it can be dragged to
any edge of your window.

The ToolBar is supposed to be created as a child of a Toplevel (MainWindow is
a Toplevel widget). You are free to experiment otherwise, but expect the unexpected :-)

=head1 WIDGET-SPECIFIC OPTIONS

The ToolBar widget takes the following arguments:

=over 4

=item B<-side>

This option tells the ToolBar what edge to stick to. Can be one of 'top', 'bottom',
'left' or 'right'. Defaults to 'top'. This option can be set only during object
creation.

=item B<-movable>

This option specifies whether the ToolBar is dockable or not. A dockable ToolBar
can be dragged around with the mouse to any edge of the window. Default is 1.

=item B<-close>

This options specifies, in pixels, how close we have to drag the ToolBar an edge for the
ToolBar to stick to it.

=back

=head1 WIDGET METHODS

=over 4

=item I<$ToolBar>-E<gt>B<ToolButton>(?-type => I<buttonType>,? I<options>)

This method creates a new button inside the ToolBar. The buttons are ordered
in the same order they're created. The I<-type> option can be used to specify
what kind of button to create. Can be on of 'Button', 'Checkbutton', or
'Radiobutton'. Any other options will be passed directly to the constructor
of the button. The button object is returned.

=back

=head1 BUGS

Not really a bug, but a feature ;-)
The ToolBar widget assumes that you use I<pack> in your main window.
Actually, it will I<pack> itself in your window. If you are using
another geometry manager, then you I<MIGHT> get some weird behaviour.
I have tested it very quickly, and found no surprises, but let me know
if you do.

=head1 INSTALLATION

Either the usual:

	perl Makefile.PL
	make
	make install

or just stick it somewhere in @INC where perl can find it. It's in pure Perl.

=head1 AUTHOR

Ala Qumsieh I<aqumsieh@cpan.org>

=head1 COPYRIGHTS

This module is distributed under the same terms as Perl itself.

=cut

