package Tk::ToolBar;

use strict;
use Tk::Frame;
use Tk::Balloon;

use base qw/Tk::Frame/;
use Tk::widgets qw(Frame);

use Carp;

Construct Tk::Widget 'ToolBar';

use vars qw/$VERSION/;
$VERSION = 0.05;

my $edgeH = 24;
my $edgeW = 3;

my %sideToSticky = qw(
		      top    n
		      right  e
		      left   w
		      bottom s
		      );

my $packIn     = '';
my @allWidgets = ();
my %packIn;
my %containers;
my %isDummy;
my ($ox, $oy);

1;

sub ClassInit {
    my ($class, $mw) = @_;
    $class->SUPER::ClassInit($mw);
}

sub Populate {
    my ($self, $args) = @_;

    $self->SUPER::Populate($args);
    $self->{MW}     = $self->parent;
    $self->{SIDE}   = exists $args->{-side}          ? delete $args->{-side}          : 'top';
    $self->{STICKY} = exists $args->{-sticky}        ? delete $args->{-sticky}        : 'nsew';
    $self->{USECC}  = exists $args->{-cursorcontrol} ? delete $args->{-cursorcontrol} : 1;

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

    $self->_packEdge($edge);

    $self->ConfigSpecs(
		       -movable => [qw/METHOD movable Movable 1/],
		       -close   => [qw/METHOD close   Close   15/],
		      );

    push @allWidgets => $self;

    $containers{$self->{CONTAINER}} = $self;

    $self->{HIGHLIGHT} = $self->{MW}->Frame(-bg => 'white');
    $self->{BALLOON}   = $self->{MW}->Balloon;

    # check for Tk::CursorControl
    $self->{CC} = undef;
    if ($self->{USECC}) {
	local $^W = 0; # suppress message from Win32::API
	eval "require Tk::CursorControl";
	unless ($@) {
	    # CC is installed. Use it.
	    $self->{CC} = $self->{MW}->CursorControl;
	}
    }
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
    my $e    = shift;

    my $s    = $self->{SIDE};

    my ($pack, $pad, $nopad, $fill);

    if ($s eq 'top' or $s eq 'bottom') {
	$e->configure(-height => $edgeH, -width => $edgeW);
	$pack  = 'left';
	$pad   = '-padx';
	$nopad = '-pady';
	$fill  = 'y';
    } else {
	$e->configure(-height => $edgeW, -width => $edgeH);
	$pack  = 'top';
	$pad   = '-pady';
	$nopad = '-padx';
	$fill  = 'x';
    }

    if (exists $self->{SEPARATORS}{$e}) {
	$e->configure(-cursor => $pack eq 'left' ? 'sb_h_double_arrow' : 'sb_v_double_arrow');
	$self->{SEPARATORS}{$e}->pack(-side   => $pack,
				      -fill   => $fill);
    }

    $e->pack(-side  => $pack, $pad => 5,
	     $nopad => 0,  -expand => 0);
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

    $e->bind('<1>' => sub { $self->{CC}->confine($self->{MW}) })
	if defined $self->{CC};

    $e->bind('<B1-Motion>' => sub {
		 $self->{ISMOVED} = 1;

		 my ($x, $y) = ($self->pointerx - $self->{MW}->rootx,
				$self->pointery - $self->{MW}->rooty);

		 if (defined $self->{DUMMY}) {
		     $self->{DUMMY}->place('-x' => $x,
					   '-y' => $y,
					  );
		 } else {
		     $self->{DUMMY} = $self->{MW}->Frame(
							 qw/
							 -borderwidth 2
							 -relief ridge
							 /,
							   )->place('-x' => $x,
								    '-y' => $y,
								    );
		     $self->{DUMMY}->raise;
		     $self->{CPACKINFO} = [$self->{CONTAINER}->packInfo];
		     $self->{SPACKINFO} = $self->manager ? [$self->packInfo] : undef;
		     $self->{CONTAINER}->packForget;
		     $self->packForget;

		     $self->{CONTAINER}->pack(-in => $self->{DUMMY});
		     $self->{CONTAINER}->raise;
		     ref($_) eq 'Tk::Frame' && $_->raise for $self->{CONTAINER}->packSlaves;
		 }

		 $self->{HIGHLIGHT}->packForget;
		 return unless $self->_whereAmI;

		 # highlight the close edge.
		 my ($op, $pp) = $self->{SIDE} =~ /top|bottom/ ?
		     ([qw/-height 5/], [qw/-fill x/]) : ([qw/-width 5/], [qw/-fill y/]);

		 $self->{HIGHLIGHT}->configure(@$op);
		 $self->{HIGHLIGHT}->pack(-side => $self->{SIDE},
					  @$pp);
	     });

    $e->bind('<ButtonRelease-1>' => sub {
	return unless $self->{ISMOVED};

	$self->{ISMOVED} = 0;
	$self->{DUMMY}->destroy;
	$self->{DUMMY}   = undef;
	$self->{CC}->free($self->{MW}) if defined $self->{CC};

	return unless $self->_whereAmI(1);
	$self->{HIGHLIGHT}->packForget;

	# repack everything now.
	my @allSlaves = grep {$_ ne $e} $self->{CONTAINER}->packSlaves;
	$_   ->packForget for $self, @allSlaves, $self->{CONTAINER};

	$self->_packSelf;
	$self->_packEdge($e);
	$self->_packWidget($_) for @allSlaves;
    });
}

sub _whereAmI {
    my $self = shift;
    my $flag = shift;

    my $e       = $self->_edge;
    my $p       = $e->parent;
    my ($x, $y) = ($self->pointerx - $self->{MW}->rootx,
		   $self->pointery - $self->{MW}->rooty);

    my $x2 = $x + $self->{CONTAINER}->width;
    my $y2 = $y + $self->{CONTAINER}->height;

    my $w  = $self->{MW}->Width;
    my $h  = $self->{MW}->Height;

    # bound check
    $x     = 1      if $x  <= 0;
    $y     = 1      if $y  <= 0;
    $x     = $w - 1 if $x  >= $w;
    $y     = $h - 1 if $y  >= $h;

    $x2    = 0      if $x2 <= 0;
    $y2    = 0      if $y2 <= 0;
    $x2    = $w - 1 if $x2 >= $w;
    $y2    = $h - 1 if $y2 >= $h;

    my $dx = 0;
    my $dy = 0;

    my $close = $self->{CLOSE};
	
    if    ($x       < $close) { $dx = $x }
    elsif ($w - $x2 < $close) { $dx = $x2 - $w }
    #elsif ($w - $x < $close) { $dx = $x - $w }

    if    ($y       < $close) { $dy = $y }
    elsif ($h - $y2 < $close) { $dy = $y2 - $h }
    #elsif ($h - $y < $close) { $dy = $y - $h }

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
	return undef unless $self->{STICKY} =~ /$sideToSticky{$newSide}/;

	$self->{SIDE} = $newSide;
	return $newSide;
    } elsif ($flag) {
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

	unless ($packIn) {
	    # Repack everything the way it was.
	    $self->_restoreMe;
	    return undef;
	}

	$self->{SIDE} = $packIn->{SIDE};
    } else {
	return undef;
    }

    return 1;
}

sub _restoreMe {
    my $self = shift;

    $self->pack(@{$self->{SPACKINFO}}) if defined $self->{SPACKINFO};
    $self->{CONTAINER}->pack(@{$self->{CPACKINFO}});
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

    my $m = delete $args{-tip}         || '';
    my $x = delete $args{-accelerator} || '';

    my $b = $self->{CONTAINER}->$type(%args,
				      -relief  => 'flat',
				      );

    $self->_createButtonBindings($b);

    push @{$self->{WIDGETS}} => $b;
    $self->_packWidget($b);

    $self->{BALLOON}->attach($b, -balloonmsg => $m) if $m;
    $self->{MW}->bind($x => [$b, 'invoke'])         if $x;

    # change the bind tags.
    #$b->bindtags([$b, ref($b), $b->toplevel, 'all']);

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
    my %args = @_;

    my $m = delete $args{-tip} || '';
    my $l = $self->{CONTAINER}->Entry(%args, -width => 5);

    push @{$self->{WIDGETS}} => $l;

    $self->_packWidget($l);
    $self->{BALLOON}->attach($b, -balloonmsg => $m) if $m;

    return $l;
}

sub ToolLabEntry {
    my $self = shift;
    my %args = @_;

    require Tk::LabEntry;
    my $m = delete $args{-tip} || '';
    my $l = $self->{CONTAINER}->LabEntry(%args, -width => 5);

    push @{$self->{WIDGETS}} => $l;

    $self->_packWidget($l);
    $self->{BALLOON}->attach($b, -balloonmsg => $m) if $m;

    return $l;
}

sub separator {
    my $self = shift;

    my $f    = $self->{CONTAINER}->Frame(qw/-width 0
					 -height 0/);

    my $sep  = $self->{CONTAINER}->Frame(qw/
					 -borderwidth 5
					 -relief sunken
					 /);

    $isDummy{$f} = $self->{SIDE};

    push @{$self->{WIDGETS}} => $sep;
    $self->{SEPARATORS}{$sep} = $f;
    $self->_packWidget($sep);

    $self->_createSeparatorBindings($sep);

    return 1;
}

sub _packWidget {
    my ($self, $b) = @_;

    return $self->_packEdge($b) if exists $self->{SEPARATORS}{$b};

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

	$top->_packEdge($e);
	$top->_packWidget($_) for @allSlaves;
    }

    if (exists $isDummy{$b}) { # swap width/height if we need to.
	my ($w, $h);

	if ($side eq 'left' && $isDummy{$b} =~ /left|right/) {
	    $w = 0;
	    $h = $b->height;
	} elsif ($side eq 'top'  && $isDummy{$b} =~ /top|bottom/) {
	    $w = $b->width;
	    $h = 0;
	}

	$b->configure(-width => $h, -height => $w) if defined $w;
	$isDummy{$b} = $self->{SIDE};
    }

    $b->pack(-side => $side, $pad => 4, $nopad => 0, @extra);
}

sub _packWidget_old {
    my ($self, $b) = @_;

    return $self->_packEdge($b) if exists $self->{SEPARATORS}{$b};

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

	$top->_packEdge($e);
	$top->_packWidget($_) for @allSlaves;
    }

    $b->pack(-side => $side, $pad => 4, $nopad => 0, @extra);
}

sub _createButtonBindings {
    my ($self, $b) = @_;

    $b->bind('<Enter>' => sub { $b->configure(qw/-relief raised/) });
    $b->bind('<Leave>' => sub { $b->configure(qw/-relief flat/)   });
}

sub _createSeparatorBindings {
    my ($self, $s) = @_;

    $s->bind('<1>'         => sub {
	$ox = $s->XEvent->x;
	$oy = $s->XEvent->y;
    });

    $s->bind('<B1-Motion>' => sub {
	my $x = $s->XEvent->x;
	my $y = $s->XEvent->y;

	my $f = $self->{SEPARATORS}{$s};

	if ($self->{SIDE} =~ /top|bottom/) {
	    my $dx = $x - $ox;

	    my $w  = $f->width + $dx;
	    $w     = 0 if $w < 0;

	    $f->GeometryRequest($w, $f->height);
	} else {
	    my $dy = $y - $oy;

	    my $h  = $f->height + $dy;
	    $h     = 0 if $h < 0;

	    $f->GeometryRequest($f->width, $h);
	}
    });
}

sub Button   { goto &ToolButton }
sub Label    { goto &ToolLabel }
sub Entry    { goto &ToolEntry }
sub LabEntry { goto &ToolLabEntry }

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

        $tb->ToolButton  (-text  => 'Button',
                          -tip   => 'tool tip',
                          -command => sub { print "hi\n" });
        $tb->ToolLabel   (-text  => 'A Label');
        $tb->Label       (-text  => 'Another Label');
        $tb->ToolLabEntry(-label => 'A LabEntry',
                          -labelPack => [-side => "left",
                                         -anchor => "w"]);

        MainLoop;

=head1 DESCRIPTION

This module implements a dockable toolbar. It is in the same spirit as the
"short-cut" toolbars found in most major applications, such as most web browsers
and text editors (where you find the "back" or "save" and other shortcut buttons).

Buttons of any type (regular, menu, check, radio) can be created inside this widget.
You can also create Label, Entry and LabEntry widgets.
Moreover, the ToolBar itself can be made dockable, such that it can be dragged to
any edge of your window. Dragging is done in "real-time" so that you can see the
contents of your ToolBar as you are dragging it. Furthermore, if you are close to
a stickable edge, a visual indicator will show up along that edge to guide you.
Also, multiple ToolBars are embeddable inside each other.

If you drag a ToolBar to within 15 pixels of an edge, it will stick to that
edge. If the ToolBar is further than 15 pixels away from an edge, but you
release it over another ToolBar widget, then it will be embedded inside the
second ToolBar. You can "un-embed" an embedded ToolBar simply by dragging it
out. You can change the 15 pixel limit using the B<-close> option.

Tk::ToolBar attempts to use Tk::CursorControl if it's already installed on
the system. You can further control this using the I<-cursorcontrol> option.
See L</PREREQUISITES>.

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

=item B<-cursorcontrol>

This option specifies whether to use Tk::CursorControl to confine the cursor
during dragging. The value must be either 1 or 0. The default is 1 which
checks for Tk::CursorControl and uses it if present.

=back

=head1 WIDGET METHODS

The following methods are used to create widgets that are placed inside
the ToolBar. Widgets are ordered in the same order they are created, left to right.

For all widgets, except Labels, a tooltip can be specified via the B<-tip> option.

=over 4

=item I<$ToolBar>-E<gt>B<ToolButton>(?-type => I<buttonType>,? I<options>)

=item I<$ToolBar>-E<gt>B<Button>(?-type => I<buttonType>,? I<options>)

This method creates a new Button inside the ToolBar.
The I<-type> option can be used to specify
what kind of button to create. Can be one of 'Button', 'Checkbutton', 'Menubutton', or
'Radiobutton'. A tooltip message can be specified via the -tip option.
An accelerator binding can be specified using the -accelerator option.
The value of this option is any legal binding sequence as defined
in L<bind>. For example,
C<-accelerator =E<gt> 'E<lt>fE<gt>'> will invoke the button when the 'f' key is pressed.
Any other options will be passed directly to the constructor
of the button. The Button object is returned.

=item I<$ToolBar>-E<gt>B<ToolLabel>(I<options>)

=item I<$ToolBar>-E<gt>B<Label>(I<options>)

This method creates a new Label inside the ToolBar.
Any options will be passed directly to the constructor
of the label. The Label object is returned.

=item I<$ToolBar>-E<gt>B<ToolEntry>(I<options>)

=item I<$ToolBar>-E<gt>B<Entry>(I<options>)

This method creates a new Entry inside the ToolBar.
A tooltip message can be specified via the -tip option.
Any other options will be passed directly to the constructor
of the entry. The Entry object is returned.

=item I<$ToolBar>-E<gt>B<ToolLabEntry>(I<options>)

=item I<$ToolBar>-E<gt>B<LabEntry>(I<options>)

This method creates a new LabEntry inside the ToolBar.
A tooltip message can be specified via the -tip option.
Any other options will be passed directly to the constructor
of the labentry. The LabEntry object is returned.
In horizontal ToolBars, the label of the LabEntry widget
will be packed to the left of the entry. On vertical
ToolBars, the label will be packed on top of the entry.

=item I<$ToolBar>-E<gt>B<separator>

This method inserts a separator. Separators are movable.

=back

=head1 BUGS

Not really a bug, but a feature ;-)
The ToolBar widget assumes that you use I<pack> in its parent.
Actually, it will I<pack()> itself inside its parent. If you are using
another geometry manager, then you I<MIGHT> get some weird behaviour.
I have tested it very quickly, and found no surprises, but let me know
if you do.


=head1 TODO

I have implemented everything I wanted, and then some.
Here are things that were requested, but are not implemented yet.
If you want more, send me requests.

=over 4

=item o Allow buttons to be "tied" to menu items. Somewhat taken care of
with the -accelerator method for buttons.

=item o Include some built-in pixmaps for some of the more common ToolButtons,
like "Save", "New", "Open", etc ..

=item o Implement Drag-n-Drop to be able to move Tool* widgets interactively.


=back


=head1 PREREQUISITES

Tk::ToolBar uses only core pTk modules. So you don't need any special
prerequisites. But, if Tk::CursorControl is installed on your system,
then Tk::ToolBar will use it to confine the cursor to your window when
dragging ToolBars (unless you tell it not to).

Note also that Tk::CursorControl is defined as a prerequisite in
Makefile.PL. So, during installation you might get a warning saying:

C<Warning: prerequisite Tk::CursorControl failed to load ...>

if you don't have it installed. You can ignore this warning if you
don't want to install Tk::CursorControl. Tk::ToolBar will continue
to work properly.

=head1 INSTALLATION

Either the usual:

	perl Makefile.PL
	make
	make install

or just stick it somewhere in @INC where perl can find it. It's in pure Perl.

=head1 ACKNOWLEDGEMENTS

The following people have given me helpful comments and bug reports to keep me busy:
Chris Whiting, Jack Dunnigan and Robert Brooks.

=head1 AUTHOR

Ala Qumsieh I<aqumsieh@cpan.org>

=head1 COPYRIGHTS

This module is distributed under the same terms as Perl itself.

=cut

