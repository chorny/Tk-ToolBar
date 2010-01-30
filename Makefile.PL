# -*- mode: perl; c-basic-offset: 4; indent-tabs-mode: nil; -*-
use 5.005;
use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
	      NAME             => 'Tk::ToolBar',
	      VERSION_FROM     => 'ToolBar.pm',
	      PREREQ_PM        => {
	        Tk::CursorControl => 0,
	        'Test' => 0,
	      },
	      ($] >= 5.005 ?
	       (ABSTRACT_FROM  => 'ToolBar.pm',
		AUTHOR         => 'Ala Qumsieh <aqumsieh@cpan.org>') : ()),
	      PM               => {
				   'ToolBar.pm'       => '$(INST_LIB)/Tk/ToolBar.pm',
				   'toolbar.pl'       => ($] >= 5.005 ?
							  '$(INST_ARCHLIB)' :
							  '$(INST_LIB)') .
				   '/Tk/demos/widtrib/toolbar.pl',
				   'ToolBar/tkIcons'  => '$(INST_LIB)/Tk/ToolBar/tkIcons',
				  },
	     );
