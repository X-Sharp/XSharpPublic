///////////////////////////////////////////////////////////////////////////
// VulcanStdDefs.vh
//
// Copyright (c) Grafx Database Systems, Inc.  All rights reserved.
//
// Vulcan.NET Standard Preprocessor Directives
//
// This file is automatically included in every source file
// unless /nostddefs is used.
//
// Caution: this file should not be modified as it will be
// overwritten during any product updates or a repair reinstall.
//

// Unsupported VO keywords
// #command PUBLIC <x>      => #error PUBLIC is not supported
#command MEMVAR <x>      => #error MEMVAR is not supported
#command PARAMETERS <x>  => #error PARAMETERS is not supported
#command EXTERNAL <x>    => #error EXTERNAL is not supported

// Harmless VO keywords
#command DECLARE <*x*>   => 

// Unsupported VO UDCs
#command SET PROCLINE <x> => #warning SET PROCLINE is not supported
#command SET PROCNAME <x> => #warning SET PROCNAME is not supported

// Commands

#command ASSERT <exp>     => IF ! (<exp>) ;;
                             System.Windows.Forms.MessageBox.Show( #<exp>, String.Format( "Failed in {0}, {1}", __ENTITY__, __LINE__ ), System.Windows.Forms.MessageBoxButtons.OK, System.Windows.Forms.MessageBoxIcon.Exclamation );;
                             ENDIF
 
#translate pause()        => __pause( __ENTITY__, __LINE__ )

#command DEFAULT <var> TO <value> [, <varN> TO <valueN>] => Default( <var>, <value> ) [; Default( <varN>, <valueN> )]

#command CANCEL              => _quit()
#command QUIT                => _quit()

#command RUN <*cmd*>         => __run( #<cmd> )
#command RUN = <xpr>         => RUN = <xpr>
#command RUN := <xpr>        => RUN := <xpr>

#command ACCEPT [<c>]        => _accept(<c>)
#command ACCEPT [<c>] TO <v> => <v> := _accept(<c>)
#command WAIT [<c>]          => _wait(<c>)
#command WAIT [<c>] TO <v>   => <v> := _wait(<c>)

//#define CRLF e"\r\n"
#define CRLF chr(13)+chr(10)

// eof
