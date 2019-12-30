#ifndef __XSHARP_RT__
#include "VOGUIClasses.vh"
#endif

CLASS $safeitemrootname$ INHERIT Menu

CONSTRUCTOR( oOwner )

	SELF:PreInit()

	SUPER( ResourceID { "$safeitemrootname$" , _GetInst( ) } )

#define IDM_$safeitemrootname$_File_ID 15001
	SELF:RegisterItem(IDM_$safeitemrootname$_File_ID, ;
		HyperLabel{ #$safeitemrootname$_File , "File" , NULL_STRING , NULL_STRING })

	SELF:PostInit()

	RETURN

END CLASS
