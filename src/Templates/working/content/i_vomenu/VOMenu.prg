#ifndef __XSHARP_RT__
#include "VOGUIClasses.vh"
#endif

CLASS VOMenu INHERIT Menu

CONSTRUCTOR( oOwner )

	SELF:PreInit()

	SUPER( ResourceID { "VOMenu" , _GetInst( ) } )

#define IDM_VOMenu_File_ID 15001
	SELF:RegisterItem(IDM_VOMenu_File_ID, ;
		HyperLabel{ #VOMenu_File , "File" , NULL_STRING , NULL_STRING })

	SELF:PostInit()

	RETURN

END CLASS
