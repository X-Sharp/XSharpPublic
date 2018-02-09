//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

// All Dynamic memory functions that are not supported or dummies
// And also all the OldSpace functions

[ObsoleteAttribute( "'DynMemDump()' is not supported and always returns FALSE" )] ;
FUNCTION DynMemDump(cFile AS STRING,nOptions AS DWORD) AS LOGIC
	RETURN FALSE   

[ObsoleteAttribute( "'DynCheckError()' is not supported and always returns 0" )] ;
FUNCTION DynCheckError() AS DWORD
   RETURN 0
   
[ObsoleteAttribute( "'DynInfoFree()' is not supported", true )] ;
FUNCTION DynInfoFree() AS DWORD
   RETURN 0
   
[ObsoleteAttribute( "'DynInfoMax()' is not supported", true )] ;
FUNCTION DynInfoMax() AS DWORD
   RETURN 0

[ObsoleteAttribute( "'DynInfoSize()' is not supported", true )] ;
FUNCTION DynInfoSize() AS DWORD
   RETURN 0

[ObsoleteAttribute( "'DynInfoUsed()' is not supported", true )] ;
FUNCTION DynInfoUsed() AS DWORD
   RETURN 0
   
[ObsoleteAttribute( "'DynLock()' is not supported and has no effect" )] ;
FUNCTION DynLock() AS VOID
   RETURN
   
[ObsoleteAttribute( "'DynProtect()' is not supported", true )] ;
FUNCTION DynProtect( lSet AS LOGIC ) AS LOGIC
   RETURN FALSE
   
[ObsoleteAttribute( "'DynShrink()' is not supported", true )] ;
FUNCTION DynShrink() AS DWORD
   RETURN 0
   
[ObsoleteAttribute( "'DynSize()' is not supported", true )] ;
FUNCTION DynSize() AS DWORD
   RETURN 0
   
[Obsolete( "DynToOldSpace() is not supported", true )] ; 
FUNCTION DynToOldSpace( u AS __Usual ) AS __Usual
   RETURN __Usual{}

[Obsolete( "DynToOldSpaceArray() is not supported", true )] ; 
FUNCTION DynToOldSpaceArray( a AS __Array ) AS __Array
   RETURN NULL

[Obsolete( "DynToOldSpaceFloat() is not supported (FLOAT is not a reference type in Vulcan.NET)", true )] ; 
FUNCTION DynToOldSpaceFloat( f AS __VoFloat ) AS __VoFloat
   RETURN 0

[Obsolete( "DynToOldSpaceObject() is not supported", true )] ; 
FUNCTION DynToOldSpaceObject( o AS OBJECT ) AS OBJECT
   RETURN NULL

[Obsolete( "DynToOldSpaceString() is not supported (System.Runtime.InteropServices.Marshal.StringToHGlobalAnsi() may be a possible alternative)", true )] ; 
FUNCTION DynToOldSpaceString( s AS STRING ) AS STRING
   RETURN NULL

[ObsoleteAttribute( "'DynUnlock()' is not supported and has no effect" )] ;
FUNCTION DynUnLock() AS VOID
   RETURN


[ObsoleteAttribute( "'IsOldSpaceFloat()' is not supported and always returns FALSE" )] ;
FUNCTION IsOldSpaceFloat(f AS __VOFloat) AS LOGIC
	RETURN FALSE   


[ObsoleteAttribute( "'IsOldSpaceString()' is not supported and always returns FALSE" )] ;
FUNCTION IsOldSpaceString(c AS STRING) AS LOGIC
	RETURN FALSE 


[ObsoleteAttribute( "'OldSpaceFreeFloat()' is not supported", true )] ;
FUNCTION OldSpaceFreeFloat(f AS __VOFloat) AS VOID
	RETURN

[ObsoleteAttribute( "'RegisterKid()' is not supported", true )] ;
FUNCTION RegisterKid(ptrKid AS IntPtr,dwCount AS DWORD,lItem AS LOGIC) AS VOID
	RETURN

[ObsoleteAttribute( "'FreeStaticObject()' is not supported", true )] ;
FUNCTION FreeStaticObject(o AS OBJECT) AS LOGIC
RETURN FALSE   

[ObsoleteAttribute( "'OldSpaceFree()' is not supported and has no effect" )] ;
FUNCTION OldSpaceFree(u AS __Usual) AS VOID
RETURN

[ObsoleteAttribute( "'OldSpaceFreeObject()' is not supported and has no effect" )] ;
FUNCTION OldSpaceFreeObject(o AS OBJECT) AS VOID
RETURN


[ObsoleteAttribute( "'IsOldSpaceArray()' is not supported and always returns FALSE" )] ;
FUNCTION IsOldSpaceArray(a AS __Array) AS LOGIC
	RETURN FALSE   

[ObsoleteAttribute( "'OldSpaceFreeArray()' is not supported and has no effect" )] ;
FUNCTION OldSpaceFreeArray(a AS __Array) AS VOID
	RETURN 

[ObsoleteAttribute( "'IsOldSpace()' is not supported and always returns FALSE" )] ;
FUNCTION IsOldSpace(u AS __Usual) AS LOGIC
	RETURN FALSE   

[ObsoleteAttribute( "'IsOldSpaceObject()' is not supported and always returns FALSE" )] ;
FUNCTION IsOldSpaceObject(o AS OBJECT) AS LOGIC
	RETURN FALSE   

[ObsoleteAttribute( "'MemCheckPtr()' is not supported and always returns TRUE" )] ;
UNSAFE FUNCTION MemCheckPtr( pMemory AS PTR, dwSize AS DWORD ) AS LOGIC
	RETURN TRUE

	
[ObsoleteAttribute( "'SetKidStackSize()' is not supported and always returns 0" )] ;
FUNCTION SetKidStackSize(dwBytes AS DWORD) AS DWORD
RETURN 0   

[ObsoleteAttribute( "'SetMaxDynSize()' is not supported and always returns 0" )] ;
FUNCTION SetMaxDynSize(dwBytes AS DWORD) AS DWORD
RETURN 0   

[ObsoleteAttribute( "'SetMaxRegisteredAxitMethods()' is not supported and always returns 0" )] ;
FUNCTION SetMaxRegisteredAxitMethods(dwCount AS DWORD) AS DWORD
RETURN 0   

[ObsoleteAttribute( "'SetMaxRegisteredKids()' is not supported and always returns 0" )] ;
FUNCTION SetMaxRegisteredKids(dwCount AS DWORD) AS DWORD
RETURN 0   

[ObsoleteAttribute( "'SetMaxThreadDynSize()' is not supported and always returns 0" )] ;
FUNCTION SetMaxThreadDynSize(dwBytes AS DWORD) AS DWORD
RETURN 0   

[ObsoleteAttribute( "'SetWipeDynSpace()' is not supported and always returns FALSE" )] ;
FUNCTION SetWipeDynSpace(lWipe AS LOGIC) AS LOGIC
RETURN FALSE   


[ObsoleteAttribute( "'PtrLen()' is not supported and always returns 0" )] ;
FUNCTION PtrLen( lpv AS IntPtr ) AS DWORD
	RETURN 0
	
[ObsoleteAttribute( "'PtrLenWrite()' is not supported and always returns 0" )] ;
FUNCTION PtrLenWrite( lpv AS IntPtr ) AS DWORD
	RETURN 0