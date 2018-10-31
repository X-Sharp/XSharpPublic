//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System.IO
BEGIN NAMESPACE XSharp.RDD
CLASS DbMemo                
	#region Fields
	PROTECTED _BlockSize	AS LONG
	PROTECTED _NewSize		AS LONG
	PROTECTED _Stream		AS FileStream
	PROTECTED _Extension	AS STRING
	PROTECTED _Type			AS BYTE 
	PROTECTED _Version		AS BYTE 
	PROTECTED _oRdd			AS Workarea
	#endregion
	#region Properties
	PROPERTY IsOpen 	AS LOGIC 	GET _Stream != NULL_OBJECT 
	PROPERTY BlockSize 	AS LONG 	GET _BlockSize
	PROPERTY NewSize 	AS LONG 	GET _NewSize
	PROPERTY Extension 	AS STRING 	GET _Extension
	PROPERTY Version 	AS BYTE 	GET _Version
	PROPERTY MemoType	AS BYTE		GET _Type
	PROPERTY Owner		AS Workarea 	GET _oRDD SET _oRDD := VALUE
		
	#endregion
		
	CONSTRUCTOR()  
		RETURN		
		
END CLASS
END NAMESPACE	
