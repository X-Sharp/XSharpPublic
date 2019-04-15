//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp.RDD.CDX
	// Index options represented as the sum of the following values:
    [Flags];
	INTERNAL ENUM CdxOptions AS BYTE
        MEMBER None             := 0x00     // Uninitialized
		MEMBER Unique			:= 0x01		// Unique
		MEMBER While   		    := 0x02		// WHILE, ...
		MEMBER Custom			:= 0x04		// is a custom built Index
		MEMBER HasFor			:= 0x08		// FOR Clause
		MEMBER BitVector		:= 0x10		// Bit vector (SoftC)
		MEMBER Compact		    := 0x20		// Compact index format
		MEMBER Tag			    := 0x40		// Tag inside CDX (Compounding index header)
		MEMBER Header			:= 0x80		// CDX Header (contains the names of the tags)
			
	END ENUM

    [Flags];
    INTERNAL ENUM CdxPageType AS WORD
        MEMBER Branch  := 0
        MEMBER Root    := 1
        MEMBER Leaf    := 2
        MEMBER Undefined := 0xFFFF
    END ENUM

    INTERNAL ENUM CdxActionType
        MEMBER Ok              
        MEMBER AddKey
        MEMBER DeleteKey
        MEMBER InsertKey
        MEMBER AddLeaf
        MEMBER Delete
        MEMBER Balance
        MEMBER InsertParent    
        MEMBER ChangeParent
        MEMBER AddBranch     
        MEMBER DeleteFromParent
        MEMBER ExpandRecnos
        MEMBER OutOfBounds
    END ENUM
	
END NAMESPACE 
