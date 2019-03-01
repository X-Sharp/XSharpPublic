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

    INTERNAL ENUM CdxPageType AS WORD
        MEMBER Branch  := 0
        MEMBER Root    := 1
        MEMBER Leaf    := 2
        MEMBER TagList := 3
        MEMBER None    := 4
        MEMBER Unused  := 0xFF
    END ENUM

    [Flags];
    INTERNAL ENUM CdxResult
        MEMBER Ok              := 0
        MEMBER SplitLeaf       := 1 << 0
        MEMBER AddLeaf         := 1 << 1
        MEMBER Delete          := 1 << 2
        MEMBER InsertParent    := 1 << 3
        MEMBER ChangeParent    := 1 << 4
        MEMBER SplitParent     := 1 << 5
        MEMBER DeleteFromParent:= 1 << 6
        MEMBER ExpandRecnos    := 1 << 7
        MEMBER OutofBounds     := 1 << 8
    END ENUM
	
END NAMESPACE 
