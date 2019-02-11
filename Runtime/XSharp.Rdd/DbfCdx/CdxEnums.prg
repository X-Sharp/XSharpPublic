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
		MEMBER IsUnique			:= 0x01		// Unique
		MEMBER IsWhile   		:= 0x02		// WHILE, ...
		MEMBER IsCustom			:= 0x04		// is a custom built Index
		MEMBER HasFor			:= 0x08		// FOR Clause
		MEMBER BitVector		:= 0x10		// Bit vector (SoftC)
		MEMBER IsCompact		:= 0x20		// Compact index format
		MEMBER IsTag			:= 0x40		// Tag inside CDX (Compounding index header)
		MEMBER IsHeader			:= 0x80		// CDX Header (contains the names of the tags)
			
	END ENUM
	
	
END NAMESPACE 
